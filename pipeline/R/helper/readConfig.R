readConfig <- function(configPath, DEFAULT) {
        message("Checking format")

        if (str_detect(configPath, ".xlsx$")) {
                message("Start reading config from file.")
                config <- import(file = configPath, 
                                 which = "Parameters", 
                                 range = readxl::cell_cols("A:C"), 
                                 setclass = "data.table")
                config[, parType := na_locf(parType)]

                message(" -  Checking missing parameters.")
                config.phase <- config[parName == "phase"]
                config       <- config[parName != "phase"]
                
                # fill in default parameters
                config <- merge(
                                x = config, 
                                y = DEFAULT$base[parName != "phase"], 
                                by = c("parType", "parName"), 
                                all = TRUE, 
                                suffix = c("", ".Defaule")
                        )

                if (nrow(config[is.na(parValue)]) > 0) {
                        msg <- config[is.na(parValue), str_c(parName, collapse = ", ")] %.>%
                                sprintf("Parameters (%s) are missing, use default values.", .)
                        message(msg)
                        config <- unique(config[is.na(parValue), parValue := parValue.Defaule])
                } else {
                        message("No parameter is missing.")
                }
                config <- unique(config[, parValue.Defaule := NULL])

                config.phase[, parMode := "character"]
                config.phase[, parMode := as.character(parMode)]
                config <- rbind(config, config.phase)
                
                message(" -  Checking missing phases info.")
                phases.dir    <- basename(list.dirs("./mzXML")[-1])
                phases.sheets <- config[parName == "phase", parValue]
                phases.config <- lapply(
                        phases.sheets,
                        FUN = function(sheet.name, config.path) {
                                tryCatch({
                                        phase.configs <- import(
                                                sheet.name,
                                                file = config.path,
                                                format = "xlsx",
                                                range = readxl::cell_cols("A:C"),
                                                setclass = "data.table"
                                        )
                                }, error = function(err){
                                        cat(sprintf("Sheet %s not found", sheet.name))
                                        phase.configs <- data.table(
                                                parType  = vector("character", 0),
                                                parSet   = vector("character", 0),
                                                parValue = vector("character", 0)
                                        )
                                })
                                return(phase.configs)
                        },
                        config.path = configPath
                )
                names(phases.config) <- phases.sheets
                
                phase.mzXMLPath <- vapply(
                        X = phases.config, 
                        FUN = lambda(x, x[parName == "mzXMLDir", basename(parValue)]), 
                        FUN.VALUE = character(1))

                if (length(setdiff(phase.mzXMLPath, phases.dir)) > 0) {
                        stop(sprintf("%s\n\t > Folder %s are found in ./mzXML", 
                                msg,
                                str_c(
                                        phases, 
                                        collapse = ", ")
                                )
                        )
                }

                config <- split(config, by = c("parType"), sorted = TRUE)
                config <- lapply(config, split, by = "parName", sorted = TRUE)
                config$PATH <- dirname(configPath)

                class(config) <- c("metPipelineConfigs", "list")

                message(" -  Parsing column mapping information.")
                config$ColumnMapping <- lapply(config$ColumnMapping, function(x) {
                        get(str_c("as_", x$parMode))(x$parValue)
                })

                config$SheetName <- lapply(config$SheetName, function(x) {
                        get(str_c("as_", x$parMode))(x$parValue)
                })

                config$ReservedWords <- lapply(config$ReservedWords, function(x) {
                        get(str_c("as_", x$parMode))(x$parValue)
                })

                message(" -  Parsing phase specific parameters.")
                for (phase in config$SheetName$phase) {
                        config$Phases[[phase]] <- phases.config[[phase]]
                        default.phase.pars     <- phases.config[[phase]][parName == "phase", parValue]
                        
                        if (str_to_upper(default.phase.pars) %chin% c("NEG", "NEGATIVE", "N")) {
                                DEFAULT.phase <- DEFAULT$phase[["NEG"]]
                        } else if (str_to_upper(default.phase.pars) %chin% c("POS", "POSITIVE", "P")) {
                                DEFAULT.phase <- DEFAULT$phase[["POS"]]
                        } else {
                                warning(sprintf("Cannot find default phase specific parameters for phase: %s.", phase))
                        }

                        if (nrow(DEFAULT.phase) > 0) {
                                config$Phases[[phase]] <- merge(
                                        config$Phases[[phase]], 
                                        DEFAULT.phase, by = c("parType", "parName"), 
                                        all = T, 
                                        suffix = c("", ".Defaule")
                                )
                                missingPar <- config$Phases[[phase]][is.na(parValue), parName]
                                if (length(missingPar) > 0) {
                                        msg <- str_c(missingPar, collapse = ", ") %.>% 
                                                str_trunc(., width = 80) %.>%
                                                sprintf("%s are missing, use default value.", .)
                                        message(msg)
                                }
                                config$Phases[[phase]][is.na(parValue), parValue := parValue.Defaule]
                        } else {
                                missingPar <- setdiff(
                                        unique(DEFAULT$phase$POS$parName), 
                                        config$Phases[[phase]]$parName
                                )
                                if (length(missingPar) > 0) {
                                        msg <- str_c(missingPar, collapse = ", ") %.>% 
                                                str_trunc(., width = 80) %.>%
                                                sprintf("%s are missing, and no use default value could be use, please check your input.", .)
                                        stop(msg)
                                }
                        }
                        
                        config$Phases[[phase]] <- unique(config$Phases[[phase]][, -"parValue.Defaule"])
                        config$Phases[[phase]] <- split(config$Phases[[phase]], by = "parType") 
                        
                        config$Phases[[phase]]$Phase <- config$Phases[[phase]]$Phase$parValue
                        if (length(config$Phases[[phase]]$Phase) > 1) {
                                phss <- str_c(config$Phases[[phase]]$Phase, collapse = ", ")
                                stop(sprintf(
                                        "Phase %s has more than one phase: %s\nPlease check your config file.", 
                                        phase, 
                                        phss
                                ))
                        }

                        rsrvwd <- config$Phases[[phase]]$ReservedWords[, .(parName, parValue)]
                        rsrvwd <- setNames(as.list(rsrvwd$parValue), rsrvwd$parName)
                        config$Phases[[phase]]$ReservedWords <- rsrvwd

                        if (is.null(config$Phases[[phase]]$ReservedWords$QC)) {
                                config$Phases[[phase]]$ReservedWords$QC <- config$ReservedWords$QC
                        }

                        AutoTuner <- list()
                        attnr <- config$Phases[[phase]]$AutoTuner[parName == "autotune", str_trim(parValue)]
                        switch(attnr,
                               "0" = FALSE,
                               "1" = TRUE,
                               attnr)
                        AutoTuner$AutoTuner <- as_logical(attnr)
                        AutoTuner$Pars      <- config$Phases[[phase]]$AutoTuner[parName != "autotune", c("parName", "parValue")]
                        AutoTuner$Pars[parName == "parSet", parName := str_c(parName, 1 : .N, sep = "_")]
                        AutoTuner$Pars[, c("lag", "threshold", "influence") := tstrsplit(parValue, ",")]
                        AutoTuner$Pars[, parValue := NULL]
                        AutoTuner$Pars[, c("lag", "threshold", "influence") := lapply(.SD, as.numeric), .SDcols = c("lag", "threshold", "influence")] 
                        config$Phases[[phase]]$AutoTuner <- AutoTuner
                        config$Phases[[phase]]$Path      <- config$Phases[[phase]]$Path$parValue
                        config$Phases[[phase]]$Files     <- list.files(path = config$Phases[[phase]]$Path, pattern = c("mzXML", "mzData"))

                        if(length(config$Phases[[phase]]$Files) < 1) {
                                stop(
                                        sprintf("No mzXML/mzData file was found in %s", 
                                                config$Phases[[phase]]$Path)
                                )
                        }

                        config$Phases[[phase]]$XCMS[str_to_lower(parValue) == "auto", parValue := "-1"]
                        config$Phases[[phase]]$XCMS[, parValue := as_numeric(parValue)]
                        XCMS        <- as.list(config$Phases[[phase]]$XCMS$parValue)
                        names(XCMS) <- config$Phases[[phase]]$XCMS$parName
                        config$Phases[[phase]]$XCMS <- XCMS        
                }

                message(" -  Reading mapping table.")
                mappingTb <- import(file = configPath,
                                    which = config$SheetName$mappingTable,
                                    setclass = "data.table")

                if (is.null(mappingTb[[config$ColumnMapping$injectionOrder]])) {
                        warning("Injection order is missing. Set the order of sample as the injection order.")
                        mappingTb[, injectionOrder := 1:length(sampleName), by = sampleMode]
                        # set(mappingTb, j = config$ColumnMapping$injectionOrder, value = 1:nrow(mappingTb))
                }
                
                setnames(x  = mappingTb,
                        old = do.call(c, config$ColumnMapping),
                        new = names(config$ColumnMapping)
                )

                mappingTb <- split(
                        unique(mappingTb),
                        by = "sampleMode"
                )
                
                for (phase in config$SheetName$phase) {
                        cat(phase, "\n")
                        config$Phases[[phase]]$MappingTb <- mappingTb[[phase]]

                        set(x = config$Phases[[phase]]$MappingTb,
                            i = which(config$Phases[[phase]]$MappingTb[["sampleGroup"]] == config$ReservedWords),
                            j = "sampleGroup", 
                            value = "QC")

                        missingFilesIdx <- which(!(
                                        config$Phases[[phase]]$MappingTb[["fileName"]] %chin% 
                                        config$Phases[[phase]]$Files)
                        )

                        if (length(missingFilesIdx) > 0) {
                                missingFiles <- str_c(
                                        .subset2(config$Phases[[phase]]$MappingTb[missingFilesIdx], 
                                                 "fileName"
                                        ),
                                        collapse = ", ")
                                
                                missingSmpNm <- str_c(
                                        .subset2(
                                                config$Phases[[phase]]$MappingTb[missingFilesIdx],
                                                "sampleName"
                                        ),
                                        collapse = ", ")
                                
                                warning(
                                        sprintf("Cannot find %s in directory %s\n - Remove %s from the mapping table.",
                                                missingFiles, 
                                                config$Phases[[phase]]$Path,
                                                missingSmpNm
                                        )
                                )

                                config$Phases[[phase]]$MappingTb <- config$Phases[[phase]]$MappingTb[!missingFilesIdx]
                        }
                        config$Phases[[phase]]$Files  <- NULL

                        if (!config$Phases[[phase]]$AutoTuner$AutoTuner) {
                                XCMS <- sapply(config$Phases[[phase]]$XCMS, lambda(x, x < 0))
                                if (any(XCMS)) {
                                        stop(sprintf("For phase %s, %s are missing. (AutoTuner = FALSE)", 
                                                     str_to_upper(phase), 
                                                     str_c(names(XCMS)[!XCMS], collapse = ", ")))
                                }
                        }

                        class(config$Phases[[phase]]) <- c("metPipelinePhaseConfigs", "list")
                }

                message(" -  Reading factor table.")
                if (config$SheetName$factorTable %in% readxl::excel_sheets(configPath)) {
                        config$FactorTable <- import(configPath, which = config$SheetName$factorTable, setclass = "data.table")
                        setnames(config$FactorTable, new = "sampleName", old = config$ColumnMapping$sampleName)
                } else {
                        message("No factor table was found, return an empty list.")
                        config$FactorTable <- list()
                }


        } else if (str_detect(configPath, ".json$")) {
                #! DO SOMETHING
                stop("Config file format error (should be .xlsx)")
        } else {
                #! DO SOMETHING
                stop("Config file format error (should be .xlsx)")
                # stop("Config file format error (should be .json or .xlsx)")
        }
        
        message(print(config))
        return(config)
}

print.metPipelinePhaseConfigs <- function(obj) {
        if (obj$AutoTuner$AutoTuner) {
                cat("Use the AutoTuner package (McLean et al, 2020) to select xcms parameters.\n")
                cat(sprintf("The mzXML/mzData files are stored in %s.\n", obj$Path))
                
                if (nrow(obj$AutoTuner$Pars) == 1) {
                        cat("There is one autotuner parameter set.\n")
                } else {
                        cat(str_c(obj$AutoTuner$Pars[, sprintf("\t- %s (lag = %4.1f, threshold = %3.1f, influence = %3.1f)", parName, lag, threshold, influence)], collapse = "\n"), "\n")
                }
                
                
        } else {
                cat("Skip xcms parameter auto-selection process, and use self-provided xcms parameters.\n")
        }
        
        qc.str <- obj$ReservedWords$QC
        smpNum <- obj$MappingTb[, .N, by = sampleGroup]
        smpNum[, is.qc := ifelse(sampleGroup == qc.str, "(*)", " ")]
        cat(sprintf("There are %d samples and %d QCs (%s).\n", 
                smpNum[sampleGroup != qc.str, sum(N)], 
                smpNum[sampleGroup == qc.str, N],
                qc.str))
        cat(str_c(smpNum[, 
                sprintf("\t-%3d samples in group %s%s", N, sampleGroup, is.qc)], 
                collapse = "\n"), "\n"
        )
}

print.metPipelineConfigs <- function(obj) {
        if (!all(c("SheetName", "Phases", "FactorTable") %chin% names(obj))) {
                stop("The object is not a metPipelineConfigs.")
        }
        
        cat(sprintf("%s\n", str_c(rep("=", 80), collapse = "")))
        if (length(obj$SheetName$phase) > 1) {
                cat(sprintf("The analysis includes %d phases:\n\t- %s.\n", length(obj$SheetName$phase), str_c(obj$SheetName$phase, collapse = ", ")))
        } else {
                cat(sprintf("The analysis only includes phase %s.\n", str_c(obj$SheetName$phase)))
        }
        
        cat(sprintf("%s\n", str_c(rep("=", 80), collapse = "")))
        cat("Phase specific parameters\n")
        
        for (p in names(obj$Phases)) {
                cat(sprintf("%s\n", str_c(rep("- ", 40), collapse = "")))
                cat(sprintf("For phase %s\n", p))
                print(obj$Phases[[p]])
        }
        cat(sprintf("%s\n", str_c(rep("=", 80), collapse = "")))
        
        # cat("Factors map\n")
        # obj$FactorTable[, lapply(.SD, lambda(x, length(unique(x)))), .SDcols = -"sampleName"] %.>%
        #         as.list(.) %.>%
        #         sprintf("\t> Factor %s with %d levels", names(.), as_vector(.)) %.>%
        #         str_c(., collapse = "\n") %.>%
        #         cat(., "\n")
        # cat(sprintf("%s\n", str_c(rep("=", 80), collapse = "")))
}
