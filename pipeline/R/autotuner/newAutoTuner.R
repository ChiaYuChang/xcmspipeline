#' Function for constructing AutoTuner S4 object
#' @param config.phase metPipelinePhaseConfigs object
#' @param sampleSize how many samples in each group should be used for XCMS parameters 
#' estimation. If sampleSize = "all", all samples will be used.
#' @return AutoTuner S4 object
newAutoTuner <- function(config.phase, sampleSize = "all", mzRange = c(50, 900), is.drake = FALSE) {
        if (is.drake) {
                phase <- names(config.phase)
                config.phase <- config.phase[[1]]
                if (!is.null(config.phase$ReservedWords$QC)) {
                        qc.label <- config.phase$ReservedWords$QC
                } else {
                        qc.label <- "QC"
                }
        }
        
        if (config.phase$AutoTuner$AutoTuner) {
                if (sampleSize == "all") {
                        dt <- config.phase$MappingTb
                } else {
                        # select samples for xcms parameters estimation
                        sampleSize <- min(
                                sampleSize, 
                                config.phase$MappingTb[
                                        sampleGroup != qc.label, 
                                        .N, 
                                        by = sampleGroup][, N]
                        )
                        
                        # select random sample
                        selectedSmpl <- config.phase$MappingTb[
                                  sampleGroup != qc.label,
                                .(sampleName = sample(
                                        x = sampleName, 
                                        size = sampleSize)
                                ),
                                sampleGroup][, sampleName]
                        
                        # select all QC
                        QC <- config.phase$MappingTb[
                                sampleGroup == qc.label,
                                sampleName]
                        
                        if (length(QC) < 1) {
                                warning("There is no QC samples.")
                        } else {
                                message("Phase: ", phase)
                                message(sprintf(
                                        "Select %d samples and %d QCs.",
                                        length(selectedSmpl),
                                        length(QC)
                                ))
                        }
                        message("Selected samples: ", selectedSmpl)

                        selectedSmpl <- c(QC, selectedSmpl)
                        
                        dt <- config.phase$MappingTb[sampleName %chin% selectedSmpl]
                }
                
                # try to creat an autotuner object
                tryCatch({
                        autotuner <- suppressMessages(createAutotuner(
                                data_paths = str_c(config.phase$Path, dt$fileName, sep = "/"),
                                runfile    = data.frame(dt), 
                                file_col   = "fileName",
                                # mzRange = c(50, 900),
                                factorCol  = "sampleGroup"))
                }, error = function(err) {
                        message("error in constructing autotuner for phase", phase)
                        message(err)
                        stop("unknown error")
                })
                
                if (is.drake) {
                        autotuner <- list(autotuner)
                        names(autotuner) <- phase
                }
                
        } else {
                autotuner <- NULL
        }
        return(autotuner)
}
# showMethods(class = "Autotuner")
# setMethod(f = "initialize", signature = c(.Object = "Autotuner", mzRange = "numeric"), 
#         function(.Object, data_paths, runfile, file_col, mzRange = c(50, 900), factorCol) {

#                 message("~~~ Autotuner: Initializator ~~~ \n")
#                 message("~~~ Parsing Raw Data into R ~~~ \n")


#                 raw <- suppressMessages(MSnbase::readMSData(data_paths,
#                                                             msLevel. = 1,
#                                                             mode = "onDisk"))
#                 raw <- filterMz(mzData, mz = mzRange, msLevel. = 1)

#                 # determining time and intensity data for each sample
#                 time <- list()
#                 intensity <- list()
#                 message(paste("~~~ Extracting the Raw Data from Individual",
#                             "Samples ~~~ \n"))
#                 for(index in seq_len(nrow(runfile))) {

#                     signal_data <- MSnbase::filterFile(raw, file = index)
#                     time[[index]] <- MSnbase::rtime(signal_data)
#                     intensity[[index]] <- MSnbase::tic(signal_data)

#                     ## 2019-07-21 - Adding check for chromatograph data
#                     if(sum(intensity[[index]]) == 0) {

#                         if(index == 1) {
#                             message("No TIC data was found.")
#                             message("Using integrated intensity data instead.")
#                         }

#                         allInts <- MSnbase::intensity(signal_data)
#                         storeInt <- list()
#                         for(i in seq_along(allInts)) {
#                             storeInt[[i]] <- sum(unlist(allInts[i]))
#                         }
#                         intensity[[index]] <- unlist(storeInt)
#                         rm(allInts, storeInt)
#                     }

#                 }

#                 message("~~~ Storing Everything in Autotuner Object ~~~ \n")
#                 .Object <- setAutoTime(time = time, Autotuner = .Object)
#                 .Object <- setAutoIntensity(intensity = intensity,
#                                             Autotuner = .Object)
#                 .Object <- setAutoMetadata(metadata = runfile,
#                                                     Autotuner = .Object)
#                 .Object <- setAutoFile_paths(file_paths = data_paths,
#                                                         Autotuner = .Object)
#                 .Object <- setAutoFile_col(file_col = file_col,
#                                                     Autotuner = .Object)
#                 .Object <- setAutoFactorCol(factorCol = factorCol,
#                                             Autotuner = .Object)

#                 message("~~~ The Autotuner Object has been Created ~~~ \n")
#                 return(.Object)

# })
