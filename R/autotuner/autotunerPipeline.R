#' Estamate xcms parameters based on sets of autotuner parameters
#' @param autotuner a autotuner object or a autotuner object within a named list
#' @param config the configuration for the pipeline
#' @param phase the name of the phase, when autotuner object is in a named list, 
#' the name of the list will be use as the phase name
#' @param returned_peaks the number of peaks for estamating XCMS parameters, see 
#' desrciptions in Autotuners package
#' @massThresh see desrciptions in Autotuners package
#' @return a data.tables that contains the estimation results
autoTunerPipeLine <- function(autotuner, config, phase = NULL,
                              returned_peaks = 20, massThresh = 0.005) {
        # extract the name of the phase
        if (is.list(autotuner)) {
                phase      <- names(autotuner)
                autotuner  <- autotuner[[phase]]
                if (!class(autotuner) == "Autotuner") stop("The object should be a Autotuner.")
        } else {
                if (phase) stop("If autotuner object is not stored in a list, the name of the phase should be provided.")
        }
        
        if (config$Phases[[phase]]$AutoTuner$AutoTuner) {
                # select phase specific autotuner config
                config.phase <- config$Phases[[phase]]
                
                message("Start estimate xcms parameters ...")
                
                # Estimating XCMS parameters
                pars <- future_lapply(
                        X              = split(config.phase$AutoTuner$Pars, by = "parName"), 
                        FUN            = .autoTunerPipeLine, 
                        autotuner      = autotuner, 
                        returned_peaks = returned_peaks, 
                        massThresh     = massThresh, 
                        phase          = phase)
                
                pars <- pars[lengths(pars) == 3]
                # return(pars)
                # merging EIC parameters estimating result
                eicParams <- data.table(
                        parameters = pars[[1]]$estimates$eicParams$Parameters,
                        do.call(cbind, lapply(pars, lambda(x, x$estimates$eicParams$estimates)))
                )
                eicParams <- melt(eicParams, id.vars = "parameters", variable.name = "parSet")
                eicParams[, parGrp := "EIC"]
                
                # merging TIC parameters estimating result
                ticParams <- data.table(
                        parameters = pars[[1]]$estimates$ticParams$descriptions,
                        do.call(cbind, lapply(pars, lambda(x, x$estimates$ticParams$estimates)))
                )
                ticParams <- melt(ticParams, id.vars = "parameters", variable.name = "parSet")
                ticParams[, parGrp := "TIC"]
                
                pars  <- as.data.table(rbind(eicParams, ticParams))
                setcolorder(pars, c("parGrp", "parameters", "parSet", "value"))
                setorder(pars, parGrp, parameters, parSet)
                pars  <- list(pars) 
        } else {
                pars  <- list(NA) 
        }
        names(pars) <- phase
        return(pars)
}

.autoTunerPipeLine <- function(parsAT, autotuner, phase,
                               returned_peaks = 20, massThresh = 0.005) {
        cat("Lag: ", parsAT$lag,
            "Threshold: ", parsAT$threshold,
            "influence: ", parsAT$influence,
            "\n")
        
        signals <- lapply(getAutoIntensity(autotuner), 
                          ThresholdingAlgo, 
                          lag = parsAT$lag, 
                          threshold = parsAT$threshold, 
                          influence = parsAT$influence)
        
        sltPks <- isolatePeaks(Autotuner = autotuner,
                               returned_peaks = returned_peaks,
                               signals = signals)
        
        peakNum <- max(sapply(sltPks@peaks, lambda(x, ncol(x))))
        
        
        tryCatch({
                cat("Start estimating..\n")
                eicParamEsts <- suppressMessages(EICparams(Autotuner = sltPks, 
                                                           massThresh = massThresh, 
                                                           verbose = FALSE,
                                                           returnPpmPlots = FALSE,
                                                           useGap = TRUE))
                parsEst <- list()
                parsEst$phase     <- phase
                parsEst$peakNum   <- peakNum
                parsEst$estimates <- returnParams(eicParamEsts, autotuner)
                cat("Finish estimation.\n")
                return(parsEst)
        }, error = function(e) {
                msg <- sprintf("Encounter error when Lag: %f, Threshold: %f, influence: %f", 
                               parsAT$lag, parsAT$threshold, parsAT$influence)
                warning(msg)
                warning("Error message from xcms parameters estimation: \n\t", e)
                return(list())
                # parsEst <- list()
                # parsEst$phase     <- NULL
                # parsEst$peakNum   <- NULL
                # parsEst$estimates <- NULL
        })
}

