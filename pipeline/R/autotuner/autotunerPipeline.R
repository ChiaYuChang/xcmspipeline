#' Estamate xcms parameters based on sets of autotuner parameters
#' @param autotuner a autotuner object or a autotuner object within a named list
#' @param config the configuration for the pipeline
#' @param phase the name of the phase, when autotuner object is in a named list, 
#' the name of the list will be use as the phase name
#' @param returned_peaks the number of peaks for estamating XCMS parameters, see 
#' desrciptions in Autotuners package
#' @massThresh see desrciptions in Autotuners package
#' @return a data.tables that contains the estimation results
autoTunerPipeLine <- function(autoTunersParSet, returned_peaks = 20,
        massThresh = 0.005, is.drake = FALSE, verbose = TRUE) {
        # extract the name of the phase
        if (is.drake) {
                autoTunersParSet <- autoTunersParSet[[1]]
                phase <- autoTunersParSet$phase
        }

        if (autoTunersParSet$est) {
                # 
                # select phase specific autotuner config
                if (verbose) message("Start estimate xcms parameters ...")

                # Estimating XCMS parameters
                pars <- .autoTunerPipeLine(
                        lag            = autoTunersParSet$lag, 
                        threshold      = autoTunersParSet$threshold,
                        influence      = autoTunersParSet$influence,
                        autotuner      = autoTunersParSet$AutoTuners[[1]],
                        phase          = autoTunersParSet$phase,
                        returned_peaks = returned_peaks,
                        massThresh     = massThresh,
                        verbose        = verbose
                )
                     
                if (pars$state == 0) {
                        # merging EIC parameters estimating result
                        eicParams <- as.data.table(pars$estimates$eicParams[, c("Parameters", "estimates")])
                        setnames(eicParams, c("parameters", "value")) 
                        eicParams[, parGrp := "EIC"]
                        
                        # merging TIC parameters estimating result
                       
                        ticParams <- data.table(pars$estimates$ticParams[, c("descriptions", "estimates")])
                        setnames(ticParams, c("parameters", "value"))
                        ticParams[, parGrp := "TIC"]
                        
                        pars  <- as.data.table(rbind(eicParams, ticParams))
                        pars[, phase   := autoTunersParSet$phase]
                        pars[, parName := autoTunersParSet$parName]
                        
                        setcolorder(pars, c("parName", "phase", "parGrp", "parameters", "value"))
                        setorder(pars, parGrp, parameters)
                        pars <- list(
                                estPars = pars,
                                flag = 0,
                                parName = autoTunersParSet$parName,
                                phase = autoTunersParSet$phase
                        ) 
                } else {
                        warning("For phase", 
                                phase, 
                                ", Autotuner could no estimate the parameters, please chech given parameters (lag, threshold, and influence)\nCheck https://github.com/crmclean/Autotuner/ for help.")
                        pars <- data.table(
                                parName = autoTunersParSet$parName,
                                phase = autoTunersParSet$phase,
                                parGrp = rep(c("EIC", "TIC"), c(7, 3)),
                                parameters = c("Max Peakwidth", "Min Peakwidth", "noise", "ppm", "preIntensity", "preScan", 
                                               "snThresh", "group_diff", "max_width", "min_width"),
                                value = NA_real_
                        )
                        pars <- list(
                                estPars = pars,
                                flag = 1,
                                parName = autoTunersParSet$parName,
                                phase = autoTunersParSet$phase
                        )
                }
        } else {
                pars  <- list(
                                estPars = data.table(
                                        parName = character(0), 
                                        phase = character(0), 
                                        parGrp = character(0), 
                                        parameters = character(0),
                                        value = numeric(0)
                                ),
                                flag = 0,
                                parName = autoTunersParSet$parName,
                                phase = autoTunersParSet$phase
                        )
        }
        if (is.drake) {
                pars <- list(pars)
        }
        
        names(pars) <- phase
        return(pars)
}

.autoTunerPipeLine <- function(lag, threshold, influence, autotuner, phase,
                               returned_peaks = 20, massThresh = 0.005, verbose = TRUE) {
        if (verbose) {
                message(sprintf("Lag: %05.2f Threshold: %05.2f Influence: %05.2f",
                        lag, threshold, influence
                ))
        }
        
        tryCatch({
                signals <- lapply(getAutoIntensity(autotuner), 
                          ThresholdingAlgo, 
                          lag = lag, 
                          threshold = threshold, 
                          influence = influence)
        
                sltPks <- isolatePeaks(Autotuner = autotuner,
                                returned_peaks = returned_peaks,
                                signals = signals)
        
                peakNum <- max(sapply(sltPks@peaks, lambda(x, ncol(x))))
        
                # cat("Start estimating..\n")
                eicParamEsts <- suppressMessages(
                        EICparams(
                                Autotuner = sltPks, 
                                massThresh = massThresh, 
                                verbose = FALSE,
                                returnPpmPlots = FALSE,
                                useGap = TRUE
                        )
                )
                
                parsEst <- list(
                        phase     = phase,
                        peakNum   = peakNum,
                        estimates = returnParams(eicParamEsts, autotuner),
                        state     = 0,
                        err.msg   = ""
                )
                # cat("Finish estimation.\n")
                return(parsEst)
        }, error = function(err) {
                msg <- sprintf("Encounter error when Lag: %f, Threshold: %f, influence: %f", 
                                lag, threshold, influence)
                warning(msg)
                warning("Error message from xcms parameters estimation: \n\t", err)
                return(list(
                        phase     = phase,
                        peakNum   = 0,
                        estimates = NULL,
                        state     = 1,
                        err.msg   = err
                ))
                # parsEst <- list()
                # parsEst$phase     <- NULL
                # parsEst$peakNum   <- NULL
                # parsEst$estimates <- NULL
        })
}

