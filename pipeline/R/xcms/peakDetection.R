peakDetection <- function(config.xcms, mzData, fitgauss = TRUE, 
        integrate = 1L, mzdiff = 0.01, BPPARAM = bpparam()) {
        
        # strange but workable
        # Set BiocParallel backend to future
        # register(BiocParallel.FutureParam::FutureParam())
        
        # Centwave paramaters setting
        phase <- names(config.xcms)
        config.xcms <- config.xcms[[1]]
        mzData <- mzData[[1]]

        message("     - Phase:", phase)
        tryCatch({
                cwPar <- CentWaveParam(
                        ppm = config.xcms$ppm,
                        peakwidth = c(config.xcms$peakWidth_lb, 
                                      config.xcms$peakWidth_ub),
                        snthresh  = config.xcms$snthresh,
                        prefilter = c(config.xcms$prefilterScan,
                                      config.xcms$prefilterInt),
                        fitgauss = fitgauss,
                        verboseColumns = TRUE,
                        mzCenterFun = "wMean",
                        integrate = integrate,
                        mzdiff = mzdiff)
        
                # chromatographic peak  detection
                xdata <- findChromPeaks(mzData, param = cwPar, BPPARAM = BPPARAM)
                
                # extract chromatographic peak 
                chromPeaks <- as.data.table(chromPeaks(xdata), keep.rownames = "peakId")
                
                pDeteced <- list(list(
                        algorithm = "centwave",
                        cwPar = cwPar,
                        xdata = xdata,
                        chromPeaks = chromPeaks,
                        err.msg = "",
                        state = 0
                ))
                
        }, error = function(err) {
                warning("     x Peak detection error in phase", phase)
                pDeteced <- list(list(
                        algorithm = "centwave",
                        cwPar = NULL,
                        xdata = NULL,
                        chromPeaks = NULL,
                        err.msg = err,
                        state = 1
                ))
        })

        names(pDeteced) <- phase
        return(pDeteced)
}

