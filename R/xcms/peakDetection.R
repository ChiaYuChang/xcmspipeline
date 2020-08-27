peakDetection <- function(config.xcms, mzData, fitgauss = T, integrate = 1L, mzdiff = 0.01) {
        
        # strange but workable
        # Set BiocParallel backend to future
        register(BiocParallel.FutureParam::FutureParam())
        
        # Centwave paramaters setting
        phase <- names(config.xcms)
        config.xcms <- config.xcms[[1]]
        mzData <- mzData[[1]]
        
        cwPar <- CentWaveParam(ppm = config.xcms$ppm,
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
        xdata <- findChromPeaks(mzData, param = cwPar)
        
        # extract chromatographic peak 
        chromPeaks <- as.data.table(chromPeaks(xdata), keep.rownames = "peakId")
        
        pDeteced <- list(
                algorithm = "centwave",
                cwPar = cwPar,
                xdata = xdata,
                chromPeaks = chromPeaks        
        )
        pDeteced <- list(pDeteced)
        names(pDeteced) <- phase
        
        return(pDeteced)
}

