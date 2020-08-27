correspondence <- function(obiwarp = NULL, obiwarpVis = NULL,
                           peakGrp = NULL, peakGrpVis = NULL,
                           config.xcms, 
                           peakDensity.minfrac = 0.8, peakDensity.minSamples  = 3) {
        
        if (is.null(obiwarp) & is.null(peakGrp)) {
                stop("At least of of the result from obiwarp or peak grouping should be provided.")
        }
        
        BiocParallel::register(BiocParallel.FutureParam::FutureParam())
        
        obiwarpScr  <- min(sapply(obiwarpVis, lambda(x, x$scr)))
        peakGrpScr  <- min(sapply(peakGrpVis, lambda(x, x$scr)))
        
        if (is.null(obiwarp)) {
                xdataTmAdj <- lapply(peakGrp, lambda(x, x$xdataTmAdj))
        } else if (is.null(peakGrp)) {
                xdataTmAdj <- lapply(obiwarp, lambda(x, x$xdataTmAdj))
        } else {
                if (obiwarpScr > peakGrpScr) {
                        xdataTmAdj <- lapply(obiwarp, lambda(x, x$xdataTmAdj))
                } else {
                        xdataTmAdj <- lapply(peakGrp, lambda(x, x$xdataTmAdj))
                }
        }
        
        xdataAligned <- mapply(FUN = function(xdataTmAdj, config.xcms, 
                                              minFraction, minSamples) {
                sampleGroup <- factor(config.xcms$MappingTb$sampleGroup)
                pdPar <- PeakDensityParam(
                                bw = ifelse(test = is.na(config.xcms$bw), 
                                        yes  = 20, 
                                        no   = config.xcms$bw),
                                minFraction  = minFraction,
                                minSamples   = min(min(table(sampleGroup)), minSamples),
                                sampleGroups = sampleGroup)
                groupChromPeaks(xdataTmAdj, pdPar)
        }, xdataTmAdj  = xdataTmAdj,
        config.xcms = config.xcms,
        minFraction = peakDensity.minfrac, 
        minSamples  = peakDensity.minSamples)
        
        return(xdataAligned)
}