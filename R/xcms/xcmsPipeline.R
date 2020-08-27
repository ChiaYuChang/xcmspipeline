xcmsPipeLine <- function(mzData, config.xcms, config.phase) {
        register(BiocParallel.FutureParam::FutureParam())
        
        message("Detecting Peaks...")
        pDeteced = mapply(FUN = peakDetection, config.xcms, mzData, SIMPLIFY = F)
        
        message("Aligning Signals ...")
        pAligned = mapply(FUN = signalAlignment, pDeteced, config.phase, config.xcms, SIMPLIFY = F)
        return(pAligned)
}
