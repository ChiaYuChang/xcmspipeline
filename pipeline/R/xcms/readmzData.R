readmzData <- function(config.phase, mzRange = c(50, 900), cntrd = TRUE, smth = TRUE,
        smth.method = "SavitzkyGolay", smth.halfWindowSize = 4L, BPPARAM, is.drake = FALSE) {
        BiocParallel::register(BPPARAM)
        
        if (is.drake) {
                phase <- names(config.phase)
                config.phase <- config.phase[[1]]
        }

        mzFileName <- str_c(config.phase$Path,
                            config.phase$MappingTb$fileName,
                            sep = "/")
        mzData <- readMSData(files = mzFileName,
                             pdata = new("NAnnotatedDataFrame", 
                                     as.data.frame(config.phase$MappingTb)),
                             mode  = "onDisk",
                             verbose = FALSE,
                             msLevel. = 1)
        mzData <- filterMz(mzData, mz = mzRange, msLevel. = 1)

        if (smth) {
                mzData <- ProtGenerics::smooth(
                        x = mzData, 
                        method = smth.method,
                        halfWindow = smth.halfWindowSize,
                        verbose = FALSE
                )
        }

        if (cntrd) {
                mzData <- pickPeaks(mzData)
        }

        if (is.drake) {
               mzData <- list(mzData)
               names(mzData) <- phase 
        }

        return(mzData)
}