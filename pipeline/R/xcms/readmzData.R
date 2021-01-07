readmzData <- function(config.phase, is.drake = FALSE) {
        BiocParallel::register(BiocParallel.FutureParam::FutureParam())
        
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
        if (is.drake) {
               mzData <- list(mzData)
               names(mzData) <- phase 
        }

        return(mzData)
}