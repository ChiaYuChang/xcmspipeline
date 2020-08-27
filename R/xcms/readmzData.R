readmzData <- function(config.phase) {
        mzFileName <- str_c(config.phase$Path,
                            config.phase$MappingTb$fileName,
                            sep = "/")
        mzData <- readMSData(files = mzFileName,
                             pdata = new("NAnnotatedDataFrame", 
                                     as.data.frame(config.phase$MappingTb)),
                             mode  = "onDisk", 
                             msLevel. = 1)
        return(mzData)
}