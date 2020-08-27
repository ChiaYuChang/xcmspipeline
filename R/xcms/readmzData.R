readmzData <- function(config.phase) {
        mzData <- str_c(config.phase$Path,
                        config.phase$MappingTb$fileName,
                        sep = "/") %.>%
                readMSData(files = .,
                           pdata = new("NAnnotatedDataFrame", 
                                       as.data.frame(config.phase$MappingTb)),
                           mode  = "onDisk", 
                           msLevel. = 1)
        return(mzData)
}