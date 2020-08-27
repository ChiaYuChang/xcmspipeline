fillAndExtractFTb <- function(pCorrspd, fill = TRUE) {
        features <- lapply(pCorrspd, function(xdata, fill) {
                if (fill) {
                        message("Filling NA")
                        xdata <- fillChromPeaks(xdata)
                }
                
                fDef  <- as.data.table(x = featureDefinitions(xdata),
                                       keep.rownames = "featureID")
                fTb   <- data.table(featureValues(xdata, 
                                                      value = "into"),
                                    keep.rownames = "featureID")
                fSmry <- data.table(featureSummary(xdata, 
                                                       group = xdata@phenoData@data$sampleGroup),
                                    keep.rownames = "featureID")
                return(list(
                        object     = xdata,
                        definition = fDef,
                        summary    = fSmry,
                        table      = fTb
                ))
        }, fill = fill)
        return(features)
}
