fillAndExtractFTb <- function(pCorrspd, fill = TRUE, BPPARAM = bpparam(), is.drake = FALSE) {

        if (is.drake) {
                phase <- names(pCorrspd)
                pCorrspd <- pCorrspd[[1]]
                # BiocParallel::register(BPPARAM = BPPARAM)
        }
        
        if (pCorrspd$state == 0) {
                xdata <- pCorrspd$groupChromPeaks
                tryCatch({
                        if (fill) {
                                message("Filling NA")
                                xdata <- fillChromPeaks(xdata, BPPARAM = BPPARAM)
                        }
                        
                        fDef  <- as.data.table(x = featureDefinitions(xdata),
                                        keep.rownames = "featureID")
                        fTb   <- data.table(featureValues(xdata, 
                                                        value = "into"),
                                        keep.rownames = "featureID")
                        fSmry <- data.table(featureSummary(xdata, 
                                                        group = xdata@phenoData@data$sampleGroup),
                                        keep.rownames = "featureID")
                        features <- list(
                                object     = xdata,
                                definition = fDef,
                                summary    = fSmry,
                                table      = fTb,
                                err.msg    = "",
                                fill       = fill,
                                state      = 0
                        )
                }, error = function(err) {
                        features <- list(
                                object     = NULL,
                                definition = NULL,
                                summary    = NULL,
                                table      = NULL,
                                err.msg    = err,
                                fill       = fill,
                                state      = 1
                        )
                })
        } else {
                features <- list(
                        object     = NULL,
                        definition = NULL,
                        summary    = NULL,
                        table      = NULL,
                        err.msg    = "Error(s) occurs in the previous steps.",
                        fill       = fill,
                        state      = 1
                )
        }

        if (is.drake) {
                features <- list(features)
                names(features) <- phase
        }
        return(features)
}