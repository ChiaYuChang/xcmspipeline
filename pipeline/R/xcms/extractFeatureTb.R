extractFeatureTb <- function(feature, config.xcms, is.drake = FALSE) {
        
        if (is.drake) {
                phase <- names(feature)
                if (phase != names(config.xcms)) {
                        stop("     x Phases of xData and config are different.")
                }
                feature     <- feature[[1]]
                config.xcms <- config.xcms[[1]]
        }

        if (feature$state == 0) {
                featureTb <- merge(
                        x = config.xcms$MappingTb,
                        y = melt(feature$table,
                                id.vars = "featureID",
                                variable.name = "fileName", 
                                value.name = "int"),
                        all = TRUE
                )
                featureTb[, phase := phase]
                featureTb[, injectionOrder := as.integer(injectionOrder)]
                setorder(featureTb, sampleGroup, injectionOrder, featureID)
                setcolorder(featureTb, c("phase", "featureID", "fileName"))
        } else {
                featureTb <- data.table(
                        phase = character(0),
                        featureID = character(0),
                        fileName = character(0),
                        sampleName = character(0),
                        sampleMode = character(0),
                        sampleGroup = character(0),
                        injectionOrder = integer(0),
                        int = numeric(0)
                )
        }
        
               
        if (is.drake) {
                featureTb <- list(featureTb)
                names(featureTb) <- phase
        }

        return(featureTb)
}
