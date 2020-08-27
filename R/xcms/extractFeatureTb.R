extractFeatureTb <- function(features, config.xcms) {
        mapply(FUN = function(mappingTb, peakTable) {
                dt <- merge(x = mappingTb$MappingTb,
                            y = melt(peakTable$table,
                                     id.vars = "featureID",
                                     variable.name = "fileName", 
                                     value.name = "int")
                )
                setcolorder(dt, c("featureID", "fileName"))
        }, mappingTb = config.xcms,
        peakTable = features,
        SIMPLIFY  = FALSE
        )
}
