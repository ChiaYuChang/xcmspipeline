extractFeatureDf <- function(feature, is.drake = FALSE) {
        
        if (is.drake) {
                phase   <- names(feature)
                feature <- feature[[1]]
        }

        if (feature$state == 0) {
                featureDf <- feature$definition
                featureDf[, phase := phase]
                setorder(featureDf, featureID)
                setcolorder(featureDf, "phase")
        } else {
                featureDf <- data.table(
                        phase = character(0),
                        featureID = character(0),
                        mzmed = numeric(0),
                        mzmin = numeric(0),
                        mzmax = numeric(0),
                        rtmed = numeric(0),
                        rtmin = numeric(0),
                        rtmax = numeric(0), 
                        npeaks = numeric(0),
                        serum = numeric(0),
                        peakidx = list(),
                        ms_level = integer(0)
                )
        }
        
        if (is.drake) {
                featureDf <- list(featureDf)
                names(featureDf) <- phase
        }

        return(featureDf)
}