extractFeatureDf <- function(features) {
        Map(phase = names(features), 
            def   = lapply(features, lambda(x, x$definition)),
            f = function(phase, def) {
                    def <- def[, phase := phase]
                    setcolorder(def, "phase")
                }) %.>%
        rbindlist(.)
} 
