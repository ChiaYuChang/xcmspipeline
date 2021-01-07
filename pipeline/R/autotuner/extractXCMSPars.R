extractXCMSPars <- function(estXCMSPars, config, DEFAULT) {
        
        estXCMSParsMeta <- data.table(
                parName = sapply(estXCMSPars, lambda(x, x$parName)), 
                phase   = sapply(estXCMSPars, lambda(x, x$phase)),
                flag    = sapply(estXCMSPars, lambda(x, x$flag))
        )
        estXCMSParsFlags <- estXCMSParsMeta[, .(flag = all(flag == 1)), by = .(phase)]

        estXCMSPars <- rbindlist(
                lapply(
                        estXCMSPars, 
                        lambda(x, x$estPars)
                ), 
                use.names = TRUE
        )
        estXCMSPars <- estXCMSPars[, 
                .(value = quantile(value, .50, na.rm = TRUE)),
                by = .(phase, parGrp, parameters)]
        estXCMSPars <- merge(estXCMSPars, estXCMSParsFlags)
        estXCMSPars[parameters == "group_diff", parameters := "bw"]
        estXCMSPars <- merge(estXCMSPars, DEFAULT$xcms, by = c("parameters", "parGrp"), all.x = TRUE)
        estXCMSPars[is.na(value), value := default]
        estXCMSPars[value < min, value := min]
        estXCMSPars[value > max, value := max]
        estXCMSPars <- estXCMSPars[parGrp == "EIC" | parameters == "bw"]
        estXCMSPars[parameters == "Max Peakwidth", parameters := "peakWidth_ub"]
        estXCMSPars[parameters == "Min Peakwidth", parameters := "peakWidth_lb"]
        estXCMSPars[parameters == "preIntensity" , parameters := "prefilterInt"]
        estXCMSPars[parameters == "preScan"      , parameters := "prefilterScan"]
        estXCMSPars[parameters == "snThresh"     , parameters := "snthresh"]

        estXCMSPars <- split(estXCMSPars[, .(parameters, phase, value)], by = c("phase")) %.>%
                lapply(X = ., function(x) {
                        lst <- as.list(x$value)
                        names(lst) <- x$parameters
                        return(lst)
                })

        config.xcms <- lapply(config$Phases, lambda(x, x$XCMS))

        for (phase in names(config.xcms)) {
                cat("Phase:", phase, "\n")
                for (par in names(config.xcms[[phase]])) {
                        if ( !is.na(config.xcms[[phase]][[par]]) && 
                           config.xcms[[phase]][[par]] > 0) {
                               cat(sprintf("%15s\t%7.3f   (%7.3f)\n", 
                                        par, 
                                        config.xcms[[phase]][[par]], 
                                        estXCMSPars[[phase]][[par]]))  
                        }

                        if( is.na(config.xcms[[phase]][[par]]) || 
                           config.xcms[[phase]][[par]] < 0) {
                                cat(sprintf("%15s\t%7.3f -> %7.3f\n", 
                                        par, 
                                        config.xcms[[phase]][[par]], 
                                        estXCMSPars[[phase]][[par]]))
                                config.xcms[[phase]][[par]] = estXCMSPars[[phase]][[par]]
                        }
                }
                cat("\n")
        }

        for (phase in names(config$Phases)) {
                config.xcms[[phase]]$MappingTb <- config$Phases[[phase]]$MappingTb
        }
        
        return(config.xcms)
}
