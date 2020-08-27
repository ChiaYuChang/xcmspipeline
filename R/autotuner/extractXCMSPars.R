extractXCMSPars <- function(estXCMSPars, config, DEFAULT) {
        
        returnValueBetween <- function(value, ub, lb, default = NA) {
                if (is.na(value)) {
                        return(default)
                }
                if (between(value, lower = lb, upper = ub, incbounds = T)) {
                        return(value)
                } else {
                        return(ifelse(test = value > ub, 
                                      yes  = ub, 
                                      no   = lb))
                }
        }
        
        estXCMSPars <- lapply(estXCMSPars, function(est) {
                if (is.data.frame(est)) {
                        NparSet <- est[, length(unique(parSet))]
                        est[is.infinite(value), Ninf := .N, by = c("parGrp", "parameters")]
                        
                        allInf  <- unique(est[Ninf == NparSet, c("parGrp", "parameters")])
                        allInf[, value := NA]
                        
                        est <- est[!is.infinite(value), .(value = mean(value)), by = c("parGrp", "parameters")]
                        est <- rbind(est, allInf)
                        return(est)
                } else {
                        if (is.na(est)) {
                                return(est)
                        } else {
                                stop("Unknown Error.")
                        }
                }
        })
        
        config.xcms <- mapply(function(config.Phases, est){
                if (is.data.frame(est)) {
                        if (!is.data.table(est)) {
                                as.data.table(est)
                        }
                        setkey(est, parameters)
                        setkey(DEFAULT$xcms, parName)
                        
                        config.Phases$XCMS[["noise"]] = returnValueBetween(
                                value = est["noise", value], 
                                ub = DEFAULT$xcms["noise", max],
                                lb = DEFAULT$xcms["noise", min], 
                                default = DEFAULT$xcms["noise", default]
                        )
                        
                        config.Phases$XCMS[["peakWidth_lb"]] = returnValueBetween(
                                value = est["Min Peakwidth", value], 
                                ub = DEFAULT$xcms["peakWidth_lb", max],
                                lb = DEFAULT$xcms["peakWidth_lb", min], 
                                default = DEFAULT$xcms["peakWidth_lb", default]
                        )
                        
                        config.Phases$XCMS[["peakWidth_ub"]] = returnValueBetween(
                                value = est["Max Peakwidth", value], 
                                ub = DEFAULT$xcms["peakWidth_ub", max],
                                lb = DEFAULT$xcms["peakWidth_ub", min], 
                                default = DEFAULT$xcms["peakWidth_ub", default]
                        )
                                
                        config.Phases$XCMS[["prefilterInt"]]  = returnValueBetween(
                                value = est["preIntensity", value],
                                ub = DEFAULT$xcms["prefilterInt", max],
                                lb = DEFAULT$xcms["prefilterInt", min], 
                                default = DEFAULT$xcms["prefilterInt", default]
                        )
                        
                        config.Phases$XCMS[["prefilterScan"]] = returnValueBetween(
                                value = est["preScan", value],
                                ub = DEFAULT$xcms["prefilterScan", max],
                                lb = DEFAULT$xcms["prefilterScan", min], 
                                default = DEFAULT$xcms["prefilterScan", default]
                        )
                        
                        config.Phases$XCMS[["snthresh"]] = returnValueBetween(
                                value = est["snThresh", value],
                                ub = DEFAULT$xcms["snthresh", max],
                                lb = DEFAULT$xcms["snthresh", min], 
                                default = DEFAULT$xcms["snthresh", default]
                        )
                        
                        config.Phases$XCMS[["bw"]] = returnValueBetween(
                                value = est["group_diff", value],
                                ub = DEFAULT$xcms["bw", max],
                                lb = DEFAULT$xcms["bw", min], 
                                default = DEFAULT$xcms["bw", default]
                        )
                        
                        return(config.Phases$XCMS)
                } else {
                        if (is.na(est)) {
                                return(config.Phases$XCMS)
                        } else {
                                stop("Unknown Error.")
                        }
                }
                
        }, config$Phases, estXCMSPars, SIMPLIFY = F)
        
        for (phase in names(config$Phases)) {
                config.xcms[[phase]]$MappingTb <- config$Phases[[phase]]$MappingTb
        }
        
        return(config.xcms)
}
