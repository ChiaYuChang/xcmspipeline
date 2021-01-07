#' Select better alignment methods
#' @param obiwarp object obiwarp
correspondence <- function(obiwarp = NULL, obiwarpVis = NULL,
                           peakGrp = NULL, peakGrpVis = NULL,
                           config.xcms, 
                           peakDensity.minfrac = 0.8, peakDensity.minSamples  = 3,
                           is.drake = FALSE) {
        
        if (!setequal(names(obiwarp), names(peakGrp))) {
                stop("Element names of obiwarp and peakGrp are different.")
        }
        
        if (!setequal(names(obiwarp), names(obiwarpVis))) {
                stop("Element names of obiwarp and obiwarpVis are different.")
        }
        
        if (!setequal(names(peakGrp), names(peakGrpVis))) {
                stop("Element names of peakGrp and peakGrpVis are different.")
        }
        
        peakGrpState <- vapply(peakGrp, lambda(x, x$state), numeric(1))
        obiwarpState <- vapply(obiwarp, lambda(x, x$state), numeric(1))
        obiwarpScr   <- min(vapply(obiwarpVis, lambda(x, x$scr), numeric(1)))
        peakGrpScr   <- min(vapply(peakGrpVis, lambda(x, x$scr), numeric(1)))

        if (all(peakGrpState == 0) && all(obiwarpState == 0)) {
                if (peakGrpScr > obiwarpScr) {
                        aliMthd    <- "peakGrp"
                        xdataTmAdj <- lapply(peakGrp, lambda(x, x$xdataTmAdj))
                } else {
                        aliMthd    <- "obiwarp"
                        xdataTmAdj <- lapply(obiwarp, lambda(x, x$xdataTmAdj))
                }
        } else if (all(obiwarpState == 0)) {
                aliMthd    <- "obiwarp"
                xdataTmAdj <- lapply(obiwarp, lambda(x, x$xdataTmAdj))
        } else if (all(peakGrpState == 0)) {
                aliMthd    <- "peakGrp"
                xdataTmAdj <- lapply(peakGrp, lambda(x, x$xdataTmAdj))
        } else {
                warning("Error in RT adjustment.")
                aliMthd  <- ""
                rtAdjRst <- list(
                        groupChromPeaks = NULL,
                        rtAdjMethod = aliMthd,
                        err.msg = "Error in both RT adjustment methods.",
                        state = 1
                )
                rtAdjRst <- rep(list(rtAdjRst), length(config.xcms))
                names(rtAdjRst) <- names(config.xcms)
        }

        if (aliMthd != "") {
                rtAdjRst <- mapply(FUN = function(
                                xdataTmAdj, config.xcms, minFraction, 
                                minSamples, rtAdjMethod) {
                                tryCatch({
                                        sampleGroup <- factor(config.xcms$MappingTb$sampleGroup)
                                        pdPar <- PeakDensityParam(
                                                        bw = ifelse(test = is.na(config.xcms$bw), 
                                                                yes  = 20, 
                                                                no   = config.xcms$bw),
                                                        minFraction  = minFraction,
                                                        minSamples   = min(min(table(sampleGroup)), minSamples),
                                                        sampleGroups = sampleGroup)
                                        gCrmPks <- groupChromPeaks(xdataTmAdj, pdPar)
                                        return(list(
                                                groupChromPeaks = gCrmPks,
                                                rtAdjMethod = rtAdjMethod,
                                                err.msg = "",
                                                state = 0  
                                        ))
                                }, error = function(err) {
                                        return(list(
                                                groupChromPeaks = NULL,
                                                rtAdjMethod = rtAdjMethod,
                                                err.msg = err,
                                                state = 1
                                        ))
                                })
                        },
                        xdataTmAdj  = xdataTmAdj,
                        config.xcms = config.xcms,
                        MoreArgs = list(
                                minFraction = peakDensity.minfrac, 
                                minSamples  = peakDensity.minSamples,
                                rtAdjMethod = aliMthd
                        ),
                        SIMPLIFY = FALSE
                )
        }
        
        if (is.drake) {
                return(rtAdjRst)
        } else {
                return(rtAdjRst)
        }
}