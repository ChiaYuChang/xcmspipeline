#! DEPRACATED FUNCTION
signalAlignment <- function(pDeteced, config.xcms, method = "both", binSize = 0.1, minfrac = 0.75) {
        if (!method %chin% c("obiwarp", "peakgroups", "both")) {
                stop("The method should be 'obiwarp', 'peakgroups' or 'both'")
        }
        
        # strange but workable
        BiocParallel::register(BiocParallel.FutureParam::FutureParam())
        
        phase       <- names(pDeteced)
        pDeteced    <- pDeteced[[1]]
        config.xcms <- config.xcms[[1]]
        
        
        xdata  <- pDeteced$xdata
        sampleGroup <- factor(config.xcms$MappingTb$sampleGroup)
        centerSample <- ceiling(length(sampleGroup)/2)
        
        if (method != "peakgroups") {
                # 1 RT adjustment by the Obiwarp method ------------------------------------------------
                # align by dinamic progamming
                message("Applying Obiwarp...")
                # 1-1 read default parameters ---------------------------------------------
                obiwarpPar <- list(
                        binSize      = binSize, # small binSize -> long compute time
                        centerSample = centerSample,
                        localAlignment = FALSE
                )
                
                # 1-2 setting obiwarp parameters -----------------------------------------------
                ObiwarpPar <- ObiwarpParam(binSize = obiwarpPar$binSize, 
                                           centerSample = obiwarpPar$centerSample,
                                           localAlignment = obiwarpPar$localAlignment,
                                           initPenalty = 0)
                
                # 1-3 apply obiwarp algorithm ---------------------------------------------
                xdataTmAdjObiwarp      <- adjustRtime(object = xdata, param = ObiwarpPar)
                xdataTmAdjObiwarpPeaks <- as.data.table(chromPeaks(xdataTmAdjObiwarp), keep.rownames = "peakId")
                
                # Get the base peaks of chromatograms.
                # bpisAdjObiwarp <- chromatogram(xdataTmAdjObiwarp, aggregationFun = "max")
                # plot(bpisAdjObiwarp, col = smpGrp, peakType = "none")
                # plotAdjustedRtime(xdataTmAdjObiwarp, col = smpGrp)
                
                # 1-4 visualizing result --------------------------------------------------
                dTimeObiwarp <- rtime(xdataTmAdjObiwarp, adjusted = TRUE) - rtime(xdataTmAdjObiwarp, adjusted = FALSE)
                dTimeObiwarp <- data.table(
                        dTime  = dTimeObiwarp,
                        smpOdr = fromFile(xdataTmAdjObiwarp),
                        xRt    = rtime(xdataTmAdjObiwarp, adjusted = T)
                )
                dTimeObiwarp[, smpGrp := factor(smpOdr, levels = 1:23, labels = smpGrp)]
                dTimeObiwarp.p <- ggplot(dTimeObiwarp, aes(x = xRt, y = dTime, group = smpOdr, color = smpGrp)) +
                        geom_line() + 
                        geom_hline(yintercept = 0) +
                        labs(y = bquote(RT[adj]-RT[ori]),
                             x = bquote(RT[adj])) +
                        theme_bw() +
                        theme(legend.position = "none")
        } else {
                dTimeObiwarp.p <- NULL
        }
        
        if (method != "obiwarp") {
                # 2 RT adjustment by Peak Groups --------------------------------------------
                # group correspondence peaks -> adjust RT -> group correspondence peaks
                # group correspondence peaks in each group
                # 2-1 peak grouping -------------------------------------------------------
                message("Applying Peak Groups...")
                # 2-1-1 setting peak density parameters -----------------------------------------
                pdPar <- PeakDensityParam(
                        # bw: the bandwidth (standard deviation ot the smoothing kernel) to be used
                        bw = ifelse(is.na(config.xcms$bw), yes = 20, config.xcms$bw),
                        # minFraction: the minimum fraction of samples in at least one sample group 
                        # in which the peaks have to be present to be considered as a peak group
                        minFraction = .8,
                        # minSamples: the minimum number of samples in at least one sample group  
                        # in which the peaks have to be detected to be considered a peak group
                        minSamples  = 3,
                        # sampleGroups: A vector of the same length than samples defining the sample group assignments
                        sampleGroups = smpGrp)
                
                # 2-1-2 group correspondence peaks -----------------------------------------
                xdataGrpCrmP <- groupChromPeaks(object = xdata, param = pdPar)
                
                
                # 2-2 RT adjustment  ----------------------------------------------------------
                # 2-2-1 setting peak groups alignment parameters-----------------------------------------------------------------
                pgPar <- PeakGroupsParam(
                        # minFraction: the minimum required fraction of samples in which peaks for the 
                        # peak group were identified
                        minFraction = .5,
                        smooth = "loess",
                        family = "gaussian")
                
                # 2-2-2 apply groups alignment algorithm -----------------------------------------------------------------
                xdataTmAdjPG      <- adjustRtime(object = xdataGrpCrmP, param = pgPar)
                
                # 2-3 visualizing result --------------------------------------------------
                xdataTmAdjPGPeaks <- as.data.table(chromPeaks(xdataTmAdjPG), keep.rownames = "peakId")
                # bpis_adj <- chromatogram(xdataTmAdjPG, aggregationFun = "max")
                # plot(bpis_adj, col = smpGrp, peakType = "none")
                # plotAdjustedRtime(xdataTmAdjPG, col = smpGrp)
                dTimePG <- rtime(xdataTmAdjPG, adjusted = TRUE) - rtime(xdataTmAdjPG, adjusted = FALSE) # usually take long time
                dTimePG <- data.table(
                        dTime  = dTimePG,
                        smpOdr = fromFile(xdataTmAdjPG),
                        xRt    = rtime(xdataTmAdjPG, adjusted = T)
                )
                dTimePG[, smpGrp := factor(smpOdr, levels = 1:23, labels = smpGrp)]
                dTimePG.p <- ggplot(dTimePG, aes(x = xRt, y = dTime, group = smpOdr, color = smpGrp)) +
                        geom_line() + 
                        geom_hline(yintercept = 0) +
                        labs(y = bquote(RT[adj]-RT[ori]),
                             x = bquote(RT[adj])) +
                        theme_bw() +
                        theme(legend.position = "none")
        } else {
                dTimePG.p <- NULL
        }
        
        
        
        # 3 Comparing RT adjustment result from the 2 algroithms --------------------
        # Too large differences between adjusted and raw retention times could indicate poorly performing samples or alignment.
        
        if (method != "peakgroups") {
                dTimeObiwarp.p90  <- dTimeObiwarp[, .(dTime.p90 = quantile(abs(dTime), probs = .9)), by = smpGrp]
                dTimeObiwarpScore <- 1/reduce(.x = dTimeObiwarp.p90$dTime.p90, .f = `*`)
                
                dTimeObiwarp.distr.p <- ggplot(dTimeObiwarp, aes(x = dTime, group = smpOdr, fill = smpGrp)) +
                        geom_density(alpha = .5) +
                        facet_wrap(~smpGrp) + 
                        labs(title = "Alignment by Obiwarp Algorithm",
                             x = bquote(RT[adj]-RT[ori]),
                             y = "Density",
                             fill = "Sample Group") +
                        theme_bw() + 
                        theme(legend.position = "bottom")
        } else {
                dTimeObiwarpScore    <- -Inf
                dTimeObiwarp.distr.p <- NULL
        }
        
        if (method != "obiwarp") {
                dTimePG.p90   <- dTimePG[, .(dTime.p90 = quantile(abs(dTime), probs = .9)), by = smpGrp]
                dTimePGScore  <- 1/reduce(.x = dTimePG.p90$dTime.p90, .f = `*`)
                
                dTimePG.distr.p <- ggplot(dTimePG, aes(x = dTime, group = smpOdr, fill = smpGrp)) +
                        geom_density(alpha = .5) +
                        facet_wrap(~smpGrp) + 
                        labs(title = "Alignment by Peak Groups Algorithm",
                             x = bquote(RT[adj]-RT[ori]),
                             y = "Density",
                             fill = "Sample Group") +
                        theme_bw() +
                        theme(legend.position = "bottom")
        } else {
                dTimePGScore    <- -Inf
                dTimePG.distr.p <- NULL
        }
        
        
        dTimeCompared.p <- list(dTimeObiwarp.distr.p = dTimeObiwarp.distr.p, 
                                dTimeObiwarp.p       = dTimeObiwarp.p,
                                dTimePG.distr.p      = dTimePG.distr.p, 
                                dTimePG.p            = dTimePG.p)
        
        if (dTimeObiwarpScore > dTimePGScore) {
                xdataAligned <- xdataTmAdjObiwarp
        } else {
                xdataAligned <- xdataTmAdjPG
        }
        
        if (!hasAdjustedRtime(xdataAligned)) {
                warning("Signals have not been aligned.")
        }
        
        # Correspondence features
        pdPar <- PeakDensityParam(
                bw = ifelse(is.na(config.xcms$bw), yes = 20, config.xcms$bw),
                minFraction = .8,
                minSamples  = 3,
                sampleGroups = smpGrp)
        
        xdataAligned <- groupChromPeaks(object = xdataAligned, param = pdPar)
        xdatzAligned <- list(
                xdataAligned = xdataAligned,
                p = dTimeCompared.p
        )
        xdatzAligned <- list(xdatzAligned)
        names(xdatzAligned) <- phase
        return(xdatzAligned)
}

#' Align signals by obiwarp algorithm
#' @param pDeteced
#' @param config.xcms
signalAlignmentObiwarp <- function(pDeteced, config.xcms, binSize = 0.05) {
        BiocParallel::register(BiocParallel.FutureParam::FutureParam())

        phase       <- names(pDeteced)
        pDeteced    <- pDeteced[[1]]
        config.xcms <- config.xcms[[1]]

        xdata     <- pDeteced$xdata
        MappingTb <- config.xcms$MappingTb
        smpGrp    <- factor(MappingTb$sampleGroup)
        centerSample <- ceiling(length(smpGrp)/2)

        # 1 RT adjustment by the Obiwarp method ------------------------------------------------
        # align by dinamic progamming
        message("Applying Obiwarp...")
        # 1-1 read default parameters ---------------------------------------------
        obiwarpPar <- list(
                binSize      = binSize, # small binSize -> long compute time
                centerSample = centerSample,
                localAlignment = FALSE
        )

        # 1-2 setting obiwarp parameters -----------------------------------------------
        ObiwarpPar <- ObiwarpParam(binSize = obiwarpPar$binSize,
                                   centerSample = obiwarpPar$centerSample,
                                   localAlignment = obiwarpPar$localAlignment,
                                   initPenalty = 0)

        # 1-3 apply obiwarp algorithm ---------------------------------------------
        xdataTmAdjObiwarp      <- adjustRtime(object = xdata, param = ObiwarpPar)

        obiwarp <- list(
                        method     = "obiwarp",
                        xdataTmAdj = xdataTmAdjObiwarp
                )
        obiwarp <- list(obiwarp)
        names(obiwarp) <- phase

        return(obiwarp)
}

#' @param pDeteced
#' @param config.xcms
signalAlignmentPeakGrp <- function(pDeteced, config.xcms, 
                                   peakDensity.minSamples = 3, peakDensity.minfrac = 0.8, 
                                   peakGroups.minfrac = 0.5) {
        BiocParallel::register(BiocParallel.FutureParam::FutureParam())
        
        phase       <- names(pDeteced)
        pDeteced    <- pDeteced[[1]]
        config.xcms <- config.xcms[[1]]
        
        xdata     <- pDeteced$xdata
        MappingTb <- config.xcms$MappingTb
        smpGrp    <- factor(MappingTb$sampleGroup)
        centerSample <- ceiling(length(smpGrp)/2)
        
        # 2 RT adjustment by Peak Groups --------------------------------------------
        # group correspondence peaks -> adjust RT -> group correspondence peaks
        # group correspondence peaks in each group
        # 2-1 peak grouping -------------------------------------------------------
        message("Applying Peak Groups...")
        # 2-1-1 setting peak density parameters -----------------------------------------
        pdPar <- PeakDensityParam(
                # bw: the bandwidth (standard deviation ot the smoothing kernel) to be used
                bw = ifelse(is.na(config.xcms$bw), yes = 20, config.xcms$bw),
                # minFraction: the minimum fraction of samples in at least one sample group 
                # in which the peaks have to be present to be considered as a peak group
                minFraction = peakDensity.minfrac,
                # minSamples: the minimum number of samples in at least one sample group  
                # in which the peaks have to be detected to be considered a peak group
                minSamples  = min(min(table(smpGrp)), peakDensity.minSamples),
                # sampleGroups: A vector of the same length than samples defining the sample group assignments
                sampleGroups = smpGrp)
        
        # 2-1-2 group correspondence peaks -----------------------------------------
        xdataGrpCrmP <- groupChromPeaks(object = xdata, param = pdPar)
        
        
        # 2-2 RT adjustment  ----------------------------------------------------------
        # 2-2-1 setting peak groups alignment parameters-----------------------------------------------------------------
        pgPar <- PeakGroupsParam(
                # minFraction: the minimum required fraction of samples in which peaks for the 
                # peak group were identified
                minFraction = peakGroups.minfrac,
                smooth = "loess",
                family = "gaussian")
        
        # 2-2-2 apply groups alignment algorithm -----------------------------------------------------------------
        xdataTmAdjPG      <- adjustRtime(object = xdataGrpCrmP, param = pgPar)
        
        peakGrp <- list(
                        method     = "peakGrouping",
                        xdataTmAdj = xdataTmAdjPG
                )
        peakGrp <- list(peakGrp)
        names(peakGrp) <- phase
        return(peakGrp)
}

#' @param xdataTmAdj
#' @param config.xcms
visualizingAlignmentRst <- function(xdataTmAdj, config.xcms) {
        rtAdjmethod <- xdataTmAdj$method
        xdataTmAdj  <- xdataTmAdj$xdataTmAdj
        xdataTmAdjPeaks <- as.data.table(chromPeaks(xdataTmAdj), keep.rownames = "peakId")
        
        sampleGroup <- config.xcms$MappingTb[, factor(sampleGroup)]
        # bpis_adj <- chromatogram(xdataTmAdj, aggregationFun = "max")
        # plot(bpis_adj, col = sampleGroup, peakType = "none")
        # plotAdjustedRtime(xdataTmAdj, col = sampleGroup)
        
        dTime <- rtime(xdataTmAdj, adjusted = TRUE) - rtime(xdataTmAdj, adjusted = FALSE) # usually take long time
        dTime <- data.table(
                dTime  = dTime,
                injectionOrder = fromFile(xdataTmAdj),
                xRt    = rtime(xdataTmAdj, adjusted = T)
        )
        
        dTime <- dTime[config.xcms$MappingTb, on = .(injectionOrder = injectionOrder)]
        dTime_pdRT <- ggplot(dTime, aes(x = xRt, y = dTime, 
                                    group = sampleName, color = sampleGroup)) +
                geom_line() + 
                geom_hline(yintercept = 0) +
                labs(y = bquote(RT[adj] - RT[ori]),
                     x = bquote(RT[adj]),
                     color = "Sample Groups",
                     title = "Adjusted Retention Time (RT)") +
                theme_bw() +
                theme(legend.position = "bottom")
        dTime_p90   <- dTime[, .(p90 = quantile(abs(dTime), probs = .9, na.rm = TRUE)), by = sampleGroup]
        dTimeScore  <- sum(1/abs(dTime_p90$p90))
        
        dTime_pDistr <- ggplot(dTime, aes(x = dTime, group = sampleGroup, fill = sampleGroup)) +
                geom_density(alpha = .5) +
                facet_wrap(~sampleGroup) + 
                labs(title = "Alignment by Peak Groups Algorithm",
                     x = bquote(RT[adj]-RT[ori]),
                     y = "Density",
                     fill = "Sample Group") +
                theme_bw() +
                theme(legend.position = "bottom")
        return(list(
                dRT = dTime,
                scr = dTimeScore,
                p   = list(
                        dRT    = dTime_pdRT,
                        pDistr = dTime_pDistr
                )
        ))
}
