dplan <- drake_plan(
        # read configs and default configs
        DEFAULT     = target(command = readDefaultConfig(file_in(!!DFAULT_CONFIG_PATH),
                                                         file_in(!!XCMS_PARS_BOUNDS_PATH))),
        config      = target(command = readConfig(configPath = file_in(!!arguments$input), DEFAULT = DEFAULT), hpc = FALSE),
        # estimating XCMS parameters
        AutoTuners  = target(command = future_lapply(X = config$Phases, FUN = newAutoTuner, sampleSize = 5),
                             resources = list(cores = 10, gpus = 0)),
        estXCMSPars = target(command = autoTunerPipeLine(AutoTuners, config, returned_peaks = 12),
                             dynamic = map(AutoTuners),
                             resources = list(cores = 10, gpus = 0)),
        # XCMS
        config.xcms = target(command = extractXCMSPars(estXCMSPars, config, DEFAULT), hpc = FALSE),
        # read all mzXML files
        mzData      = target(command = future_lapply(config$Phases, readmzData)),
        # peaks detection by the centwave algorithm
        pDeteced    = target(command = peakDetection(config.xcms, mzData),
                             dynamic = map(config.xcms, mzData)),
        # pAligned    = target(command = signalAlignment(pDeteced, config.xcms, method = "both"),
                             # dynamic = map(pDeteced, config.xcms)),
        # signals alignment by two algorithms
        obiwarp     = target(command = signalAlignmentObiwarp(pDeteced, config.xcms),
                             dynamic = map(pDeteced, config.xcms)),
        peakGrp     = target(command = signalAlignmentPeakGrp(pDeteced, config.xcms),
                             dynamic = map(pDeteced, config.xcms)),
        obiwarpVis  = target(command = mapply(FUN = visualizingAlignmentRst,
                             xdataTmAdj  = obiwarp,
                             config.xcms = config.xcms,
                             SIMPLIFY = FALSE)),
        peakGrpVis  = target(command = mapply(FUN = visualizingAlignmentRst,
                             xdataTmAdj  = peakGrp,
                             config.xcms = config.xcms,
                             SIMPLIFY = FALSE)),
        # extract correspodence paeks
        pCorrspd    = target(command = correspondence(
                             obiwarp = obiwarp, obiwarpVis = obiwarpVis,
                             peakGrp = peakGrp, peakGrpVis = peakGrpVis,
                             config.xcms)),
        features    = target(command = fillAndExtractFTb(pCorrspd)),
        featureTb   = target(command = extractFeatureTb(features, config.xcms)),
        featureDef  = target(command = extractFeatureDf(features))
)