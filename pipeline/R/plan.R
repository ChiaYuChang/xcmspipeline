dplan <- drake_plan(
        # read configs and default configs
        DEFAULT = target(command = readDefaultConfig(
                        file_in(!!DFAULT_CONFIG_PATH),
                        file_in(!!XCMS_PARS_BOUNDS_PATH)
                )
        ),

        config = target(command = readConfig(
                        configPath = file_in(!!arguments$input), 
                        DEFAULT = DEFAULT
                )
        ),
        
        config.phase = target(command = config$Phase),
        
        # estimating XCMS parameters
        AutoTuners = target(command = newAutoTuner(
                                config.phase = config.phase,
                                sampleSize = 10,
                                is.drake = TRUE
                ),
                dynamic = map(
                        config.phase
                )
        ),

        autoTunersParSet = target(command = newAutoTunersParSet(
                        AutoTuners = AutoTuners,
                        config.phase = config.phase
                )
        ),
        
        estXCMSPars = target(command = autoTunerPipeLine(
                        autoTunersParSet = autoTunersParSet, 
                        returned_peaks = 20, 
                        massThresh = 0.005, 
                        is.drake = TRUE,
                        verbose = FALSE
                ),
                dynamic = map(
                        autoTunersParSet
                )
        ),
        
        # XCMS
        config.xcms = target(command = extractXCMSPars(
                        estXCMSPars, 
                        config, 
                        DEFAULT
                ), 
                hpc = FALSE
        ),

        # read all mzXML files
        mzData = target(command = readmzData(
                       config.phase = config.phase, 
                       is.drake = TRUE
                ),
                dynamic = map(
                        config.phase
                )
        ),
        
        TICpath = target(command = printTIC(
                        config.phase = config.phase,
                        mzData = mzData,
                        outputDir = arguments$output,
                        aggregationFun = "max",
                        is.drake = TRUE
                ),
                dynamic = map(
                        config.phase,
                        mzData
                )
        ),

        # peaks detection by the centwave algorithm
        pDeteced = target(command = peakDetection(
                        config.xcms = config.xcms,
                        mzData = mzData,
                        fitgauss = TRUE, 
                        integrate = 1L, 
                        mzdiff = 0.01,
                        BPPARAM = BPPARAM
                ),
                dynamic = map(
                        config.xcms,
                        mzData
                ),
                hpc = FALSE
        ),
         
        # pAligned = target(command = signalAlignment(
        #               pDeteced, 
        #               config.xcms, 
        #               method = "both"
        #       ),
        #       dynamic = map(
        #               pDeteced,
        #               config.xcms)
        # ),
        # signals alignment by two algorithms
        
        obiwarp = target(command = signalAlignmentObiwarp(
                        pDeteced = pDeteced, 
                        config.xcms = config.xcms,
                        BPPARAM = BPPARAM,
                        is.drake = TRUE
                ),
                dynamic = map(
                        pDeteced,
                        config.xcms
                ),
                # hpc = FALSE
                resources = list(cores = 3, gpus = 0)
        ),

        peakGrp = target(command = signalAlignmentPeakGrp(
                        pDeteced = pDeteced,
                        config.xcms = config.xcms,
                        BPPARAM = BPPARAM,
                        is.drake = TRUE
                ),
                dynamic = map(
                        pDeteced,
                        config.xcms
                ),
                resources = list(cores = 10, gpus = 0)
        ),
        
        obiwarpVis  = target(command = visualizingAlignmentRst(
                        xdataTmAdj = obiwarp,
                        config.xcms = config.xcms,
                        is.drake = TRUE
                ),
                dynamic = map(
                        obiwarp,
                        config.xcms
                )
        ),
        
        peakGrpVis = target(command = visualizingAlignmentRst(
                        xdataTmAdj = peakGrp,
                        config.xcms = config.xcms,
                        is.drake = TRUE
                ),
                dynamic = map(
                        peakGrp,
                        config.xcms
                )
        ),
        
        # # extract correspodence paeks
        pCorrspd = target(command = correspondence(
                obiwarp = obiwarp,
                obiwarpVis = obiwarpVis,
                peakGrp = peakGrp,
                peakGrpVis = peakGrpVis,
                config.xcms = config.xcms,
                is.drake = TRUE)
        ),

        features  = target(command = fillAndExtractFTb(
                        pCorrspd = pCorrspd,
                        fill = TRUE,
                        BPPARAM = BPPARAM,
                        is.drake = TRUE
                ),
                dynamic = map(
                        pCorrspd
                ),
                hpc = FALSE
        ),

        featureTb   = target(command = extractFeatureTb(
                        feature = features,
                        config.xcms = config.xcms,
                        is.drake = TRUE
                ),
                dynamic = map(
                        features,
                        config.xcms
                )
        ),

        featureDef  = target(command = extractFeatureDf(
                        feature = features,
                        is.drake = TRUE
                ),
                dynamic = map(
                        features
                )
        ),
        
        exportFiles = target(command = exportFeatures(
                outputDir = arguments$output,
                featureTb = featureTb,
                featureDef = featureDef,
                config.phase = config.phase
        )),

        jsonConfig = target(command = toJsonConfig(
                config = config,
                configDir = !!arguments$input,
                config.phase = config.phase,
                config.xcms = config.xcms
        ))
)
