printTIC <- function(config.phase, mzData, 
                     outputDir, aggregationFun = "max", 
                     is.drake = TRUE) {
    if (is.drake) {
        phase         <- names(config.phase)
        config.phase  <- config.phase[[1]]
        mzData        <- mzData[[1]]
    }
    
    suppressWarnings(dir.create(str_c(outputDir, "TIC", sep = "/")))
    outputDir <- str_c(outputDir, "TIC", config.phase$Phase, sep = "/")
    suppressWarnings(dir.create(outputDir))

    # smpName   <- config.phase$MappingTb$sampleName

    smpName   <- factor(phenoData(mzData)@data[, "sampleName"])

    mzDataLst <- splitByFile(mzData, factor(smpName))
    plotflnm  <- future_mapply(
        mzData = mzDataLst,
        smpName = smpName,
        FUN = function(mzData, smpName, outputDir) {
            fn   <- sprintf("%s/%s.png", outputDir, smpName)
            bpis <- chromatogram(mzData, aggregationFun = "max", BPPARAM = SerialParam())
            p    <- ggplot(as.data.frame(bpis[1, 1]), 
                           aes(x = rtime, y = intensity)) +
                           geom_line() +
                           labs(title = smpName) +
                           theme_bw()
            
            ggsave(filename = fn, 
                   plot = p,
                   width = 15,
                   height = 10,
                   device = "png"
            )

            return(fn)
        },
        MoreArgs = list(
            outputDir = outputDir
        )
    )

    # bpis <- chromatogram(mzData, aggregationFun = aggregationFun, BPPARAM = BPPARAM)
    # for (i in 1:ncol(bpis)) {
    #     message(sprintf("> TIC plot: %s - saving %s...", phase, smpName[i]))
    #     p <- ggplot(as.data.frame(bpis[, i]), 
    #         aes(x = rtime, y = intensity)) +
    #         geom_line() +
    #         labs(title = smpName[i]) +
    #         theme_bw()
    #     ggsave(plotflnm[i], plot = p, width = 15, height = 10)
    # }

    if (is.drake) {
        plotflnm <- list(plotflnm)
        names(plotflnm) <- phase
    }
    return(plotflnm)
}
