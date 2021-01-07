newAutoTunersParSet <- function(AutoTuners, config.phase) {
    AutoTuners <- data.table(
        phase = names(AutoTuners),
        AutoTuners = AutoTuners
    )
    
    parSet <- lapply(config.phase, lambda(x, x$AutoTuner$Pars))
    for ( i in 1:length(parSet)) {
        parSet[[i]][, phase := names(parSet)[i]]
    }
    parSet <- rbindlist(parSet)
    parSet <- merge(
        parSet,
        data.table(
            phase = names(config.phase),
            est   = sapply(config.phase, lambda(x, x$AutoTuner$AutoTuner))
        )
    )
    AutoTunersParSet <- merge(
        parSet,
        AutoTuners
    )
    AutoTunersParSet <- split(
        AutoTunersParSet, 
        by = c("phase", "parName")
    )
    AutoTunersParSet <- lapply(
            AutoTunersParSet, 
            as.list
    )
    class(AutoTunersParSet) <- c("list", "AutoTunersParSet")
    return(AutoTunersParSet)
}

print.AutoTunersParSet <- function(AutoTunersParSet) {
    cat("Total number of autotuner parameter set:", 
        length(AutoTunersParSet),
        "\n")
    
    for (i in 1:length(AutoTunersParSet)) {
        if (AutoTunersParSet[[1]]$est) {
            cat(sprintf("%02d. Phase: %s (%s)\n    - Lag: %05.2f, Threshold: %05.2f, Influence: %05.2f\n",
                i,
                AutoTunersParSet[[i]]$phase, 
                AutoTunersParSet[[i]]$parName, 
                AutoTunersParSet[[i]]$lag, 
                AutoTunersParSet[[i]]$threshold,
                AutoTunersParSet[[i]]$influence
            ))
        } else {
            cat(sprintf("%02d. Phase: %s (%s)\n Skip XCMS estimation\n",
                i,
                AutoTunersParSet[[1]]$phase))
        }
    }
}