#' Function for constructing AutoTuner S4 object
#' @param config.phase metPipelinePhaseConfigs object
#' @param sampleSize how many samples in each group should be used for XCMS parameters 
#' estimation. If sampleSize = "all", all samples will be used.
#' @return AutoTuner S4 object
newAutoTuner <- function(config.phase, sampleSize = "all", is.drake = FALSE) {
        if (is.drake) {
                phase <- names(config.phase)
                config.phase <- config.phase[[1]]
                if (!is.null(config.phase$ReservedWords$QC)) {
                        qc.label <- config.phase$ReservedWords$QC
                } else {
                        qc.label <- "QC"
                }
        }
        
        if (config.phase$AutoTuner$AutoTuner) {
                if (sampleSize == "all") {
                        dt <- config.phase$MappingTb
                } else {
                        sampleSize <- min(
                                sampleSize, 
                                config.phase$MappingTb[
                                        sampleGroup != qc.label, 
                                        .N, 
                                        by = sampleGroup][, N]
                        )
                        
                        # select random sample
                        selectedSmpl <- config.phase$MappingTb[
                                  sampleGroup != qc.label,
                                .(sampleName = sample(
                                        x = sampleName, 
                                        size = sampleSize)
                                ),
                                sampleGroup][, sampleName]
                        
                        # select all QC
                        QC <- config.phase$MappingTb[
                                sampleGroup == qc.label,
                                sampleName]
                        
                        if (length(QC) < 1) {
                                warning("There is no QC samples.")
                        } else {
                                message("Phase: ", phase)
                                message(sprintf(
                                        "Select %d samples and %d QCs.",
                                        length(selectedSmpl),
                                        length(QC)
                                ))
                        }
                        
                        selectedSmpl <- c(QC, selectedSmpl)
                        
                        dt <- config.phase$MappingTb[sampleName %chin% selectedSmpl]
                }
                
                tryCatch({
                        autotuner <- suppressMessages(createAutotuner(
                                data_paths = str_c(config.phase$Path, dt$fileName, sep = "/"),
                                runfile    = data.frame(dt), 
                                file_col   = "fileName", 
                                factorCol  = "sampleGroup"))
                }, error = function(err) {
                        message("error in constructing autotuner for phase", phase)
                        message(err)
                        stop("unknown error")
                })
                
                if (is.drake) {
                        autotuner <- list(autotuner)
                        names(autotuner) <- phase
                }
                
        } else {
                autotuner <- NULL
        }
        return(autotuner)
}
