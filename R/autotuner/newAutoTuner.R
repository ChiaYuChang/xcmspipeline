#' Function for constructing AutoTuner S4 object
#' @param config.phase metPipelinePhaseConfigs object
#' @param sampleSize how many samples in each group should be used for XCMS parameters 
#' estimation. If sampleSize = "all", all samples will be used.
#' @return AutoTuner S4 object
newAutoTuner <- function(config.phase, sampleSize = "all") {
        if (sampleSize == "all") {
                dt <- config.phase$MappingTb
        } else {
                sampleSize <- min(sampleSize, config.phase$MappingTb[sampleGroup != "QC", .N, by = sampleGroup][, N])
                
                # select random sample
                selectedSmpl <- config.phase$MappingTb[sampleGroup != "QC",
                                                       .(sampleName = sample(x = sampleName, size = sampleSize)),
                                                       sampleGroup]
                selectedSmpl <- selectedSmpl$sampleName
                
                # select all QC
                QC <- config.phase$MappingTb[sampleGroup == "QC", sampleName]
                warning("There is no QC samples.")
                selectedSmpl <- c(QC, selectedSmpl)
                
                dt <- config.phase$MappingTb[sampleName %chin% selectedSmpl]  
        }
        
        autotuner <- suppressMessages(createAutotuner(
                data_paths = str_c(config.phase$Path, dt$fileName, sep = "/"),
                runfile    = data.frame(dt), 
                file_col   = "fileName", 
                factorCol  = "sampleGroup"))
        return(autotuner)
}
