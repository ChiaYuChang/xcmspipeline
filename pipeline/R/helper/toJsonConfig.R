toJsonConfig <- function(config, configDir = ".", config.phase = NULL, config.xcms = NULL) {
        phases <- names(config$Phases)
        
        if (!is.null(config.phase) && !is.null(config.xcms)) {
                for (phase in phases) {
                        config$Phases[[phase]]  <- config.phase[[phase]]
                        config$Phases[[phase]]$XCMS <- config.xcms[[phase]]
                        config$Phases[[phase]]$XCMS$MappingTb <- NULL
                }
        }

        class(config) <- c("list")
        for (phase in phases) {
                class(config$Phases[[phase]]) <- c("list")
        }

        jsonConfig <- toJSON(config)
        configDir  <- dirname(configDir)
        configNm   <- str_match(basename(configDir), "(.+)\\.(.+$)")
        nm  <- configNm[1, 2]
        fmt <- configNm[1, 3]
        
        # SAVE JSONConfig
        # cat(configDir)
        flNm <- sprintf("%s/%s.json", configDir, "config")

        tryCatch({
                cat(sprintf("%s/%s.%s -> %s\n", configDir, nm, fmt, flNm))
                write_json(x = jsonConfig, path = flNm, pretty = TRUE)
        }, error = function(err) {
                warning(sprintf("Error in writting json file.\nThe original error message: '%s'", err))                
        })
        return(jsonConfig)
}
