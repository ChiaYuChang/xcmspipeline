exportFeatures <- function(outputDir, featureTb, featureDef, config.phase, fmt = "csv") {
        if (setequal(names(featureTb), names(featureDef)) &&
            setequal(names(featureTb), names(config.phase))) {
                phases <- names(featureTb)
        } else {
                warning("Something went wrong, phases not match")
                phases <- intersect(names(featureTb), names(featureDef))
                phases <- intersect(phases, names(config.phase))
        }
        
        phases <- data.table(
                phase = vapply(config.phase, lambda(x, x$Phase), character(1)),
                subPhase = phases
        )

        fileName <- list()
        for (phase in phases$subPhase) {
                rstDir <- sprintf("%s/%s/", outputDir, config.phase[[phase]]$Phase)
                if (!dir.exists(rstDir)) {
                        dir.create(rstDir)
                }
                
                featureTable <- sprintf("%s%s_featureTable.%s", rstDir, phase, fmt)
                export(x = dcast(data = featureTb[[phase]], 
                                 formula = featureID ~ sampleGroup + sampleName, 
                                 value.var = "int", sep = "_"),
                       file = featureTable,
                       format = fmt
                )
                
                featureDefinition <- sprintf("%s%s_featureDefinition.%s", rstDir, phase, fmt)
                export(x = featureDef[[phase]], 
                       file = featureDefinition,
                       format = fmt
                )

                fileName[[phase]] <- list(
                        featureTable = featureTable,
                        featureDefinition = featureDefinition
                )
        }
        return(fileName)
}