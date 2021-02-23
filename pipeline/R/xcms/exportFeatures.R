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
                
                
                # Save feature table
                featureTable <- sprintf("%s%s_featureTable.%s", rstDir, phase, fmt)
                
                e <- try({
                        export(
                                x = dcast.data.table(
                                        data = featureTb[[phase]], 
                                        formula = featureID ~ sampleGroup + sampleName, 
                                        value.var = "int",
                                        sep = "_"
                                ),
                                file = featureTable,
                                format = fmt
                        )
                        unlink(featureTable)
                })

                dplyrInstalled <- all(c("dplyr", "tidyr") %chin% rownames(installed.packages()))
                if (class(e) == "try-error" && dplyrInstalled) {
                        warning("Try to dcast data by dplyr.")
                        e <- try({
                                export(
                                        x = featureTb[[phase]] %>%
                                        tidyr::unite(sampleFull,sampleGroup,sampleName,sep = "_") %>%
                                        dplyr::select(featureID,sampleFull,int) %>%
                                        spread(key = sampleFull, value = int),
                                        file = featureTable,
                                        format = fmt
                                )

                        })
                        unlink(featureTable)
                }


                if (class(e) == "try-error") {
                        warning("Faid to dcast data, export result as a .rds file.")
                        featureTable <- sprintf("%s%s_featureTable.%s", rstDir, phase, "rds")
                        saveRDS(
                                object = featureTb[[phase]],
                                file = featureTable
                        )
                }

                # save feature definition
                featureDefinition <- sprintf("%s%s_featureDefinition.%s", rstDir, phase, fmt)
                
                e <- try({
                        export(
                                x = featureDef[[phase]], 
                                file = featureDefinition,
                                format = fmt
                        )
                        unlink(featureDefinition)
                })

                if (class(e) == "try-error") {
                        warning("Faid to dcast data, export result as a .rds file.")
                        featureDefinition <- sprintf("%s%s_featureDefinition.%s", rstDir, phase, "rds")
                        saveRDS(
                                object = featureDef[[phase]],
                                file = featureDefinition
                        )
                }

                fileName[[phase]] <- list(
                        featureTable = featureTable,
                        featureDefinition = featureDefinition
                )
        }
        return(fileName)
}