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
                featureTable.status <- 0
                ftb <- NULL
                message(sprintf("Export featureTable to %s", rstDir))
                e <- try({
                        ftb <- dcast.data.table(
                                        data = featureTb[[phase]],
                                        formula = featureID ~ sampleGroup + sampleName, 
                                        value.var = "int",
                                        sep = "_"
                        )
                        fwrite(
                                x = ftb,
                                file = featureTable
                        )
                        0 # return 0 if the saving process is completed
                        # export(
                        #         x = ftb,
                        #         file = featureTable,
                        #         format = fmt
                        # )
                        # unlink(featureTable)
                })

                if (class(e) == "try-error" && dplyrInstalled) {
                        dplyrInstalled <- all(c("dplyr", "tidyr", "readr") %chin% rownames(installed.packages()))
                        warning("Try to dcast data by dplyr.")
                        e <- try({
                                ftb <- featureTb[[phase]] %>%
                                        tidyr::unite(sampleFull,sampleGroup,sampleName,sep = "_") %>%
                                        dplyr::select(featureID,sampleFull,int) %>%
                                        spread(key = sampleFull, value = int)
                                readr::write_csv(
                                        x = ftb,
                                        path = featureTable
                                )
                                0 # return 0 if the saving process is completed
                                # export(
                                #         x = ftb,
                                #         file = featureTable,
                                #         format = fmt
                                # )
                                # unlink(featureTable)
                        })
                        featureTable.status <- 1
                }


                if (class(e) == "try-error") {
                        warning("Faid to dcast data, export result as a .rds file.")
                        featureTable <- sprintf("%s%s_featureTable.%s", rstDir, phase, "rds")
                        saveRDS(
                                object = featureTb[[phase]],
                                file = featureTable
                        )
                        featureTable.status <- 2
                }

                # save feature definition
                featureDefinition <- sprintf("%s%s_featureDefinition.%s", rstDir, phase, fmt)
                message(sprintf("Export featureDefinition to %s", rstDir))
                featureDefinition.status <- 0
                fdf <- NULL
                e <- try({
                        fdf <- featureDef[[phase]]
                        fwrite(
                                x = fdf,
                                file = featureDefinition
                        )
                        0 # return 0 if the saving process is completed
                        # export(
                        #         x = fdf, 
                        #         file = featureDefinition,
                        #         format = fmt
                        # )
                        # unlink(featureDefinition)
                })

                if (class(e) == "try-error") {
                        warning("Faid to dcast data, export result as a .rds file.")
                        featureDefinition <- sprintf("%s%s_featureDefinition.%s", rstDir, phase, "rds")
                        fdf <- featureDef[[phase]]
                        saveRDS(
                                object = fdf,
                                file = featureDefinition
                        )
                        featureDefinition.status <- 1
                }

                fileName[[phase]] <- list(
                        featureTable = list(
                                dir    = featureTable,
                                status = featureTable.status,
                                data   = ftb
                        ),
                        featureDefinition = list(
                                dir    = featureDefinition,
                                status = featureDefinition.status,
                                data   = featureDef[[phase]]
                        )
                )
        }
        return(fileName)
}