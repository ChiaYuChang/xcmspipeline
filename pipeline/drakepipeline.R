#!/usr/bin/env Rscript
options(future.globals.maxSize = 1024^2*2000)

DRAKE_PIPELINE_PATH = "/pipeline"
arguments = commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages(library("optparse"))
message("> Parsing arguments...")
optionList <- list(
        make_option(c("-i", "--input"), action = "store", type = "character",
                default = "./config.xlsx", help = "the path to the project"),
        make_option(c("-o", "--output"), action = "store", type = "character",
                default = "./Result", help = "the directory for result storage"),
        make_option(c("-s", "--sheet"), action = "store", type = "character",
                default = "Parameters", help = "the sheet contains parameters"),
        make_option(c("-p", "--parallel"), action = "store", type = "integer",
                default = 2, help = "number of core to use for constructing drake graph"),
        make_option(c("-m", "--smooth"), action = "store", type = "logical",
                default = FALSE, help = "whether smooth the mzXML"),
        make_option(c("-n", "--centroid"), action = "store", type = "logical",
                default = FALSE, help = "whether smooth the mzXML"),
        make_option(c("-c", "--clean"), action = "store", type = "logical",
                default = FALSE, help = "whether clean the cache"),
        make_option(c("-u", "--unlock"), action = "store", type = "logical",
                default = FALSE, help = "whether unlock the cache")
)

arguments <- parse_args(
        OptionParser(
                option_list = optionList
        ),
        args = arguments
)

# usethis::proj_set("/project", force = TRUE)
# usethis::proj_activate("/project")
usethis::proj_set(dirname(arguments$input), force = TRUE)
usethis::proj_activate(dirname(arguments$input))
arguments$input <- normalizePath(arguments$input)
usethis::use_directory(stringr::str_remove(arguments$output, dirname(arguments$input)))

message("▶ 0. Set up cache folder...")
if (!dir.exists(arguments$output)) {
        message(sprintf("%s is created", arguments$output))
        dir.create(arguments$output)
}

if (arguments$unlock && dir.exists("./.drake/")) {
        drake::drake_cache("./.drake/")$unlock()
        message("     - The cache has been unlocked.")
}

if (arguments$clean) {
        drake::clean()
        message("     - The cache has been removed.")
}

# Load packages
message("▶ 1. Loading packages...")
source(stringr::str_c(DRAKE_PIPELINE_PATH, "packages.R", sep = "/"))

DFAULT_CONFIG_PATH    <- str_c(DRAKE_PIPELINE_PATH, "DefaultParameters/defaultConfig.xlsx", sep = "/")
XCMS_PARS_BOUNDS_PATH <- str_c(DRAKE_PIPELINE_PATH, "DefaultParameters/xcmsParUbLb.csv", sep = "/")

# Load functions
message("▶ 2. Loading functions...")
for (srp in str_subset(list.files(str_c(DRAKE_PIPELINE_PATH, "R", sep = "/"), full.names = T, recursive = T), "\\.R$")) {
        source(srp)
}

# Checking the existence --------------------------------------------------
message("▶ 3. Checking whether the file exist...")
if (!file.exists(arguments$input)) {
        stop(sprintf("File %s does not exist.", arguments$input))
} else {
        arguments$input  <- path.expand(arguments$input)
        DRAKE_CACHE      <- dirname(arguments$input)
        usethis::use_directory(".drake")

        if (str_detect(arguments$input, ".xlsx$")) {
                config <- rio::import(
                        file = arguments$input, 
                        which = arguments$sheet, 
                        range = readxl::cell_cols("A:C"), 
                        setclass = "data.table"
                )
                config[, parType := na_locf(parType)]
                Nphase <- nrow(config[parName == "phase"])
                rm(config)
        } else if (str_detect(arguments$input, "\\.json$")) {
                #! DO SOMETHINGS
                stop(".json will be support in the near future")
                Nphase <- 2
        } else if (str_detect(arguments$input, "\\.xml$")) {
                #! DO SOMETHINGS
                stop(".xml will be support in the near future")
                Nphase <- 2
        } else {
           stop("Unknown config format.")
        }
        message(sprintf("     - There are %d phases", Nphase))
}

# Setting up parallel computing env
message("▶ 4. Set up parallel computing environment...")
if (arguments$parallel > 1) {
        arguments$parallel <- min(arguments$parallel, future::availableCores())
        message(sprintf("     - Using %d core/thread", arguments$parallel))
        # setting up future plan
        message("     ❯ 4.1 Setting BiocParallel...")
        BiocParallel::register(BiocParallel.FutureParam::FutureParam())
        BPPARAM <- bpparam()
        
        message("     ❯ 4.2 Setting future...")
        future::plan(list(tweak(multisession, workers = arguments$parallel),
                          tweak(multisession, workers = floor(arguments$parallel/Nphase)),
                          tweak(multisession, workers = floor(arguments$parallel/Nphase))))
        
} else {
        message("     - Compute sequentially...")
        future::plan(sequential)
        BiocParallel::register(BiocParallel::SerialParam())
        BPPARAM <- bpparam()
}

message("▶ 5. Start drake pipeline...")
n_job   <- ifelse(arguments$parallel > 1, yes = 6, no = 1)
n_retry <- 3

make(dplan,
     lock_envir = TRUE,
     parallelism = "future",
     recover = FALSE,
     memory_strategy = "lookahead",
     garbage_collection = TRUE,
     retries = n_retry,
     jobs = n_job
)