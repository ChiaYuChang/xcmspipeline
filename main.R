#!/usr/bin/env Rscript
options(future.globals.maxSize = 1024^2*2000)

DRAKE_PIPELINE_PATH = "/home/cychang/Documents/drakepipeline"

message("Start XCMS pipeLine")

arguments = commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages(library("optparse"))

message(" -  Parsing arguments...")
optionList <- list(
        optparse::make_option(c("-i", "--input"), action = "store", type = "character",
                              default = "./config.xlsx", help = "the path to the project"),
        optparse::make_option(c("-o", "--output"), action = "store", type = "character",
                              default = "./Result", help = "the directory for result storage"),
        optparse::make_option(c("-s", "--sheet"), action = "store", type = "character",
                              default = "Parameters", help = "the sheet contains parameters"),
        optparse::make_option(c("-p", "--parallel"), action = "store", type = "integer",
                              default = 2, help = "number of core to use for constructing drake graph"),
        optparse::make_option(c("-c", "--clean"), action = "store", type = "logical",
                              default = FALSE, help = "whether clean the cache"),
        optparse::make_option(c("-u", "--unlock"), action = "store", type = "logical",
                              default = FALSE, help = "whether unlock the cache")
)

arguments <- optparse::parse_args(optparse::OptionParser(option_list = optionList), 
                                  args = arguments)

usethis::proj_set(normalizePath("."), force = TRUE)
usethis::proj_activate(normalizePath("."))
arguments$input <- normalizePath(arguments$input)
usethis::use_directory(arguments$output)

if (arguments$unlock) {
        drake::drake_cache("./.drake/")$unlock()
        message("The cache has been unlocked.")
}

if (arguments$clean) {
        drake::clean()
        message("The cache has been removed.")
}

# Load packages. ----------------------------------------------------------
source(stringr::str_c(DRAKE_PIPELINE_PATH, "packages.R", sep = "/"))

# Setting up parallel computing env
if (arguments$parallel > 1) {
        message("Set up parallel computing environment...")
        arguments$parallel <- min(arguments$parallel, future::availableCores())
        message(sprintf(" -  Using %d core/thread", arguments$parallel))
        
        
        # setting up future plan
        message("   Setting future...")
        future::plan(list(tweak(multisession, workers = 20),
                          tweak(multisession, workers = arguments$parallel),
                          tweak(multisession, workers = arguments$parallel)))
        
        message("   Setting BiocParallel...")
        BiocParallel::register(BiocParallel.FutureParam::FutureParam())
} else {
        future::plan(sequential)
}

# Checking the existence --------------------------------------------------
message(" -  Checking whether the file exist...")
if (!file.exists(arguments$input)) {
        stop(sprintf("File %s does not exist.", arguments$input))
} else {
        arguments$input  <- path.expand(arguments$input)
        DRAKE_CACHE      <- dirname(arguments$input)
        usethis::use_directory(".drake")
}

DFAULT_CONFIG_PATH    <- str_c(DRAKE_PIPELINE_PATH, "DefaultParameters/defaultConfig.xlsx", sep = "/")
XCMS_PARS_BOUNDS_PATH <- str_c(DRAKE_PIPELINE_PATH, "DefaultParameters/xcmsParUbLb.csv", sep = "/")

# Load functions ----------------------------------------------------------
for (srp in str_subset(list.files(str_c(DRAKE_PIPELINE_PATH, "R", sep = "/"), full.names = T, recursive = T), "\\.R$")) {
        source(srp)
}

make(dplan,
     lock_envir = TRUE,
     parallelism = "future",
     recover = FALSE,
     memory_strategy = "lookahead",
     garbage_collection = TRUE,
     jobs = arguments$parallel)
