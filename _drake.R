#!/usr/bin/env Rscript
options(future.globals.maxSize = 1024^2*2000)

# arguments = commandArgs(trailingOnly=TRUE)
arguments <- c(
        "-i",
        "~/Documents/drakepipeline/C103ConfigMinimal.xlsx",
        "-o",
        "./Result",
        "-s",
        "Parameters",
        "-p",
        "30"
)

suppressPackageStartupMessages(library("optparse"))

message(" -  Parsing arguments...")
optionList <- list(
        optparse::make_option(c("-i", "--input"), action = "store",
                              default = "./Config.xlsx", help = "the path to the project"),
        optparse::make_option(c("-o", "--output"), action = "store",
                              default = "./Result", help = "the directory for result storage"),
        optparse::make_option(c("-s", "--sheet"), action = "store",
                              default = "Parameters", help = "the sheet contains parameters"),
        optparse::make_option(c("-p", "--parallel"), action = "store",
                              default = 2, help = "number of core to use for constructing drake graph", type = "integer")
)

arguments <- optparse::parse_args(optparse::OptionParser(option_list = optionList), args = arguments)

# Load packages. ----------------------------------------------------------
source("./packages.R")

# Setting up parallel computing env
if (arguments$parallel > 1) {
        message("Set up parallel computing environment...")
        arguments$parallel <- min(arguments$parallel, future::availableCores())
        message(sprintf("-  Using %d core/thread", arguments$parallel))
        
        
        # setting up future plan
        message("   Setting future...")
        future::plan(list(tweak(multisession, workers = 10),
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
        DRAKE_CACHE <- dirname(arguments$input)
        usethis::use_directory(str_c(path.expand(DRAKE_CACHE), "/.drake"))
}

# DFAULT_CONFIG_PATH    = "./DefaultParameters/defaultConfig.xlsx"
# XCMS_PARS_BOUNDS_PATH = "./DefaultParameters/xcmsParUbLb.csv"
# Load functions ----------------------------------------------------------
for (srp in str_subset(list.files("./R", full.names = T, recursive = T), "\\.R$")) {
        source(srp)
}

# purrr::walk(grepv("\\.R$", list.files("./R", full.names = TRUE, recursive = T)), source)

# _drake.R must end with a call to drake_config().
# The arguments to drake_config() are basically the same as those to make().
# lock_envir allows functions that alter the random seed to be used. The biggest
# culprits of this seem to be interactive graphics e.g. plotly and mapdeck.
drake_config(dplan,
             lock_envir  = TRUE,
             parallelism = "future",
             log_progress = TRUE,
             recover = FALSE,
             memory_strategy =  "lookahead",
             garbage_collection = TRUE,
             jobs = arguments$parallel)
