#' Checking whether given packages were installed
#' @param libs a character vector contains package names
#' @return a boolean vector
.is_pkg_installed <- function(libs) {
        fun <- function(package_name) {
                package_name %in% libs
        }
        return(fun)
}
is_pkg_installed <- .is_pkg_installed(rownames(installed.packages()))

if (!is_pkg_installed("optparse")) stop("Package optparse should be installed.")
if (!is_pkg_installed("usethis"))  stop("Package usethis should be installed.")
if (!is_pkg_installed("readxl"))   stop("Package usethis should be installed.")

suppressMessages(suppressPackageStartupMessages({
        # drake pipeline
        library(conflicted, quietly = T) # resolving name conflict between packages
        library(dotenv, quietly = T)     # enviorment variables
        library(drake, quietly = T)      # pipeline constructing

        # libraries for data manipulation
        library(data.table, quietly = T) # an extention of data.frame
        # library(tidytable, quietly = T)  # dplyr liked
        library(wrapr, quietly = T)      # dot pipe (%.>%) and lambda
        library(stringr, quietly = T)    # string data manipulation                        
        library(jsonlite, quietly = T)
        
        # libraies for MS data preporcessing
        library(xcms, quietly = T)       # peaks detection, signals alignment, peaks grouping
        library(Autotuner, quietly = T)  # xcms parameters selection

        # libraries for data visualization
        library(ggplot2, quietly = T)    # plotting
        # library(ggthemes, quietly = T)   # theme for plotting                        

        # libraries for I/O
        library(rio, quietly = T)        # read and write files              
        library(openxlsx, quietly = T)   # read Excel files
        # library(readxl, quietly = T)

        # libraries for parallel computing
        library(future, quietly = T)
        library(BiocParallel.FutureParam, quietly = T)
        # library(doFuture)
        library(BiocParallel, quietly = T)
        # library(doFuture, quietly = T)
        # library(globals, quietly = T)                        

        # libraries for functional programming
        library(future.apply, quietly = T)
        # library(purrr, quietly = T)
        # library(foreach, quietly = T)
        # library(iterators, quietly = T)

}))

# Resolving conflict
conflict_prefer("paste", "BiocGenerics")
conflict_prefer("trimws", "MSnbase")
conflict_prefer(":=", "data.table")

# autoInstallDependentPkgs <- function() {
#         message(" -  Checking dependency")
#         dpndtPkgs <-  c("pacman", "optparse", "stringr", "drake", "data.table", "conflicted", "doFuture",
#                         "tidytable", "ggplot2", "wrapr", "xcms", "Autotuner", "rio", "dotenv",
#                         "future", "future.apply", "purrr", "openxlsx", "BiocManager", "future.callr")
#         notInstalledPkgs <- setdiff(dpndtPkgs, installed.packages()[, 1])
        
#         if (length(notInstalledPkgs) > 0) {
#                 warning(" -  Installing dependency")
#                 message(sprintf("\tTry to install %s to default directory", paste(notInstalledPkgs, collapse = ", ")))
#                 if (!requireNamespace("pacman", quietly = TRUE)) {
#                         install.packages("pacman")
#                 }
                
#                 if (!requireNamespace("BiocManager", quietly = TRUE)) {
#                         install.packages("BiocManager")
#                 }
                
#                 for(pkg in notInstalledPkgs) {
#                         pacman::p_install(package = pkg, character.only = T, try.bioconductor = TRUE)
#                 }  
#                 notInstalledPkgs <- setdiff(dpndtPkgs, installed.packages()[, 1])
                
#                 if (length(notInstalledPkgs) > 0) {
#                         if ("ncdf4" %in% notInstalledPkgs) {
#                                 warning(
# 'Problem occured while installing the"ncdf4" package.
#  Please first install "libnetcdf-dev" before installing "ncdf4."
#  1) Download the sourse of the package from http://cirrus.ucsd.edu/~pierce/ncdf/
#  2) Check up path to nc-config
#  3) Install by the following command
#  \tsudo R CMD INSTALL ncdf4_1.13.tar.gz --configure-args="--with-nc-config=<path to nc-config>/nc-config"')
#                         }
#                         warning(sprintf("%s cannot be installed. Please try to install manually", notInstalledPkgs))
#                         return(FALSE)
#                 } else {
#                         message("All dependencies have be installed\n")
#                         return(TRUE)
#                 }
#         } else {
#                 return(TRUE)
#         }
# }

# if (autoInstallDependentPkgs()) {
#         message(" -  Importing libraries...")
#         suppressMessages({
#                 # Import libraries
#                 # pipeline tools
#                 suppressPackageStartupMessages({
#                         library(conflicted, quietly = T)
#                         library(dotenv, quietly = T)
#                         library(drake, quietly = T)
                        
#                         # libraries for data manipulation
#                         library(data.table, quietly = T) # an extention of data.frame
#                         # library(tidytable, quietly = T)  # dplyr liked
#                         library(wrapr, quietly = T)      # dot pipe (%.>%) and lambda
#                         library(stringr, quietly = T)    # string data manipulation
                        
#                         # libraies for MS data preporcessing
#                         library(xcms, quietly = T) # peaks detection, signals alignment, peaks grouping
#                         suppressPackageStartupMessages(
#                                 library(Autotuner, quietly = T) # xcms parameters selection
#                         ) 
                        
#                         # libraries for data visualization
#                         library(ggplot2, quietly = T)   # plotting
#                         library(ggthemes, quietly = T)  # theme for plotting
                        
#                         # libraries for I/O
#                         library(rio, quietly = T)
                        
#                         # libraries for parallel computing
#                         library(future, quietly = T)
#                         library(BiocParallel.FutureParam, quietly = T)
#                         library(BiocParallel, quietly = T)
#                         # library(doFuture)
#                         # library(doFuture, quietly = T)
#                         # library(globals, quietly = T)
                        
#                         # libraries for functional programming
#                         library(future.apply, quietly = T)
#                         # library(purrr, quietly = T)
#                         # library(foreach, quietly = T)
#                         # library(iterators, quietly = T)
#                 })
#         })
# } else {
#         stop("Please install all dependencies.\n")
# }
