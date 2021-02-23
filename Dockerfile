FROM r-base

LABEL maintainer="Chiayu Chang" 

RUN apt-get update && apt-get install -y \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libnetcdf-dev \
    libudunits2-dev \
    libgdal-dev \
    libmagick++-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libfribidi-dev
RUN apt-get clean

RUN R -e "install.packages(c('remotes', 'pacman') , dependencies = T, repos = 'http://cran.us.r-project.org')"

# Imported functions
RUN R -e "pacman::p_install(package = c('optparse', 'usethis'))"

# Package manager
RUN R -e "pacman::p_install(package = c('BiocManager'))"
# RUN R -e "install.packages('pacman'     , dependencies = T, repos = 'http://cran.us.r-project.org')"

# Drake pipeline
RUN R -e "pacman::p_install(package = c('conflicted', 'dotenv', 'drake'))"

# Data manipulation
RUN R -e "pacman::p_install(package = c('data.table', 'wrapr', 'stringr'))"
# RUN R -e "install.packages('tidytable', dependencies = T, repos = 'http://cran.us.r-project.org')"

# Figure drawing
RUN R -e "pacman::p_install(package = c('ggplot2'))"
# RUN R -e "install.packages('ggthemes', dependencies = T, repos = 'http://cran.us.r-project.org')"

# Files I/O
RUN R -e "pacman::p_install(package = c('rio', 'openxlsx', 'fst', 'readxl', 'jsonlite'))"

# Parallel computing backend and tools
RUN R -e "pacman::p_install(package = c('future', 'future.apply', 'fst', 'readxl', 'jsonlite'))"
RUN R -e "BiocManager::install('BiocParallel')"
RUN R -e "remotes::install_github('HenrikBengtsson/BiocParallel.FutureParam', ref='master')"
# RUN R -e "install.packages('purrr', dependencies = T, repos = 'http://cran.us.r-project.org')"

# MS data preprocessing
RUN R -e "BiocManager::install('Autotuner')"
RUN R -e "BiocManager::install('xcms')"

# RUN mkdir tmp
COPY pipeline.tar.gz /tmp
RUN tar zxvf /tmp/pipeline.tar.gz
RUN rm /tmp/pipeline.tar.gz
RUN chmod +x /pipeline/drakepipeline.R
RUN ln -s /pipeline/drakepipeline.R /bin/drakepipeline
RUN mkdir /project
RUN cd /project

# RUN R -e "pacman::p_install(package = c('dplyr', 'tidyr'))"