
# Code and data for the paper metabarcoding/traditional method meta-analysis

[![DOI](https://zenodo.org/badge/XXXXXX.svg)](https://zenodo.org/badge/latestdoi/XXXXX)

This repository provides R code and data to reproduce results and figures from the paper:

Keck, F. et al. XXXXX


## Analyses

Scripts can be found in the `/R` directory. They are organized in separated modules but all the analyses can be ran from the master script `RUN_main.R`.
Once the analyses are completed, figures can be generated from `R/MS_figures.R`.
Supplementary materials can be generated from `Rmd/supplementary_info.Rmd`.

## Dependencies

Install R packages from CRAN:

    install.packages(c("DataExplorer", "scales", "effectsize", "car",
    		       "tidyverse", "patchwork", "glmmTMB", "emmeans",
    		       "janitor", "sf", "tmap", "rmarkdown"))

The session info with version of each packages used for the analyses is available in `session_info.txt`.

