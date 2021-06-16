source("R/AN_load.R")
source("R/AN_frac_ratio.R")
source("R/AN_frac_proportion.R")

rmarkdown::render("Rmd/supplementary_info.Rmd", output_dir = "documents", output_file = "supplementary_info.pdf")
browseURL("documents/supplementary_info.pdf")


#writeLines(capture.output(sessionInfo()), "session_info.txt")
