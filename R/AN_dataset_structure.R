# Data exploration

DataExplorer::introduce(dat)

# Number of studies
dat$study_ID %>% unique() %>% length()

# Number of comparison for alpha
sum(!is.na(dat$ratio_alpha_mean_trad_eDNA))

# Number of comparison for alpha (with the both fraction)
sum(!is.na(dat$alpha_mean_total))

# Number of comparison for gamma
sum(!is.na(dat$N_taxa_total))

DataExplorer::plot_missing(dat, list("B1" = -1, "Missing values" = 1)) + theme(legend.position = "none")
DataExplorer::plot_bar(dat, ncol = 4, nrow = 9)
DataExplorer::plot_histogram(dat, ncol = 4, nrow = 8)

dat %>%
  select(biome_1,
         biome_2,
         group_target,
         taxonomic_level,
         marker,
         sequencing_technology) %>% 
  DataExplorer::plot_bar(nrow = 3, ncol = 2)



