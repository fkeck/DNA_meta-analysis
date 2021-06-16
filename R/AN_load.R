library(tidyverse)
library(patchwork)
library(glmmTMB)
library(emmeans)

# Read data

dat <- read_csv("data/data.csv", skip = 1, na = c("", "NA"))
data_dic <- read_csv("data/data_dictionnary.csv", na = c("", "NA"), col_select = 1:2)
papers <- read_csv("data/selected_papers.csv", na = c("", "NA"))

ref_meta <- read_csv("data/references_metadata.csv") %>% 
  janitor::clean_names() %>% 
  rename(study_ID = study_id)

variables_recorded <- colnames(dat)
variables_fancy <- read_csv("data/variables_fancy.csv")


# Column with mixed types are numeric
# Compute mean values for ranges
dat <- dat %>% mutate(across(where(is.list), function(x){
                               x[map_lgl(x, is.null)] <- NA
                               unlist(x) %>%
                               str_split("-|;") %>%
                               map(as.numeric) %>%
                               map_dbl(mean)}
                             )
                      )

# Compute columns for standardized (proportion) gamma diversity
dat <- dat %>%
  mutate(N_taxa_total = N_taxa_trad_only + N_taxa_eDNA_only + N_taxa_both,
         prop_N_taxa_trad_only = N_taxa_trad_only/N_taxa_total,
         prop_N_taxa_eDNA_only = N_taxa_eDNA_only/N_taxa_total,
         prop_N_taxa_both = N_taxa_both/N_taxa_total,
         ratio_N_taxa_trad_eDNA = (N_taxa_eDNA_only + N_taxa_both) / (N_taxa_trad_only + N_taxa_both),
         .after = N_taxa_both)

# Compute extra columns for standardized (proportion) alpha diversity
dat <- dat %>%
  mutate(alpha_mean_total = trad_alpha_mean + DNA_alpha_mean - both_alpha_mean,
         alpha_mean_trad_only = trad_alpha_mean - both_alpha_mean,
         alpha_mean_eDNA_only = DNA_alpha_mean - both_alpha_mean,
         alpha_mean_both = both_alpha_mean,
         prop_alpha_mean_trad_only = (trad_alpha_mean - both_alpha_mean) / alpha_mean_total,
         prop_alpha_mean_eDNA_only = (DNA_alpha_mean - both_alpha_mean) / alpha_mean_total,
         prop_alpha_mean_both = both_alpha_mean / alpha_mean_total,
         ratio_alpha_mean_trad_eDNA = DNA_alpha_mean/trad_alpha_mean,
         .after = both_alpha_max)

# Others :
# vascular plants
# fish, invertebrate & zooplankton
# corals
# metazoan
# plant
# coccolithophore

# Simplify categories (new columns)
dat <- dat %>% 
  mutate(group_target_simple =
           case_when(group_target %in% c("fish") ~ "Fish",
                     group_target %in% c("macroinvertebrates", "EPT", "mosquitos", "oligochaetes", "nematodes", "xenacoelomorpha") ~ "Macroinvertebrates",
                     group_target %in% c("diatoms", "zooplankton", "phytoplankton", "plankton", "cyanobacteria", "protists") ~ "Microorganisms",
                     TRUE ~ "Others"),
         .after = group_target
  ) %>% 
  mutate(biome_1_simple =
           case_when(biome_1 %in% c("marine", "brackish") ~ "Marine",
                     biome_1 %in% c("freshwater") ~ "Freshwater"),
         .after = biome_1
  ) %>% 
  mutate(biome_2_simple =
           case_when(biome_2 %in% c("ditch") ~ "Pond",
                     biome_2 %in% c("pelagic", "open water", "fjord", "coast") ~ "Sea & Ocean",
                     str_detect(biome_2, ";") ~ "Combined biomes",
                     TRUE ~ str_to_sentence(as.character(biome_2))),
         .after = biome_2
  ) %>% 
  mutate(taxonomic_level_simple =
           case_when(taxonomic_level %in% c("mixed", "phylum", "order") ~ "Others",
                     TRUE ~ str_to_sentence(as.character(taxonomic_level))),
         .after = taxonomic_level
  ) %>% 
  mutate(marker_simple =
           case_when(str_detect(marker, ";") ~ "Multi",
                     TRUE ~ as.character(marker)),
         .after = marker
  ) %>% 
  mutate(sequencing_technology_simple =
           case_when(sequencing_technology == "Oxford Nanopore MinION" ~ "ONT MinION",
                     sequencing_technology == "454 pyrosequencing" ~ "454 pyroseq.",
                     str_detect(sequencing_technology, ";") ~ "Multi",
                     TRUE ~ str_to_sentence(as.character(sequencing_technology))),
         .after = sequencing_technology
  ) %>%
  mutate(eDNA_origin = str_to_sentence(eDNA_origin))


# Exclude historical data
dat <- dat %>%
  filter(trad_sampling_method != "historical data")

# This function can be applied to dat before ggplot
# to rename and reorder factors in a better looking way
set_order_factor <- function (x) {
  if(exists("fraction", x)) {
    x$fraction[str_detect(x$fraction, "N_taxa_trad_only|alpha_mean_trad_only")] <- "Traditional only"
    x$fraction[str_detect(x$fraction, "N_taxa_eDNA_only|alpha_mean_eDNA_only")] <- "DNA only"
    x$fraction[str_detect(x$fraction, "N_taxa_both|alpha_mean_both")] <- "Both"
    x$fraction <- factor(x$fraction, levels = c("Traditional only", "Both", "DNA only"))
  }
  
  if(exists("diversity", x)) {
    x$diversity[str_detect(x$diversity, "ratio_alpha_mean_trad_eDNA")] <- "Alpha (mean)"
    x$diversity[str_detect(x$diversity, "ratio_N_taxa_trad_eDNA")] <- "Gamma"
    x$diversity <- factor(x$diversity, levels = c("Alpha (mean)", "Gamma"))
  }
  
  if(exists("group_target_simple", x)) {
    x$group_target_simple <- factor(x$group_target_simple,
                                    levels = c("Microorganisms",
                                               "Macroinvertebrates",
                                               "Fish",
                                               "Others"))
  }
  
  if(exists("taxonomic_level_simple", x)) {
    x$taxonomic_level_simple <- factor(x$taxonomic_level_simple,
                                    levels = c("Species",
                                               "Genus",
                                               "Family",
                                               "Others"))
  }
  
  return(x)
}


# Function to compute stats for ggplots
give_n <- function(x){
  c(x = 2, y = -0.05, label = length(x)/3)
}


# Setup ggplot global theme
theme_set(theme_minimal())

# Darjeeling Limited theme: dark green/yellow/red 
color_trad <- "#317878"
color_edna <- "#d75b66"
color_both <- "#efba48"

# Rosie's theme light: 
# color_trad <- "#f0e442"
# color_edna <- "#56b3e6"
# color_both <- "#009e73"

# Rosie's theme dark:
# color_trad <- "#f0e442"
# color_edna <- "#0073b3"
# color_both <- "#009e73"




color_fraction <- function() {
  scale_fill_manual(values = c(color_trad,
                               color_both,
                               color_edna))
}


# Colorbrewer
color_micro <- "#7fc97f"
color_macro <- "#beaed4"
color_vertb <- "#fdc086"
color_other <- "#595959"
color_other <- "#C0C0C0"

# Triad
color_micro <- "#71C020"
color_macro <- "#2071C0"
color_vertb <- "#C02071"
color_other <- "#595959"
color_other <- "#C0C0C0"

# Triad
color_micro <- "#9BC52C"
color_macro <- "#629DDD"
color_vertb <- "#DE7F2F"
color_other <- "#595959"
color_other <- "#C0C0C0"


scale_color_organism <- function() {
  scale_color_manual(values = c(color_micro,
                               color_macro,
                               color_vertb,
                               color_other),
                    breaks = c("Microorganisms",
                               "Macroinvertebrates",
                               "Fish",
                               "Others"))
}


scale_fill_organism <- function() {
  scale_fill_manual(values = c(color_micro,
                               color_macro,
                               color_vertb,
                               color_other),
                    breaks = c("Microorganisms",
                               "Macroinvertebrates",
                               "Fish",
                               "Others"))
}


