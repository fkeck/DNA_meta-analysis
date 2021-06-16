
##### Figure 2 Map #####


figure_2_left_panel <- map_data("world") %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "#595959", colour = "#595959") +
  geom_point(aes(x = lon, y = lat, fill = group_target_simple), shape = 21, size = 2, color = "white",
             data = filter(dat, !is.na(lon), !is.na(lat))) +
  theme_void() +
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  scale_fill_organism() +
  coord_map(xlim = c(-180, 180), ylim = c(-50, 70))

# figure_2_lower_ribbon <- dat %>%
#   select(biome_1_simple,
#          eDNA_origin,
#          taxonomic_level_simple,
#          marker_simple,
#          sequencing_technology_simple) %>% 
#   pivot_longer(everything()) %>% 
#   group_by(name, value) %>% 
#   count() %>% 
#   left_join(variables_fancy, by = c("name" = "col_names")) %>% 
#   mutate(fancy_names = factor(fancy_names,
#                               levels = c("Biome",
#                                          "Taxonomic level",
#                                          "DNA marker",
#                                          "DNA origin",
#                                          "Sequencing technology"))
#   ) %>% 
#   ggplot() +
#   geom_col(aes(fct_reorder(value, n), n)) +
#   facet_wrap(~fancy_names, nrow = 1, scales = "free") +
#   xlab("") +
#   coord_flip()


figure_2_right_panel <- dat %>%
  select(group_target_simple) %>% 
  group_by(group_target_simple) %>% 
  count() %>% 
  ggplot() +
  geom_col(aes(fct_reorder(group_target_simple, n), n, fill = group_target_simple)) +
  xlab("") +
  coord_flip() +
  scale_fill_organism() +
  theme(legend.position = "none")


figure_2_lower_biome <- dat %>%
  select(biome_1_simple) %>% 
  group_by(biome_1_simple) %>% 
  count() %>% 
  ggplot() +
  geom_col(aes(fct_reorder(biome_1_simple, n), n)) +
  xlab("") +
  coord_flip() +
  scale_fill_organism() +
  scale_y_continuous(breaks = c(0, 30, 60, 90, 120)) +
  theme(legend.position = "none")

figure_2_lower_taxo <- dat %>%
  select(taxonomic_level_simple) %>% 
  group_by(taxonomic_level_simple) %>% 
  count() %>% 
  ggplot() +
  geom_col(aes(fct_reorder(taxonomic_level_simple, n), n)) +
  xlab("") +
  coord_flip() +
  scale_fill_organism() +
  theme(legend.position = "none")

figure_2_lower_marker <- dat %>%
  select(marker_simple) %>% 
  group_by(marker_simple) %>% 
  count() %>% 
  ggplot() +
  geom_col(aes(fct_reorder(marker_simple, n), n)) +
  xlab("") +
  coord_flip() +
  scale_fill_organism() +
  theme(legend.position = "none")

figure_2_lower_origin <- dat %>%
  select(eDNA_origin) %>% 
  group_by(eDNA_origin) %>% 
  count() %>% 
  ggplot() +
  geom_col(aes(fct_reorder(eDNA_origin, n), n)) +
  xlab("") +
  coord_flip() +
  scale_fill_organism() +
  theme(legend.position = "none")

figure_2_lower_seqtech <- dat %>%
  select(sequencing_technology_simple) %>% 
  group_by(sequencing_technology_simple) %>% 
  count() %>% 
  ggplot() +
  geom_col(aes(fct_reorder(sequencing_technology_simple, n), n)) +
  xlab("") +
  coord_flip() +
  scale_fill_organism() +
  theme(legend.position = "none")


# figure_2_empty_plot <- ggplot()


# (figure_2_left_panel + figure_2_right_panel) / figure_2_lower_ribbon + 
#   plot_layout(widths = c(NA, 1), heights = c(4, 1))

figure_2_lower_ribbon <- (
  figure_2_lower_biome +
    figure_2_lower_taxo +
    figure_2_lower_marker +
    figure_2_lower_origin +
    figure_2_lower_seqtech) +
  plot_layout(nrow = 1, ncol = 5)

figure_2_top_ribbon <- (figure_2_left_panel + figure_2_right_panel) +
  plot_layout(widths = c(3, 1))

figure_2 <- 
  figure_2_top_ribbon / figure_2_lower_ribbon +
  plot_annotation(tag_levels = 'A') +
  plot_layout(heights = c(5, 2))

#PDF 10 x 5.8




##### Figure 3 log-ratio ######

figure_3 <- 
  dat %>% 
  filter(group_target_simple != "Others") %>% 
  select(group_target_simple,
         ratio_N_taxa_trad_eDNA,
         ratio_alpha_mean_trad_eDNA) %>% 
  pivot_longer(ratio_N_taxa_trad_eDNA:ratio_alpha_mean_trad_eDNA, names_to = "diversity") %>% 
  set_order_factor() %>% 
  mutate(diversity = str_replace(diversity, "Alpha \\(mean\\)", "Alpha diversity")) %>% 
  mutate(diversity = str_replace(diversity, "Gamma", "Gamma diversity")) %>% 
  mutate(diversity = factor(diversity, levels = c("Gamma diversity",
                                                  "Alpha diversity"))) %>% 
  ggplot(aes(log(value))) +
  geom_histogram(aes(y = ..density..)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  geom_density(aes(color = group_target_simple), size = 0.7, bw = 0.4, show.legend = FALSE) +
  stat_density(aes(color = group_target_simple), geom = "line", position = "identity", size = NA) +
  facet_grid(cols = vars(diversity)) +
  theme(legend.position = "bottom") +
  labs(color = "") +
  guides(colour = guide_legend(override.aes = list(size = 0.7))) +
  xlab("log-ratio") +
  scale_color_organism()

#PDF 6.7 x 4


##### Figure 4 fractions ######


figure_4_gamma <- map(c("Microorganisms", "Macroinvertebrates", "Fish"), function(x) {
  dat %>%
    filter(taxonomic_level_simple == "Species",
           group_target_simple == x) %>% 
    select(prop_N_taxa_trad_only,
           prop_N_taxa_eDNA_only,
           prop_N_taxa_both) %>% 
    mutate(grp_obs = seq_len(nrow(.))) %>%
    filter(!is.na(prop_N_taxa_trad_only),
           !is.na(prop_N_taxa_eDNA_only),
           !is.na(prop_N_taxa_both)) %>% 
    pivot_longer(prop_N_taxa_trad_only:prop_N_taxa_both, names_to = "fraction") %>% 
    set_order_factor() %>% 
    ggplot() +
    geom_boxplot(aes(fraction, value, fill = fraction), outlier.shape = NA) +
    geom_line(aes(as.numeric(as.factor(fraction)), value, group = grp_obs), alpha = 0.2) +
    stat_summary(aes(x = 2, y = 1), fun.data = "give_n", geom = "label", size = 2) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = NULL, y = "Proportion", fill = "Fraction") +
    color_fraction() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    coord_cartesian(ylim = c(0, 1), clip = "off")
})


figure_4_alpha <- map(c("Microorganisms", "Macroinvertebrates", "Fish"), function(x) {
  dat %>%
    filter(taxonomic_level_simple == "Species",
           group_target_simple == x) %>% 
    select(prop_alpha_mean_trad_only,
           prop_alpha_mean_eDNA_only,
           prop_alpha_mean_both) %>% 
    mutate(grp_obs = seq_len(nrow(.))) %>%
    filter(!is.na(prop_alpha_mean_trad_only),
           !is.na(prop_alpha_mean_eDNA_only),
           !is.na(prop_alpha_mean_both)) %>% 
    pivot_longer(prop_alpha_mean_trad_only:prop_alpha_mean_both, names_to = "fraction") %>% 
    set_order_factor() %>% 
    ggplot() +
    geom_boxplot(aes(fraction, value, fill = fraction), outlier.shape = NA) +
    geom_line(aes(as.numeric(as.factor(fraction)), value, group = grp_obs), alpha = 0.2) +
    stat_summary(aes(x = 2, y = 1), fun.data = "give_n", geom = "label", size = 2) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = NULL, y = "Proportion", fill = "Fraction") +
    color_fraction() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    coord_cartesian(ylim = c(0, 1), clip = "off")
  
})


figure_4 <-
  (figure_4_gamma[[1]] + figure_4_gamma[[2]] + figure_4_gamma[[3]]) /
  (figure_4_alpha[[1]] + figure_4_alpha[[2]] + figure_4_alpha[[3]]) & theme(legend.position = "bottom")

figure_4 <-
  figure_4 +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')


#PDF 6.7 x 5

##### Table 1 List of included studies ######

papers %>%
  filter(study_ID %in% unique(dat$study_ID)) %>%
  mutate(short_citation = paste0(
    str_remove(Author, ",.*"),
    " et al. (",
    `Publication Year`,
    ")"
  )) %>% 
  left_join(dat %>% 
              select(study_ID, group_target_simple) %>% 
              distinct(), by = "study_ID") %>% 
  group_by(group_target_simple) %>% 
  summarise(short_citation = paste0(sort(short_citation), collapse = ", ")) %>% 
  write_csv("documents/table_list_refs.csv")
