
p1 <- dat %>%
  filter(taxonomic_level_simple == "Species",
         group_target_simple != "Others") %>% 
  select(group_target_simple,
         prop_N_taxa_trad_only,
         prop_N_taxa_eDNA_only,
         prop_N_taxa_both) %>% 
  mutate(grp_obs = seq_len(length(group_target_simple))) %>%
  filter(!is.na(prop_N_taxa_trad_only),
         !is.na(prop_N_taxa_eDNA_only),
         !is.na(prop_N_taxa_both)) %>% 
  pivot_longer(prop_N_taxa_trad_only:prop_N_taxa_both, names_to = "fraction") %>% 
  set_order_factor() %>% 
  ggplot() +
  geom_boxplot(aes(as.numeric(as.factor(fraction)), value, fill = fraction), outlier.shape = NA) +
  geom_line(aes(as.numeric(as.factor(fraction)), value, group = grp_obs), alpha = 0.2) +
  stat_summary(aes(x = 2, y = 0), fun.data = "give_n", geom = "text", fun = length) +
  facet_grid(cols = vars(group_target_simple)) +
  scale_x_continuous(breaks = 1:3, minor_breaks = NULL,
                     labels = c("Traditional only", "Both", "DNA only")) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(x = NULL, y = "Proportion",
       fill = "Fraction",
       title = "Gamma diversity") +
  color_fraction() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(1, "lines")) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())



p2 <- dat %>%
  filter(taxonomic_level_simple == "Species",
         group_target_simple != "Others") %>% 
  select(group_target_simple,
         prop_alpha_mean_trad_only,
         prop_alpha_mean_eDNA_only,
         prop_alpha_mean_both) %>% 
  mutate(grp_obs = seq_len(length(group_target_simple))) %>%
  filter(!is.na(prop_alpha_mean_trad_only),
         !is.na(prop_alpha_mean_eDNA_only),
         !is.na(prop_alpha_mean_both)) %>% 
  pivot_longer(prop_alpha_mean_trad_only:prop_alpha_mean_both, names_to = "fraction") %>% 
  set_order_factor() %>% 
  ggplot() +
  geom_boxplot(aes(as.numeric(as.factor(fraction)), value, fill = fraction), outlier.shape = NA) +
  geom_line(aes(as.numeric(as.factor(fraction)), value, group = grp_obs), alpha = 0.2) +
  stat_summary(aes(x = 2, y = 0), fun.data = "give_n", geom = "text", fun = length) +
  facet_grid(cols = vars(group_target_simple)) +
  scale_x_continuous(breaks = 1:3, minor_breaks = NULL,
                     labels = c("Traditional only", "Both", "DNA only")) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(x = NULL, y = "Proportion",
       fill = "Fraction",
       title = "Alpha diversity") +
  color_fraction() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(1, "lines")) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


p3 <- p1/p2 & theme(legend.position = "right")
p3 +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')
