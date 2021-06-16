
# GAMMMA diversity

plots_frac <- list()


# Raw data
plots_frac$gamma_raw <-
  map(c("biome_1_simple", "eDNA_origin", "group_target_simple"),
    function(var) {
      
      dat %>%
        select(.data[[var]],
               taxonomic_level_simple,
               N_taxa_trad_only,
               N_taxa_eDNA_only,
               N_taxa_both) %>% 
        mutate(grp_obs = seq_len(nrow(dat))) %>% 
        filter(!is.na(N_taxa_trad_only),
               !is.na(N_taxa_eDNA_only),
               !is.na(N_taxa_both)) %>% 
        pivot_longer(N_taxa_trad_only:N_taxa_both, names_to = "fraction") %>% 
        mutate(value = sqrt(value)) %>% 
        set_order_factor() %>% 
        ggplot() +
        geom_boxplot(aes(as.numeric(as.factor(fraction)), value, fill = fraction), outlier.shape = NA) +
        geom_line(aes(as.numeric(as.factor(fraction)), value, group = grp_obs), alpha = 0.2) +
        stat_summary(aes(x = 2, y = 0), fun.data = "give_n", geom = "text", fun = length) +
        facet_grid(rows = vars(.data[[var]]), cols = vars(taxonomic_level_simple)) +
        scale_x_continuous(breaks = 1:3, minor_breaks = NULL,
                           labels = c("Traditional only", "Both", "DNA only")) +
        labs(x = NULL, y = expression( sqrt("Number of taxa")),
             title = paste("Gamma diversity", var, sep = " by ")) +
        color_fraction() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        theme(panel.spacing = unit(2, "lines"))
    })


# Standardized (proportion) data

plots_frac$gamma_prop <-
  map(c("biome_1_simple", "eDNA_origin", "group_target_simple"),
    function(var) {
      
      dat %>%
        select(.data[[var]],
               taxonomic_level_simple,
               prop_N_taxa_trad_only,
               prop_N_taxa_eDNA_only,
               prop_N_taxa_both) %>% 
        mutate(grp_obs = seq_len(nrow(dat))) %>%
        filter(!is.na(prop_N_taxa_trad_only),
               !is.na(prop_N_taxa_eDNA_only),
               !is.na(prop_N_taxa_both)) %>% 
        pivot_longer(prop_N_taxa_trad_only:prop_N_taxa_both, names_to = "fraction") %>% 
        set_order_factor() %>% 
        ggplot() +
        geom_boxplot(aes(as.numeric(as.factor(fraction)), value, fill = fraction), outlier.shape = NA) +
        geom_line(aes(as.numeric(as.factor(fraction)), value, group = grp_obs), alpha = 0.2) +
        stat_summary(aes(x = 2, y = 0), fun.data = "give_n", geom = "text", fun = length) +
        facet_grid(rows = vars(.data[[var]]), cols = vars(taxonomic_level_simple)) +
        scale_x_continuous(breaks = 1:3, minor_breaks = NULL,
                           labels = c("Traditional only", "Both", "DNA only")) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = NULL, y = "Proportion",
             title = paste("Gamma diversity (proportions)", var, sep = " by ")) +
        color_fraction() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        theme(panel.spacing.x = unit(2, "lines"),
              panel.spacing.y = unit(1, "lines"))
    })



# Testing Gamma

frac_mod <- list()

frac_mod$mod_gamma <-
  dat %>%
  select(study_ID,
         group_target_simple,
         taxonomic_level_simple,
         prop_N_taxa_trad_only,
         prop_N_taxa_eDNA_only,
         prop_N_taxa_both) %>%
  mutate(grp_obs = seq_len(nrow(dat))) %>%
  filter(!is.na(prop_N_taxa_trad_only),
         !is.na(prop_N_taxa_eDNA_only),
         !is.na(prop_N_taxa_both),
         taxonomic_level_simple == "Species",
         group_target_simple != "Others") %>%
  pivot_longer(prop_N_taxa_trad_only:prop_N_taxa_both, names_to = "fraction") %>%
  mutate(value = effectsize::normalize(value, include_bounds = FALSE)) %>%
  set_order_factor() %>%
  glmmTMB(value ~ group_target_simple * fraction +(1|study_ID/grp_obs),
          data = ., family = beta_family(link = "logit"))

summary(frac_mod$mod_gamma)
car::Anova(frac_mod$mod_gamma)

frac_mod$mod_gamma_emmeans <- emmeans(frac_mod$mod_gamma, specs = ~group_target_simple * fraction)

pwpm(frac_mod$mod_gamma_emmeans)
pairs(frac_mod$mod_gamma_emmeans)




# ALPHA
# Raw data
plots_frac$alpha_raw <-
map(c("biome_1_simple", "eDNA_origin", "group_target_simple"),
    function(var) {
      dat %>%
        select(.data[[var]],
               taxonomic_level_simple,
               alpha_mean_trad_only,
               alpha_mean_eDNA_only,
               alpha_mean_both) %>% 
        mutate(grp_obs = seq_len(nrow(dat))) %>% 
        filter(!is.na(alpha_mean_trad_only),
               !is.na(alpha_mean_eDNA_only),
               !is.na(alpha_mean_both)) %>% 
        pivot_longer(alpha_mean_trad_only:alpha_mean_both, names_to = "fraction") %>% 
        mutate(value = sqrt(value)) %>% 
        set_order_factor() %>% 
        ggplot() +
        geom_boxplot(aes(as.numeric(as.factor(fraction)), value, fill = fraction), outlier.shape = NA) +
        geom_line(aes(as.numeric(as.factor(fraction)), value, group = grp_obs), alpha = 0.2) +
        stat_summary(aes(x = 2, y = 0), fun.data = "give_n", geom = "text", fun = length) +
        facet_grid(rows = vars(.data[[var]]), cols = vars(taxonomic_level_simple)) +
        scale_x_continuous(breaks = 1:3, minor_breaks = NULL,
                           labels = c("Traditional only", "Both", "DNA only")) +
        labs(x = NULL, y = expression( sqrt("Number of taxa")),
             title = paste("Alpha diversity", var, sep = " by ")) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        color_fraction()
    })



# Standardized (proportion) data
plots_frac$alpha_prop <-
  map(c("biome_1_simple", "eDNA_origin", "group_target_simple"),
    function(var) {
      
      dat %>%
        select(.data[[var]],
               taxonomic_level_simple,
               prop_alpha_mean_trad_only,
               prop_alpha_mean_eDNA_only,
               prop_alpha_mean_both) %>% 
        mutate(grp_obs = seq_len(nrow(dat))) %>% 
        filter(!is.na(prop_alpha_mean_trad_only),
               !is.na(prop_alpha_mean_eDNA_only),
               !is.na(prop_alpha_mean_both)) %>% 
        pivot_longer(prop_alpha_mean_trad_only:prop_alpha_mean_both, names_to = "fraction") %>% 
        set_order_factor() %>% 
        ggplot() +
        geom_boxplot(aes(as.numeric(as.factor(fraction)), value, fill = fraction), outlier.shape = NA) +
        geom_line(aes(as.numeric(as.factor(fraction)), value, group = grp_obs), alpha = 0.2) +
        stat_summary(aes(x = 2, y = 0), fun.data = "give_n", geom = "text", fun = length) +
        facet_grid(rows = vars(.data[[var]]), cols = vars(taxonomic_level_simple)) +
        scale_x_continuous(breaks = 1:3, minor_breaks = NULL,
                           labels = c("Traditional only", "Both", "DNA only")) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = NULL, y = "Proportion",
             title = paste("Alpha diversity (proportions)", var, sep = " by ")) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        color_fraction()
    })


# Testing Alpha

frac_mod$mod_alpha <-
  dat %>%
  select(study_ID,
         group_target_simple,
         taxonomic_level_simple,
         prop_alpha_mean_trad_only,
         prop_alpha_mean_eDNA_only,
         prop_alpha_mean_both) %>%
  mutate(grp_obs = seq_len(nrow(dat))) %>%
  filter(!is.na(prop_alpha_mean_trad_only),
         !is.na(prop_alpha_mean_eDNA_only),
         !is.na(prop_alpha_mean_both),
         taxonomic_level_simple == "Species",
         group_target_simple != "Others") %>%
  pivot_longer(prop_alpha_mean_trad_only:prop_alpha_mean_both, names_to = "fraction") %>%
  mutate(value = effectsize::normalize(value, include_bounds = FALSE)) %>%
  set_order_factor() %>%
  glmmTMB(value ~ group_target_simple * fraction +(1|study_ID/grp_obs),
          data = ., family = beta_family(link = "logit"))

summary(frac_mod$mod_alpha)
car::Anova(frac_mod$mod_alpha)

frac_mod$mod_alpha_emmeans <- emmeans(frac_mod$mod_alpha, specs = ~group_target_simple * fraction)

pwpm(frac_mod$mod_alpha_emmeans)
pairs(frac_mod$mod_alpha_emmeans)




# 
# # TERNARY PLOTS
# 
# # Be careful: ggtern changes ggplot settings
# library(ggtern)
# 
# # Gamma
# dat %>%
#   filter(taxonomic_level_simple == "Species",
#          group_target_simple != "Others") %>% 
#   set_order_factor() %>% 
#   ggtern(aes(x = N_taxa_trad_only, y = N_taxa_both, z = N_taxa_eDNA_only)) +
#   geom_point(aes(color = group_target_simple)) +
#   geom_confidence_tern(aes(colour = group_target_simple), breaks = 0.8) +
#   scale_shape_manual(values = 21) +
#   theme_bw() +
#   theme(tern.panel.mask.show = FALSE, legend.position = c(0, 1),
#         legend.justification = c(0, 1), legend.box.just = "left",
#         legend.background = element_rect(fill = "transparent"),
#         tern.axis.arrow.show = TRUE,
#         tern.axis.arrow.text = element_text(size = 12),
#         tern.axis.arrow.L = element_line(colour = color_trad),
#         tern.axis.arrow.text.L  = element_text(colour = color_trad),
#         tern.axis.arrow.R = element_line(colour = color_edna),
#         tern.axis.arrow.text.R  = element_text(colour = color_edna),
#         tern.axis.arrow.T = element_line(colour = color_both),
#         tern.axis.arrow.text.T  = element_text(colour = color_both),
#         tern.axis.title.show = FALSE) +
#   labs(x = "Traditional only", y = "Both", z = "DNA only",
#        title = "Gamma diversity (proportions)",
#        subtitle = "Species level only")
# 
# 
# 
# # Alpha
# dat %>%
#   filter(taxonomic_level_simple == "Species",
#          group_target_simple != "Others") %>% 
#   set_order_factor() %>% 
#   ggtern(aes(x = alpha_mean_trad_only, y = alpha_mean_both, z = alpha_mean_eDNA_only)) +
#   geom_point(aes(color = group_target_simple)) +
#   geom_confidence_tern(aes(colour = group_target_simple), breaks = 0.8) +
#   scale_shape_manual(values = 21) +
#   theme_bw() +
#   theme(tern.panel.mask.show = FALSE, legend.position = c(0, 1),
#         legend.justification = c(0, 1), legend.box.just = "left",
#         legend.background = element_rect(fill = "transparent"),
#         tern.axis.arrow.show = TRUE,
#         tern.axis.arrow.text = element_text(size = 12),
#         tern.axis.arrow.L = element_line(colour = color_trad),
#         tern.axis.arrow.text.L  = element_text(colour = color_trad),
#         tern.axis.arrow.R = element_line(colour = color_edna),
#         tern.axis.arrow.text.R  = element_text(colour = color_edna),
#         tern.axis.arrow.T = element_line(colour = color_both),
#         tern.axis.arrow.text.T  = element_text(colour = color_both),
#         tern.axis.title.show = FALSE) +
#   labs(x = "Traditional only", y = "Both", z = "DNA only",
#        title = "Alpha diversity (proportions)",
#        subtitle = "Species level only")
# 
# 

