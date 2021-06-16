

map(c("biome_1_simple", "eDNA_origin", "group_target_simple"),
    function(var) {
      
      dat %>% 
        select(.data[[var]],
               taxonomic_level_simple,
               ratio_N_taxa_trad_eDNA,
               ratio_alpha_mean_trad_eDNA) %>% 
        pivot_longer(ratio_N_taxa_trad_eDNA:ratio_alpha_mean_trad_eDNA, names_to = "diversity") %>% 
        set_order_factor() %>% 
        ggplot() +
        geom_boxplot(aes(.data[[var]], log(value))) +
        facet_grid(rows = vars(diversity), scales = "free_y") +
        labs(title = var)
    })




map(c("biome_1_simple", "eDNA_origin", "group_target_simple"),
    function(var) {
      
      dat %>% 
        filter(group_target_simple != "Others") %>% 
        select(.data[[var]],
               taxonomic_level_simple,
               ratio_N_taxa_trad_eDNA,
               ratio_alpha_mean_trad_eDNA) %>% 
        pivot_longer(ratio_N_taxa_trad_eDNA:ratio_alpha_mean_trad_eDNA, names_to = "diversity") %>% 
        set_order_factor() %>% 
        ggplot(aes(log(value))) +
        geom_histogram(aes(y = ..density..)) +
        geom_density(bw = 0.3) +
        geom_vline(xintercept = 0, color = 2, linetype = "dashed") +
        facet_grid(cols = vars(diversity), rows = vars(.data[[var]])) +
        labs(title = var)
    })


dat %>% 
  filter(group_target_simple != "Others") %>% 
  select(group_target_simple,
         ratio_N_taxa_trad_eDNA,
         ratio_alpha_mean_trad_eDNA) %>% 
  pivot_longer(ratio_N_taxa_trad_eDNA:ratio_alpha_mean_trad_eDNA, names_to = "diversity") %>% 
  set_order_factor() %>% 
  ggplot(aes(log(value))) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(aes(color = group_target_simple), bw = 0.4) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  facet_grid(cols = vars(diversity)) +
  theme(legend.position = "bottom")


dat %>% 
  select(taxonomic_level_simple,
         ratio_N_taxa_trad_eDNA,
         ratio_alpha_mean_trad_eDNA) %>% 
  pivot_longer(ratio_N_taxa_trad_eDNA:ratio_alpha_mean_trad_eDNA, names_to = "diversity") %>% 
  set_order_factor() %>% 
  ggplot(aes(log(value))) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(aes(color = taxonomic_level_simple), bw = 0.4) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  facet_grid(cols = vars(diversity)) +
  theme(legend.position = "bottom")


# Testing Gamma
LR_mod <- list()

log_summary <- function (x, col) {
  x <- x[, col, drop = TRUE]
  res <- c(mean(log(x), na.rm = TRUE),
           sd(log(x), na.rm = TRUE))
  res <- round(res, 3)
  print(res)
}

LR_mod$mod_gamma_0 <- dat %>% 
  select(study_ID,
         group_target_simple,
         ratio_N_taxa_trad_eDNA,
         ratio_alpha_mean_trad_eDNA) %>% 
  pivot_longer(ratio_N_taxa_trad_eDNA:ratio_alpha_mean_trad_eDNA, names_to = "diversity") %>% 
  set_order_factor() %>% 
  filter(diversity == "Gamma") %T>%
  log_summary("value") %>% 
  glmmTMB(log(value) ~ 1 + (1|study_ID),
          data = ., family = gaussian(link = "identity"))

LR_mod$mod_gamma_0 %>% summary()

LR_mod$mod_gamma_1 <- dat %>% 
  select(study_ID,
         group_target_simple,
         ratio_N_taxa_trad_eDNA,
         ratio_alpha_mean_trad_eDNA) %>% 
  pivot_longer(ratio_N_taxa_trad_eDNA:ratio_alpha_mean_trad_eDNA, names_to = "diversity") %>% 
  set_order_factor() %>% 
  filter(diversity == "Gamma") %T>% 
  log_summary("value") %>% 
  glmmTMB(log(value) ~ group_target_simple + (1|study_ID),
          data = ., family = gaussian(link = "identity"))

LR_mod$mod_gamma_1 %>% summary()
car::Anova(LR_mod$mod_gamma_1)


# Testing Alpha

LR_mod$mod_alpha_0 <- dat %>% 
  select(study_ID,
         group_target_simple,
         ratio_N_taxa_trad_eDNA,
         ratio_alpha_mean_trad_eDNA) %>% 
  pivot_longer(ratio_N_taxa_trad_eDNA:ratio_alpha_mean_trad_eDNA, names_to = "diversity") %>% 
  set_order_factor() %>% 
  filter(diversity == "Alpha (mean)") %T>%
  log_summary("value") %>% 
  glmmTMB(log(value) ~ 1 + (1|study_ID),
          data = ., family = gaussian(link = "identity"))

LR_mod$mod_alpha_0 %>% summary()

LR_mod$mod_alpha_1 <- dat %>% 
  select(study_ID,
         group_target_simple,
         ratio_N_taxa_trad_eDNA,
         ratio_alpha_mean_trad_eDNA) %>% 
  pivot_longer(ratio_N_taxa_trad_eDNA:ratio_alpha_mean_trad_eDNA, names_to = "diversity") %>% 
  set_order_factor() %>% 
  filter(diversity == "Alpha (mean)") %T>%
  log_summary("value") %>% 
  glmmTMB(log(value) ~ group_target_simple + (1|study_ID),
          data = ., family = gaussian(link = "identity"))

LR_mod$mod_alpha_1 %>% summary()
car::Anova(LR_mod$mod_alpha_1)
