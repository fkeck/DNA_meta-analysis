
# Gamma diversity (both)
dat %>% 
  filter(taxonomic_level_simple == "Species", group_target_simple != "Others") %>% 
  left_join(ref_meta, by = "study_ID") %>% 
  set_order_factor %>% 
  ggplot(aes(publication_year, prop_N_taxa_both)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(rows = vars(group_target_simple)) +
  labs(title = "Gamma diversity (intersection fraction)")


# Alpha diversity (both)
dat %>% 
  filter(taxonomic_level_simple == "Species", group_target_simple != "Others") %>% 
  left_join(ref_meta, by = "study_ID") %>% 
  set_order_factor %>% 
  ggplot(aes(publication_year, prop_alpha_mean_both)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(rows = vars(group_target_simple)) +
  labs(title = "Alpha diversity (intersection fraction)")



# Gamma diversity (both + eDNA)
dat %>% 
  filter(taxonomic_level_simple == "Species", group_target_simple != "Others") %>% 
  left_join(ref_meta, by = "study_ID") %>% 
  set_order_factor() %>% 
  ggplot(aes(publication_year, prop_N_taxa_both + prop_N_taxa_eDNA_only)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(rows = vars(group_target_simple)) +
  labs(title = "Gamma diversity (all DNA)")


# Alpha diversity (both + eDNA)
dat %>% 
  filter(taxonomic_level_simple == "Species", group_target_simple != "Others") %>% 
  left_join(ref_meta, by = "study_ID") %>% 
  set_order_factor() %>% 
  ggplot(aes(publication_year, prop_alpha_mean_both + prop_alpha_mean_eDNA_only)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(rows = vars(group_target_simple)) +
  labs(title = "Alpha diversity (all DNA)")



# Alpha
dat %>% 
  filter(taxonomic_level_simple == "Species", group_target_simple == "Macroinvertebrates") %>% 
  left_join(ref_meta, by = "study_ID") %>% 
  set_order_factor() %>% 
  mutate(value = effectsize::normalize(prop_N_taxa_both + prop_N_taxa_eDNA_only,
                                       include_bounds = FALSE),
         year_scaled = scale(publication_year)) %>% 
  glmmTMB(value ~ year_scaled + (1|study_ID),
          data = ., family = beta_family(link = "logit")) %>% 
  summary()


# Gamma
dat %>% 
  filter(taxonomic_level_simple == "Species", group_target_simple == "Fish") %>% 
  left_join(ref_meta, by = "study_ID") %>% 
  set_order_factor() %>% 
  mutate(value = effectsize::normalize(prop_N_taxa_both + prop_N_taxa_eDNA_only,
                                       include_bounds = FALSE),
         year_scaled = scale(publication_year)) %>% 
  glmmTMB(value ~ year_scaled + (1|study_ID),
          data = ., family = beta_family(link = "logit")) %>% 
  summary()

# Alpha
dat %>% 
  filter(taxonomic_level_simple == "Species", group_target_simple == "Microorganisms") %>% 
  left_join(ref_meta, by = "study_ID") %>% 
  set_order_factor() %>% 
  mutate(value = effectsize::normalize(prop_alpha_mean_both + prop_alpha_mean_eDNA_only,
                                       include_bounds = FALSE),
         year_scaled = scale(publication_year)) %>% 
  glmmTMB(value ~ year_scaled + (1|study_ID),
          data = ., family = beta_family(link = "logit")) %>% 
  summary()
