
####### Explore networks by Plant Species  #######
## organize network info in networks_input.R first

TRFU_full <- left_join(TRFU, spring, by = "site_year") #check how rows work out
TRFU_close <- left_join(TRFU, spring_close, by = "site_year")
TRFU_between <- left_join(TRFU, spring_between, by = "site_year")
TRFU_jaccard <- left_join(TRFU, spring_jaccard, by = "site_year")
TRFU_sorensen <- left_join(TRFU, spring_sorensen, by = "site_year")

TRFU_full %>% filter(metric == "VIVI_on_TRFU")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = visit_quanity, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

TRFU_full %>% filter(metric == "TRFU_on_VIVI")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = visit_quanity, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)


TRFU_full %>% filter(metric == "Ind_Contribution_TRFU")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

TRFU_full %>% filter(metric == "Ind_Contribution_VIVI")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

TRFU_close  %>% ggplot() +
  geom_point(mapping = aes(x = VIVI, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

TRFU_between  %>% ggplot() +
  geom_point(mapping = aes(x = VIVI, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

TRFU_jaccard  %>% ggplot() +
  geom_point(mapping = aes(x = Jaccard_Value, y = visit_quanity, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

TRFU_sorensen  %>% ggplot() +
  geom_point(mapping = aes(x = Sorensen_Value, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

##### Vetch

VIVI_full <- left_join(VIVI, spring, by = "site_year") #check how rows work out
VIVI_close <- left_join(VIVI, spring_close, by = "site_year")
VIVI_between <- left_join(VIVI, spring_between, by = "site_year")
VIVI_jaccard <- left_join(VIVI, spring_jaccard, by = "site_year")
VIVI_sorensen <- left_join(VIVI, spring_sorensen, by = "site_year")

VIVI_full %>% filter(metric == "VIVI_on_TRFU")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

VIVI_full %>% filter(metric == "TRFU_on_VIVI")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)


VIVI_full %>% filter(metric == "Ind_Contribution_TRFU")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

VIVI_full %>% filter(metric == "Ind_Contribution_VIVI")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

VIVI_close  %>% ggplot() +
  geom_point(mapping = aes(x = VIVI, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

VIVI_between  %>% ggplot() +
  geom_point(mapping = aes(x = VIVI, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

VIVI_jaccard  %>% ggplot() +
  geom_point(mapping = aes(x = Jaccard_Value, y = visit_quanity, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

VIVI_sorensen  %>% ggplot() +
  geom_point(mapping = aes(x = Sorensen_Value, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

########## LACA

LACA_full <- left_join(LACA, spring, by = "site_year") #check how rows work out
LACA_close <- left_join(LACA, spring_close, by = "site_year")
LACA_between <- left_join(LACA, spring_between, by = "site_year")
LACA_jaccard <- left_join(LACA, spring_jaccard, by = "site_year")
LACA_sorensen <- left_join(LACA, spring_sorensen, by = "site_year")

LACA_full %>% filter(metric == "VIVI_on_TRFU")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

LACA_full %>% filter(metric == "TRFU_on_VIVI")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = visit_quanity, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)


LACA_full %>% filter(metric == "Ind_Contribution_TRFU")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

LACA_full %>% filter(metric == "Ind_Contribution_VIVI")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

LACA_close  %>% ggplot() +
  geom_point(mapping = aes(x = VIVI, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

LACA_between  %>% ggplot() +
  geom_point(mapping = aes(x = VIVI, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

LACA_jaccard  %>% ggplot() +
  geom_point(mapping = aes(x = Jaccard_Value, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

LACA_sorensen  %>% ggplot() +
  geom_point(mapping = aes(x = Sorensen_Value, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)
####### HEEX

HEEX_full <- left_join(HEEX, summer, by = "site_year") #check how rows work out
HEEX_close <- left_join(HEEX, summer_close, by = "site_year")
HEEX_between <- left_join(HEEX, summer_between, by = "site_year")
HEEX_jaccard <- left_join(HEEX, summer_jaccard, by = "site_year")
HEEX_sorensen <- left_join(HEEX, summer_sorensen, by = "site_year")

HEEX_full %>% filter(metric == "CESO_on_HEEX")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

HEEX_full %>% filter(metric == "CESO_on_HEEX")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)


HEEX_full %>% filter(metric == "Ind_Contribution_HEEX")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

HEEX_full %>% filter(metric == "Ind_Contribution_CESO")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

HEEX_close  %>% ggplot() +
  geom_point(mapping = aes(x = CESO, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

HEEX_between  %>% ggplot() +
  geom_point(mapping = aes(x = HEEX, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

HEEX_jaccard  %>% ggplot() +
  geom_point(mapping = aes(x = Jaccard_Value, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

HEEX_sorensen  %>% ggplot() +
  geom_point(mapping = aes(x = Sorensen_Value, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

######## CESO

CESO_full <- left_join(CESO, summer, by = "site_year") #check how rows work out
CESO_close <- left_join(CESO, summer_close, by = "site_year")
CESO_between <- left_join(CESO, summer_between, by = "site_year")
CESO_jaccard <- left_join(CESO, summer_jaccard, by = "site_year")
CESO_sorensen <- left_join(CESO, summer_sorensen, by = "site_year")

CESO_full %>% filter(metric == "CESO_on_HEEX")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

CESO_full %>% filter(metric == "HEEX_on_CESO")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)


CESO_full %>% filter(metric == "Ind_Contribution_HEEX")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

CESO_full %>% filter(metric == "Ind_Contribution_CESO")  %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

CESO_close  %>% ggplot() +
  geom_point(mapping = aes(x = HEEX, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

CESO_between  %>% ggplot() +
  geom_point(mapping = aes(x = HEEX, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

CESO_jaccard  %>% ggplot() +
  geom_point(mapping = aes(x = Jaccard_Value, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

CESO_sorensen  %>% ggplot() +
  geom_point(mapping = aes(x = Sorensen_Value, y = reward_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version)

###### Explore Networks--Network Level properties ######
network_wide <- network %>%
  select(metric, site_year, raw_value) %>%
  pivot_wider(names_from = metric, values_from = raw_value)

plant_long <- left_join(plant_long, network_wide, by = "site_year")
plant_summary <- left_join(plant_summary, network_wide, by = "site_year")



### overall plant responses 
plant_summary %>%  ggplot() +
  geom_point(mapping = aes(x = NODF, y = visit_quanity, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version) 

plant_summary %>% ggplot() +
  geom_point(mapping = aes(x = `weighted NODF`, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version) 

plant_summary %>% ggplot() +
  geom_point(mapping = aes(x = NODFc, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version) 

plant_summary %>% ggplot() +
  geom_point(mapping = aes(x = niche.overlap.HL, y = sum_pol, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version) 

plant_summary %>% ggplot() +
  geom_point(mapping = aes(x = niche.overlap.LL, y = visit_quanity, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version) 

##### by species

plant_long %>% ggplot() +
  geom_point(mapping = aes(x = NODFc, y = visit_quanity, color = version, shape = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~PLANT, scales = "free") 

plant_long %>% filter(PLANT == "TRFU") %>%  ggplot() +
  geom_point(mapping = aes(x = version, y = visit_quality, color = Ind_Contribution_TRFU, shape = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic()  

plant_long %>%
  filter(PLANT == "TRFU") %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = version, y = visit_quanity, color = AF), 
               alpha = 0.7) + 
  theme_classic()

plant_summary %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = version, y = sum_pol, color = AF), 
               alpha = 0.7) + 
  theme_classic() + geom_jitter(mapping = aes(x = version, y = sum_pol, color = AF), 
                                position = position_jitter(width = 0.2, height = 0), 
                                alpha = 0.2) + 
  theme_classic()

plant_long %>% filter(PLANT == "LACA") %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = version, y = plant_abundance, color = AF), 
               alpha = 0.7) + 
  theme_classic() + geom_jitter(mapping = aes(x = version, y = plant_abundance, color = AF), 
                                position = position_jitter(width = 0.2, height = 0), 
                                alpha = 0.2) + 
  theme_classic()

plant_long %>% filter(PLANT == "CESO") %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = version, y = plant_abundance, color = AF), 
               alpha = 0.7) + 
  theme_classic() + geom_jitter(mapping = aes(x = version, y = plant_abundance, color = AF), 
                                position = position_jitter(width = 0.2, height = 0), 
                                alpha = 0.2) + 
  theme_classic()


plant_long %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = version, y = plant_abundance, color = AF), 
               alpha = 0.7) + 
  theme_classic() + geom_jitter(mapping = aes(x = version, y = plant_abundance, color = AF), 
                                position = position_jitter(width = 0.2, height = 0), 
                                alpha = 0.2) + 
  theme_classic() + facet_wrap(~PLANT)

plant_long %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = version, y = visit_quanity, color = AF), 
               alpha = 0.7) + 
  theme_classic() + geom_jitter(mapping = aes(x = version, y = visit_quanity, color = AF), 
                                position = position_jitter(width = 0.2, height = 0), 
                                alpha = 0.2) + 
  theme_classic() + facet_wrap(~PLANT, scales = "free")

plant_long %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = version, y = visit_quality, color = AF), 
               alpha = 0.7) + 
  theme_classic() + geom_jitter(mapping = aes(x = version, y = visit_quality, color = AF), 
                                position = position_jitter(width = 0.2, height = 0), 
                                alpha = 0.2) + 
  theme_classic() + facet_wrap(~PLANT)

######## Plants by soil type ############
plant_long <- plant_long %>%
  mutate(Soil_Type = case_when(
    PLANT %in% c('ASER', 'VIVI', 'CESO', 'MEIN', 'PHAQ', 'ANAR', 'AMME', 
                 'ERCI', 'mustard', 'yellow_aster', 'dandelion', 'MEPO', 'SEVU') ~ 'Non-Serpentine',
    TRUE ~ 'Serpentine'  # All other plants get 'serpentine'
  ))


## yes share polliantors with NS plants or is a NS plant
## no don't share pollinators with NS plants 
plant_long <- plant_long %>%
  mutate(Overlap = case_when(
    PLANT %in% c("TRFU", "VIVI", "PLER", "ANFI", "LACA", "DICA", "ACBR", "RACA", "AMME", "ESCA",
                 "AGHE", "LUSU", "ERCI", "LUBI", "SIBE", "ERGU", "GICA", "LACH", "TRLA",
                 "LODA", "EUSP", "LUMI", "CRHI", "MIGU", "CARU", "ASJE", "THCA", "URLI",
                 "ASBR", "TRER", "GITR", "AGBR", "MICA", "LOHO", "DEVA", "TRHI", "HIIN",
                 "PLNO", "LUNA", "WYAN", "RILE", "ACWR", "TRBI", "CASAN", "ERCA", "ERLA",
                 "PHIM", "CLPU", "LUNA/LUBI", "DEUL", "Sidalcea_sp.", "PHTA", "MIDO",
                 "LAMI", "LUAL", "ACMO", "Asteraceae_sp.", "RHAR", "CHGL", "URCI", "DEHE",
                 "WYAU", "CAEX", "TOVE", "ALAM", "CALU", "COSP", "ERHI", "TAOF", "SEVU",
                 "TRAL", "MEPO", "GEDI", "CADE1", "GRCA", "ERLU", "ASFA", "HECU", "ERLA",
                 "ASER", "HEEX", "HECO", "ERNU", "CAPA", "PEKE", "CESO", "ACAM", "STAL",
                 "HOMA", "HEAR", "LULU", "LERA", "ZETR", "ESCA", "CAPY", "CLPU", "SAVE",
                 "TRLA", "HOVI", "ACWR", "MEIN", "Clarkia", "CUCA", "VIVI", "ERGU", "PHAQ",
                 "ACMI", "CLGR", "NOMA", "CIVU", "SOAS", "SOCA", "ANAR") ~ "Yes",
    TRUE ~ "No"
  ))



plant_long %>% filter(Soil_Type == "Serpentine") %>% filter(version != "3") %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = version, y = plant_abundance, color = Overlap), 
               alpha = 0.7) + 
  theme_classic() + geom_jitter(mapping = aes(x = version, y = plant_abundance, color = Overlap), 
                                position = position_jitter(width = 0.2, height = 0), 
                                alpha = 0.2) + 
  theme_classic() + facet_wrap(~site_year)

plant_long %>% filter(Soil_Type == "Serpentine") %>% filter(version != "3") %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = version, y = visit_quanity, color = AF), 
               alpha = 0.7) + 
  theme_classic() + geom_jitter(mapping = aes(x = version, y = visit_quanity, color = AF), 
                                position = position_jitter(width = 0.2, height = 0), 
                                alpha = 0.2) + 
  theme_classic() + facet_wrap(~site_year, scales = "free")

plant_long %>% filter(Soil_Type == "Non-Serpentine") %>% filter(version != "2") %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = version, y = plant_abundance, color = AF), 
               alpha = 0.7) + 
  theme_classic() + geom_jitter(mapping = aes(x = version, y = plant_abundance, color = AF), 
                                position = position_jitter(width = 0.2, height = 0), 
                                alpha = 0.2) + 
  theme_classic() + facet_wrap(~site_year)

plant_long %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = version, y = plant_abundance, color = Soil_Type), 
               alpha = 0.7) +
  theme_classic() + facet_wrap(~site_year)

plant_long %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = version, y = visit_quanity, color = Soil_Type), 
               alpha = 0.7) +
  theme_classic() + facet_wrap(~site_year, scales = "free")

plant_long %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = version, y = visit_quality, color = Soil_Type), 
               alpha = 0.7) +
  theme_classic() + facet_wrap(~site_year)

plant_long %>%   filter(AF == "2") %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = Soil_Type, y = visit_quality, color = version), 
               alpha = 0.7) +
  theme_classic() + facet_wrap(~site_year, scales = 'free') +   scale_color_viridis_d()

plant_long %>%   filter(AF == "2") %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = Soil_Type, y = visit_quanity, color = version), 
               alpha = 0.7) +
  theme_classic() + facet_wrap(~site_year, scales = 'free') +   scale_color_viridis_d() 

plant_long %>%   filter(AF == "2") %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = Soil_Type, y = plant_abundance, color = version), 
               alpha = 0.7) +
  theme_classic() + facet_wrap(~site_year) +   scale_color_viridis_d() 


plant_long %>%   filter(AF == "2") %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = Soil_Type, y = initial_plant_abundance, color = version), 
               alpha = 0.7) +
  theme_classic() + facet_wrap(~site_year) +   scale_color_viridis_d() 

plant_long %>% filter(AF == "2") %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = Soil_Type, y = plant_abundance, color = version), 
               alpha = 1) + 
  theme_classic() +   scale_color_viridis_d() 

plant_long %>%  filter(AF == "2") %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = Soil_Type, y = visit_quality, color = version), 
               alpha = 1) + 
  theme_classic() +   scale_color_viridis_d() 

plant_long %>% 
  filter(AF == "2") %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = Soil_Type, y = visit_quanity, color = version), 
               alpha = 1) +   theme_classic() +   
  scale_color_viridis_d() 



#+    coord_cartesian(ylim = c(0, 10000)) 



#+ geom_jitter(mapping = aes(x = version, y = plant_abundance, color = Soil_Type), 
#position = position_jitter(width = 0.2, height = 0), 
# alpha = 0.1) 


library(viridis)

plant_long %>% filter(Soil_Type == "Serpentine") %>% filter(version != "3") %>% 
  ggplot() +
  geom_point(mapping = aes(x = visit_quanity, y = plant_abundance, color = version, shape = Soil_Type), 
             alpha = 0.7) +
  scale_color_viridis_d() +   coord_cartesian(xlim = c(0, 10000), ylim = c(0, 1)) +
  theme_classic() #limits outlier 

plant_long %>% filter(Soil_Type == "Serpentine") %>% filter(version != "3") %>% 
  ggplot() +
  geom_point(mapping = aes(x = visit_quality, y = plant_abundance, color = version, shape = Soil_Type), 
             alpha = 0.7) +
  scale_color_viridis_d() +
  theme_classic()

plant_long %>% filter(Soil_Type == "Non-Serpentine") %>% filter(version != "2") %>% 
  ggplot() +
  geom_point(mapping = aes(x = visit_quality, y = plant_abundance, color = version, shape = Soil_Type), 
             alpha = 0.7) +
  scale_color_viridis_d() +
  theme_classic()

plant_long %>% filter(PLANT == "VIVI") %>% filter(AF == "2") %>% 
  ggplot() +
  geom_point(mapping = aes(x = Ind_Contribution_VIVI, y = visit_quality, color = version), 
             alpha = 0.7) +  geom_smooth(mapping = aes(x = Ind_Contribution_VIVI, y = visit_quality, color = version), 
                                         method = "lm", se = FALSE) +
  scale_color_viridis_d() +
  theme_classic()

plant_long %>% filter(PLANT == "CESO") %>% filter(AF == "2") %>% 
  ggplot() +
  geom_point(mapping = aes(x = Ind_Contribution_CESO, y = visit_quality, color = version), 
             alpha = 0.7) +  geom_smooth(mapping = aes(x = Ind_Contribution_CESO, y = visit_quality, color = version), 
                                         method = "lm", se = FALSE) +
  scale_color_viridis_d() +
  theme_classic()

