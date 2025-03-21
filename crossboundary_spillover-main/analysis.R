######## Analyze MATLAB Outputs ######
#### Code for analyzing matlab outputs into R dataframes #####
##### for Becca and Taran spillover project #######
###### date created: 3-17-2025 ###########
######### date last modified: 3-21-2025 #######
rm(list = ls())

###### Load required packages ######
library(tidyverse)

###### Load combined outputs ######
animal <- read.csv("animal_outputs.csv")
plant <- read.csv("plant_outputs.csv")
network <- read.csv("networks_full.csv") #empirical networks 

## version 1 is both, version 2 is serpentine, version 3 is non-serpentine
## 1 is AF, 2 is no AF 

###### Explore Plants ##########


# Create the site_year column by extracting the site, year, and summer (if applicable)


plant <- plant %>%
  mutate(
    site_year = gsub("^P_([A-Za-z0-9_]+)_([0-9]{4})(_summer)?_.*$", "\\1_\\2\\3", source_dataframe),
    site_year = case_when(
      site_year %in% c("P_Coyote_24_version1", "P_Coyote_24_version2", "P_Coyote_24_version3") ~ "Coyote_24",
      site_year %in% c("P_Pond_202_version1", "P_Pond_202_version2", "P_Pond_202_version3") ~ "Pond_2022",
      TRUE ~ site_year  # Keep all other values unchanged
    )
  )

# View unique site_year values
unique(plant$site_year)

# View unique values
#unique(test[, c("site_year", "version")])




plant_long <- plant %>%
  pivot_longer(
    cols = -c(PLANT, source_dataframe, version, site_year),  
    names_to = c(".value", "AF"),   
    names_pattern = "(.*)_run(\\d+)" 
  ) %>%
  mutate(AF = as.integer(AF))  

head(plant_long)
plant_long$version <- as.factor(plant_long$version)
plant_long$AF <- as.factor(plant_long$AF)


# Summarize sums for each response variable by source_dataframe and version
plant_summary <- plant_long %>%
  group_by(source_dataframe, site_year, version, AF) %>%
  summarise(across(c("extinct_level_P", "initial_plant_abundance", "plant_abundance", 
                     "reward_abundance", "sum_pol", "visit_quanity", "visit_quality", 
                     "foraging_effort"), 
                   sum, na.rm = TRUE))

## each dot is a unique site/year, AF combo

ggplot(data = plant_summary) +
  geom_point(mapping = aes(x = version, y = plant_abundance, color = AF)) + theme_classic() + facet_wrap(~site_year)
## NS (3) has less plants 

ggplot(data = plant_summary) +
  geom_point(mapping = aes(x = version, y = reward_abundance, color = AF)) + theme_classic() + facet_wrap(~site_year) + facet_wrap(~site_year)

ggplot(data = plant_summary) +
  geom_point(mapping = aes(x = version, y = extinct_level_P, color = AF)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = plant_summary) +
  geom_point(mapping = aes(x = version, y = sum_pol, color = AF)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = plant_summary) +
  geom_point(mapping = aes(x = version, y = visit_quanity, color = AF)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = plant_summary) +
  geom_point(mapping = aes(x = version, y = visit_quality, color = AF)) + theme_classic() + facet_wrap(~site_year)


ggplot(data = plant_long) +
  geom_point(mapping = aes(x = version, y = plant_abundance, color = AF)) + theme_classic() + facet_wrap(~PLANT) + theme(legend.position = "none")

ggplot(data = plant_long) +
  geom_point(mapping = aes(x = version, y = reward_abundance, color = AF)) + theme_classic() + facet_wrap(~PLANT) + theme(legend.position = "none")

ggplot(data = plant_long) +
  geom_point(mapping = aes(x = version, y = visit_quanity, color = AF)) + theme_classic() + facet_wrap(~PLANT) + theme(legend.position = "none")

ggplot(data = plant_long) +
  geom_point(mapping = aes(x = version, y = visit_quality, color = AF)) + theme_classic() + facet_wrap(~PLANT) + theme(legend.position = "none")

plant_long %>% filter(PLANT == "HEEX") %>% ggplot() +
  geom_point(mapping = aes(x = version, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7)  + theme_classic() + facet_wrap(~site_year)

plant_long %>% filter(PLANT == "CESO") %>% ggplot() +
  geom_point(mapping = aes(x = AF, y = visit_quanity, color = version), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7)  + theme_classic() + facet_wrap(~site_year)

plant_long %>% 
  filter(PLANT == "CESO") %>% 
  ggplot(aes(x = AF, y = visit_quanity, fill = version)) + 
  geom_boxplot(alpha = 0.7) + 
  theme_classic() 

plant_long %>% 
  filter(PLANT == "CRHI") %>% 
  ggplot(aes(x = AF, y = visit_quanity, color = version)) + geom_point() + 
  theme_classic() + facet_wrap(~site_year)

plant_long <- plant_long %>%
  mutate(Soil_Type = case_when(
    PLANT %in% c('ASER', 'VIVI', 'CESO', 'MEIN', 'PHAQ', 'ANAR', 'AMME', 
                 'ERCI', 'mustard', 'yellow_aster', 'dandelion', 'MEPO', 'SEVU') ~ 'Non-Serpentine',
    TRUE ~ 'Serpentine'  # All other plants get 'serpentine'
  ))


plant_long %>% 
  filter(Soil_Type == "Non-Serpentine") %>% 
  ggplot(aes(x = version, y = visit_quanity, fill = AF)) + 
  geom_boxplot(alpha = 0.7) + 
  theme_classic() + facet_wrap(~PLANT, scales = 
                                 "free")

plant_long %>% 
  filter(Soil_Type == "Non-Serpentine") %>% 
  ggplot(aes(x = AF, y = visit_quanity, fill = version)) + 
  geom_boxplot(alpha = 0.7) + 
  theme_classic() + facet_wrap(~PLANT, scales = 
                                 "free")




###### TRFU #############
# Summarize sums for each response variable by source_dataframe and version
TRFU <- plant_long %>% filter(PLANT == "TRFU") %>% 
  group_by(source_dataframe, site_year, version, AF) %>%
  summarise(across(c("extinct_level_P", "initial_plant_abundance", "plant_abundance", 
                     "reward_abundance", "sum_pol", "visit_quanity", "visit_quality", 
                     "foraging_effort"), 
                   sum, na.rm = TRUE)) %>% filter(version != 3)

## each dot is a unique site/year combo
ggplot(data = TRFU) +
  geom_point(mapping = aes(x = version, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)


ggplot(data = TRFU) +
  geom_point(mapping = aes(x = version, y = reward_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)


ggplot(data = TRFU) +
  geom_point(mapping = aes(x = version, y = sum_pol, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year, scales = "free")


ggplot(data = TRFU) +
  geom_point(mapping = aes(x = version, y = visit_quanity, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year, scales = "free")

ggplot(data = TRFU) +
  geom_point(mapping = aes(x = version, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)

ggplot(data = TRFU) +
  geom_point(mapping = aes(x = visit_quanity, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
theme_classic() + facet_wrap(~version)

###### LACA #############
# Summarize sums for each response variable by source_dataframe and version
LACA <- plant_long %>% filter(PLANT == "LACA") %>% 
  group_by(source_dataframe, site_year, version, AF) %>%
  summarise(across(c("extinct_level_P", "initial_plant_abundance", "plant_abundance", 
                     "reward_abundance", "sum_pol", "visit_quanity", "visit_quality", 
                     "foraging_effort"), 
                   sum, na.rm = TRUE)) %>% filter(version != 3)

## each dot is a unique site/year combo

ggplot(data = LACA) +
  geom_point(mapping = aes(x = version, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)


ggplot(data = LACA) +
  geom_point(mapping = aes(x = version, y = reward_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)




ggplot(data = LACA) +
  geom_point(mapping = aes(x = version, y = sum_pol, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)


ggplot(data = LACA) +
  geom_point(mapping = aes(x = version, y = visit_quanity, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)


ggplot(data = LACA) +
  geom_point(mapping = aes(x = version, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)


ggplot(data = LACA) +
  geom_point(mapping = aes(x = visit_quanity, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() 

ggplot(data = LACA) +
  geom_point(mapping = aes(x = visit_quanity, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + the 
  theme_classic() + facet_wrap(~version)

###### VIVI #############
# Summarize sums for each response variable by source_dataframe and version
VIVI <- plant_long %>% filter(PLANT == "VIVI") %>% 
  group_by(source_dataframe, site_year, version, AF) %>%
  summarise(across(c("extinct_level_P", "initial_plant_abundance", "plant_abundance", 
                     "reward_abundance", "sum_pol", "visit_quanity", "visit_quality", 
                     "foraging_effort"), 
                   sum, na.rm = TRUE)) %>% filter(version != 2)

  ggplot(data = VIVI) +
    geom_point(mapping = aes(x = version, y = plant_abundance, color = AF), 
               position = position_jitter(width = 0.2, height = 0), 
               alpha = 0.7) + 
    theme_classic() + 
    facet_wrap(~site_year)

  ggplot(data = VIVI) +
    geom_point(mapping = aes(x = version, y = reward_abundance, color = AF), 
               position = position_jitter(width = 0.2, height = 0), 
               alpha = 0.7) + 
    theme_classic() + 
    facet_wrap(~site_year)
  
  
  ggplot(data = VIVI) +
    geom_point(mapping = aes(x = version, y = sum_pol, color = AF), 
               position = position_jitter(width = 0.2, height = 0), 
               alpha = 0.7) + 
    theme_classic() + 
    facet_wrap(~site_year)
  
  ggplot(data = VIVI) +
    geom_point(mapping = aes(x = version, y = visit_quanity, color = AF), 
               position = position_jitter(width = 0.2, height = 0), 
               alpha = 0.7) + 
    theme_classic() + 
    facet_wrap(~site_year, scales = "free")
  
  ggplot(data = VIVI) +
    geom_point(mapping = aes(x = version, y = visit_quality, color = AF), 
               position = position_jitter(width = 0.2, height = 0), 
               alpha = 0.7) + 
    theme_classic() + 
    facet_wrap(~site_year)
  
ggplot(data = VIVI) +
  geom_point(mapping = aes(x = visit_quanity, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
theme_classic() + facet_wrap(~version)

###### HEEX #############
# Summarize sums for each response variable by source_dataframe and version
HEEX <- plant_long %>% filter(PLANT == "HEEX") %>% 
  group_by(source_dataframe, site_year, version, AF) %>%
  summarise(across(c("extinct_level_P", "initial_plant_abundance", "plant_abundance", 
                     "reward_abundance", "sum_pol", "visit_quanity", "visit_quality", 
                     "foraging_effort"), 
                   sum, na.rm = TRUE)) %>% filter(version != 3)

ggplot(data = HEEX) +
  geom_point(mapping = aes(x = version, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)

ggplot(data = HEEX) +
  geom_point(mapping = aes(x = version, y = reward_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)

ggplot(data = HEEX) +
  geom_point(mapping = aes(x = version, y = sum_pol, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year, scales =
             "free")

ggplot(data = HEEX) +
  geom_point(mapping = aes(x = version, y = visit_quanity, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year, scales = "free")

ggplot(data = HEEX) +
  geom_point(mapping = aes(x = version, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)

ggplot(data = HEEX) +
  geom_point(mapping = aes(x = visit_quanity, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + facet_wrap(~version)


###### CESO #############
# Summarize sums for each response variable by source_dataframe and version
CESO <- plant_long %>% filter(PLANT == "CESO") %>% 
  group_by(source_dataframe, site_year, version, AF) %>%
  summarise(across(c("extinct_level_P", "initial_plant_abundance", "plant_abundance", 
                     "reward_abundance", "sum_pol", "visit_quanity", "visit_quality", 
                     "foraging_effort"), 
                   sum, na.rm = TRUE)) %>% filter(version != 2)

ggplot(data = CESO) +
  geom_point(mapping = aes(x = version, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)

ggplot(data = CESO) +
  geom_point(mapping = aes(x = version, y = reward_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)

ggplot(data = CESO) +
  geom_point(mapping = aes(x = version, y = sum_pol, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)

ggplot(data = CESO) +
  geom_point(mapping = aes(x = version, y = visit_quanity, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)

ggplot(data = CESO) +
  geom_point(mapping = aes(x = version, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)

ggplot(data = CESO) +
  geom_point(mapping = aes(x = visit_quanity, y = visit_quality, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + facet_wrap(~version)

###### Persistence #####

plant_long %>%
  select(AF, PLANT, version, site_year, extinct_level_P, Soil_Type) %>%
  count(AF, PLANT, version, site_year, Soil_Type, extinct_level_P)

plant_long %>%
  group_by(Soil_Type, version, AF) %>%
  summarise(
    extinct_1_prop = mean(extinct_level_P, na.rm = TRUE),
    count = n()
  ) %>%
  ungroup()

##  AF decreases the proportion of NS and Serp sp that go extinct




####### Explore animal responses ######
# Create the site_year column by extracting the site, year, and summer (if applicable)
animal <- animal %>%
  mutate(
    site_year = gsub("^A_([A-Za-z0-9_]+)_([0-9]{4})(_summer)?_.*$", "\\1_\\2\\3", source_dataframe),
    site_year = case_when(
      site_year %in% c("A_Coyote_24_version1", "A_Coyote_24_version2", "A_Coyote_24_version3") ~ "Coyote_24",
      site_year %in% c("A_Pond_202_version1", "A_Pond_202_version2", "A_Pond_202_version3") ~ "Pond_2022",
      TRUE ~ site_year  # Keep all other values unchanged
    )
  )



animal_long <- animal %>%
  pivot_longer(
    cols = -c(ARTH, source_dataframe, version, site_year),  
    names_to = c(".value", "AF"),   
    names_pattern = "(.*)_run(\\d+)" 
  ) %>%
  mutate(AF = as.integer(AF))  

head(animal_long)
animal_long$version <- as.factor(animal_long$version)
animal_long$AF <- as.factor(animal_long$AF)



animal_summary <- animal_long %>%
  group_by(source_dataframe, site_year, version, AF) %>%
  summarise(across(c("extinct_level_A", "animal_abundance", "sum_extract"), 
                   sum, na.rm = TRUE))

network_wide <- network %>%
  select(metric, site_year, raw_value) %>%
  pivot_wider(names_from = metric, values_from = raw_value)

animal_long <- left_join(animal_long, network_wide, by = "site_year")
animal_summary <- left_join(animal_summary, network_wide, by = "site_year")

## each dot is a unique site/year combo
animal_summary %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = version, y = animal_abundance, color = version), 
               alpha = 0.7) +
  theme_classic() +  scale_color_viridis_d() 

animal_summary %>% filter(AF == "2") %>% 
  ggplot() +
  geom_point(aes(x = NODFc, y = sum_extract, color = version), 
             alpha = 0.7) +   geom_smooth(mapping = aes(x = Ind_Contribution_VIVI, y = sum_extract, color = version), 
                                          method = "lm", se = FALSE) +
  scale_color_viridis_d() + 
  theme_classic()  


animal_long %>%  filter(ARTH %in% c("APME", "BOVO", "BOCA", "BOVO", "LAIN", "HALI_LATI", "ANCA", "EUAC", "EUAL", "ANBA", "ANLE")) %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = version, y = animal_abundance, color = version), 
               alpha = 0.7) +
  theme_classic() +  scale_color_viridis_d() + facet_wrap(~ARTH, scales = "free")

ggplot(data = animal_summary) +
  geom_point(mapping = aes(x = version, y = animal_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year, scales = "free")

ggplot(data = animal_summary) +
  geom_point(mapping = aes(x = version, y = sum_extract, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year, scales = "free")


#### APME #######
APME <- animal_long %>% filter(ARTH == "APME") 

ggplot(data = APME) +
  geom_point(mapping = aes(x = version, y = animal_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year, scales = "free")


ggplot(data = APME) +
  geom_point(mapping = aes(x = version, y = sum_extract, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year, scales = "free")

## other pollinators
animal_long %>% filter(ARTH == "BOVO") %>% ggplot() +
  geom_point(mapping = aes(x = version, y = animal_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)


animal_long %>% filter(ARTH == "BOVO") %>% ggplot() +
  geom_point(mapping = aes(x = version, y = sum_extract, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~site_year)


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



