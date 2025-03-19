######## Analyze MATLAB Outputs ######
#### Code for analyzing matlab outputs into R dataframes #####
##### for Becca and Taran spillover project #######
###### date created: 3-17-2025 ###########
######### date last modified: 3-18-2025 #######
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
  mutate(site_year = gsub("^P_([A-Za-z0-9_]+)_([0-9]{4})(_summer)?_.*$", "\\1_\\2\\3", source_dataframe))



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


####### Explore animal responses ######
# Create the site_year column by extracting the site, year, and summer (if applicable)
animal <- animal %>%
  mutate(site_year = gsub("^A_([A-Za-z0-9_]+)_([0-9]{4})(_summer)?_.*$", "\\1_\\2\\3", source_dataframe))

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

## each dot is a unique site/year combo

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
plant_long <- left_join(plant_long, network, by = "site_year")
plant_summary <- left_join(plant_summary, network, by = "site_year")

### overall plant responses 
plant_summary %>% filter(metric == "NODF") %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = visit_quanity, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version) +  labs(x = "NODF")

plant_summary %>% filter(metric == "weighted NODF") %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version) +  labs(x = "weighted NODF")

plant_summary %>% filter(metric == "niche.overlap.HL") %>% ggplot() +
  geom_point(mapping = aes(x = raw_value, y = sum_pol, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7) + 
  theme_classic() + 
  facet_wrap(~version) +  labs(x = "niche.overlap.HL")
