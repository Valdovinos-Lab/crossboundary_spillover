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



