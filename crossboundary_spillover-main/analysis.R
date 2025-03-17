######## Analyze MATLAB Outputs ######
#### Code for analyzing matlab outputs into R dataframes #####
##### for Becca and Taran spillover project #######
###### date created: 3-17-2025 ###########
######### date last modified: 3-17-2025 #######
rm(list = ls())

###### Load required packages ######
library(tidyverse)

###### Load combined outputs ######
animal <- read.csv("animal_outputs.csv")
plant <- read.csv("plant_outputs.csv")


## version 1 is both, version 2 is serpentine, version 3 is non-serpentine

###### Explore Plants ##########


# Create the site_year column by extracting the site, year, and summer (if applicable)
plant <- plant %>%
  mutate(site_year = gsub("^P_([A-Za-z0-9_]+)_([0-9]{4})(_summer)?_.*$", "\\1_\\2\\3", source_dataframe))


# Summarize sums for each response variable by source_dataframe and version
plant_summary <- plant %>%
  group_by(source_dataframe, site_year, version) %>%
  summarise(across(c("extinct_level_P_run1", "initial_plant_abundance_run1", "plant_abundance_run1", 
                     "reward_abundance_run1", "sum_pol_run1", "visit_quanity_run1", "visit_quality_run1", 
                     "foraging_effort_run1", "extinct_level_P_run2", "initial_plant_abundance_run2", 
                     "plant_abundance_run2", "reward_abundance_run2", "sum_pol_run2", "visit_quanity_run2",
                     "visit_quality_run2", "foraging_effort_run2"), 
                   sum, na.rm = TRUE))

## each dot is a unique site/year combo

ggplot(data = plant_summary) +
  geom_point(mapping = aes(x = version, y = plant_abundance_run2)) + theme_classic() + facet_wrap(~site_year)
## NS (3) has less plants 

ggplot(data = plant_summary) +
  geom_point(mapping = aes(x = version, y = reward_abundance_run1)) + theme_classic() + facet_wrap(~site_year) + facet_wrap(~site_year)

ggplot(data = plant_summary) +
  geom_point(mapping = aes(x = version, y = extinct_level_P_run1)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = plant_summary) +
  geom_point(mapping = aes(x = version, y = sum_pol_run1)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = plant_summary) +
  geom_point(mapping = aes(x = version, y = visit_quanity_run1)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = plant_summary) +
  geom_point(mapping = aes(x = version, y = visit_quality_run1)) + theme_classic() + facet_wrap(~site_year)

plant$version <- as.factor(plant$version)
ggplot(data = plant) +
  geom_point(mapping = aes(x = version, y = plant_abundance_run1, color = site_year)) + theme_classic() + facet_wrap(~PLANT) + theme(legend.position = "none")

###### TRFU #############
# Summarize sums for each response variable by source_dataframe and version
TRFU <- plant %>% filter(PLANT == "TRFU") %>% 
  group_by(source_dataframe, site_year, version) %>%
  summarise(across(c("extinct_level_P_run1", "initial_plant_abundance_run1", "plant_abundance_run1", 
                     "reward_abundance_run1", "sum_pol_run1", "visit_quanity_run1", "visit_quality_run1", 
                     "foraging_effort_run1", "extinct_level_P_run2", "initial_plant_abundance_run2", 
                     "plant_abundance_run2", "reward_abundance_run2", "sum_pol_run2", "visit_quanity_run2",
                     "visit_quality_run2", "foraging_effort_run2"), 
                   sum, na.rm = TRUE)) %>% filter(version != 3)

## each dot is a unique site/year combo
TRFU$version <- as.factor(TRFU$version)
ggplot(data = TRFU) +
  geom_point(mapping = aes(x = version, y = plant_abundance_run1)) + theme_classic() + facet_wrap(~site_year)
## NS (3) has less plants 

ggplot(data = TRFU) +
  geom_point(mapping = aes(x = version, y = reward_abundance_run1)) + theme_classic() + facet_wrap(~site_year) + facet_wrap(~site_year)

ggplot(data = TRFU) +
  geom_point(mapping = aes(x = version, y = extinct_level_P_run1)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = TRFU) +
  geom_point(mapping = aes(x = version, y = sum_pol_run1)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = TRFU) +
  geom_point(mapping = aes(x = version, y = visit_quanity_run1)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = TRFU) +
  geom_point(mapping = aes(x = version, y = visit_quality_run1)) + theme_classic() + facet_wrap(~site_year)


###### LACA #############
# Summarize sums for each response variable by source_dataframe and version
LACA <- plant %>% filter(PLANT == "LACA") %>% 
  group_by(source_dataframe, site_year, version) %>%
  summarise(across(c("extinct_level_P_run1", "initial_plant_abundance_run1", "plant_abundance_run1", 
                     "reward_abundance_run1", "sum_pol_run1", "visit_quanity_run1", "visit_quality_run1", 
                     "foraging_effort_run1", "extinct_level_P_run2", "initial_plant_abundance_run2", 
                     "plant_abundance_run2", "reward_abundance_run2", "sum_pol_run2", "visit_quanity_run2",
                     "visit_quality_run2", "foraging_effort_run2"), 
                   sum, na.rm = TRUE)) %>% filter(version != 3)

## each dot is a unique site/year combo
LACA$version <- as.factor(LACA$version)
ggplot(data = LACA) +
  geom_point(mapping = aes(x = version, y = plant_abundance_run1)) + theme_classic() + facet_wrap(~site_year)
## NS (3) has less plants 

ggplot(data = LACA) +
  geom_point(mapping = aes(x = version, y = reward_abundance_run1)) + theme_classic() + facet_wrap(~site_year) + facet_wrap(~site_year)

ggplot(data = LACA) +
  geom_point(mapping = aes(x = version, y = extinct_level_P_run1)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = LACA) +
  geom_point(mapping = aes(x = version, y = sum_pol_run1)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = LACA) +
  geom_point(mapping = aes(x = version, y = visit_quanity_run1)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = LACA) +
  geom_point(mapping = aes(x = version, y = visit_quality_run1)) + theme_classic() + facet_wrap(~site_year)

###### VIVI #############
# Summarize sums for each response variable by source_dataframe and version
VIVI <- plant %>% filter(PLANT == "VIVI") %>% 
  group_by(source_dataframe, site_year, version) %>%
  summarise(across(c("extinct_level_P_run1", "initial_plant_abundance_run1", "plant_abundance_run1", 
                     "reward_abundance_run1", "sum_pol_run1", "visit_quanity_run1", "visit_quality_run1", 
                     "foraging_effort_run1", "extinct_level_P_run2", "initial_plant_abundance_run2", 
                     "plant_abundance_run2", "reward_abundance_run2", "sum_pol_run2", "visit_quanity_run2",
                     "visit_quality_run2", "foraging_effort_run2"), 
                   sum, na.rm = TRUE)) %>% filter(version != 2)

## each dot is a unique site/year combo
VIVI$version <- as.factor(VIVI$version)
ggplot(data = VIVI) +
  geom_point(mapping = aes(x = version, y = plant_abundance_run1)) + theme_classic() + facet_wrap(~site_year)
## NS (3) has less plants 

ggplot(data = VIVI) +
  geom_point(mapping = aes(x = version, y = reward_abundance_run1)) + theme_classic() + facet_wrap(~site_year) + facet_wrap(~site_year)

ggplot(data = VIVI) +
  geom_point(mapping = aes(x = version, y = extinct_level_P_run1)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = VIVI) +
  geom_point(mapping = aes(x = version, y = sum_pol_run1)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = VIVI) +
  geom_point(mapping = aes(x = version, y = visit_quanity_run1)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = VIVI) +
  geom_point(mapping = aes(x = version, y = visit_quality_run1)) + theme_classic() + facet_wrap(~site_year)

###### HEEX #############
# Summarize sums for each response variable by source_dataframe and version
HEEX <- plant %>% filter(PLANT == "HEEX") %>% 
  group_by(source_dataframe, site_year, version) %>%
  summarise(across(c("extinct_level_P_run1", "initial_plant_abundance_run1", "plant_abundance_run1", 
                     "reward_abundance_run1", "sum_pol_run1", "visit_quanity_run1", "visit_quality_run1", 
                     "foraging_effort_run1", "extinct_level_P_run2", "initial_plant_abundance_run2", 
                     "plant_abundance_run2", "reward_abundance_run2", "sum_pol_run2", "visit_quanity_run2",
                     "visit_quality_run2", "foraging_effort_run2"), 
                   sum, na.rm = TRUE)) %>% filter(version != 3)

## each dot is a unique site/year combo
HEEX$version <- as.factor(HEEX$version)
ggplot(data = HEEX) +
  geom_point(mapping = aes(x = version, y = plant_abundance_run1)) + theme_classic() + facet_wrap(~site_year)
## NS (3) has less plants 

ggplot(data = HEEX) +
  geom_point(mapping = aes(x = version, y = reward_abundance_run1)) + theme_classic() + facet_wrap(~site_year) + facet_wrap(~site_year)

ggplot(data = HEEX) +
  geom_point(mapping = aes(x = version, y = extinct_level_P_run1)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = HEEX) +
  geom_point(mapping = aes(x = version, y = sum_pol_run1)) + theme_classic() + facet_wrap(~site_year, scales = "free")

ggplot(data = HEEX) +
  geom_point(mapping = aes(x = version, y = visit_quanity_run1)) + theme_classic() + facet_wrap(~site_year, scales = "free")

ggplot(data = HEEX) +
  geom_point(mapping = aes(x = version, y = visit_quality_run1)) + theme_classic() + facet_wrap(~site_year)

###### CESO #############
# Summarize sums for each response variable by source_dataframe and version
CESO <- plant %>% filter(PLANT == "CESO") %>% 
  group_by(source_dataframe, site_year, version) %>%
  summarise(across(c("extinct_level_P_run1", "initial_plant_abundance_run1", "plant_abundance_run1", 
                     "reward_abundance_run1", "sum_pol_run1", "visit_quanity_run1", "visit_quality_run1", 
                     "foraging_effort_run1", "extinct_level_P_run2", "initial_plant_abundance_run2", 
                     "plant_abundance_run2", "reward_abundance_run2", "sum_pol_run2", "visit_quanity_run2",
                     "visit_quality_run2", "foraging_effort_run2"), 
                   sum, na.rm = TRUE)) %>% filter(version != 2)

## each dot is a unique site/year combo
CESO$version <- as.factor(CESO$version)
ggplot(data = CESO) +
  geom_point(mapping = aes(x = version, y = plant_abundance_run1)) + theme_classic() + facet_wrap(~site_year)
## NS (3) has less plants 

ggplot(data = CESO) +
  geom_point(mapping = aes(x = version, y = reward_abundance_run1)) + theme_classic() + facet_wrap(~site_year) + facet_wrap(~site_year)

ggplot(data = CESO) +
  geom_point(mapping = aes(x = version, y = extinct_level_P_run1)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = CESO) +
  geom_point(mapping = aes(x = version, y = sum_pol_run1)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = CESO) +
  geom_point(mapping = aes(x = version, y = visit_quanity_run1)) + theme_classic() + facet_wrap(~site_year)

ggplot(data = CESO) +
  geom_point(mapping = aes(x = version, y = visit_quality_run1)) + theme_classic() + facet_wrap(~site_year)

####### Explore animal responses ######
# Create the site_year column by extracting the site, year, and summer (if applicable)
animal <- animal %>%
  mutate(site_year = gsub("^A_([A-Za-z0-9_]+)_([0-9]{4})(_summer)?_.*$", "\\1_\\2\\3", source_dataframe))


# Summarize sums for each response variable by source_dataframe and version
animal$version <- as.factor(animal$version)

animal_summary <- animal %>%
  group_by(source_dataframe, site_year, version) %>%
  summarise(across(c("extinct_level_A_run1", "animal_abundance_run1", "sum_extract_run1", 
                     "extinct_level_A_run2", "animal_abundance_run2", "sum_extract_run2"), 
                   sum, na.rm = TRUE))

## each dot is a unique site/year combo

ggplot(data = animal_summary) +
  geom_point(mapping = aes(x = version, y = animal_abundance_run2)) + theme_classic() + facet_wrap(~site_year, scales = "free")

ggplot(data = animal_summary) +
  geom_point(mapping = aes(x = version, y =sum_extract_run1)) + theme_classic() + facet_wrap(~site_year, scales = "free")

#ggplot(data = animal) +
 # geom_point(mapping = aes(x = version, y = animal_abundance_run1, color = site_year)) + theme_classic() + facet_wrap(~ARTH, scales = "free") + theme(legend.position = "none")

#### APME #######
APME <- animal %>% filter(ARTH == "APME") 


ggplot(data = APME) +
  geom_point(mapping = aes(x = version, y = animal_abundance_run1)) + theme_classic() + facet_wrap(~site_year)
ggplot(data = APME) +
  geom_point(mapping = aes(x = version, y = sum_extract_run1)) + theme_classic() + facet_wrap(~site_year)



## other pollinators
animal %>% filter(ARTH == "BOVO") %>% ggplot() +
  geom_point(mapping = aes(x = version, y = animal_abundance_run1)) + theme_classic() + facet_wrap(~site_year)




