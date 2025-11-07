######## Analyze MATLAB Outputs ######
#### Code for analyzing matlab outputs into R dataframes #####
##### for Becca and Taran spillover project #######
###### date created: 3-17-2025 ###########
######### date last modified: 8-4-2025 #######
###### analyze simulation and empirical data #######
rm(list = ls())

###### Load required packages ######
library(tidyverse)

###### Load combined outputs ######
#simulation output tables
animal <- read.csv("output/animal_outputs.csv")
plant <- read.csv("output/plant_outputs.csv")

net_metrics <- read.csv("empirical_summaries/empirical_network_metrics.csv")
empirical_div <- read.csv("empirical_summaries/pollinator_diversity_empirical.csv")
empirical_sp <- read.csv("empirical_summaries/pollinator_properties_by_soil.csv")
## empirical data summaries


#network <- read.csv("networks_full.csv") #empirical networks from outdated file

## version 1 is both, version 2 is serpentine, version 3 is non-serpentine
## 1 is AF, 2 is no AF 

###### Explore Plants ##########


# Create the site_year column by extracting the site, year, and summer (if applicable)


plant <- plant %>%
  mutate(
    site_year = str_extract(source_dataframe, "(?<=P_)[A-Za-z0-9_]+_[0-9]{4}(_(spring|summer))?")
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
 
plant_long %>% filter(PLANT == "HEEX") %>% ggplot() +
  geom_point(mapping = aes(x = version, y = plant_abundance, color = AF), 
             position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7)  + theme_classic() + facet_wrap(~site_year)


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


####### Explore animal responses ######
# Create the site_year column by extracting the site, year, and summer (if applicable)
animal <-  animal %>%
  mutate(
    site_year = str_extract(source_dataframe, "(?<=A_)[A-Za-z0-9_]+_[0-9]{4}(_(spring|summer))?")
  )

  
unique(animal$site_year)

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


animal_summary_spillover_only <- animal_long_full %>% filter(full_degree > 0) %>% filter(serp_degree > 0) %>% filter(nonserp_degree > 0) %>% 
  group_by(source_dataframe, site_year, version, AF) %>%
  summarise(across(c("extinct_level_A", "animal_abundance", "sum_extract"), 
                   sum, na.rm = TRUE))

#network_wide <- network %>%
 # select(metric, site_year, raw_value) %>%
  #pivot_wider(names_from = metric, values_from = raw_value)

#animal_long <- left_join(animal_long, network_wide, by = "site_year")
#animal_summary <- left_join(animal_summary, network_wide, by = "site_year")

## each dot is a unique site/year combo
animal_summary %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = version, y = animal_abundance, color = version), 
               alpha = 0.7) +
  theme_classic() +  scale_color_viridis_d() 


####### summarize empirical data #####
total_visits_by_network <- empirical_sp %>%
  group_by(network_name) %>%
  summarise(
    total_full_visits = sum(full_visits, na.rm = TRUE),
    total_serp_visits = sum(serp_visits, na.rm = TRUE),
    total_nonserp_visits = sum(nonserp_visits, na.rm = TRUE)
  )
###### Differences in plant response ########

plant_deltas <- plant_long %>%
  mutate(version = factor(version, levels = c(1, 2, 3), labels = c("Both", "S", "NS"))) %>%
  filter(
    (Soil_Type == "Serpentine" & version %in% c("Both", "S")) |
      (Soil_Type == "Non-Serpentine" & version %in% c("Both", "NS"))
  ) %>%
  group_by(site_year, Soil_Type, version) %>%
  summarise(
    plant_abundance = mean(plant_abundance, na.rm = TRUE),
    visit_quanity = mean(visit_quanity, na.rm = TRUE),
    visit_quality = mean(visit_quality, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = version,
    values_from = c(plant_abundance, visit_quanity, visit_quality)
  ) %>%
  mutate(
    delta_plant_abundance = plant_abundance_Both - coalesce(plant_abundance_S, plant_abundance_NS),
    delta_visit_quantity = visit_quanity_Both - coalesce(visit_quanity_S, visit_quanity_NS),
    delta_visit_quality = visit_quality_Both - coalesce(visit_quality_S, visit_quality_NS)
  ) %>%
  select(site_year, Soil_Type, starts_with("delta_"))


net_metrics$site_year <- net_metrics$network_name
plant_delta_full <- left_join(plant_deltas, net_metrics, by = "site_year")

empirical_sp$site_year <- empirical_sp$network_name
empirical_sp$ARTH <- empirical_sp$pollinator
animal_long_full <- left_join(animal_long, empirical_sp, by = c("site_year", "ARTH"))
