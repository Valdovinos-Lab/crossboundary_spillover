######## Visualize and analyze MATLAB Outputs ######
######## updated version for newer MATLAB code ##############
###### date created: 3-17-2025 ###########
######### date last modified: 11-7-2025 #######
###### analyze simulation and empirical data #######

rm(list = ls())

###### Load required packages ######
library(tidyverse)

###### Load combined outputs ######
#simulation output tables
animal <- read.csv("Spillover_code/simulation_results/Animals_AllMetrics.csv")
plant <- read.csv("Spillover_code/simulation_results/Plants_AllMetrics.csv")

## empirical data summaries
net_metrics <- read.csv("empirical_summaries/empirical_network_metrics.csv")
empirical_div <- read.csv("empirical_summaries/pollinator_diversity_empirical.csv")
empirical_sp <- read.csv("empirical_summaries/pollinator_properties_by_soil.csv")

############ Animals ############

## sum across all animal species of each animal output for each simulation, version, and Dataset (site-year)
animal_sums <- animal %>%  group_by(VersionName, Version, Dataset, Simulation) %>% 
  summarise(across(c("A", "N_extract", "MeanSigmaA" , "VisitsA_percap", "VisitsA_total" ), 
                   sum, na.rm = TRUE))

### mean of each summed animal output across simulation for a given version and Dataset (site-yer)
animal_means <- animal_sums %>%  group_by(VersionName, Version, Dataset) %>% 
  summarise(across(c("A", "N_extract", "MeanSigmaA" , "VisitsA_percap", "VisitsA_total" ), 
                   mean, na.rm = TRUE))

####### summarize empirical data 
total_visits_by_network <- empirical_sp %>%
  group_by(network_name) %>%
  summarise(
    total_full_visits = sum(full_visits, na.rm = TRUE),
    total_serp_visits = sum(serp_visits, na.rm = TRUE),
    total_nonserp_visits = sum(nonserp_visits, na.rm = TRUE)
  )

### explore animal data: means  ###########
### mean values for pollinators across simulation
animal_means %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = VersionName, y = A, color = VersionName), 
               alpha = 0.7) +
  theme_classic() +  scale_color_viridis_d() 
## NS has less pollinators than serpentine and full. Full has slightly more--combined effects of specialist pollinators and potentially more shared pollinators

animal_means %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = VersionName, y = N_extract, color = VersionName), 
               alpha = 0.7) +
  theme_classic() +  scale_color_viridis_d() 
## less extracted in NS, slightly more extract in full network 

animal_means %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = VersionName, y = MeanSigmaA, color = VersionName), 
               alpha = 0.7) +
  theme_classic() +  scale_color_viridis_d() 
## lowest sigma in NS


animal_means %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = VersionName, y = VisitsA_percap, color = VersionName), 
               alpha = 0.7) +
  theme_classic() +  scale_color_viridis_d() 

animal_means %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = VersionName, y = VisitsA_total, color = VersionName), 
               alpha = 0.7) +
  theme_classic() +  scale_color_viridis_d() 
## considerably more total visits in full network

###### Explore animal data by site #########
#%>% filter(Dataset == "Vineyard_2024_summer_all_versions") 
animal_sums %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = VersionName, y = A, color = VersionName), 
               alpha = 0.7) +
  theme_classic() +  scale_color_viridis_d() + facet_wrap(~Dataset)

animal_sums %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = VersionName, y = N_extract, color = VersionName), 
               alpha = 0.7) +
  theme_classic() +  scale_color_viridis_d() + facet_wrap(~Dataset)


animal_sums %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = VersionName, y = MeanSigmaA, color = VersionName), 
               alpha = 0.7) +
  theme_classic() +  scale_color_viridis_d() + facet_wrap(~Dataset)


animal_sums %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = VersionName, y = VisitsA_percap, color = VersionName), 
               alpha = 0.7) +
  theme_classic() +  scale_color_viridis_d() + facet_wrap(~Dataset)

animal_sums %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = VersionName, y = VisitsA_total, color = VersionName), 
               alpha = 0.7) +
  theme_classic() +  scale_color_viridis_d() + facet_wrap(~Dataset)

### most sights follow the general pattern of full having the most visits, and NS the least visits 


############### Plants ##################

# Function to classify plants by NS vs S soil type
classify_plants <- function(df) {
  s_plants <- df %>%
    filter(VersionName == "serpentine") %>%
    pull(Plant_FullID)
  
  ns_plants <- df %>%
    filter(VersionName == "non_serpentine") %>%
    pull(Plant_FullID)
  
  df %>%
    mutate(
      Plant_Group = case_when(
        Plant_FullID %in% s_plants  ~ "serpentine",
        Plant_FullID %in% ns_plants ~ "non_serpentine",
        TRUE                        ~ NA_character_
      )
    )
}

# Apply function to each combination of Simulation × Dataset (site-year)
plant_classified <- plant %>%
  group_by(Simulation, Dataset) %>%
  group_modify(~ classify_plants(.x)) %>%
  ungroup()

## sum across all plant species within a soil type for each plant output for each simulation, version, and Dataset (site-year)
plant_sums <- plant_classified %>%  group_by(VersionName, Version, Dataset, Simulation, Plant_Group) %>% 
  summarise(across(c("P", "Gamma", "Seeds", "PolServ", "MeanSigmaP", "VisitsP_percap" , "VisitsP_total" ), 
                   sum, na.rm = TRUE))

### mean of each summed plant output across simulation for a given version and Dataset (site-year)
plant_means <- plant_sums %>%  group_by(VersionName, Version, Dataset, Plant_Group) %>% 
  summarise(across(c("P", "Gamma", "Seeds", "PolServ", "MeanSigmaP", "VisitsP_percap" , "VisitsP_total" ), 
                   mean, na.rm = TRUE))
###### explore plant data: means #############

plant_means %>% 
  ggplot(aes(x = Plant_Group, y = P, fill = VersionName)) + 
  geom_boxplot(alpha = 0.7) + 
  scale_fill_viridis_d()   + theme_classic() 

plant_means %>% 
  ggplot(aes(x = Plant_Group, y = Gamma, fill = VersionName)) + 
  geom_boxplot(alpha = 0.7) + 
  scale_fill_viridis_d()   + theme_classic() 

plant_means %>% 
  ggplot(aes(x = Plant_Group, y = Seeds, fill = VersionName)) + 
  geom_boxplot(alpha = 0.7) + 
  scale_fill_viridis_d()   + theme_classic() 

plant_means %>% 
  ggplot(aes(x = Plant_Group, y = PolServ, fill = VersionName)) + 
  geom_boxplot(alpha = 0.7) + 
  scale_fill_viridis_d()   + theme_classic() 

plant_means %>% 
  ggplot(aes(x = Plant_Group, y = MeanSigmaP, fill = VersionName)) + 
  geom_boxplot(alpha = 0.7) + 
  scale_fill_viridis_d()   + theme_classic() 

plant_means %>% 
  ggplot(aes(x = Plant_Group, y = VisitsP_percap, fill = VersionName)) + 
  geom_boxplot(alpha = 0.7) + 
  scale_fill_viridis_d()   + theme_classic() 

plant_means %>% 
  ggplot(aes(x = Plant_Group, y = VisitsP_total, fill = VersionName)) + 
  geom_boxplot(alpha = 0.7) + 
  scale_fill_viridis_d()   + theme_classic() 

############ explore data: by site-year ##########

plant_sums %>% 
  ggplot(aes(x = Plant_Group, y = P, fill = VersionName)) + 
  geom_boxplot(alpha = 0.7) + 
  scale_fill_viridis_d()   + theme_classic() + facet_wrap(~Dataset)

plant_sums %>% 
  ggplot(aes(x = Plant_Group, y = Gamma, fill = VersionName)) + 
  geom_boxplot(alpha = 0.7) + 
  scale_fill_viridis_d()   + theme_classic() + facet_wrap(~Dataset)

plant_sums %>% 
  ggplot(aes(x = Plant_Group, y = Seeds, fill = VersionName)) + 
  geom_boxplot(alpha = 0.7) + 
  scale_fill_viridis_d()   + theme_classic() + facet_wrap(~Dataset)

plant_sums %>% 
  ggplot(aes(x = Plant_Group, y = PolServ, fill = VersionName)) + 
  geom_boxplot(alpha = 0.7) + 
  scale_fill_viridis_d()   + theme_classic() + facet_wrap(~Dataset)

plant_sums %>% 
  ggplot(aes(x = Plant_Group, y = MeanSigmaP, fill = VersionName)) + 
  geom_boxplot(alpha = 0.7) + 
  scale_fill_viridis_d()   + theme_classic() + facet_wrap(~Dataset)

plant_sums %>% 
  ggplot(aes(x = Plant_Group, y = VisitsP_percap, fill = VersionName)) + 
  geom_boxplot(alpha = 0.7) + 
  scale_fill_viridis_d()   + theme_classic() + facet_wrap(~Dataset)

plant_sums %>% 
  ggplot(aes(x = Plant_Group, y = VisitsP_total, fill = VersionName)) + 
  geom_boxplot(alpha = 0.7) + 
  scale_fill_viridis_d()   + theme_classic() + facet_wrap(~Dataset)

