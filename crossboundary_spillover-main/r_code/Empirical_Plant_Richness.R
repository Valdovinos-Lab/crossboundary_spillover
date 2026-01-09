####################################################
###########################################################
######## Empirical Plant Richness Spillover Data-Theory Integration #######
######## Code by Rebecca Nelson ###############
###### created: 9-8-25 #########################
######### last updated: 1-8-26 ############
#################################################
## generate plant richnesss info for empirical networks for comparison to simulation 

source("r_code/Generate_Networks.R")

library(tidyverse)

non_serp_plants <- c('ASER', 'VIVI', 'CESO', 'MEIN', 'PHAQ', 'ANAR', 
                     'AMME', 'ERCI', 'HIIN', 'Asteraceae sp.', 
                     'TAOF', 'MEPO', 'SEVU')

# Directory with network CSVs
network_dir <- "input_networks"
network_files <- list.files(network_dir, pattern = "\\.csv$", full.names = TRUE)

plant_results_list <- list()

# Function to calculate number of plant species
process_plant_summary <- function(net_matrix, net_name) {
  
  # rownames = plant species, colnames = pollinator species
  row_ids <- rownames(net_matrix)
  
  # Masks for serpentine and non-serpentine plants
  nonserp_mask <- row_ids %in% non_serp_plants
  serp_mask <- !nonserp_mask
  
  # (A) All plants with at least one interaction
  full_plants <- row_ids[rowSums(net_matrix) > 0]
  
  # (B) Serpentine plants with at least one interaction
  serp_plants <- row_ids[serp_mask & rowSums(net_matrix) > 0]
  
  # (C) Non-serpentine plants with at least one interaction
  nonserp_plants_present <- row_ids[nonserp_mask & rowSums(net_matrix) > 0]
  
  # Calculate proportions relative to full network
  prop_serp_plants <- length(serp_plants) / length(full_plants)
  prop_nonserp_plants <- length(nonserp_plants_present) / length(full_plants)
  
  tibble(
    network_name = net_name,
    n_plants_full = length(full_plants),
    n_plants_serp = length(serp_plants),
    n_plants_nonserp = length(nonserp_plants_present),
    prop_plants_serp = prop_serp_plants,
    prop_plants_nonserp = prop_nonserp_plants
  )
}

# Loop through networks
for (file_path in network_files) {
  net_name <- tools::file_path_sans_ext(basename(file_path))
  net_matrix <- read.csv(file_path, row.names = 1, check.names = FALSE)
  net_matrix <- as.matrix(net_matrix)
  
  plant_results_list[[net_name]] <- process_plant_summary(net_matrix, net_name)
}

# Combine into one data frame
plant_summary <- bind_rows(plant_results_list)

print(plant_summary)
write.csv(plant_summary, "plant_diversity_empirical.csv", row.names = FALSE)
