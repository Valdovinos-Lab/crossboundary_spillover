####################################################
###########################################################
######## Pollinator by Soil Lookup Spillover Data-Theory Integration #######
######## Code by Rebecca Nelson ###############
###### created: 1-14-26 #########################
######### last updated: 1-14-26 ############
#################################################
## look up which pollinator visits plants on which soils 

#source("Generate_Networks.R")

library(tidyverse)

library(tidyverse)
library(purrr)

## define which plants are on which soil 
non_serp_plants <- c(
  'ASER', 'VIVI', 'CESO', 'MEIN', 'PHAQ', 'ANAR',
  'AMME', 'ERCI', 'HIIN', 'Asteraceae sp.',
  'TAOF', 'MEPO', 'SEVU'
)

## load empirical networks 
network_dir <- "input_networks"
network_files <- list.files(network_dir, pattern = "\\.csv$", full.names = TRUE)

pollinator_soil_results <- list()

## look up function 
process_pollinator_soils <- function(net_matrix, net_name) {
  
  plant_ids <- rownames(net_matrix)
  pollinator_ids <- colnames(net_matrix)
  
  nonserp_mask <- plant_ids %in% non_serp_plants
  serp_mask <- !nonserp_mask
  
  serp_plants <- plant_ids[serp_mask]
  nonserp_plants_in_net <- plant_ids[nonserp_mask]
  
  # who visits what plant 
  pollinator_plants <- apply(net_matrix > 0, 2, function(x) {
    plant_ids[x]
  })
  
  pollinator_summary <- tibble(
    network_name = net_name,
    pollinator_name = pollinator_ids,
    
    present_on_serpentine = sapply(pollinator_ids, function(p) {
      any(pollinator_plants[[p]] %in% serp_plants)
    }),
    
    present_on_nonserpentine = sapply(pollinator_ids, function(p) {
      any(pollinator_plants[[p]] %in% nonserp_plants_in_net)
    })
  ) %>%
    mutate(
      soil_category = case_when(
        present_on_serpentine & present_on_nonserpentine ~ "both",
        present_on_serpentine ~ "serpentine_only",
        present_on_nonserpentine ~ "nonserpentine_only",
        TRUE ~ NA_character_
      )
    )
  
  pollinator_summary
}

## apply to all networks 
for (file_path in network_files) {
  
  net_name <- tools::file_path_sans_ext(basename(file_path))
  
  net_matrix <- read.csv(file_path, row.names = 1, check.names = FALSE)
  net_matrix <- as.matrix(net_matrix)
  
  pollinator_soil_results[[net_name]] <-
    process_pollinator_soils(net_matrix, net_name)
}

pollinator_soil_by_network <- bind_rows(pollinator_soil_results, .id = "network_id")

## create aggregate list 
combined_pollinator_soils <- pollinator_soil_by_network %>%
  group_by(pollinator_name) %>%
  summarise(
    present_on_serpentine_overall = any(present_on_serpentine, na.rm = TRUE),
    present_on_nonserpentine_overall = any(present_on_nonserpentine, na.rm = TRUE),
    n_networks = n(),
    .groups = "drop"
  ) %>%
  mutate(
    soil_category_overall = case_when(
      present_on_serpentine_overall & present_on_nonserpentine_overall ~ "both",
      present_on_serpentine_overall ~ "serpentine_only",
      present_on_nonserpentine_overall ~ "nonserpentine_only",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(soil_category_overall)

#save info
write.csv(
  pollinator_soil_by_network,
  "empirical_summaries/pollinator_soil_lookup_bynetwork.csv",
  row.names = FALSE
)

write.csv(
  combined_pollinator_soils,
  "empirical_summaries/pollinator_soil_lookup.csv",
  row.names = FALSE
)
