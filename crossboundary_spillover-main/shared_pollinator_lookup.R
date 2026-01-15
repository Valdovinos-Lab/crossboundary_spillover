####################################################
###########################################################
######## Shared Pollinator Lookup Spillover Data-Theory Integration #######
######## Code by Rebecca Nelson ###############
###### created: 1-12-26 #########################
######### last updated: 1-12-26 ############
#################################################
## look up who is sharing pollinators across soil types in the empirical data 

#source("Generate_Networks.R")

library(tidyverse)


# Define soils 
non_serp_plants <- c('ASER', 'VIVI', 'CESO', 'MEIN', 'PHAQ', 'ANAR', 
                     'AMME', 'ERCI', 'HIIN', 'Asteraceae sp.', 
                     'TAOF', 'MEPO', 'SEVU')

# load and store empirical network  info
network_dir <- "input_networks"


network_files <- list.files(network_dir, pattern = "\\.csv$", full.names = TRUE)

shared_pollinator_results <- list()

# identify shared pollinators function 
process_shared_pollinators <- function(net_matrix, net_name) {
  
  row_ids <- rownames(net_matrix)
  
  
  nonserp_mask <- row_ids %in% non_serp_plants
  serp_mask <- !nonserp_mask
  
  serp_plants <- row_ids[serp_mask]
  nonserp_plants_in_net <- row_ids[nonserp_mask]
  

  plant_pollinators <- apply(net_matrix > 0, 1, function(x) colnames(net_matrix)[x])
  
  # 1) Serpentine plants that share at least one pollinator with any non-serpentine plant
  serp_shared_any <- sapply(serp_plants, function(p) {
    any(sapply(nonserp_plants_in_net, function(np) {
      length(intersect(plant_pollinators[[p]], plant_pollinators[[np]])) > 0
    }))
  })
  
  # 2) Serpentine plants that share with non-serpentine CESO
  serp_shared_CESO <- sapply(serp_plants, function(p) {
    if("CESO" %in% nonserp_plants_in_net) {
      length(intersect(plant_pollinators[[p]], plant_pollinators[["CESO"]])) > 0
    } else FALSE
  })
  
  # 3) Serpentine plants that share with non-serpentine VIVI
  serp_shared_VIVI <- sapply(serp_plants, function(p) {
    if("VIVI" %in% nonserp_plants_in_net) {
      length(intersect(plant_pollinators[[p]], plant_pollinators[["VIVI"]])) > 0
    } else FALSE
  })
  
  # 4) Non-serpentine plants that share pollinators with any serpentine plant species 
  nonserp_shared_any <- sapply(nonserp_plants_in_net, function(np) {
    any(sapply(serp_plants, function(sp) {
      length(intersect(plant_pollinators[[np]], plant_pollinators[[sp]])) > 0
    }))
  })
  

  tibble(
    network_name = net_name,
    plant_name = c(serp_plants, nonserp_plants_in_net),
    serpentine = c(rep(TRUE, length(serp_plants)), rep(FALSE, length(nonserp_plants_in_net))),
    shared_any = c(serp_shared_any, nonserp_shared_any),
    shared_with_CESO = c(serp_shared_CESO, rep(NA, length(nonserp_plants_in_net))),
    shared_with_VIVI = c(serp_shared_VIVI, rep(NA, length(nonserp_plants_in_net)))
  )
}

for(file_path in network_files) {
  net_name <- tools::file_path_sans_ext(basename(file_path))
  
  net_matrix <- read.csv(file_path, row.names = 1, check.names = FALSE)
  net_matrix <- as.matrix(net_matrix)
  
  shared_pollinator_results[[net_name]] <- process_shared_pollinators(net_matrix, net_name)
}


shared_pollinator_summary <- bind_rows(shared_pollinator_results, .id = "network_id")


##### combine across networks

library(purrr)


clean_network <- function(df) {
  df %>%
    mutate(
      shared_any = as.logical(shared_any),
      shared_with_CESO = as.logical(shared_with_CESO),
      shared_with_VIVI = as.logical(shared_with_VIVI)
    )
}


cleaned_list <- map(shared_pollinator_results, clean_network)


all_networks <- bind_rows(cleaned_list)

combined_all_networks <- all_networks %>%
  group_by(plant_name, serpentine) %>%
  summarise(
    shared_any_overall = any(shared_any, na.rm = TRUE),
    shared_with_CESO_overall = any(shared_with_CESO, na.rm = TRUE),
    shared_with_VIVI_overall = any(shared_with_VIVI, na.rm = TRUE),
    n_networks = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(shared_any_overall))



write.csv(all_networks, "empirical_summaries/shared_pollinator_lookup_bynetwork.csv", row.names = FALSE)
write.csv(combined_all_networks, "empirical_summaries/shared_pollinator_lookup.csv", row.names = FALSE)
