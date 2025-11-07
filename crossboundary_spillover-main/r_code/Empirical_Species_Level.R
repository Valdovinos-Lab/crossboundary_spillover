####################################################
###########################################################
######## Empirical Species Level Spillover Data-Theory Integration #######
######## Code by Rebecca Nelson ###############
###### created: 8-4-24 #########################
######### last updated: 8-4-25 ############
#################################################
## generate pollinator species level info for empirical networks for comparison to simulation 
source(Generate_Networks.R)

library(tidyverse)

non_serp_plants <- c('ASER', 'VIVI', 'CESO', 'MEIN', 'PHAQ', 'ANAR', 
                     'AMME', 'ERCI', 'HIIN', 'Asteraceae sp.', 
                     'TAOF', 'MEPO', 'SEVU')

# Function to calculate visits, degree, and specialization
pollinator_properties_by_soil <- function(net, net_name) {
  row_ids <- rownames(net)
  nonserp_mask <- row_ids %in% non_serp_plants
  serp_mask <- !nonserp_mask
  
  # Subnetworks
  serp_net <- net
  serp_net[!serp_mask, ] <- 0
  
  nonserp_net <- net
  nonserp_net[!nonserp_mask, ] <- 0
  
  # Total visits (column sums)
  full_visits <- colSums(net)
  serp_visits <- colSums(serp_net)
  nonserp_visits <- colSums(nonserp_net)
  
  # Degrees (number of plants visited)
  full_degree <- colSums(net > 0)
  serp_degree <- colSums(serp_net > 0)
  nonserp_degree <- colSums(nonserp_net > 0)
  
  # Classification
  classify <- function(degree_vec) ifelse(degree_vec == 1, "specialist", "generalist")
  
  # Combine all in one dataframe
  poll_df <- tibble(
    pollinator = names(full_visits),
    full_visits = full_visits,
    serp_visits = serp_visits,
    nonserp_visits = nonserp_visits,
    full_degree = full_degree,
    serp_degree = serp_degree,
    nonserp_degree = nonserp_degree,
    full_specialization = classify(full_degree),
    serp_specialization = classify(serp_degree),
    nonserp_specialization = classify(nonserp_degree),
    network_name = net_name
  )
  
  return(poll_df)
}

# Directory and files
network_dir <- "input_networks"
network_files <- list.files(network_dir, pattern = "\\.csv$", full.names = TRUE)

# Process all networks
visit_results_list <- list()

for (file_path in network_files) {
  net_name <- tools::file_path_sans_ext(basename(file_path))
  net_matrix <- as.matrix(read.csv(file_path, row.names = 1, check.names = FALSE))
  
  visit_results_list[[net_name]] <- pollinator_properties_by_soil(net_matrix, net_name)
}

# Combine into one df
pollinator_properties_all <- bind_rows(visit_results_list)

# View or export
print(pollinator_properties_all)
#write.csv(pollinator_properties_all, "pollinator_properties_by_soil.csv", row.names = FALSE)
## total number of visits, degree, and whether or not specialist (degree = 1) by soil type 
## some generalists are actually for degree = 0 not present