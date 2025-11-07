####################################################
###########################################################
######## Empirical Visits Spillover Data-Theory Integration #######
######## Code by Rebecca Nelson ###############
###### created: 3-5-24 #########################
######### last updated: 8-4-25 ############
#################################################
## generate visitation info for empirical networks for comparison to simulation 

source("Generate_Networks.R")

library(tidyverse)


non_serp_plants <- c('ASER', 'VIVI', 'CESO', 'MEIN', 'PHAQ', 'ANAR', 
                     'AMME', 'ERCI', 'HIIN', 'Asteraceae sp.', 
                     'TAOF', 'MEPO', 'SEVU')

# The directory where your saved networks are located
network_dir <- "input_networks"

# List all network CSV files
network_files <- list.files(network_dir, pattern = "\\.csv$", full.names = TRUE)

# To store results for each network
visit_results_list <- list()

# Function to calculate total visits and pollinator proportions
process_visitation_summary <- function(net_matrix, net_name) {
  
  # rownames = plant species, colnames = pollinator species
  row_ids <- rownames(net_matrix)
  
  # Masks for serpentine and non-serpentine plants
  nonserp_mask <- row_ids %in% non_serp_plants
  serp_mask <- !nonserp_mask
  
  # (A) Full network pollinator totals (sum across all plants)
  full_pollinator_totals <- colSums(net_matrix)
  full_pollinators <- names(full_pollinator_totals)[full_pollinator_totals > 0]
  
  # (B) Serpentine network pollinator totals
  serp_net <- net_matrix
  serp_net[!serp_mask, ] <- 0
  serp_pollinator_totals <- colSums(serp_net)
  serp_pollinators <- names(serp_pollinator_totals)[serp_pollinator_totals > 0]
  
  # (C) Non-serpentine network pollinator totals
  nonserp_net <- net_matrix
  nonserp_net[!nonserp_mask, ] <- 0
  nonserp_pollinator_totals <- colSums(nonserp_net)
  nonserp_pollinators <- names(nonserp_pollinator_totals)[nonserp_pollinator_totals > 0]
  
  # Calculate proportions of pollinator species richness relative to full
  prop_serp_pollinators <- length(serp_pollinators) / length(full_pollinators)
  prop_nonserp_pollinators <- length(nonserp_pollinators) / length(full_pollinators)
  
  tibble(
    network_name = net_name,
    n_pollinators_full = length(full_pollinators),
    n_pollinators_serp = length(serp_pollinators),
    n_pollinators_nonserp = length(nonserp_pollinators),
    prop_pollinators_serp = prop_serp_pollinators,
    prop_pollinators_nonserp = prop_nonserp_pollinators
  )
}

# Loop through each network file and summarize
for (file_path in network_files) {
  net_name <- tools::file_path_sans_ext(basename(file_path))
  
  net_matrix <- read.csv(file_path, row.names = 1, check.names = FALSE)
  net_matrix <- as.matrix(net_matrix)
  
  visit_results_list[[net_name]] <- process_visitation_summary(net_matrix, net_name)
}

# Combine all results into one data frame
visit_summary <- bind_rows(visit_results_list)

# View or save results
print(visit_summary)
#write.csv(visit_summary, "pollinator_diversity_empirical.csv", row.names = FALSE)
## this gives you the pollinator richness on full vs each soil type and the proportion of pollinators within the total network present on each soil type 