####################################################
###########################################################
######## Empirical Network Properties Spillover Data-Theory Integration #######
######## Code by Rebecca Nelson ###############
###### created: 3-5-24 #########################
######### last updated: 8-4-25 ############
#################################################
## generate networks properties for empirical networks 

source("Generate_Networks.R")

# Load required packages
library(tidyverse)
library(maxnodf)  
library(vegan)    


non_serp_plants <- c('ASER', 'VIVI', 'CESO', 'MEIN', 'PHAQ', 'ANAR', 
                     'AMME', 'ERCI', 'HIIN', 'Asteraceae sp.', 
                     'TAOF', 'MEPO', 'SEVU')

# Directory containing your saved network CSV files
network_dir <- "input_networks"

# List all CSV files in the directory (spring + summer)
network_files <- list.files(network_dir, pattern = "\\.csv$", full.names = TRUE)

# Initialize results 
results_list <- list()

# Helper function 
process_network <- function(net, net_name) {
  nodfc_full <- tryCatch(NODFc(net), error = function(e) NA)
  
  row_ids <- rownames(net)
  nonserp_mask <- row_ids %in% non_serp_plants
  serp_mask <- !nonserp_mask
  
  serp_net <- net
  serp_net[!serp_mask, ] <- 0
  
  nonserp_net <- net
  nonserp_net[!nonserp_mask, ] <- 0
  
  poll_serp <- colSums(serp_net) > 0
  poll_nonserp <- colSums(nonserp_net) > 0
  
  jaccard_dist <- tryCatch(
    vegdist(rbind(poll_serp, poll_nonserp), method = "jaccard")[1],
    error = function(e) NA
  )
  
  jaccard_sim <- 1 - jaccard_dist
  
  tibble(
    network_name = net_name,
    NODFc_full = nodfc_full,
    Jaccard_pollinator_similarity = jaccard_sim
  )
}

# Loop through all networks
for (file_path in network_files) {
  # Extract network name 
  net_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Read network CSV
  net_matrix <- read.csv(file_path, row.names = 1, check.names = FALSE)
  
  # Convert to matrix if needed
  net_matrix <- as.matrix(net_matrix)
  
  # Process and save 
  results_list[[net_name]] <- process_network(net_matrix, net_name)
}

# Combine all results 
final_results <- bind_rows(results_list)

# View or save 
print(final_results)
#write.csv(final_results, "empirical_network_metrics.csv", row.names = FALSE)
## gives you NODFc of FULL network and jaccard index of pollinator similarity between serpentine vs non-serpentine network 
