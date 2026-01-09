####################################################
###########################################################
######## Empirical Sigma/Quality Spillover Data-Theory Integration #######
######## Code by Rebecca Nelson ###############
###### created: 1-8-26 #########################
######### last updated: 1-8-26 ############
#################################################


### Load packages #######

#source("r_code/Generate_Networks.R") ## to generate original empirical networks 

library(tidyverse)

non_serp_plants <- c('ASER', 'VIVI', 'CESO', 'MEIN', 'PHAQ', 'ANAR', 
                     'AMME', 'ERCI', 'HIIN', 'Asteraceae sp.', 
                     'TAOF', 'MEPO', 'SEVU')

# Directory with network CSVs
network_dir <- "input_networks"
network_files <- list.files(network_dir, pattern = "\\.csv$", full.names = TRUE)


######## Helper Functions ############
calc_sigma <- function(V) {
  col_sums <- colSums(V)
  
  sigma <- sweep(V, 2, col_sums, "/")
  sigma[is.na(sigma)] <- 0   # deals with zero-column pollinators
  
  return(sigma)
}

calc_Q <- function(V, sigma) {
  
  numerator   <- rowSums(sigma * V)
  denominator <- rowSums(V)
  
  Q <- numerator / denominator
  Q[is.na(Q)] <- NA  # deals with plants with zero total visits
  
  tibble(
    plant = rownames(V),
    Q     = Q
  )
}

############# Compute sigma and Q for empirical network ########
results <- map_dfr(network_files, function(f) {
  
  network_name <- tools::file_path_sans_ext(basename(f))
  
  V <- read_csv(f, show_col_types = FALSE)
  

  plant_names <- V[[1]]
  
  V <- V %>%
    select(-1) %>%
    as.matrix()
  
  rownames(V) <- plant_names
  
  
  # -----------------
  # FULL NETWORK
  # -----------------
  sigma_full <- calc_sigma(V)
  Q_full     <- calc_Q(V, sigma_full) %>%
    mutate(
      network = network_name,
      subset  = "full"
    )
  
  # -----------------
  # NON-SERPENTINE
  # -----------------
  V_nonserp <- V[rownames(V) %in% non_serp_plants, , drop = FALSE]
  
  sigma_nonserp <- calc_sigma(V_nonserp)
  Q_nonserp     <- calc_Q(V_nonserp, sigma_nonserp) %>%
    mutate(
      network = network_name,
      subset  = "non_serpentine"
    )
  
  # -----------------
  # SERPENTINE
  # -----------------
  V_serp <- V[!rownames(V) %in% non_serp_plants, , drop = FALSE]
  
  sigma_serp <- calc_sigma(V_serp)
  Q_serp     <- calc_Q(V_serp, sigma_serp) %>%
    mutate(
      network = network_name,
      subset  = "serpentine"
    )
  
  bind_rows(Q_full, Q_nonserp, Q_serp)
})


##### inspect results ######
glimpse(results)

results %>%
  group_by(network, subset) %>%
  summarise(
    mean_Q = mean(Q, na.rm = TRUE),
    sd_Q   = sd(Q, na.rm = TRUE),
    .groups = "drop"
  )

####### save results #######
write_csv(results, "empirical_summaries/visit_quality_empirical_networks.csv")


