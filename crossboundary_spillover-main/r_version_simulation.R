# Load necessary libraries
if (!requireNamespace("Matrix", quietly = TRUE)) {
  install.packages("Matrix")
}
library(Matrix)

# Main function to run simulations
run <- function(networks = NULL) {
  global_network <<- NULL
  
  # Default range of networks
  if (is.null(networks)) {
    networks <- 1:2
  }
  
  # Loop through each network
  for (network_index in seq_along(networks)) {
    global_network <<- networks[network_index]
    
    # Death case fixed to 3
    death_case <- 3
    
    # File name based on network and death case
    file_name <- c(global_network, death_case)
    
    # Run the invasion process
    results <- run_inv_PC(file_name)
    
    # Extract results
    Alpha <- results$Alpha
    P <- results$P
    A <- results$A
    
    # Write data to CSV files
    write.csv(as.matrix(P[[1]]), sprintf("data/P_n%04d_m%d.csv", global_network, death_case), row.names = FALSE)
    write.csv(as.matrix(P[[2]]), sprintf("data/P_n%04d_m%d.csv", global_network, death_case), row.names = FALSE)
    write.csv(as.matrix(A[[1]]), sprintf("data/A_n%04d_m%d.csv", global_network, death_case), row.names = FALSE)
    write.csv(as.matrix(A[[2]]), sprintf("data/A_n%04d_m%d.csv", global_network, death_case), row.names = FALSE)
    write.csv(as.matrix(Alpha[[1]]), sprintf("data/Alpha_n%04d_m%d.csv", global_network, death_case), row.names = FALSE)
    write.csv(as.matrix(Alpha[[2]]), sprintf("data/Alpha_n%04d_m%d.csv", global_network, death_case), row.names = FALSE)
  }
}

# Function to initialize and run invasion simulations
run_inv_PC <- function(file_name) {
  global_J_pattern <<- NULL
  global_network_metadata <<- NULL
  
  # Load network data (replace with actual file path)
  m1200 <- readRDS("1200m.rds")
  network <- file_name[1]
  network_data <- as.matrix(m1200[[network]])
  
  # Seed for reproducibility
  seed <- 0
  set.seed(seed + network)
  
  # Reformat network data by ascending degree
  index_a <- order(colSums(network_data))
  index_p <- order(rowSums(network_data))
  network_data <- network_data[index_p, index_a]
  global_network_metadata <<- set_up(network_data, file_name)
  
  # Create Jacobian matrix pattern
  global_J_pattern <<- jacobian_pattern(network_data)
  
  # Run the main driver function
  driver_inv_PC(network_data, file_name)
}

# Function to drive invasion simulations
driver_inv_PC <- function(data, file_name) {
  global_extinct_level_p <<- 2e-2
  global_extinct_level_a <<- 1e-3
  
  # Time span for simulation
  tspan <- c(0, 10000)
  final_parameters <- NULL
  
  # Prepare result containers
  Alpha <- vector("list", 2)
  P <- vector("list", 2)
  A <- vector("list", 2)
  
  for (simulation in 1:2) {
    # Define initial state
    if (simulation == 1) {
      initial_state <- set_initial_state()
    } else {
      initial_state <- set_initial_state_inv(final_parameters)
    }
    
    # Integrate network data
    final_parameters <- integrate(file_name, initial_state, tspan)
    
    # Expand results
    expanded <- expand(final_parameters)
    p <- expanded$p
    n <- expanded$n
    a <- expanded$a
    alpha <- expanded$alpha
    
    # Apply extinction thresholds
    p <- ifelse(p < global_extinct_level_p, 0, p)
    a <- ifelse(a < global_extinct_level_a, 0, a)
    
    # Compute sums
    computed <- compute_sums(p, n, a, alpha)
    
    # Record results
    Alpha[[simulation]] <- alpha
    P[[simulation]] <- cbind(p < global_extinct_level_p, p, n, computed$sum_pol, 
                             computed$quantity, computed$quality, computed$foraging_effort)
    A[[simulation]] <- cbind(a < global_extinct_level_a, a, computed$sum_extract)
  }
  
  return(list(Alpha = Alpha, P = P, A = A))
}

# Placeholder for setting up metadata
set_up <- function(network_data, file_name) {
  death_case <- file_name[2]
  var_p <- 1e-1; mean_e <- 0.8; mean_g <- 0.4; mean_beta <- 0.2
  mean_epsilon <- 1; mean_u <- 0.06; mean_w <- 1.2; mean_phi <- 0.04
  var_a <- 0; mean_tau <- 1; mean_b <- 0.4; mean_c <- 0.2; mean_G <- 2
  
  mortality <- switch(as.character(death_case),
                      "1" = list(mu_a = 0.05, mu_p = 0.001),
                      "2" = list(mu_a = 0.001, mu_p = 0.02),
                      "3" = list(mu_a = 0.001, mu_p = 0.001),
                      "4" = list(mu_a = 0.03, mu_p = 0.005))
  
  make_metadata(network_data, mean_c, mortality$mu_p, mortality$mu_a, mean_beta)
}

# Placeholder for creating Jacobian matrix
jacobian_pattern <- function(network_data) {
  # Logic from the translated jacobian_pattern.m
}

# Placeholder for setting initial state
set_initial_state <- function() {
  # Logic from set_initial_state.m
}

# Placeholder for other functions
set_initial_state_inv <- function(final_parameters) { final_parameters }
integrate <- function(file_name, initial_state, tspan) { runif(length(initial_state)) }
expand <- function(final_parameters) { list(p = 0, n = 0, a = 0, alpha = matrix(0, nrow = 1)) }
compute_sums <- function(p, n, a, alpha) { list(sum_pol = 0, quantity = 0, quality = 0, sum_extract = 0) }
make_metadata <- function(...) { list(data = matrix(0), indices = 1) }
