######## Proccess MATLAB Outputs ######
#### Code for processing matlab outputs into R dataframes #####
##### for Becca and Taran spillover project #######

###### date created: 2-19-2025 ###########
######### date last modified: 8-4-2025 #######
rm(list = ls())

# Load libraries
library(dplyr)
library(here)
library(stringr)

# Define relative paths from repo root
folder_path <- here("data")
folder_path_2 <- here("input_networks")
output_path <- here("output")


# List all CSV files in data folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

### ------------------- Read & assign data ------------------- ###
for (file in csv_files) {
  filename <- basename(file)
  if (str_detect(filename, "^[AP]_")) {
    df <- read.csv(file, header = FALSE, stringsAsFactors = FALSE)
    df_name <- str_remove(filename, "\\.csv$")
    assign(df_name, df)
  }
}

### ------------------- (Optional) Read input_networks ------------------- ###
#reference_files <- list.files(folder_path_2, pattern = "\\.csv$", full.names = TRUE)
#for (file in reference_files) {
 # df <- read.csv(file)
 # df_name <- str_remove(basename(file), "\\.csv$")
  #assign(df_name, df)
#}

### ------------------- Animal Outputs ------------------- ###
animal_dfs <- ls(pattern = "^A_")
animal_colnames <- c("ARTH", "extinct_level_A_run1", "animal_abundance_run1", "sum_extract_run1",
                     "extinct_level_A_run2", "animal_abundance_run2", "sum_extract_run2")

animal_list <- list()
for (df_name in animal_dfs) {
  df <- get(df_name)
  if (ncol(df) >= length(animal_colnames)) {
    colnames(df)[1:length(animal_colnames)] <- animal_colnames
  } else {
    warning(paste("Skipping", df_name, "- not enough columns"))
    next
  }
  df$source_dataframe <- df_name
  df$version <- str_extract(df_name, "version\\d+$") %>% str_remove("version")
  animal_list[[df_name]] <- df
}

animal_combined <- bind_rows(animal_list)
write.csv(animal_combined, file = file.path(output_path, "animal_outputs.csv"), row.names = FALSE)

### ------------------- Plant Outputs ------------------- ###
plant_dfs <- ls(pattern = "^P_")
plant_colnames <- c("PLANT", 
                    "extinct_level_P_run1", "initial_plant_abundance_run1", "plant_abundance_run1",
                    "reward_abundance_run1", "sum_pol_run1", "visit_quanity_run1", "visit_quality_run1", "foraging_effort_run1",
                    "extinct_level_P_run2", "initial_plant_abundance_run2", "plant_abundance_run2",
                    "reward_abundance_run2", "sum_pol_run2", "visit_quanity_run2", "visit_quality_run2", "foraging_effort_run2")

plant_list <- list()
for (df_name in plant_dfs) {
  df <- get(df_name)
  if (ncol(df) >= length(plant_colnames)) {
    colnames(df)[1:length(plant_colnames)] <- plant_colnames
  } else {
    warning(paste("Skipping", df_name, "- not enough columns"))
    next
  }
  df$source_dataframe <- df_name
  df$version <- str_extract(df_name, "version\\d+$") %>% str_remove("version")
  plant_list[[df_name]] <- df
}

plant_combined <- bind_rows(plant_list)
write.csv(plant_combined, file = file.path(output_path, "plant_outputs.csv"), row.names = FALSE)
