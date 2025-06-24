######## Proccess MATLAB Outputs ######
#### Code for processing matlab outputs into R dataframes #####
##### for Becca and Taran spillover project #######

###### date created: 2-19-2025 ###########
######### date last modified: 3-17-2025 #######
rm(list = ls())
# Load necessary libraries
library(dplyr)

# Define the path to the folder containing the .csv files
folder_path <- "/Users/Becca/Desktop/Harrison Lab/crossboundary_spillover/crossboundary_spillover-main/data"
folder_path_2 <- "/Users/Becca/Desktop/Harrison Lab/crossboundary_spillover/crossboundary_spillover-main/input_networks"

# Make each network a dataframe for files in the plant group
csv_files <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)

# Process all the CSV files for plant and animal groups
for (file in csv_files) {
  # Skip files that start with "A" or "P"
  if (grepl("^[A|P]", basename(file))) {
    # Read the current CSV file
    df <- read.csv(file, header = FALSE, stringsAsFactors = FALSE)
    
    # Create a variable name based on the file name (without the .csv extension)
    df_name <- gsub(".csv", "", basename(file))
    
    # Use assign() to create a data frame with the name from the file name
    assign(df_name, df)
  }
}

# List of original files to use as reference for row and column names
original_files <- list.files(folder_path_2, pattern = "*.csv", full.names = TRUE)

# Read original files into data frames
#for (file in original_files) 
{
 # # Read the current original CSV file
  df <- read.csv(file)
  
  # Create a variable name based on the file name (without the .csv extension)
  df_name <- gsub(".csv", "", basename(file))
  
  # Use assign() to create a data frame with the name from the file name
  assign(df_name, df)
}


###### Update and Merge Animal Colnames ########

### Version 1(Both)
##### Version 2 (Serp)
### Version 3 (NS)

#  Alpha{simulation} = alpha;
#P{simulation} = [p < extinct_level_p initial_plants p n sum_pol quantity quality foraging_effort];
#A{simulation} = [a < extinct_level_a a sum_extract];




# List of dataframes to process
dataframes <- c(
            "A_Aikawa_2022_summer_version1", "A_Aikawa_2022_summer_version2", 
                "A_Aikawa_2022_summer_version3", "A_aikawa_2022_version1", 
                "A_aikawa_2022_version2", "A_aikawa_2022_version3", 
                "A_Aikawa_2024_version1", "A_Aikawa_2024_version2", 
                "A_Aikawa_2024_version3", "A_Anu_2024_version1", 
                "A_Anu_2024_version2", "A_Anu_2024_version3", 
                "A_Banana_2022_version1", "A_Banana_2022_version2", 
                "A_Banana_2022_version3", "A_Bertha_2022_summer_version1", 
                "A_Bertha_2022_summer_version2", "A_Bertha_2022_summer_version3", 
                "A_bertha_2022_version1", "A_bertha_2022_version2", 
                "A_bertha_2022_version3", "A_Bertha_2023_summer_version1", 
                "A_Bertha_2023_summer_version2", "A_Bertha_2023_summer_version3", 
                "A_Bertha_2023_version1", "A_Bertha_2023_version2", 
                "A_Bertha_2023_version3", "A_Bertha_2024_version1", 
                "A_Bertha_2024_version2", "A_Bertha_2024_version3", 
                "A_Coyote_24_version1", "A_Coyote_24_version2", 
                "A_Coyote_24_version3", "A_Felch_2024_version1", 
                "A_Felch_2024_version2", "A_Felch_2024_version3", 
                "A_Goatgrass_2022_version1", "A_Goatgrass_2022_version2", 
                "A_Goatgrass_2022_version3", "A_Goatgrass_2024_version1", 
                "A_Goatgrass_2024_version2", "A_Goatgrass_2024_version3", 
                "A_Long_2022_version1", "A_Long_2022_version2", 
                "A_Long_2022_version3", "A_Lower_Banana_2022_version1", 
                "A_Lower_Banana_2022_version2", "A_Lower_Banana_2022_version3", 
                "A_Pond_202_version1", "A_Pond_202_version2", 
                "A_Pond_202_version3", "A_Pond_2023__version1", 
                "A_Pond_2023__version2", "A_Pond_2023__version3", 
                "A_Pond_2024_version1", "A_Pond_2024_version2", 
                "A_Pond_2024_version3", "A_Quarry_1_2024_version1", 
                "A_Quarry_1_2024_version2", "A_Quarry_1_2024_version3", 
                "A_Quarry_2_2024_version1", "A_Quarry_2_2024_version2", 
                "A_Quarry_2_2024_version3", "A_Quarry_2022_version1", 
                "A_Quarry_2022_version2", "A_Quarry_2022_version3", 
                "A_Quarry_3_2024_version1", "A_Quarry_3_2024_version2", 
                "A_Quarry_3_2024_version3", "A_Quarry_4_2024_version1", 
                "A_Quarry_4_2024_version2", "A_Quarry_4_2024_version3", 
                "A_Quarry_5_2024_version1", "A_Quarry_5_2024_version2", 
                "A_Quarry_5_2024_version3", "A_Quarry_Close_2022_version1", 
                "A_Quarry_Close_2022_version2", "A_Quarry_Close_2022_version3", 
                "A_Quarry_Close_2023_version1", "A_Quarry_Close_2023_version2", 
                "A_Quarry_Close_2023_version3", "A_Quarry_Close_2024_version1", 
                "A_Quarry_Close_2024_version2", "A_Quarry_Close_2024_version3", 
                "A_Quarry_Far_2022_version1", "A_Quarry_Far_2022_version2", 
                "A_Quarry_Far_2022_version3", "A_Quarry_Far_2023_version1", 
                "A_Quarry_Far_2023_version2", "A_Quarry_Far_2023_version3", 
                "A_Quarry_Far_2024_version1", "A_Quarry_Far_2024_version2", 
                "A_Quarry_Far_2024_version3", "A_Quarry1_2023_version1", 
                "A_Quarry1_2023_version2", "A_Quarry1_2023_version3", 
                "A_Randy_2022_version1", "A_Randy_2022_version2", 
                "A_Randy_2022_version3", "A_Randy_2023_version1", 
                "A_Randy_2023_version2", "A_Randy_2023_version3", 
                "A_Rock_2022_version1", "A_Rock_2022_version2", 
                "A_Rock_2022_version3", "A_Rock_2023_version1", 
                "A_Rock_2023_version2", "A_Rock_2023_version3", 
                "A_Rock_2024_version1", "A_Rock_2024_version2", 
                "A_Rock_2024_version3", "A_South_Goatgrass_2024_version1", 
                "A_South_Goatgrass_2024_version2", "A_South_Goatgrass_2024_version3", 
                "A_Upper_Grid_2_2024_version1", "A_Upper_Grid_2_2024_version2", 
                "A_Upper_Grid_2_2024_version3", "A_Vineyard_2022_version1", 
                "A_Vineyard_2022_version2", "A_Vineyard_2022_version3", 
                "A_Vineyard_2024_version1", "A_Vineyard_2024_version2", 
                "A_Vineyard_2024_version3")

# Define the new column names
new_colnames <- c("ARTH", 
                  "extinct_level_A_run1", 
                  "animal_abundance_run1", 
                  "sum_extract_run1",  
                  "extinct_level_A_run2", 
                  "animal_abundance_run2", 
                  "sum_extract_run2")

# Loop through each dataframe name and rename columns
for (df_name in dataframes) {
  if (exists(df_name)) {  # Check if the dataframe exists in the environment
    df <- get(df_name)  # Retrieve the dataframe
    if (ncol(df) >= length(new_colnames)) {  # Ensure it has enough columns
      colnames(df)[1:length(new_colnames)] <- new_colnames  # Rename columns
      assign(df_name, df)  # Save it back to the environment
    } else {
      warning(paste("Skipping", df_name, "- not enough columns"))
    }
  } else {
    warning(paste("Skipping", df_name, "- dataframe does not exist"))
  }
}

library(dplyr)

#### merge animal dfs

# Initialize an empty list to store the modified dataframes
df_list <- list()

# Loop through each dataframe name
for (df_name in dataframes) {
  if (exists(df_name)) {  # Check if dataframe exists
    df <- get(df_name)  # Retrieve the dataframe
    
    # Add a new column with the dataframe name
    df$source_dataframe <- df_name
    
    # Extract version info (assuming it appears at the end after "version")
    df$version <- sub(".*_version", "", df_name)  
    
    # Append to list
    df_list[[df_name]] <- df
  } else {
    warning(paste("Skipping", df_name, "- dataframe does not exist"))
  }
}

# Combine all dataframes into one
big_dataframe <- bind_rows(df_list)

# View result
head(big_dataframe)
write.csv(big_dataframe, "animal_outputs.csv", row.names = FALSE)


####### Update and Merge Plant ColNames ########

### Version 1(Both)
##### Version 2 (Serp)
### Version 3 (NS)

#  Alpha{simulation} = alpha;
#P{simulation} = [p < extinct_level_p initial_plants p n sum_pol quantity quality foraging_effort];
#A{simulation} = [a < extinct_level_a a sum_extract];


# List of dataframes to process
dataframes <- c(
  "P_Aikawa_2022_summer_version1", "P_Aikawa_2022_summer_version2", 
  "P_Aikawa_2022_summer_version3", "P_aikawa_2022_version1", 
  "P_aikawa_2022_version2", "P_aikawa_2022_version3", 
  "P_Aikawa_2024_version1", "P_Aikawa_2024_version2", 
  "P_Aikawa_2024_version3", "P_Anu_2024_version1", 
  "P_Anu_2024_version2", "P_Anu_2024_version3", 
  "P_Banana_2022_version1", "P_Banana_2022_version2", 
  "P_Banana_2022_version3", "P_Bertha_2022_summer_version1", 
  "P_Bertha_2022_summer_version2", "P_Bertha_2022_summer_version3", 
  "P_bertha_2022_version1", "P_bertha_2022_version2", 
  "P_bertha_2022_version3", "P_Bertha_2023_summer_version1", 
  "P_Bertha_2023_summer_version2", "P_Bertha_2023_summer_version3", 
  "P_Bertha_2023_version1", "P_Bertha_2023_version2", 
  "P_Bertha_2023_version3", "P_Bertha_2024_version1", 
  "P_Bertha_2024_version2", "P_Bertha_2024_version3", 
  "P_Coyote_24_version1", "P_Coyote_24_version2", 
  "P_Coyote_24_version3", "P_Felch_2024_version1", 
  "P_Felch_2024_version2", "P_Felch_2024_version3", 
  "P_Goatgrass_2022_version1", "P_Goatgrass_2022_version2", 
  "P_Goatgrass_2022_version3", "P_Goatgrass_2024_version1", 
  "P_Goatgrass_2024_version2", "P_Goatgrass_2024_version3", 
  "P_Long_2022_version1", "P_Long_2022_version2", 
  "P_Long_2022_version3", "P_Lower_Banana_2022_version1", 
  "P_Lower_Banana_2022_version2", "P_Lower_Banana_2022_version3", 
  "P_Pond_202_version1", "P_Pond_202_version2", 
  "P_Pond_202_version3", "P_Pond_2023__version1", 
  "P_Pond_2023__version2", "P_Pond_2023__version3", 
  "P_Pond_2024_version1", "P_Pond_2024_version2", 
  "P_Pond_2024_version3", "P_Quarry_1_2024_version1", 
  "P_Quarry_1_2024_version2", "P_Quarry_1_2024_version3", 
  "P_Quarry_2_2024_version1", "P_Quarry_2_2024_version2", 
  "P_Quarry_2_2024_version3", "P_Quarry_2022_version1", 
  "P_Quarry_2022_version2", "P_Quarry_2022_version3", 
  "P_Quarry_3_2024_version1", "P_Quarry_3_2024_version2", 
  "P_Quarry_3_2024_version3", "P_Quarry_4_2024_version1", 
  "P_Quarry_4_2024_version2", "P_Quarry_4_2024_version3", 
  "P_Quarry_5_2024_version1", "P_Quarry_5_2024_version2", 
  "P_Quarry_5_2024_version3", "P_Quarry_Close_2022_version1", 
  "P_Quarry_Close_2022_version2", "P_Quarry_Close_2022_version3", 
  "P_Quarry_Close_2023_version1", "P_Quarry_Close_2023_version2", 
  "P_Quarry_Close_2023_version3", "P_Quarry_Close_2024_version1", 
  "P_Quarry_Close_2024_version2", "P_Quarry_Close_2024_version3", 
  "P_Quarry_Far_2022_version1", "P_Quarry_Far_2022_version2", 
  "P_Quarry_Far_2022_version3", "P_Quarry_Far_2023_version1", 
  "P_Quarry_Far_2023_version2", "P_Quarry_Far_2023_version3", 
  "P_Quarry_Far_2024_version1", "P_Quarry_Far_2024_version2", 
  "P_Quarry_Far_2024_version3", "P_Quarry1_2023_version1", 
  "P_Quarry1_2023_version2", "P_Quarry1_2023_version3", 
  "P_Randy_2022_version1", "P_Randy_2022_version2", 
  "P_Randy_2022_version3", "P_Randy_2023_version1", 
  "P_Randy_2023_version2", "P_Randy_2023_version3", 
  "P_Rock_2022_version1", "P_Rock_2022_version2", 
  "P_Rock_2022_version3", "P_Rock_2023_version1", 
  "P_Rock_2023_version2", "P_Rock_2023_version3", 
  "P_Rock_2024_version1", "P_Rock_2024_version2", 
  "P_Rock_2024_version3", "P_South_Goatgrass_2024_version1", 
  "P_South_Goatgrass_2024_version2", "P_South_Goatgrass_2024_version3", 
  "P_Upper_Grid_2_2024_version1", "P_Upper_Grid_2_2024_version2", 
  "P_Upper_Grid_2_2024_version3", "P_Vineyard_2022_version1", 
  "P_Vineyard_2022_version2", "P_Vineyard_2022_version3", 
  "P_Vineyard_2024_version1", "P_Vineyard_2024_version2", 
  "P_Vineyard_2024_version3"
)


# Define the new column names
new_colnames <- c("PLANT", 
                  "extinct_level_P_run1", 
                  "initial_plant_abundance_run1", 
                  "plant_abundance_run1",
                  "reward_abundance_run1",  
                  "sum_pol_run1", 
                  "visit_quanity_run1", 
                  "visit_quality_run1",
                  "foraging_effort_run1",
                  "extinct_level_P_run2", 
                  "initial_plant_abundance_run2", 
                  "plant_abundance_run2",
                  "reward_abundance_run2",  
                  "sum_pol_run2", 
                  "visit_quanity_run2", 
                  "visit_quality_run2",
                  "foraging_effort_run2"
                  )



# Loop through each dataframe name and rename columns
for (df_name in dataframes) {
  if (exists(df_name)) {  # Check if the dataframe exists in the environment
    df <- get(df_name)  # Retrieve the dataframe
    if (ncol(df) >= length(new_colnames)) {  # Ensure it has enough columns
      colnames(df)[1:length(new_colnames)] <- new_colnames  # Rename columns
      assign(df_name, df)  # Save it back to the environment
    } else {
      warning(paste("Skipping", df_name, "- not enough columns"))
    }
  } else {
    warning(paste("Skipping", df_name, "- dataframe does not exist"))
  }
}

library(dplyr)

#### merge plant dfs

# Initialize an empty list to store the modified dataframes
df_list <- list()

# Loop through each dataframe name
for (df_name in dataframes) {
  if (exists(df_name)) {  # Check if dataframe exists
    df <- get(df_name)  # Retrieve the dataframe
    
    # Add a new column with the dataframe name
    df$source_dataframe <- df_name
    
    # Extract version info (assuming it appears at the end after "version")
    df$version <- sub(".*_version", "", df_name)  
    
    # Append to list
    df_list[[df_name]] <- df
  } else {
    warning(paste("Skipping", df_name, "- dataframe does not exist"))
  }
}

# Combine all dataframes into one
big_dataframe <- bind_rows(df_list)

# View result
head(big_dataframe)
write.csv(big_dataframe, "plant_outputs.csv", row.names = FALSE)
     