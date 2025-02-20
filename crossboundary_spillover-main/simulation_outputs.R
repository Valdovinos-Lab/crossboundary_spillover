######## Analyze MATLAB Outputs ######

##### for Becca and Taran spillover project #######

###### date created: 2-19-2025 ###########
######### date last modified: 2-19-2025 #######
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
    df <- read.csv(file)
    
    # Create a variable name based on the file name (without the .csv extension)
    df_name <- gsub(".csv", "", basename(file))
    
    # Use assign() to create a data frame with the name from the file name
    assign(df_name, df)
  }
}

# List of original files to use as reference for row and column names
original_files <- list.files(folder_path_2, pattern = "*.csv", full.names = TRUE)

# Read original files into data frames
for (file in original_files) {
  # Read the current original CSV file
  df <- read.csv(file)
  
  # Create a variable name based on the file name (without the .csv extension)
  df_name <- gsub(".csv", "", basename(file))
  
  # Use assign() to create a data frame with the name from the file name
  assign(df_name, df)
}



# Now process the modified files based on the groups
#processing complete for all plant and animal group files.")
# Now process the modified files based on the groups

# Now process the modified files based on the groups
for (file in csv_files) {
  # Skip files that start with "A" or "P" or are test files like "1.csv"
  if (grepl("^[A|P]", basename(file)) && !grepl("^1\\.csv$", basename(file))) {
    
    # Get the corresponding original file base name (without version or suffix)
    original_file_name <- gsub("^P_|^A_", "", basename(file))  # Remove P_ or A_ prefix
    original_file_name <- sub("_version[0-9]+", "", original_file_name)  # Remove version part (e.g., _version1)
    
    # Special case for files that start with "lower" (e.g., Lower_Banana)
    if (grepl("^lower", tolower(original_file_name))) {
      cat("Special case for lower version file detected: ", basename(file), "\n")
      next  # Skip files that have "lower" in their name (like "Lower_Banana")
    }
    
    # Debugging step: Print the original file name for match
    cat("Original file name for match:", original_file_name, "\n")
    
    # Find the original files that match the modified file's base name
    original_file_matches <- original_files[grep(original_file_name, basename(original_files))]
    
    # Debugging step: Print the matched original file paths
    cat("Matching original file paths:", original_file_matches, "\n")
    
    # If there are no matches, skip the file
    if (length(original_file_matches) == 0) {
      cat("No matching original file found for:", basename(file), "\n")
      next  # Skip to the next file if no match is found
    }
    
    # If there are multiple matches, choose the most appropriate one (based on the first match or another logic)
    if (length(original_file_matches) > 1) {
      cat("Multiple matching original files found for:", basename(file), ". Choosing the first match.\n")
      original_file <- original_file_matches[1]  # Choose the first match (can be modified as needed)
    } else {
      original_file <- original_file_matches
    }
    
    # Read the original file
    original_df <- read.csv(original_file)
    
    # Read the modified file (plant or animal group)
    modified_df <- read.csv(file)
    
    # Check if row count matches before proceeding
    if (nrow(original_df) != nrow(modified_df)) {
      cat("Row count mismatch for file:", basename(file), "Skipping this file...\n")
      next  # Skip this file if the row count doesn't match
    }
    
    # If it's a plant group file (P_*)
    if (grepl("^P_", basename(file))) {
      # Set the row names of the modified file based on the original file
      rownames(modified_df) <- rownames(original_df)
      
      # Save the modified file with new row names
      output_file <- file  # You can modify the path here if you want a different output folder
      write.csv(modified_df, output_file, row.names = TRUE)
      
      cat("Updated row names for plant group file:", basename(file), "\n")
      
    } else if (grepl("^A_", basename(file))) {
      # If it's an animal group file (A_*)
      # Set the row names of the modified file based on the column names of the original file
      rownames(modified_df) <- colnames(original_df)
      
      # Save the modified file with new row names
      output_file <- file  # You can modify the path here if you want a different output folder
      write.csv(modified_df, output_file, row.names = TRUE)
      
      cat("Updated row names for animal group file:", basename(file), "\n")
    }
  }
}

cat("Processing complete for all plant and animal group files.")





     