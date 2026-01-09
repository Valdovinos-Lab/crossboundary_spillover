####################################################
###########################################################
######## Generate Networks Spillover Data-Theory Integration #######
######## Code by Rebecca Nelson ###############
###### created: 3-5-24 #########################
######### last updated: 1-8-26 ############
#################################################
## generate empirical networks used as inputs for spillover modeling in matlab


rm(list = ls())

##### load required packages #########
require(tidyverse)
require(nlme)
require(lubridate)
require(bipartite)
##### upload, clean, and merge data #####
# visitation data
spring <- read.csv("raw_data/spring_5_29_25.csv")
summer <- read.csv("raw_data/summer_5_29_25.csv")

######## Clean Data ########################
## might be already cleaned but check just in case 

spring$PLANT[spring$PLANT == "AGBR"] <- "ASBR"
spring$PLANT[spring$PLANT == "TRGA"] <- "TRGR"
spring$PLANT[spring$PLANT == "LOWR"] <- "ACWR"
spring$PLANT[spring$PLANT == "MICAL"] <- "MICA"
spring$PLANT[spring$PLANT == "MIGU"] <- "ERGU"
spring$PLANT[spring$PLANT == "URCI"] <- "URLI"

spring <-spring %>%
  filter(!ARTH %in% c("UNIN", "SAB", "UNKBEE", "UNK_HYM", "UNKBEE", "UNKBEE/FLY"))
spring$PLANT[spring$PLANT == "TOFR."] <- "TOFR"
spring$PLANT[spring$PLANT == "MICAL"] <- "MICA"


summer$ARTH[summer$ARTH == "Coleoptea"] <- "Coleoptera"
summer$ARTH[summer$ARTH == "Bombus_californica"] <- "Bombus_californicus"

### Function for Generating Networks ##########
generate_and_save_networks <- function(df, group_col, site_year_vec, filename_suffix, output_dir = "input_networks") {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  for (sy in site_year_vec) {
    site_df <- df %>% filter(!!sym(group_col) == sy)
    if (nrow(site_df) == 0) next
    
    net <- table(site_df$PLANT, site_df$ARTH) %>% as.matrix()
    site_clean <- gsub(" ", "_", sy)
    file_name <- paste0(output_dir, "/", site_clean, "_", filename_suffix, ".csv")
    write.csv(net, file_name, row.names = TRUE)
  }
}

#### Generate and save SPRING networks ####
spring$Site_Year <- paste0(spring$Site, "_", spring$Year)
spring_sites <- unique(spring$Site_Year)
generate_and_save_networks(spring, "Site_Year", spring_sites, "spring", output_dir = "input_networks")

#### Generate and save SUMMER networks ####
summer_sites <- unique(summer$Meadow_Year)
generate_and_save_networks(summer, "Meadow_Year", summer_sites, "summer", output_dir = "input_networks")

