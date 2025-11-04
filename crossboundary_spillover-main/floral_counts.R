####################################################
###########################################################
######## Clean and Process Empirical Floral Abundance Data #######
######## Code by Rebecca Nelson ###############
###### created: 9-8-25 #########################
######### last updated: 11-4-25 ############
#################################################
#process plant coverage (floral abundance counts) of empirical networks for use in simulation 

## to do: summer floral counts 

########## Load Data and Packages ###########
## load packages
library(tidyverse)

## empirical floral abundance counts 
plant_abund_22 <- read.csv("plant_abund_spring22.csv")
plant_abund_23 <- read.csv("plant_abund_spring23.csv")
plant_abund_24 <- read.csv("floral_abund_24.csv")


### combine floral abundance data ########

plant_abund_22$Transect <- plant_abund_22$Transect_ID
plant_abund_23$Transect <- plant_abund_23$Tranesect
plant_abund_22$Floral_Abundance <- plant_abund_22$Estimated_Abundance
plant_abund_22$PLANT <- plant_abund_22$Species

plant_abund_22 <- plant_abund_22 %>% dplyr::select(Year, Month, Day, Transect, Site, PLANT, Floral_Abundance)
plant_abund_23 <- plant_abund_23 %>% dplyr::select(Year, Month, Day, Transect, Site, PLANT, Floral_Abundance)
plant_abund_24 <- plant_abund_24 %>% dplyr::select(Year, Month, Day, Transect, Site, PLANT, Floral_Abundance)

plant_abund <- rbind(plant_abund_22, plant_abund_23, plant_abund_24)

#site_richness <- plant_abund %>%  group_by(Site, Year) %>% summarize(Site_Plant_Richness = length(unique(PLANT))) 

plant_abund$Floral_Abundance <- as.numeric(plant_abund$Floral_Abundance)


plant_abund <- plant_abund %>% mutate(Year = if_else(is.na(Year), 2024, Year)) %>% mutate(Month = if_else(is.na(Month), 4, Month)) 


plant_abund$Day[plant_abund$Day == "4/20/2024"] <- "20"
plant_abund$Day[plant_abund$Day == "4/24/2024"] <- "24"
plant_abund$Day[plant_abund$Day == "4/25/2024"] <- "25"


plant_abund$Day <- as.numeric(plant_abund$Day)

library(lubridate)
## make a date column that gives year, month, and day as a lubridate object
plant_abund$date = ymd(paste(plant_abund$Year, "-", plant_abund$Month, "-", plant_abund$Day))

##### fix spelling and naming
plant_abund$PLANT[plant_abund$PLANT == "Alium"] <- "ALAM"
plant_abund$PLANT[plant_abund$PLANT == "Allium"] <- "ALAM"
plant_abund$PLANT[plant_abund$PLANT == "ASRAJE"] <- "ASJE"
plant_abund$PLANT[plant_abund$PLANT == "DEVA1"] <- "DEVA"
plant_abund$PLANT[plant_abund$PLANT == "CASRUB"] <- "CARU"
plant_abund$PLANT[plant_abund$PLANT == "GERDI"] <- "GEDI"
plant_abund$PLANT[plant_abund$PLANT ==  "LACA "] <- "LACA"
plant_abund$PLANT[plant_abund$PLANT ==  "VICSAT"] <- "VISA"
plant_abund$PLANT[plant_abund$PLANT ==  "silverpuff"] <- "URLI"
plant_abund$PLANT[plant_abund$PLANT ==  "sidalcea"] <- "Sidalcea"
plant_abund$PLANT[plant_abund$PLANT ==  "tidytips"] <- "LACH"
plant_abund$PLANT[plant_abund$PLANT ==  "valley_tassel"] <- "CAAT"
plant_abund$PLANT[plant_abund$PLANT ==  "blowives"] <- "ACMO"
plant_abund$PLANT[plant_abund$PLANT ==  "blowwives"] <- "ACMO"
plant_abund$PLANT[plant_abund$PLANT ==  "fishhook"] <- "ANFI"
plant_abund$PLANT[plant_abund$PLANT ==  "fishook"] <- "ACMO"
plant_abund$PLANT[plant_abund$PLANT ==  "LACSER"] <- "LASE"
plant_abund$PLANT[plant_abund$PLANT ==  "narrowleaf_onion"  ] <- "ALAM"
plant_abund$PLANT[plant_abund$PLANT ==  "pink_onion"  ] <- "ALAM"
plant_abund$PLANT[plant_abund$PLANT ==  "PDICA"] <- "DICA"
plant_abund$PLANT[plant_abund$PLANT ==  "red_lomatium"  ] <- "LOHO"
plant_abund$PLANT[plant_abund$PLANT ==  "riggioappus" ] <- "RILE"
plant_abund$PLANT[plant_abund$PLANT ==  "pepperweed"  ] <- "LENI"
plant_abund$PLANT[plant_abund$PLANT ==  "itherial_spear"  ] <- "TRLA"
plant_abund$PLANT[plant_abund$PLANT ==  "scarlet_pimpernel"] <- "ANAR"
plant_abund$PLANT[plant_abund$PLANT ==  "it_thistle"] <- "CAPY"
plant_abund$PLANT[plant_abund$PLANT ==  "evening_snow"] <- "LIDI"
plant_abund$PLANT[plant_abund$PLANT ==  "bitteroot"] <- "LERE"
plant_abund$PLANT[plant_abund$PLANT ==  "owl_clover"] <- "CAEX"
plant_abund$PLANT[plant_abund$PLANT ==  "rusty_popcorn"] <- "PLNO"
plant_abund$PLANT[plant_abund$PLANT ==  "sowthistle"] <- "SOAS"
plant_abund$PLANT[plant_abund$PLANT ==  "bluecup"] <- "GISP"
plant_abund$PLANT[plant_abund$PLANT ==  "charlock"] <- "MUAR"               
plant_abund$PLANT[plant_abund$PLANT ==  "euro_buttercup"] <- "Ranunculus_sp"  
plant_abund$PLANT[plant_abund$PLANT ==  "Euro_buttercup" ] <- "Ranunculus_sp"  
plant_abund$PLANT[plant_abund$PLANT ==  "smooth_catsear" ] <- "HYGL"                      
plant_abund$PLANT[plant_abund$PLANT ==  "small_cats_ear" ] <- "HYGL"                      
plant_abund$PLANT[plant_abund$PLANT ==  "salsify" ] <- "TRADU"   
plant_abund$PLANT[plant_abund$PLANT ==  "lacy_orange" ] <- "CHGL"   
plant_abund$PLANT[plant_abund$PLANT ==  "hairpink"] <- "PEDU"  
plant_abund$PLANT[plant_abund$PLANT ==  "pinapple_plant"] <- "MADI"    
plant_abund$PLANT[plant_abund$PLANT ==  "purple_sanicle"] <- "SABI"  
plant_abund$PLANT[plant_abund$PLANT ==  "shortpod"] <- "HIIN"  
plant_abund$PLANT[plant_abund$PLANT ==  "tall_mustard"] <- "HIIN" 
plant_abund$PLANT[plant_abund$PLANT ==  "mustard" ] <- "HIIN" 
plant_abund$PLANT[plant_abund$PLANT ==  "musatrd"   ] <- "HIIN" 
plant_abund$PLANT[plant_abund$PLANT ==  "OC_mustard"  ] <- "HIIN" 
plant_abund$PLANT[plant_abund$PLANT ==    "oc_mustard"   ] <- "HIIN" 
plant_abund$PLANT[plant_abund$PLANT ==   "mustard_occidentalis" ] <- "HIIN" 
plant_abund$PLANT[plant_abund$PLANT ==   "occidentalis_mustard"] <- "HIIN" 
plant_abund$PLANT[plant_abund$PLANT ==   "occientalis_mustard"  ] <- "HIIN" 
plant_abund$PLANT[plant_abund$PLANT ==    "ocidentalis_mustard"  ] <- "HIIN" 
plant_abund$PLANT[plant_abund$PLANT ==    "hairypink"   ] <- "PEDU" 
plant_abund$PLANT[plant_abund$PLANT ==    "pink-onion"   ] <- "ALAM" 
plant_abund$PLANT[plant_abund$PLANT ==    "butteregg"  ] <- "TRER" 
plant_abund$PLANT[plant_abund$PLANT ==    "buttereggs"    ] <- "TRER" 


## mean abundance by flower species across season
total_floral_abund <- plant_abund %>% na.omit() %>%  group_by(Site, Year, date, PLANT) %>% summarize(floral_abundance_sum = sum(Floral_Abundance)) %>% group_by(Site, Year, PLANT)  %>% summarize(mean_floral_abundance = mean(floral_abundance_sum)) 
## totals floral abundance counts for each species across transects to get the total for a given site-year on each survey date and then takes the mean of those totals. 

## filter out poorly resolved or identified plants 
remove_species <- c("aster", "agag", "wooly_foot_apiaceae", "yellow_lomatium", "tall_pink", "pinkball", "whitepuff",  "4 Petal Yellow", "Arcgheopathis", "Clarkia", "coryopsis", "deathcamas", "yellow", "VIAM?", "yellow_aster", "Yellow Biscuit Root", "white_lomatium", "Yellow Lomatian", "riggioappus?", "small_yellow_aster", "Trcup_pink", "UNK_mustard" ,  "sunflower?",  "Round White Flower",  "round_yellow",  "Gumplant?", "dandelion", "dandlion", "dark_Yellow_Aster", "hairypink?",           "Hairypink?",  "gray_spurge",  "white_castilleja", "pale_spurge",  "fuzzy_ball",  "butter", "little_purple",       "Little-eared-phlox?")

total_floral_abund_clean <- total_floral_abund %>%
  filter(!PLANT %in% remove_species)


## save cleaned data:
write.csv(total_floral_abund_clean, "spring_empirical_coverage.csv", row.names = FALSE)
