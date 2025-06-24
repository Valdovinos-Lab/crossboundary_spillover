######## Identify Overlap between soil types ######
#### Code for analyzing matlab outputs into R dataframes #####
##### for Becca and Taran spillover project #######
###### date created: 3-19 2025 ###########
######### date last modified: 3-19-2025 #######

###### Load required packages ######
library(tidyverse)

###### Load combined outputs ######
spring <- read.csv("spring.csv")
summer <- read.csv("summer.csv")


######## find overlap ######

NS_spring <- spring %>% filter(PLANT %in% c('ASER', 'VIVI', 'CESO', 'MEIN', 'PHAQ', 'ANAR', 'AMME', 
     'ERCI', 'mustard', 'yellow_aster', 'dandelion', 'MEPO', 'SEVU')) %>% filter(!ARTH %in% c('UNIN', 'SAB', 'UNKFLY', 'UNKBEE')) %>% distinct(ARTH) 


NS_spring_ARTH <- c("Habropoda_trissema", "Anthophora_californica", "Colias_eurytheme", 
                 "Prosperpinus_clarkiae", "Apis_mellifera", "Bombus_vosnesenskii", 
                 "Osmia_cara", "Bombus_melanopygus", "gray_moth_Lepidoptera", 
                 "Eucera_actuosa", "Listrus_sp._1", "Eucera_frater_albopilosa", 
                 "Bombylius_major", "Scaeva_affinis", "Glaucophysche_lygdamus", 
                 "Junonia_grisea", "Polistes_aurifer", "Helithodes_dimutiva", 
                 "Muscidae", "Diadasia_nigrifrons", "Habropoda_depressa", 
                 "Euopeodes_fumipennis", "UNKSYR", "Trichodes_ornatus", "Osmia_sp.", 
                 "Chrysoperla_sp.", "Nomada_hesperia", "Xylocopa_sp.", 
                 "Scathophaga_stercoraria", "Hippodamia_convergens", "Bombus_californicus", 
                 "Autographa_californica", "Osmia_atrocyanea", "Braconidae", 
                 "Panurginus_sp.", "Hoplitus_hypocrita", "Bombus_sp.", "Bombus_crotchii", 
                 "Conophorus_fenestratus", "Coenonympha_californica", "Anthophora_edwardsii", 
                 "Habropoda_sp.", "Diadasia_bituberculata", "Habropoda_sp.", 
                 "Osmia_sanctaerosae", "Andrena_nigrocaerulea", "Vanessa_cardui", 
                 "Sphex_sp.", "Thysanoptera", "black_Coleoptera", "Xylocopa_tabaniformis", 
                 "Lasiglossum (Hemihalictus sp.)", "Anthophora_crotchii", 
                 "Andrena_pallidifovea", "Erynnis_propertius", "Andrena_sp.", 
                 "Osmia_nemoris", "Lasioglossum_titusi", "Andrena_pensilis", "Xylocopa_sonorina", 
                 "bluish_moth_Lepidoptera", "Sphaerophoria_sp.", "Andrena_subchalybea")

overlap_spring <- spring %>%
  filter(ARTH %in% NS_spring_ARTH) %>% distinct(PLANT)


NS_summer <- summer %>% filter(PLANT %in% c('ASER', 'VIVI', 'CESO', 'MEIN', 'PHAQ', 'ANAR', 'AMME', 
                                            'ERCI', 'mustard', 'yellow_aster', 'dandelion', 'MEPO', 'SEVU')) %>% filter(!ARTH %in% c('UNIN', 'SAB', 'UNKFLY', 'UNKBEE')) %>% distinct(ARTH) 


NS_summer_ARTH <-  c("Icaricia_acmon", "Apis_mellifera", "Ashmeadiella_aridula_astragali",
                                     "Braconidae", "large_black_beetle_Coleoptera", "Bombus_californica",
                                     "Bombus_crotchii", "Bombus_vosnesenskii", "Bombus_sp.", "Junonia_grisea",
                                     "Chrysochus_colbaltinus", "Dianthidium_dubium", "Erynnis_tristus",
                                     "Eristalis_stipador", "Heliothis_phloxiphaga", "Melissodes_lupina",
                                     "Bombylius_major", "Melissodes_robustior", "Strymon_melinus",
                                     "Agapostemon_subtilior", "Halictus_ligatus", "Halictidae",
                                     "Halictus_tripartitus", "Heliothodes_diminutivus", "Megachile_montivaga",
                                     "Megachile_frugalis_pseudofrugalis", "Megachile_apicalis",
                                     "Danaus_plexippus", "Polistes_aurifer", "Panurginus_sp.",
                                     "Villa_sp.", "Heliopetes_ericetorum", "Colias_eurytheme",
                                     "Lasioglossum_incompletum", "Nemognatha_scutellaris",
                                     "Ceratina_punctigena", "Coenonympha_californica", "Listrus_sp_1",
                                     "Hippodamia_convergens", "small_Syrphidae", "small_red_Coleoptera",
                                     "Autographa_californica", "Megachile-mimic_Syrphidae",
                                     "gray_Bombyliidae", "Listrus_sp.", "Nomada_obscurella",
                                     "black_Sphex_sp.", "black_round_beetle_Coleoptera",
                                     "small_black_red_Coleoptera", "Euodyneras_hidalgo",
                                     "Xeromelecta_californica", "Lasioglossum_titusi")

  
overlap_summer <- summer %>%
  filter(ARTH %in% NS_summer_ARTH) %>% distinct(PLANT)
