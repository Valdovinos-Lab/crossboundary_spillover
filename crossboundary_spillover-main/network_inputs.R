######## Proccess Network inputs ######
#### Code for processing emprical network properties #####
##### for Becca and Taran spillover project #######

###### date created: 3-18-2025 ###########
######### date last modified: 3-18-2025 #######

## to do: need to add nestedness and niche overlap 

### Load relevant files 
spring_close <- read.csv("network_closeness_spring.csv")
summer_close <- read.csv("network_closeness_summer.csv")

spring_between <- read.csv("network_betweenness_spring.csv")
summer_between <- read.csv("network_betweenness_summer.csv")

summer_jaccard <- read.csv("ceso_vs_heex_jaccard_values.csv")
summer_sorensen <- read.csv("ceso_vs_heex_sorensen_values.csv")
spring_jaccard <- read.csv("vivi_vs_trfu_jaccard_values.csv")
spring_jaccard <- read.csv("vivi_vs_trfu_sorensen_values.csv")

spring <- read.csv("spring_network_metrics_2_4_25.csv")
summer <- read.csv("summer_network_metrics_2_4_25.csv")

sort(unique(plant_long$site_year))
sort(unique(animal_long$site_year)) ##issue with coyote 24 and pond 22

sort(unique(summer$site))
setdiff(sort(unique(summer$site)), unique(plant_long$site_year))

summer <- summer %>%
  mutate(site = recode(site, 
                       "Aikawa_2022" = "Aikawa_2022_summer", 
                       "AIkawa_2022" = "Aikawa_2022_summer", 
                       "Bertha_2022" = "Bertha_2022_summer",
                       "Bertha_2023" = "Bertha_2023_summer"))

summer_between <- summer_between %>%
  mutate(site = recode(site, 
                       "Aikawa_2022" = "Aikawa_2022_summer", 
                       "Bertha_2022" = "Bertha_2022_summer",
                       "Bertha_2023" = "Bertha_2023_summer"))

summer_close <- summer_close %>%
  mutate(site = recode(site, 
                       "Aikawa_2022" = "Aikawa_2022_summer", 
                       "Bertha_2022" = "Bertha_2022_summer",
                       "Bertha_2023" = "Bertha_2023_summer"))

summer_jaccard <- summer_jaccard %>%
  mutate(Network = recode(Network, 
                       "Aikawa_2022" = "Aikawa_2022_summer", 
                       "Bertha_2022" = "Bertha_2022_summer",
                       "Bertha_2023" = "Bertha_2023_summer"))

summer_sorensen <- summer_sorensen %>%
  mutate(Network = recode(Network, 
                          "Aikawa_2022" = "Aikawa_2022_summer", 
                          "Bertha_2022" = "Bertha_2022_summer",
                          "Bertha_2023" = "Bertha_2023_summer"))
