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
spring_sorensen <- read.csv("vivi_vs_trfu_sorensen_values.csv")

spring <- read.csv("spring_network_metrics_2_4_25.csv")
summer <- read.csv("summer_network_metrics_2_4_25.csv")

spring_full <- read.csv("spring_metrics_10_17_24.csv")
summer_full <- read.csv("summer_metrics_10_24_24.csv")

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

summer_full <- summer_full %>%
  mutate(treatment = recode(treatment, 
                          "Aikawa_2022" = "Aikawa_2022_summer", 
                          "Bertha_2022" = "Bertha_2022_summer",
                          "Bertha_2023" = "Bertha_2023_summer"))

### standardize names
summer_sorensen <- summer_sorensen %>%
  rename(site_year = Network)
summer_jaccard <- summer_jaccard %>%
  rename(site_year = Network)
spring_sorensen <- spring_sorensen %>%
  rename(site_year = Network)
spring_jaccard <- spring_jaccard %>%
  rename(site_year = Network)


summer <- summer %>%
  rename(site_year = site)
summer_close <- summer_close %>%
  rename(site_year = site)
summer_between <- summer_between %>%
  rename(site_year = site)

spring <- spring %>%
  rename(site_year = site)
spring_close <- spring_close %>%
  rename(site_year = site)
spring_between <- spring_between %>%
  rename(site_year = site)

spring_full <- spring_full %>%
  rename(site_year = treatment)

summer_full <- summer_full %>%
  rename(site_year = treatment)

###### combine dataframes ######
networks <- rbind(spring, summer)
jaccard <- rbind(spring_jaccard, summer_jaccard)
sorensen <- rbind(spring_sorensen, summer_sorensen)
networks_full <- rbind(spring_full, summer_full)

write.csv(networks_full, "networks_full.csv", row.names = FALSE)

