#### Simulation Visualization #####
##### code by Becca Nelson  #######
###### date created: 10-31-2025 ###########
######### date last modified: 10-31-2025 #######
###### create conceptual figure of simulation design with actual data #######
library(ggplot2)
library(dplyr)
library(tidyr)

small_web <- read.csv("input_networks/Quarry_Far_2024_summer.csv")

medium_web <- read.csv("input_networks/Bertha_2024_summer.csv")

large_web <- read.csv("input_networks/Bertha_2022_spring.csv")


non_serp_plants <- c('ASER', 'VIVI', 'CESO', 'MEIN', 'PHAQ', 'ANAR', 
                     'AMME', 'ERCI', 'HIIN', 'Asteraceae sp.', 
                     'TAOF', 'MEPO', 'SEVU')



######## version 1 with shared pollinators in middle ##################

make_network_plot <- function(web, web_name, non_serp_plants) {
  

  colnames(web)[1] <- "Plant"
  
## make it binary network
  web_bin <- web %>%
    mutate(across(-Plant, ~ ifelse(. > 0, 1, 0)))
  

  plant_regions <- data.frame(
    Plant = web_bin$Plant,
    region = ifelse(web_bin$Plant %in% non_serp_plants, "NS", "S")
  )
  

  interactions <- web_bin %>%
    pivot_longer(cols = -Plant, names_to = "animal", values_to = "presence") %>%
    filter(presence > 0) %>%
    left_join(plant_regions, by = "Plant")
  

  animal_regions <- interactions %>%
    group_by(animal) %>%
    summarise(
      soils = list(unique(region)),
      region = case_when(
        length(soils[[1]]) == 2 ~ "spillover",
        soils[[1]][1] == "S" ~ "S",
        TRUE ~ "NS"
      )
    )
  

  interactions <- interactions %>%
    left_join(animal_regions, by = "animal", suffix = c("_plant", "_animal"))
  

  plants <- plant_regions %>%
    mutate(
      x = ifelse(region == "S", 1, 4),
      y = seq(10, 1, length.out = n())
    )
  
  animals <- animal_regions %>%
    mutate(
      x = case_when(
        region == "S" ~ 2,
        region == "NS" ~ 3,
        TRUE ~ 2.5
      ),
      y = seq(10, 1, length.out = n())
    )
  

  interactions <- interactions %>%
    left_join(plants, by = c("Plant" = "Plant")) %>%
    rename(xp = x, yp = y) %>%
    left_join(animals, by = "animal") %>%
    rename(xa = x, ya = y)

  cols <- c("S" = "#1f78b4", "NS" = "#ffcc00", "spillover" = "#984ea3")
  

  p <- ggplot() +
    geom_segment(
      data = interactions,
      aes(x = xp, xend = xa, y = yp, yend = ya, color = region_animal),
      alpha = 0.6
    ) +
    geom_text(data = plants,
              aes(x, y, label = Plant, color = region),
              size = 4) +
    geom_text(data = animals,
              aes(x, y, label = animal, color = region),
              size = 3.5) +
    geom_vline(xintercept = 2.5, linetype = "dashed", color = "gray40") +
    annotate("text", x = 1.5, y = max(plants$y) + 2,
             label = "Serpentine (S)", size = 6, color = "#1f78b4", vjust = 0) +
    annotate("text", x = 3.5, y = max(plants$y) + 2,
             label = "Non-serpentine (NS)", size = 6, color = "#ffcc00", vjust = 0) +
    annotate("text", x = 2.5, y = min(plants$y) - 2,
             label = "Full", size = 6, color = "#984ea3", vjust = 1) +
    scale_color_manual(values = cols, guide = "none") +
    theme_void() +
    theme(
      plot.margin = margin(40, 40, 40, 40),
      panel.border = element_rect(color = "#984ea3", fill = NA, linewidth = 1.5)
    )
  

  outfile <- paste0("conceptual_", web_name, ".pdf")
  ggsave(outfile, plot = p,
         width = 10, height = 6, units = "in", dpi = 600, device = cairo_pdf)
  
  message("✅ Saved ", outfile)
  
  return(p)
}


p_small <- make_network_plot(small_web, "small_web", non_serp_plants)
p_medium <- make_network_plot(medium_web, "medium_web", non_serp_plants)
p_large <- make_network_plot(large_web, "large_web", non_serp_plants)


########## Separate Subnetwork version ################


make_network_plot_fixed4 <- function(web, web_name, non_serp_plants) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  colnames(web)[1] <- "Plant"
  

  web_bin <- web %>% mutate(across(-Plant, ~ ifelse(. > 0, 1, 0)))
  

  web_bin <- web_bin %>% mutate(region_plant = ifelse(Plant %in% non_serp_plants, "NS", "S"))
  

  interactions <- web_bin %>%
    pivot_longer(cols=-c(Plant, region_plant), names_to="animal", values_to="presence") %>%
    filter(presence > 0)

  animals_S <- interactions %>% filter(region_plant=="S") %>% pull(animal) %>% unique()
  animals_NS <- interactions %>% filter(region_plant=="NS") %>% pull(animal) %>% unique()
  shared_animals <- intersect(animals_S, animals_NS)
  

  subnetworks <- list()
  for(side in c("S","NS")) {
    plants_side <- web_bin %>% filter(region_plant==side)
    animals_side <- interactions %>% filter(region_plant==side) %>% pull(animal) %>% unique()
    

    inter_side <- interactions %>% filter(region_plant==side & animal %in% animals_side)
    

    plants_coord <- plants_side %>% mutate(x = ifelse(side=="S", 1, 4), y = seq(10,1,length.out=n()))
    animals_coord <- data.frame(animal=animals_side, 
                                x = ifelse(side=="S", 2, 3), 
                                y = seq(10,1,length.out=length(animals_side)),
                                stringsAsFactors = FALSE)
    

    animals_coord <- animals_coord %>% 
      mutate(color = ifelse(animal %in% shared_animals, "#984ea3",
                            ifelse(side=="S","#1f78b4","#ffcc00")))
    

    inter_side <- inter_side %>%
      left_join(plants_coord %>% select(Plant, xp=x, yp=y), by="Plant") %>%
      left_join(animals_coord %>% rename(xa=x, ya=y, line_color=color), by="animal")
    
    subnetworks[[side]] <- list(interactions=inter_side, plants=plants_coord, animals=animals_coord)
  }
  

  p <- ggplot()
  for(side in names(subnetworks)) {
    inter <- subnetworks[[side]]$interactions
    plants <- subnetworks[[side]]$plants
    animals <- subnetworks[[side]]$animals
    
    p <- p +
      geom_segment(data=inter, aes(x=xp, xend=xa, y=yp, yend=ya), color=inter$line_color, alpha=0.6) +
      geom_text(data=plants, aes(x=x, y=y, label=Plant), color=ifelse(side=="S","#1f78b4","#ffcc00"), size=4) +
      geom_text(data=animals, aes(x=x, y=y, label=animal), color=animals$color, size=3.5)
  }
  

  p <- p +
    annotate("rect", xmin=0.5, xmax=4.5, ymin=0.5, ymax=11, fill=NA, color="#984ea3", size=1.5) +
    annotate("text", x=2.5, y=11.5, label="Full", size=6, color="#984ea3", vjust=0) +
    geom_vline(xintercept=2.5, linetype="dashed", color="gray40") +
    annotate("text", x=1.5, y=12, label="Serpentine (S)", size=6, color="#1f78b4") +
    annotate("text", x=3.5, y=12, label="Non-serpentine (NS)", size=6, color="#ffcc00") +
    theme_void()
  
  outfile <- paste0("conceptual_", web_name, "_separateNetworks_colored.pdf")
  ggsave(outfile, plot=p, width=10, height=6, dpi=600, device=cairo_pdf)
  message("✅ Saved ", outfile)
  
  return(p)
}


p_small <- make_network_plot_fixed4(small_web, "small_web", non_serp_plants)
p_medium <- make_network_plot_fixed4(medium_web, "medium_web", non_serp_plants)
p_large <- make_network_plot_fixed4(large_web, "large_web", non_serp_plants)

####### Combined Network ########
make_network_plot_combined <- function(web, web_name, non_serp_plants) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  colnames(web)[1] <- "Plant"
  

  web_bin <- web %>% mutate(across(-Plant, ~ ifelse(. > 0, 1, 0)))
  

  web_bin <- web_bin %>% mutate(region_plant = ifelse(Plant %in% non_serp_plants, "NS", "S"))
  

  interactions <- web_bin %>%
    pivot_longer(cols=-c(Plant, region_plant), names_to="animal", values_to="presence") %>%
    filter(presence > 0)
  

  animals_S <- interactions %>% filter(region_plant=="S") %>% pull(animal) %>% unique()
  animals_NS <- interactions %>% filter(region_plant=="NS") %>% pull(animal) %>% unique()
  shared_animals <- intersect(animals_S, animals_NS)
  

  all_animals <- unique(interactions$animal)
  

  animal_colors <- sapply(all_animals, function(a) {
    if(a %in% shared_animals) "#984ea3"
    else if(a %in% animals_S) "#1f78b4"
    else "#ffcc00"
  }, USE.NAMES = TRUE)
  

  plants_coord <- web_bin %>% 
    arrange(region_plant, Plant) %>%
    mutate(x = ifelse(region_plant=="S", 1, 4),
           y = seq(10,1,length.out=n()))
  
  animals_coord <- data.frame(
    animal = all_animals,
    x = seq(1.5,3.5,length.out=length(all_animals)),
    y = seq(10,1,length.out=length(all_animals)),
    stringsAsFactors = FALSE
  )
  

  interactions <- interactions %>%
    left_join(plants_coord %>% select(Plant, xp=x, yp=y), by="Plant") %>%
    left_join(animals_coord %>% rename(xa=x, ya=y), by="animal") %>%
    mutate(line_color = animal_colors[animal])
  

  p <- ggplot() +
    geom_segment(data=interactions,
                 aes(x=xp, xend=xa, y=yp, yend=ya),
                 color=interactions$line_color, alpha=0.6) +
    geom_text(data=plants_coord, aes(x=x, y=y, label=Plant),
              color=ifelse(plants_coord$region_plant=="S","#1f78b4","#ffcc00"), size=4) +
    geom_text(data=animals_coord, aes(x=x, y=y, label=animal),
              color=animal_colors[animals_coord$animal], size=3.5) +
    annotate("rect", xmin=0.5, xmax=4.5, ymin=0.5, ymax=11, fill=NA, color="#984ea3", size=1.5) +
    annotate("text", x=2.5, y=11.5, label="Full", size=6, color="#984ea3", vjust=0) +
    theme_void()
  

  outfile <- paste0("conceptual_", web_name, "_combinedNetwork.pdf")
  ggsave(outfile, plot=p, width=10, height=6, dpi=600, device=cairo_pdf)
  message("✅ Saved ", outfile)
  
  return(p)
}


p_small_combined <- make_network_plot_combined(small_web, "small_web", non_serp_plants)
p_medium_combined <- make_network_plot_combined(medium_web, "medium_web", non_serp_plants)
p_large_combined <- make_network_plot_combined(large_web, "large_web", non_serp_plants)

################### combined cleaner format ############
make_network_plot_combined_column <- function(web, web_name, non_serp_plants) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  colnames(web)[1] <- "Plant"
  

  web_bin <- web %>% mutate(across(-Plant, ~ ifelse(. > 0, 1, 0)))
  

  web_bin <- web_bin %>% mutate(region_plant = ifelse(Plant %in% non_serp_plants, "NS", "S"))
  

  interactions <- web_bin %>%
    pivot_longer(cols=-c(Plant, region_plant), names_to="animal", values_to="presence") %>%
    filter(presence > 0)
  

  animals_S <- interactions %>% filter(region_plant=="S") %>% pull(animal) %>% unique()
  animals_NS <- interactions %>% filter(region_plant=="NS") %>% pull(animal) %>% unique()
  shared_animals <- intersect(animals_S, animals_NS)
  

  all_animals <- unique(interactions$animal)
  

  animal_colors <- sapply(all_animals, function(a) {
    if(a %in% shared_animals) "#984ea3"
    else if(a %in% animals_S) "#1f78b4"
    else "#ffcc00"
  }, USE.NAMES = TRUE)
  

  plants_coord <- web_bin %>% 
    arrange(region_plant, Plant) %>%
    mutate(x = 1, y = seq(10, 1, length.out=n()))
  

  animals_coord <- data.frame(
    animal = all_animals,
    x = seq(2, 3, length.out=length(all_animals)),
    y = seq(10,1,length.out=length(all_animals)),
    stringsAsFactors = FALSE
  )
  

  interactions <- interactions %>%
    left_join(plants_coord %>% select(Plant, xp=x, yp=y), by="Plant") %>%
    left_join(animals_coord %>% rename(xa=x, ya=y), by="animal") %>%
    mutate(line_color = animal_colors[animal])
  

  p <- ggplot() +
    geom_segment(data=interactions,
                 aes(x=xp, xend=xa, y=yp, yend=ya),
                 color=interactions$line_color, alpha=0.6) +
    geom_text(data=plants_coord, aes(x=x, y=y, label=Plant),
              color=ifelse(plants_coord$region_plant=="S","#1f78b4","#ffcc00"), size=4) +
    geom_text(data=animals_coord, aes(x=x, y=y, label=animal),
              color=animal_colors[animals_coord$animal], size=3.5) +
    annotate("rect", xmin=0.5, xmax=4.5, ymin=0.5, ymax=11, fill=NA, color="#984ea3", size=1.5) +
    annotate("text", x=2.5, y=11.5, label="Full", size=6, color="#984ea3", vjust=0) +
    theme_void()
  
  # Save
  outfile <- paste0("conceptual_", web_name, "_combinedColumn.pdf")
  ggsave(outfile, plot=p, width=10, height=6, dpi=600, device=cairo_pdf)
  message("✅ Saved ", outfile)
  
  return(p)
}


p_small_col <- make_network_plot_combined_column(small_web, "small_web", non_serp_plants)
p_medium_col <- make_network_plot_combined_column(medium_web, "medium_web", non_serp_plants)
p_large_col <- make_network_plot_combined_column(large_web, "large_web", non_serp_plants)


