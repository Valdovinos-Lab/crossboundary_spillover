
####################################################
###########################################################
######## Empirical Sigma/Quality Graphs Spillover Data-Theory Integration #######
######## Code by Rebecca Nelson ###############
###### created: 1-8-26 #########################
######### last updated: 1-8-26 ############
#################################################

### note filter out _2023 spring in the final version and check for duplicated in abundance vs Q part 

### Load packages #######

#source("r_code/empirical_quality.R") ## to generate original empirical networks 


library(tidyverse)
library(patchwork)

##### load data and info ######

non_serp_plants <- c(
  'ASER', 'VIVI', 'CESO', 'MEIN', 'PHAQ', 'ANAR',
  'AMME', 'ERCI', 'HIIN', 'Asteraceae sp.',
  'TAOF', 'MEPO', 'SEVU'
)

version_colors <- c(
  "full"           = "#8B5FBF",
  "serpentine"     = "#1F77B4",
  "non_serpentine" = "#E6B800"
)


emp <- read_csv(
  "empirical_summaries/visit_quality_empirical_networks.csv",
  show_col_types = FALSE
)


###### organize data #######

emp <- emp %>%
  rename(
    VersionName = subset,
    Network     = network
  ) %>%
  mutate(
    # plant group from species identity
    Plant_Group = if_else(
      plant %in% non_serp_plants,
      "non_serpentine",
      "serpentine"
    ),
    
    # lock factor levels (CRITICAL)
    Plant_Group = factor(
      Plant_Group,
      levels = c("non_serpentine", "serpentine")
    ),
    VersionName = factor(
      VersionName,
      levels = c("full", "serpentine", "non_serpentine")
    )
  )

#### plot function #######

plot_empirical_panel <- function(data, yvar, ylab, title) {
  ggplot(
    data,
    aes(
      x = Plant_Group,
      y = .data[[yvar]],
      color = VersionName
    )
  ) +
    geom_boxplot(
      outlier.shape = NA,
      alpha = 0.8,
      position = position_dodge(width = 0.75)
    ) +
    geom_jitter(
      position = position_jitterdodge(
        jitter.width = 0.15,
        dodge.width = 0.75
      ),
      size = 2,
      alpha = 0.5
    ) +
    scale_color_manual(
      values = version_colors,
      drop = FALSE
    ) +
    theme_classic(base_size = 12) +
    labs(
      x = "Plant Group",
      y = ylab,
      color = "Network Version"
    ) +
    ggtitle(title)
}

##### overall Q ##########

p_Q_overall <- plot_empirical_panel(
  emp,
  "Q",
  "Plant Visit Quality (Q)",
  "A. Empirical Plant Visit Quality (Q)"
)

print(p_Q_overall)

ggsave(
  "empirical_Q_overall.pdf",
  p_Q_overall,
  width = 6,
  height = 5,
  dpi = 600
)

###### Network level Q #################

p_Q_network <- ggplot(
  emp,
  aes(
    x = Plant_Group,
    y = Q,
    color = VersionName
  )
) +
  geom_boxplot(
    outlier.shape = NA,
    alpha = 0.8,
    position = position_dodge(width = 0.75)
  ) +
  geom_jitter(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.75
    ),
    size = 1.8,
    alpha = 0.5
  ) +
  facet_wrap(~ Network, scales = "free_y") +
  scale_color_manual(
    values = version_colors,
    drop = FALSE
  ) +
  theme_classic(base_size = 11) +
  labs(
    x = "Plant Group",
    y = "Plant Visit Quality (Q)",
    color = "Network Version"
  ) +
  ggtitle("B. Empirical Plant Visit Quality (Q) by Network")

print(p_Q_network)

ggsave(
  "empirical_Q_by_network.pdf",
  p_Q_network,
  width = 14,
  height = 9,
  dpi = 600
)
############## Q vs. empirical coverage (floral abundance) #################
spring_coverage <- read_csv(
  "empirical_summaries/spring_empirical_coverage.csv",
  show_col_types = FALSE
)

summer_coverage <- read_csv(
  "empirical_summaries/summer_empirical_coverage.csv",
  show_col_types = FALSE
)

spring_coverage$Season <- "Spring"
summer_coverage$Season <- "Summer"

coverage <- rbind(spring_coverage, summer_coverage)

library(dplyr)
library(stringr)


emp2 <- emp %>%
  # Remove networks that are just "_2023_spring" (no site)
  filter(!str_starts(Network, "_")) %>%
  mutate(
    # Extract Year
    Year = as.numeric(str_extract(Network, "\\d{4}")),
    # Extract Season (text after the last underscore)
    Season = str_to_title(str_extract(Network, "(?<=\\d{4}_)[a-zA-Z]+")),
    # Extract Site (everything before _Year_Season)
    Site = str_replace(Network, "_\\d{4}_[a-zA-Z]+$", "")
  )

coverage <- coverage %>%
  mutate(Site = case_when(
    Site == "Anu_1" ~ "Anu",
    Site == "Anu_2" ~ "Anu",
    Site == "Anu_3" ~ "Anu",
    Site == "Anu_3" ~ "Anu",
    Site == "Base_of_Hill" ~ "Hill_Base",
    Site == "Lower_Grid" ~ "Lower_Grid_1",
    Site == "UG1" ~ "Upper_Grid_1",
    Site == "UG2" ~ "Upper_Grid_2",
    TRUE ~ Site
  ))


# Join with coverage
merged_df <- emp2 %>%
  left_join(coverage, by = c("plant" = "PLANT", "Site", "Year", "Season"))

glimpse(merged_df)


######### Abundance by Q plant species level  #############

library(ggplot2)
library(dplyr)

plot_df <- merged_df %>%
  filter(!is.na(mean_floral_abundance))


#### Full version only 
p <- plot_df %>%  filter(VersionName == "full") %>% ggplot(aes(x = Q, y = mean_floral_abundance, color = Plant_Group)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, size = 1) +
  scale_color_manual(values = c("serpentine" = "#1F77B4", "non_serpentine" = "#E6B800")) +
  theme_bw(base_size = 14) +
  facet_wrap(~Network, scales = "free_y") +
  labs(
    x = "Q (Pollination Quality)",
    y = "Mean Floral Abundance",
    color = "Plant Group"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")  
  )

# Save as PDF
pdf("Full_abundance_vs_Q_by_plantgroup.pdf", width = 16, height = 12)
print(p)
dev.off()

### serpentine
p <- plot_df %>%  filter(VersionName == "serpentine") %>% ggplot(aes(x = Q, y = mean_floral_abundance, color = Plant_Group)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, size = 1) + # trend lines by VersionName
  theme_bw(base_size = 14) +
  scale_color_manual(values = c("serpentine" = "#1F77B4", "non_serpentine" = "#E6B800")) +
  facet_wrap(~Network, scales = "free_y") +
  labs(
    x = "Q (Pollination Quality)",
    y = "Mean Floral Abundance",
    color = "Plant Group"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")  
  )

# Save as PDF
pdf("SerpOnly_abundance_vs_Q_by_plantgroup.pdf", width = 16, height = 12)
print(p)
dev.off()

### nonserpentine
p <- plot_df %>%  filter(VersionName == "non_serpentine") %>% ggplot(aes(x = Q, y = mean_floral_abundance, color = Plant_Group)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, size = 1) + # trend lines by VersionName
  theme_bw(base_size = 14) +
  scale_color_manual(values = c("serpentine" = "#1F77B4", "non_serpentine" = "#E6B800")) +
  facet_wrap(~Network, scales = "free_y") +
  labs(
    x = "Q (Pollination Quality)",
    y = "Mean Floral Abundance",
    color = "Plant Group"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")  
  )

# Save as PDF
pdf("NSOnly_abundance_vs_Q_by_plantgroup.pdf", width = 16, height = 12)
print(p)
dev.off()


# Get unique networks
networks <- unique(plot_df$Network)

# Open a multi-page PDF
pdf("abundance_vs_Q_by_network.pdf", width = 10, height = 6)

for(net in networks) {
  tmp <- plot_df %>% filter(Network == net)
  
  p <- ggplot(tmp, aes(x = Q, y = mean_floral_abundance, 
                       color = VersionName)) +
    geom_point(alpha = 0.5, size = 2) +
    geom_smooth(method = "lm", se = FALSE, size = 1) +
    scale_color_manual(values = version_colors) +   
    facet_wrap(~ Plant_Group, scales = "free_y") +  
    theme_bw(base_size = 14) +
    labs(
      title = net,
      x = "Q (Pollination Quality)",
      y = "Mean Floral Abundance",
      color = "Version"
    ) +
    theme(
      legend.position = "bottom",
      panel.spacing = unit(1, "lines"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(p)  # Each iteration goes to a new page
}

dev.off()


###### by plant species ########

version_colors <- c(
  "full"           = "#8B5FBF",
  "serpentine"     = "#1F77B4",
  "non_serpentine" = "#E6B800"
)

plot_df <- merged_df %>%
  filter(!is.na(mean_floral_abundance))
library(ggplot2)
library(dplyr)
library(ggforce)  # for facet_wrap_paginate

version_colors <- c(
  "full"           = "#8B5FBF",
  "serpentine"     = "#1F77B4",
  "non_serpentine" = "#E6B800"
)

plot_df <- merged_df %>%
  filter(!is.na(mean_floral_abundance))


facets_per_page <- 6 
plants <- unique(plot_df$plant)
n_pages <- ceiling(length(plants) / facets_per_page)

# Save to multi-page PDF
pdf("abundance_vs_Q_by_plant_species.pdf", width = 10, height = 6)

for(page in 1:n_pages){
  p <- plot_df %>%
    ggplot(aes(x = Q, y = mean_floral_abundance, color = VersionName)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE, size = 1) +
    scale_color_manual(values = version_colors) +
    ggforce::facet_wrap_paginate(~ plant, scales = "free_y", ncol = 2, nrow = 3, page = page) + 
    theme_bw(base_size = 14) +
    labs(
      x = "Q (Pollination Quality)",
      y = "Mean Floral Abundance",
      color = "Version",
      title = paste0("Abundance vs Q by Plant Species (Page ", page, " of ", n_pages, ")")
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.spacing = unit(1, "lines")
    )
  
  print(p)
}

dev.off()


