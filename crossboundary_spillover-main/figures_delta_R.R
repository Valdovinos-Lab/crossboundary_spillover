
######## Pollinator Figure by Delta ######
######## updated version for newer MATLAB code ##############
###### date created: 1-12-2026 ###########
######### date last modified: 1-12-2026 #######
###### create pollinator figure by site-year #######

##### load data and packages ######
library(tidyverse)
library(patchwork)

source("rcode/updated_analysis.R")
#plant_means: mean of summed plant response across simulations (plant_sums)
## plant_sums: summed by site_year

rich <- read.csv("empirical_summaries/plant_diversity_empirical.csv")
cov_Q <- read.csv("empirical_summaries/empirical_coverage_Q.csv")
spillover <- read.csv("empirical_summaries/empirical_network_metrics.csv")
## gives you NODFc of FULL network and jaccard index of pollinator similarity between serpentine vs non-serpentine network 

######### Merge Data sets ##########

plant_summary <- left_join(spillover, rich, by = "network_name")


cov_Q$network_name <- cov_Q$Network
plant_summary_species <- left_join(cov_Q, plant_summary, by = "network_name")


plant_sums_clean <- plant_sums %>%
  mutate(network_name = str_remove(Dataset, "_all_versions$"))

plant_means_clean <- plant_means %>%
  mutate(network_name = str_remove(Dataset, "_all_versions$"))

plant_network <- plant_sums_clean %>%
  left_join(
    plant_summary,
    by = "network_name"
  )

plant_overall <- plant_means_clean %>%
  left_join(
    plant_summary,
    by = "network_name"
  )

####### make figure #######

ns_compare <- plant_network %>%
  filter(
    Plant_Group == "non_serpentine",
    VersionName %in% c("full", "non_serpentine")
  ) %>%
  group_by(network_name, VersionName) %>%
  summarise(
    P_mean = mean(P, na.rm = TRUE),
    Jaccard_pollinator_similarity = first(Jaccard_pollinator_similarity),
    n_plants_full = first(n_plants_full),
    n_plants_nonserp = first(n_plants_nonserp),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = VersionName, values_from = P_mean) %>%
  mutate(
    delta = full - non_serpentine,
    soil_gt_full = non_serpentine > full
  )

p1 <- ggplot(ns_compare,
       aes(x = n_plants_nonserp,
           y = delta,
           color = soil_gt_full)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c("TRUE" = "#E6B800", "FALSE" = "#8B5FBF"),
    labels = c("FALSE" = "Full ≥ Non-serp",
               "TRUE"  = "Non-serp > Full")
  ) +
  theme_classic(base_size = 12) +
  labs(
    x = "Number of non-serpentine plants",
    y = expression(Delta * " P (full − non-serpentine)"),
    color = "Comparison",
  )

serp_compare <- plant_network %>%
  filter(
    Plant_Group == "serpentine",
    VersionName %in% c("full", "serpentine")
  ) %>%
  group_by(network_name, VersionName) %>%
  summarise(
    P_mean = mean(P, na.rm = TRUE),
    Jaccard_pollinator_similarity = first(Jaccard_pollinator_similarity),
    n_plants_full = first(n_plants_full),
    n_plants_serp = first(n_plants_serp),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = VersionName, values_from = P_mean) %>%
  mutate(
    delta = full - serpentine,
    soil_gt_full = serpentine > full
  )


p2 <- ggplot(serp_compare,
       aes(x = n_plants_serp,
           y = delta,
           color = soil_gt_full)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c("TRUE" = "#1F77B4", "FALSE" = "#8B5FBF"),
    labels = c("FALSE" = "Full ≥ Serpentine",
               "TRUE"  = "Serpentine > Full")
  ) +
  theme_classic(base_size = 12) +
  labs(
    x = "Number of serpentine plants",
    y = expression(Delta * " P (full − serpentine)"),
    color = "Comparison",
  )


fig_final <- (p1 + p2) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

fig_final




ggsave("pollinator_deltaP.pdf", fig_final, width = 14, height = 8, dpi = 600)
