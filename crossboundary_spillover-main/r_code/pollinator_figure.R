######## Pollinator Figure Code ######
######## updated version for newer MATLAB code ##############
###### date created: 3-17-2025 ###########
######### date last modified: 11-7-2025 #######
###### create pollinator figure #######

source("rcode/updated_analysis.R")

###### Empirical Pollinator Visits 
total_visits_by_network <- empirical_sp %>%
  group_by(network_name) %>%
  summarise(
    total_full_visits = sum(full_visits, na.rm = TRUE),
    total_serp_visits = sum(serp_visits, na.rm = TRUE),
    total_nonserp_visits = sum(nonserp_visits, na.rm = TRUE)
  )

total_visits_by_network <- empirical_sp %>%
  group_by(network_name) %>%
  summarise(
    total_full_visits = sum(full_visits, na.rm = TRUE),
    total_serp_visits = sum(serp_visits, na.rm = TRUE),
    total_nonserp_visits = sum(nonserp_visits, na.rm = TRUE)
  )

total_visits_long <- total_visits_by_network %>%
  pivot_longer(
    cols = starts_with("total_"),
    names_to = "visit_type",
    values_to = "total_visits"
  ) %>%
  mutate(
    visit_type = factor(visit_type,
                        levels = c("total_full_visits", "total_serp_visits", "total_nonserp_visits"),
                        labels = c("Both", "S", "NS"))
  )

total_visits_long$site_year <- total_visits_long$network_name



##### compare empirical data to simulation outputs looking across sites


library(ggplot2)
library(dplyr)
library(patchwork)


version_colors <- c(
  "full"           = "#8B5FBF",  # purple
  "serpentine"     = "#1F77B4",  # blue
  "non_serpentine" = "#E6B800"   # yellow
)


animal_means <- animal_means %>%
  mutate(
    VersionName = factor(VersionName,
                         levels = c("full", "serpentine", "non_serpentine"))
  )

total_visits_long <- total_visits_long %>%
  mutate(
    visit_type = recode(visit_type,
                        "Both" = "full",
                        "S" = "serpentine",
                        "NS" = "non_serpentine"),
    visit_type = factor(visit_type,
                        levels = c("full", "serpentine", "non_serpentine"))
  )

# Helper function 
plot_animal_panel <- function(data, yvar, ylab, title) {
  ggplot(data, aes(x = VersionName, y = !!sym(yvar), color = VersionName)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.5) +
    theme_classic(base_size = 12) +
    scale_color_manual(values = version_colors, drop = FALSE) +
    labs(
      x = "Version",
      y = ylab,
      color = "Version"
    ) +
    ggtitle(title)
}

#simulation data
p1 <- plot_animal_panel(animal_means, "A", "Pollinator Abundance", "A. Pollinator Abundance")
p2 <- plot_animal_panel(animal_means, "N_extract", "Resources extracted per animal", "B. Resources Extracted")
p3 <- plot_animal_panel(animal_means, "VisitsA_total", "Total Visits", "C. Total Visits")
p4 <- plot_animal_panel(animal_means, "VisitsA_percap", "Per-Capita Visits", "D. Per-Capita Visits")
p5 <- plot_animal_panel(animal_means, "MeanSigmaA", "Mean visit quality per animal", "E. Mean Visit Quality")

# Empirical data 
p6 <- total_visits_long %>%
  ggplot(aes(x = visit_type, y = total_visits, color = visit_type)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  theme_classic(base_size = 12) +
  scale_color_manual(values = version_colors, drop = FALSE) +
  labs(
    x = "Version",
    y = "Total Visits",
    color = "Version"
  ) +
  ggtitle("F. Empirical Data")

fig_1_final <- (p1 + p2 + p3) /
  (p4 + p5 + p6) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

fig_1_final

# Save as high-resolution PDF
ggsave("pollinator_figure.pdf", fig_1_final, width = 14, height = 8, dpi = 600)
