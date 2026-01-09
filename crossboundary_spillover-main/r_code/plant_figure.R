######## Plant Figure Code ######
######## updated version for newer MATLAB code ##############
###### date created: 3-17-2025 ###########
######### date last modified: 1-8-2026 #######
###### create plant figure #######

source("rcode/updated_analysis.R")

library(ggplot2)
library(dplyr)
library(patchwork)


version_colors <- c(
  "full"           = "#8B5FBF",  # purple
  "serpentine"     = "#1F77B4",  # blue
  "non_serpentine" = "#E6B800"   # yellow
)

# Helper Function 
plot_plant_panel <- function(data, yvar, ylab, title) {
  ggplot(data, aes(x = Plant_Group, y = !!sym(yvar), color = VersionName)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.5, size = 2) +
    theme_classic(base_size = 12) +
    scale_color_manual(values = version_colors) +
    labs(x = "Plant Group", y = ylab, fill = "Simulation Version") +
    ggtitle(title)
}


p1 <- plot_plant_panel(plant_means, "P", "Plant Abundance", "A. Plant Abundance (P)")
p2 <- plot_plant_panel(plant_means, "Gamma", "Recruitment Rate", "B. Recruitment Rate (Gamma)")
p3 <- plot_plant_panel(plant_means, "Seeds", "Total Seeds", "C. Seeds Produced")
p4 <- plot_plant_panel(plant_means, "PolServ", "Pollination Services per Plant", "D. Pollination Services")
p5 <- plot_plant_panel(plant_means, "MeanSigmaP", "Mean Visit Quality", "E. Mean Visit Quality")
p6 <- plot_plant_panel(plant_means, "VisitsP_percap", "Visits per Plant (Per-Capita)", "F. Visits Per-Capita")
p7 <- plot_plant_panel(plant_means, "VisitsP_total", "Total Visits per Plant", "G. Visits Population-Level")


fig_plant <- (p1 + p2 + p3) /
  (p4 + p5 + p6) /
  (p7 + plot_spacer() + plot_spacer()) +  # pad empty spaces
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

fig_plant

ggsave("plant_figure_new_model.pdf", fig_plant, width = 15, height = 12, dpi = 600)




