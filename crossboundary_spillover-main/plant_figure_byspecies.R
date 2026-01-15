######## Plant Figure Species Code ######
######## updated version for newer MATLAB code ##############
###### date created: 1-12-2025 ###########
######### date last modified: 1-12-2026 #######
###### create plant figure #######

### only now has plant species that share pollinators across soil types at least one meadow 

source("rcode/updated_analysis.R")

library(ggplot2)
library(dplyr)
library(patchwork)

focal_spring <- plant_species_means %>% dplyr::filter(PlantName %in% c("VIVI", "TRFU", "LUSU", "LUMI"))

focal_summer <- plant_species_means %>% dplyr::filter(PlantName %in% c("CESO", "HEEX", "GRCA", "HECO"))

focal <- plant_species_means %>% dplyr::filter(PlantName %in% c("CESO", "HEEX", "VIVI", "TRFU"))

version_colors <- c(
  "full"           = "#8B5FBF",  # purple
  "serpentine"     = "#1F77B4",  # blue
  "non_serpentine" = "#E6B800"   # yellow
)

# Helper Function 
plot_plant_panel <- function(data, yvar, ylab, title) {
  ggplot(data, aes(x = PlantName, y = !!sym(yvar), color = VersionName)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.5, size = 2) +
    theme_classic(base_size = 12) +
    scale_color_manual(values = version_colors) +
    labs(x = "Plant Species", y = ylab, fill = "Simulation Version") +
    ggtitle(title)
}


#p1 <- plot_plant_panel(focal_spring, "P", "Plant Abundance", "A. Plant Abundance (P)")
#p2 <- plot_plant_panel(focal_spring, "Q", "Pollination Quality", "B. Pollination Quality (Q)")
#p3 <- plot_plant_panel(focal_spring, "D", "Effective Density", "B. Effective Density (D)")
#p4 <- plot_plant_panel(focal_spring, "Seeds", "Total Seeds", "C. Seeds Produced")
#p5 <- plot_plant_panel(focal_summer, "P", "Plant Abundance", "A. Plant Abundance (P)")
#p6 <- plot_plant_panel(focal_summer, "Q", "Pollination Quality", "B. Pollination Quality (Q)")
#p7 <- plot_plant_panel(focal_summer, "D", "Effective Density", "B. Effective Density (D)")
#p8 <- plot_plant_panel(focal_summer, "Seeds", "Total Seeds", "C. Seeds Produced")

focal$PlantName <- factor(focal$PlantName, levels = c("VIVI", "CESO", "TRFU", "HEEX"))


p1 <- plot_plant_panel(focal, "P", "Plant Abundance", "A. Plant Abundance (P)")
p2 <- plot_plant_panel(focal, "Q", "Pollination Quality", "B. Pollination Quality (Q)")
p3 <- plot_plant_panel(focal, "D", "Effective Density", "C. Effective Density (D)")
p4 <- plot_plant_panel(focal, "Seeds", "Total Seeds", "D. Seeds Produced")
p5 <- plot_plant_panel(focal, "PolServ", "Pollination Services per Plant", "E. Pollination Services")
p6 <- plot_plant_panel(focal, "MeanSigmaP", "Mean Visit Quality", "F. Mean Visit Quality")
p7 <- plot_plant_panel(focal, "VisitsP_percap", "Visits per Plant (Per-Capita)", "G. Visits Per-Capita")
p8 <- plot_plant_panel(focal, "VisitsP_total", "Total Visits per Plant", "H. Visits Population-Level")


fig_plant_sp <- (p1 + p2 + p3) /
  (p4 + p5 + p6 ) /
  (p7 + p8 + plot_spacer()) +  # pad 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

fig_plant_sp

ggsave("plant_figure_new_species.pdf", fig_plant_sp, width = 15, height = 12, dpi = 600)
