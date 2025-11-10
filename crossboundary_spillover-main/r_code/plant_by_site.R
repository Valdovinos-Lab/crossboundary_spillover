######## Plant Figure Code ######
######## updated version for newer MATLAB code ##############
###### date created: 3-17-2025 ###########
######### date last modified: 11-7-2025 #######
###### create plant by site figure #######

library(ggplot2)
library(dplyr)


version_colors <- c(
  "full"           = "#8B5FBF",  # purple
  "serpentine"     = "#1F77B4",  # blue
  "non_serpentine" = "#E6B800"   # yellow
)


plant_sums <- plant_sums %>%
  mutate(
    VersionName = factor(VersionName, levels = c("full", "serpentine", "non_serpentine"))
  )


dataset_chunks <- split(unique(plant_sums$Dataset), ceiling(seq_along(unique(plant_sums$Dataset))/10))


response_list <- list(
  "P"               = "Plant Abundance (P)",
  "Gamma"           = "Recruitment Rate (Gamma)",
  "Seeds"           = "Seeds Produced",
  "PolServ"         = "Pollination Services per Plant",
  "MeanSigmaP"      = "Mean Visit Quality",
  "VisitsP_percap"  = "Visits per Plant (Per-Capita)",
  "VisitsP_total"   = "Total Visits per Plant"
)


plot_plant_panel <- function(data, yvar, ylab, title) {
  ggplot(data, aes(x = Plant_Group, y = !!sym(yvar), color = VersionName)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.5, size = 2) +
    theme_classic(base_size = 12) +
    scale_color_manual(values = version_colors) +
    labs(x = "Plant Group", y = ylab, color = "Simulation Version") +
    ggtitle(title)
}


for(resp_var in names(response_list)) {
  pdf_filename <- paste0("Plant_", resp_var, "_by_PlantGroup_Site_Year.pdf")
  
  pdf(pdf_filename, width = 12, height = 8)
  
  for(chunk in dataset_chunks){
    p <- plant_sums %>%
      filter(Dataset %in% chunk) %>%
      plot_plant_panel(
        yvar = resp_var,
        ylab = response_list[[resp_var]],
        title = paste0(response_list[[resp_var]], " by Plant Group and Site-Year")
      ) +
      facet_wrap(~Dataset, scales = "free_y")
    
    print(p)  
  }
  
  dev.off()
}
