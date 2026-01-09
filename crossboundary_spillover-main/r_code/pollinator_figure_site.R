######## Pollinator Figure by Site-Year Code ######
######## updated version for newer MATLAB code ##############
###### date created: 3-17-2025 ###########
######### date last modified: 1-8-2026 #######
###### create pollinator figure by site-year #######


## plots poll

source("rcode/updated_analysis.R")

library(ggplot2)
library(dplyr)


version_colors <- c(
  "full"           = "#8B5FBF",  # purple
  "serpentine"     = "#1F77B4",  # blue
  "non_serpentine" = "#E6B800"   # yellow
)


animal_sums <- animal_sums %>%
  mutate(
    VersionName = factor(VersionName, levels = c("full", "serpentine", "non_serpentine"))
  )


dataset_chunks <- split(unique(animal_sums$Dataset), ceiling(seq_along(unique(animal_sums$Dataset))/10))


pdf("Pollinator_Abundance_by_Site_Year.pdf", width = 12, height = 8)

for(chunk in dataset_chunks){
  p <- animal_sums %>%
    filter(Dataset %in% chunk) %>%
    ggplot(aes(x = VersionName, y = A, color = VersionName)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
    facet_wrap(~Dataset, scales = "free_y") +
    theme_classic(base_size = 12) +
    scale_color_manual(values = version_colors, drop = FALSE) +
    labs(
      x = "Version",
      y = "Pollinator Abundance",
      color = "Version"
    ) +
    ggtitle("Pollinator Abundance by Site-Year")
  
  print(p)  # Print each page
}

dev.off()
