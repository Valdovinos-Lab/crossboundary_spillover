######## Visualize MATLAB Outputs ######
#### Code for visualizing matlab outputs into R dataframes #####
##### for Becca and Taran spillover project #######
###### date created: 3-17-2025 ###########
######### date last modified: 9-8-2025 #######
###### analyze simulation and empirical data #######
library(patchwork)
source("analysis.R")
rich <- read.csv("empirical_summaries/plant_diversity_empirical.csv")

######### Figure 1 ##############
rich$site_year <- rich$network_name
plant_summary_full <- left_join(plant_summary, rich, by = "site_year")

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
  
total_visits_long_full <- left_join(total_visits_long, rich, by = "site_year")

##graph:
library(ggplot2)
library(dplyr)
library(patchwork)

# 1A with AF
p1 <- plant_summary_full %>%
  filter(AF == 1) %>%
  mutate(
    version = factor(version, levels = c(1, 2, 3), labels = c("Both", "S", "NS")),
    n_plants = case_when(
      version == "Both" ~ n_plants_full,
      version == "S"    ~ n_plants_serp,
      version == "NS"   ~ n_plants_nonserp
    )
  ) %>%
  ggplot(aes(x = version, y = visit_quanity, color = version, size = n_plants)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  theme_classic() +
  scale_color_viridis_d() +
  scale_size_continuous(range = c(2, 6)) +
  labs(x = "Version", y = "Visit Quantity", color = "Version", size = "Plant richness") +
  ggtitle("A. With AF")

# 1B without AF
p2 <- plant_summary_full %>%
  filter(AF == 2) %>%
  mutate(
    version = factor(version, levels = c(1, 2, 3), labels = c("Both", "S", "NS")),
    n_plants = case_when(
      version == "Both" ~ n_plants_full,
      version == "S"    ~ n_plants_serp,
      version == "NS"   ~ n_plants_nonserp
    )
  ) %>%
  ggplot(aes(x = version, y = visit_quanity, color = version, size = n_plants)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  theme_classic() +
  scale_color_viridis_d() +
  scale_size_continuous(range = c(2, 6)) +
  labs(x = "Version", y = "Visit Quantity", color = "Version", size = "Plant richness") +
  ggtitle("B. Without AF")

# 1C empirical data
p3 <- total_visits_long_full %>%
  mutate(
    n_plants = case_when(
      visit_type == "Both" ~ n_plants_full,
      visit_type == "S"    ~ n_plants_serp,
      visit_type == "NS"   ~ n_plants_nonserp
    )
  ) %>%
  ggplot(aes(x = visit_type, y = total_visits, color = visit_type, size = n_plants)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  theme_classic() +
  scale_color_viridis_d() +
  scale_size_continuous(range = c(2, 6)) +
  labs(x = "Visit Type", y = "Total Visits", color = "Visit Type", size = "Plant richness") +
  ggtitle("C. Empirical Data")

# Combine into one row
fig_1 <- (p1 + p2 + p3) +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")


# Save as high-resolution PDF
ggsave("Fig_1.pdf", fig_1, width = 12, height = 4, dpi = 600)

########## Figure 2 #########
plant_long <- left_join(plant_long, rich, by = "site_year")

# Helper function to prepare plot data and AF labels
prepare_plot_data <- function(data, response_var, AF_value, AF_label) {
  data %>%
    filter(AF == AF_value) %>%
    mutate(
      version = factor(version, levels = c(1, 2, 3), labels = c("Both", "S", "NS")),
      Soil_Type = factor(Soil_Type, levels = c("Non-Serpentine", "Serpentine")),
      keep = case_when(
        Soil_Type == "Non-Serpentine" & version %in% c("Both", "NS") ~ TRUE,
        Soil_Type == "Serpentine" & version %in% c("Both", "S") ~ TRUE,
        TRUE ~ FALSE
      ),
      x_label = case_when(
        Soil_Type == "Non-Serpentine" ~ "NS plants",
        Soil_Type == "Serpentine" ~ "S plants"
      ),
      n_plants = case_when(
        version == "Both" & Soil_Type == "Non-Serpentine" ~ n_plants_full,
        version == "NS"   & Soil_Type == "Non-Serpentine" ~ n_plants_nonserp,
        version == "Both" & Soil_Type == "Serpentine" ~ n_plants_full,
        version == "S"    & Soil_Type == "Serpentine" ~ n_plants_serp,
        TRUE ~ 1
      ),
      AF_label = AF_label
    ) %>%
    filter(keep)
}

# Responses
responses <- c("visit_quanity", "plant_abundance", "visit_quality")
y_labels <- c("Visit Quantity", "Plant Abundance", "Visit Quality")

# Create plots for AF = 1 ("AF")
plots_AF <- lapply(seq_along(responses), function(i) {
  prepare_plot_data(plant_long, responses[i], AF_value = 1, AF_label = "AF") %>%
    ggplot(aes(x = x_label, y = .data[[responses[i]]], color = version, size = n_plants)) +
    geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.75), outlier.shape = NA) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75), alpha = 0.5) +
    theme_classic() +
    scale_color_viridis_d() +
    scale_size_continuous(range = c(2, 6), name = "Network size\n(# plants)") +
  
    labs(x = NULL, y = y_labels[i], title = paste0(y_labels[i], " (", "AF", ")"))
}) 

# Create plots for AF = 2 ("No AF")
plots_NoAF <- lapply(seq_along(responses), function(i) {
  prepare_plot_data(plant_long, responses[i], AF_value = 2, AF_label = "No AF") %>%
    ggplot(aes(x = x_label, y = .data[[responses[i]]], color = version, size = n_plants)) +
    geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.75), outlier.shape = NA) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75), alpha = 0.5) +
    theme_classic() +
    scale_color_viridis_d() +
    scale_size_continuous(range = c(2, 6), name = "Network size\n(# plants)") +
    labs(x = NULL, y = y_labels[i], title = paste0(y_labels[i], " (", "No AF", ")"))
}) 

# Combine into 2 rows x 3 columns (AF row on top)
fig_2 <- (plots_AF[[2]] + plots_AF[[1]] + plots_AF[[3]] +
            plots_NoAF[[2]] + plots_NoAF[[1]] + plots_NoAF[[3]]) +
  plot_layout(ncol = 3, nrow = 2, guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", color = NA),
    plot.tag = element_text(size = 14, face = "bold")
  ) 
# Save high-resolution PDF
ggsave("fig_2.pdf", fig_2, width = 15, height = 8, dpi = 600)

############## Figure 3 Network size  ##############################


plant_long <- left_join(plant_long, rich, by = "site_year")


library(dplyr)
library(ggplot2)
library(viridis)
library(patchwork)

# List of responses and y-axis labels
responses <- c("plant_abundance", "visit_quanity", "visit_quality")
y_labels <- c("Mean Plant Abundance", "Mean Visit Quantity", "Mean Visit Quality")

# Function to create a single panel
make_panel <- function(data, response_var, y_label) {
  data %>%
    group_by(AF, version, Soil_Type, network_name) %>%
    summarise(
      value = mean(.data[[response_var]], na.rm = TRUE),
      n_plants = mean(ifelse(Soil_Type == "Serpentine", n_plants_serp, n_plants_nonserp), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(
      (Soil_Type == "Serpentine" & version %in% c(1, 2)) |  # Both, S
        (Soil_Type == "Non-Serpentine" & version %in% c(1, 3))  # Both, NS
    ) %>%
    mutate(
      version = factor(version, levels = c(1, 2, 3), labels = c("Both", "S", "NS")),
      AF = factor(AF, levels = c(1, 2), labels = c("AF", "No AF"))
    ) %>%
    ggplot(aes(x = n_plants, y = value, color = version, shape = AF)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(aes(group = version), method = "lm", se = FALSE) +
    theme_classic() +
    scale_color_viridis_d() +
    facet_wrap(~Soil_Type*AF) +
    labs(x = "Network size (# plants)", y = y_label, color = "Version", shape = "AF")
}

# Generate plots
plot_list <- lapply(seq_along(responses), function(i) {
  make_panel(plant_long, responses[i], y_labels[i])
})

# Combine into one figure (letters A, B, C)
fig_3 <- plot_list[[1]] + plot_list[[2]] + plot_list[[3]] +
  plot_layout(ncol = 1, nrow = 3, guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", color = NA),
    plot.tag = element_text(size = 14, face = "bold")
  )

# Save high-resolution PDF
ggsave("fig_3.pdf", fig_3, width = 10, height = 18, dpi = 600)


######### Stats on simulated data ###########
library(dplyr)
library(lme4)
library(broom.mixed)
library(flextable)
library(officer)

# --------------------------
# 1. Plant-level (Figure 2)
# --------------------------
plant_df <- plant_long %>%
  filter(
    (Soil_Type == "Serpentine" & version %in% c(1, 2)) |
      (Soil_Type == "Non-Serpentine" & version %in% c(1, 3))
  ) %>%
  mutate(
    version = factor(version, levels = c(1, 2, 3), labels = c("Both", "S", "NS")),
    AF = factor(AF, levels = c(1, 2), labels = c("AF", "No AF")),
    n_plants = ifelse(Soil_Type == "Serpentine", n_plants_serp, n_plants_nonserp)
  )

# Function to fit LMMs with AF
fit_lmm <- function(data, response_var) {
  formula <- as.formula(paste0(response_var, " ~ n_plants * version * AF + (1|PLANT) + (1|network_name)"))
  model <- lmer(formula, data = data)
  
  # Assumptions check
  check <- list(
    normality = shapiro.test(residuals(model)),
    homoscedasticity = plot(model) # visual inspection
  )
  
  return(list(model = model, assumptions = check, summary = summary(model)))
}

# Serpentine vs Non-serpentine
serp_plants <- filter(plant_df, Soil_Type == "Serpentine")
ns_plants <- filter(plant_df, Soil_Type == "Non-Serpentine")

# Plant-level models
serp_abund_model <- fit_lmm(serp_plants, "plant_abundance")
serp_quality_model <- fit_lmm(serp_plants, "visit_quality")
serp_quant_model <- fit_lmm(serp_plants, "visit_quanity")

ns_abund_model <- fit_lmm(ns_plants, "plant_abundance")
ns_quality_model <- fit_lmm(ns_plants, "visit_quality")
ns_quant_model <- fit_lmm(ns_plants, "visit_quanity")

# --------------------------
# 2. Network-level means (Figure 3)
# --------------------------
mean_df <- plant_long %>%
  group_by(AF, version, Soil_Type, network_name) %>%
  summarise(
    plant_abundance = mean(plant_abundance, na.rm = TRUE),
    visit_quanity = mean(visit_quanity, na.rm = TRUE),
    visit_quality = mean(visit_quality, na.rm = TRUE),
    n_plants = mean(ifelse(Soil_Type == "Serpentine", n_plants_serp, n_plants_nonserp), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    (Soil_Type == "Serpentine" & version %in% c(1, 2)) |
      (Soil_Type == "Non-Serpentine" & version %in% c(1, 3))
  ) %>%
  mutate(
    version = factor(version, levels = c(1, 2, 3), labels = c("Both", "S", "NS")),
    AF = factor(AF, levels = c(1, 2), labels = c("AF", "No AF"))
  )

# Network-level LMM
fit_lmm_mean <- function(data, response_var) {
  formula <- as.formula(paste0(response_var, " ~ n_plants * version * AF + (1|network_name)"))
  model <- lmer(formula, data = data)
  
  check <- list(
    normality = shapiro.test(residuals(model)),
    homoscedasticity = plot(model)
  )
  
  return(list(model = model, assumptions = check, summary = summary(model)))
}

serp_abund_mean <- fit_lmm_mean(filter(mean_df, Soil_Type == "Serpentine"), "plant_abundance")
serp_quality_mean <- fit_lmm_mean(filter(mean_df, Soil_Type == "Serpentine"), "visit_quality")
serp_quant_mean <- fit_lmm_mean(filter(mean_df, Soil_Type == "Serpentine"), "visit_quanity")

ns_abund_mean <- fit_lmm_mean(filter(mean_df, Soil_Type == "Non-Serpentine"), "plant_abundance")
ns_quality_mean <- fit_lmm_mean(filter(mean_df, Soil_Type == "Non-Serpentine"), "visit_quality")
ns_quant_mean <- fit_lmm_mean(filter(mean_df, Soil_Type == "Non-Serpentine"), "visit_quanity")

# --------------------------
# 3. Tidy models for Word
# --------------------------
tidy_model_summary <- function(model, model_name, level, soil) {
  broom.mixed::tidy(model, effects = "fixed") %>%
    mutate(
      model = model_name,
      level = level,
      soil = soil
    )
}

plant_summaries <- bind_rows(
  tidy_model_summary(serp_abund_model$model, "Plant Abundance", "Plant-level", "Serpentine"),
  tidy_model_summary(serp_quality_model$model, "Visit Quality", "Plant-level", "Serpentine"),
  tidy_model_summary(serp_quant_model$model, "Visit Quantity", "Plant-level", "Serpentine"),
  tidy_model_summary(ns_abund_model$model, "Plant Abundance", "Plant-level", "Non-Serpentine"),
  tidy_model_summary(ns_quality_model$model, "Visit Quality", "Plant-level", "Non-Serpentine"),
  tidy_model_summary(ns_quant_model$model, "Visit Quantity", "Plant-level", "Non-Serpentine")
)

network_summaries <- bind_rows(
  tidy_model_summary(serp_abund_mean$model, "Plant Abundance", "Network-level", "Serpentine"),
  tidy_model_summary(serp_quality_mean$model, "Visit Quality", "Network-level", "Serpentine"),
  tidy_model_summary(serp_quant_mean$model, "Visit Quantity", "Network-level", "Serpentine"),
  tidy_model_summary(ns_abund_mean$model, "Plant Abundance", "Network-level", "Non-Serpentine"),
  tidy_model_summary(ns_quality_mean$model, "Visit Quality", "Network-level", "Non-Serpentine"),
  tidy_model_summary(ns_quant_mean$model, "Visit Quantity", "Network-level", "Non-Serpentine")
)

all_model_summary <- bind_rows(plant_summaries, network_summaries) %>%
  select(model, level, soil, term, estimate, std.error, statistic, p.value) %>%
  arrange(level, soil, model)

# --------------------------
# 4. Export as Word
# --------------------------
ft <- flextable(all_model_summary) %>%
  set_header_labels(
    model = "Response",
    level = "Data Level",
    soil = "Soil Type",
    term = "Fixed Effect",
    estimate = "Estimate",
    std.error = "SE",
    statistic = "t-value",
    p.value = "p-value"
  ) %>%
  # Round numeric columns to 2 decimals
  colformat_double(j = c("estimate", "std.error", "statistic", "p.value"), digits = 2) %>%
  autofit() %>%
  theme_vanilla()


doc <- read_docx() %>%
  body_add_par("Linear Mixed-Effects Model Summary", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "LMM_summary.docx")


######### Simulated Pollinator Abundance ##########
animal_summary_spillover_only %>%
  mutate(version = factor(version, levels = c(1, 2, 3), labels = c("Both", "S", "NS"))) %>%
  ggplot(aes(x = version, y = animal_abundance, color = version)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.2, size = 1.5) +  # show data points
  theme_classic() +
  scale_color_viridis_d() +
  labs(x = "Version", y = "Spillover Animal Abundance", color = "Version") 
#+ facet_wrap(~site_year)
#+ facet_wrap(~AF)
## trend is consistent across sites and AF

####### Simulated Pollinator Visits #########
plant_summary %>%
  mutate(version = factor(version, levels = c(1, 2, 3), labels = c("Both", "S", "NS"))) %>%
  ggplot(aes(x = version, y = visit_quanity, color = version)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.2, size = 1.5) +  # show data points
  theme_classic() +
  scale_color_viridis_d() +
  labs(x = "Version", y = "Visit Quantity", color = "Version") 
#+ facet_wrap(~AF)

###### Empirical Pollinator Visits ######
# Reshape to long format
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

# Plot
ggplot(total_visits_long, aes(x = visit_type, y = total_visits, color = visit_type)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.2, size = 1.5) +
  theme_classic() +
  scale_color_viridis_d() +
  labs(x = "Visit Type", y = "Total Visits", color = "Visit Type") 
#+ facet_wrap(~network_name)

########## Simulated Plant Abundance ############
plant_long %>%
  mutate(
    version = factor(version, levels = c(1, 2, 3), labels = c("Both", "S", "NS")),
    Soil_Type = factor(Soil_Type, levels = c("Non-Serpentine", "Serpentine")),
    # Filter visit versions based on soil type:
    keep = case_when(
      Soil_Type == "Non-Serpentine" & version %in% c("Both", "NS") ~ TRUE,
      Soil_Type == "Serpentine" & version %in% c("Both", "S") ~ TRUE,
      TRUE ~ FALSE
    ),
    # Map soil type to simplified x-axis labels
    x_label = case_when(
      Soil_Type == "Non-Serpentine" ~ "NS plants",
      Soil_Type == "Serpentine" ~ "S plants"
    )
  ) %>%
  filter(keep) %>%
  ggplot(aes(x = x_label, y = plant_abundance, color = version)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.75)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75), alpha = 0.2, size = 1.5) +
  theme_classic() +
  scale_color_viridis_d() +
  labs(x = NULL, y = "Plant Abundance", color = "Version") +
  theme(
    axis.text.x = element_text(size = 12) ) 

#+ facet_wrap(~site_year)
#+ facet_wrap(~AF)
#some site-year context dependency 
######## simulated Plant Visit quantity by plant/soil ########
plant_long %>%
  mutate(
    version = factor(version, levels = c(1, 2, 3), labels = c("Both", "S", "NS")),
    Soil_Type = factor(Soil_Type, levels = c("Non-Serpentine", "Serpentine")),
    # Filter visit versions based on soil type:
    keep = case_when(
      Soil_Type == "Non-Serpentine" & version %in% c("Both", "NS") ~ TRUE,
      Soil_Type == "Serpentine" & version %in% c("Both", "S") ~ TRUE,
      TRUE ~ FALSE
    ),
    # Map soil type to simplified x-axis labels
    x_label = case_when(
      Soil_Type == "Non-Serpentine" ~ "NS plants",
      Soil_Type == "Serpentine" ~ "S plants"
    )
  ) %>%
  filter(keep) %>%
  ggplot(aes(x = x_label, y = visit_quanity, color = version)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.75)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75), alpha = 0.2, size = 1.5) +
  theme_classic() +
  scale_color_viridis_d() +
  labs(x = NULL, y = "Visit_Quantity", color = "Version") +
  theme(
    axis.text.x = element_text(size = 12) )

########### Plant Abundance by soil and empirical networks/sites #########


plant_long %>%
  mutate(
    version = factor(version, levels = c(1, 2, 3), labels = c("Both", "S", "NS")),
    Soil_Type = factor(Soil_Type, levels = c("Non-Serpentine", "Serpentine")),
    # Filter visit versions based on soil type:
    keep = case_when(
      Soil_Type == "Non-Serpentine" & version %in% c("Both", "NS") ~ TRUE,
      Soil_Type == "Serpentine" & version %in% c("Both", "S") ~ TRUE,
      TRUE ~ FALSE
    ),
    # Map soil type to simplified x-axis labels
    x_label = case_when(
      Soil_Type == "Non-Serpentine" ~ "NS plants",
      Soil_Type == "Serpentine" ~ "S plants"
    )
  ) %>%
  filter(keep) %>%
  ggplot(aes(x = x_label, y = visit_quanity, color = version)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.75)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75), alpha = 0.2, size = 1.5) +
  theme_classic() +
  scale_color_viridis_d() +
  labs(x = NULL, y = "Visit_Quantity", color = "Version") +
  theme(
    axis.text.x = element_text(size = 12) ) + facet_wrap(~NODFc_full)

#+ facet_wrap(~Jaccard_pollinator_similarity)

######### Visit Quality #############

plant_long %>%
  mutate(
    version = factor(version, levels = c(1, 2, 3), labels = c("Both", "S", "NS")),
    Soil_Type = factor(Soil_Type, levels = c("Non-Serpentine", "Serpentine")),
    # Filter visit versions based on soil type:
    keep = case_when(
      Soil_Type == "Non-Serpentine" & version %in% c("Both", "NS") ~ TRUE,
      Soil_Type == "Serpentine" & version %in% c("Both", "S") ~ TRUE,
      TRUE ~ FALSE
    ),
    # Map soil type to simplified x-axis labels
    x_label = case_when(
      Soil_Type == "Non-Serpentine" ~ "NS plants",
      Soil_Type == "Serpentine" ~ "S plants"
    )
  ) %>%
  filter(keep) %>%
  ggplot(aes(x = x_label, y = visit_quality, color = version)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.75)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75), alpha = 0.2, size = 1.5) +
  theme_classic() +
  scale_color_viridis_d() +
  labs(x = NULL, y = "Visit_Quality", color = "Version") +
  theme(
    axis.text.x = element_text(size = 12) )

###### Empirical Network: Jaccard vs NODFc ###############

net_metrics %>%
  ggplot(aes(x = Jaccard_pollinator_similarity, y = NODFc_full)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  scale_color_viridis_d() +
  labs(x = "S vs NS Pollinator Jaccard", y = "Full NODFc") 

######## Empirical Jaccard vs Differential Abundance ###########

plant_delta_full %>%
  ggplot(aes(x = Jaccard_pollinator_similarity, y = delta_plant_abundance, color = Soil_Type)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  scale_color_viridis_d() +
  labs(x = "S vs NS Pollinator Jaccard", y = "Difference in Plant Abundance between full vs single soil") 

plant_delta_full %>%
  ggplot(aes(x = Jaccard_pollinator_similarity, y = delta_visit_quality, color = Soil_Type)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  scale_color_viridis_d() +
  labs(x = "S vs NS Pollinator Jaccard", y = "Difference in Quality between full vs single soil") 

plant_delta_full %>%
  ggplot(aes(x = Jaccard_pollinator_similarity, y = delta_visit_quantity, color = Soil_Type)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  scale_color_viridis_d() +
  labs(x = "S vs NS Pollinator Jaccard", y = "Difference in Quantity between full vs single soil") 

######## Empirical Jaccard vs NODFc ###########

plant_delta_full %>%
  ggplot(aes(x = NODFc_full, y = delta_plant_abundance, color = Soil_Type)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  scale_color_viridis_d() +
  labs(x = "Full NODFc", y = "Difference in Plant Abundance between full vs single soil") 

plant_delta_full %>%
  ggplot(aes(x = NODFc_full, y = delta_visit_quality, color = Soil_Type)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  scale_color_viridis_d() +
  labs(x = "NODFc Full", y = "Difference in Quality between full vs single soil") 

plant_delta_full %>%
  ggplot(aes(x = NODFc_full, y = delta_visit_quantity, color = Soil_Type)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  scale_color_viridis_d() +
  labs(x = "NODFc Full", y = "Difference in Quantity between full vs single soil") 
######## Pollinators by Species Degree ########

animal_plot <- animal_long_full %>%
  mutate(version = factor(version, levels = c(1, 2, 3), labels = c("Both", "S", "NS"))) %>%
  ggplot(aes(x = version, y = animal_abundance, color = version)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.2, size = 1.5) +  # show data points
  theme_classic() +
  scale_color_viridis_d() +
  labs(x = "Version", y = "Animal Abundance", color = "Version") + 
  facet_wrap(~ARTH, scales = "free_y")

ggsave("animal_abundance_by_species.pdf", plot = animal_plot, width = 24, height = 20)



## only species that spillover and by degree
arth_siteyear_degree <- animal_long_full %>%
  group_by(ARTH, site_year) %>%
  summarise(siteyear_degree = mean(full_degree, na.rm = TRUE), .groups = "drop")

arth_order <- arth_siteyear_degree %>%
  group_by(ARTH) %>%
  summarise(mean_degree = mean(siteyear_degree, na.rm = TRUE)) %>%
  arrange(mean_degree) %>%
  pull(ARTH)

animal_long_full <- animal_long_full %>%
  mutate(ARTH = factor(ARTH, levels = arth_order))


spillover_animal_plot_AF <- animal_long_full %>% filter(full_degree > 0) %>% filter(serp_degree > 0) %>% filter(nonserp_degree > 0) %>% filter(AF == 1) %>% 
  mutate(version = factor(version, levels = c(1, 2, 3), labels = c("Both", "S", "NS"))) %>%
  ggplot(aes(x = version, y = animal_abundance, color = version)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.2, size = 1.5) +  # show data points
  theme_classic() +
  scale_color_viridis_d() +
  labs(x = "Version", y = "Animal Abundance", color = "Version") + 
  facet_wrap(~ARTH, scales = "free_y")

ggsave("animal_abundance_by_species_spillover_AF.pdf", plot = spillover_animal_plot_AF, width = 24, height = 20)


spillover_animal_plot_noAF <- animal_long_full %>% filter(full_degree > 0) %>% filter(serp_degree > 0) %>% filter(nonserp_degree > 0) %>% filter(AF == 2) %>% 
  mutate(version = factor(version, levels = c(1, 2, 3), labels = c("Both", "S", "NS"))) %>%
  ggplot(aes(x = version, y = animal_abundance, color = version)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.2, size = 1.5) +  # show data points
  theme_classic() +
  scale_color_viridis_d() +
  labs(x = "Version", y = "Animal Abundance", color = "Version") + 
  facet_wrap(~ARTH, scales = "free_y")

ggsave("animal_abundance_by_species_spillover_noAF.pdf", plot = spillover_animal_plot_noAF, width = 24, height = 20)
