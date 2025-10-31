
#### Code for conceptual figure #####
##### code by Becca Nelson  #######
###### date created: 10-29-2025 ###########
######### date last modified: 10-31-2025 #######
###### create conceptual figure of simulation design #######



library(ggplot2)
library(dplyr)

# hypothetical plant and animal species
plants_S  <- paste0("P", 1:10)
plants_NS <- paste0("P", 11:20)

animals_S  <- paste0("A", c(1:6, 10, 11, 15, 20))
animals_NS <- paste0("A", c(5, 6, 10, 12:18))


shared_animals <- intersect(animals_S, animals_NS)


plants <- data.frame(
  name = c(plants_S, plants_NS),
  x = c(rep(1, length(plants_S)), rep(4, length(plants_NS))),
  y = seq(10, 1, length.out = 20),
  region = c(rep("S", length(plants_S)), rep("NS", length(plants_NS)))
)

animals <- data.frame(
  name = c(animals_S, animals_NS),
  x = c(rep(2, length(animals_S)), rep(3, length(animals_NS))),
  y = seq(10, 1, length.out = length(c(animals_S, animals_NS))),
  region = c(rep("S", length(animals_S)), rep("NS", length(animals_NS)))
)


animals$shared <- animals$name %in% shared_animals

# hypothetical interactions
set.seed(123)


interactions_S <- expand.grid(plant = plants_S, animal = animals_S) %>%
  mutate(interact = rbinom(n(), 1, 0.1)) %>%
  filter(interact == 1)


interactions_NS <- expand.grid(plant = plants_NS, animal = animals_NS) %>%
  mutate(interact = rbinom(n(), 1, 0.1)) %>%
  filter(interact == 1)


interactions_shared <- expand.grid(
  plant = c(plants_S, plants_NS),
  animal = shared_animals
) %>%
  mutate(interact = rbinom(n(), 1, 0.25)) %>%
  filter(interact == 1)


interactions <- bind_rows(interactions_S, interactions_NS, interactions_shared)


interactions$type <- case_when(
  interactions$animal %in% shared_animals ~ "spillover",
  interactions$plant %in% plants_S ~ "S",
  interactions$plant %in% plants_NS ~ "NS"
)


interactions <- interactions %>%
  left_join(plants, by = c("plant" = "name")) %>%
  rename(xp = x, yp = y) %>%
  left_join(animals, by = c("animal" = "name")) %>%
  rename(xa = x, ya = y)


cols <- c("S" = "#1f78b4",       
          "NS" = "#ffcc00",      
          "spillover" = "#984ea3")  


p <- ggplot() +
  geom_segment(
    data = interactions,
    aes(x = xp, xend = xa, y = yp, yend = ya, color = type),
    alpha = 0.6
  ) +
  # Plants
  geom_text(data = plants, aes(x, y, label = name,
                               color = ifelse(region == "S", "S", "NS")),
            size = 4) +
  # Animals
  geom_text(data = animals,
            aes(x, y, label = name,
                color = ifelse(shared, "spillover",
                               ifelse(region == "S", "S", "NS"))),
            size = 4) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "gray40") +
  annotate("text", x = 1.5, y = 13, label = "Serpentine (S)", size = 6, color = "#1f78b4", vjust = 0) +
annotate("text", x = 3.5, y = 13, label = "Non-serpentine (NS)", size = 6, color = "#ffcc00", vjust = 0)+
  annotate("text", x = 2.5, y = -2, label = "Full", size = 6, color = "#984ea3", vjust = 1) +
  scale_color_manual(values = cols, guide = "none") +
  theme_void() +
  theme(
    plot.margin = margin(40, 40, 40, 40),
    panel.border = element_rect(color = "#984ea3", fill = NA, linewidth = 1.5)
  )

# Save as high-resolution PDF
ggsave("interaction_diagram.pdf", plot = p,
       width = 10, height = 6, units = "in", dpi = 600, device = cairo_pdf)


########## no spillover version ##########
library(ggplot2)
library(dplyr)


plants_S  <- paste0("P", 1:10)
plants_NS <- paste0("P", 11:20)


animals_S  <- paste0("A", c(1:10))
animals_NS <- paste0("A", c(11:20))


shared_animals <- character(0)

plants <- data.frame(
  name = c(plants_S, plants_NS),
  x = c(rep(1, length(plants_S)), rep(4, length(plants_NS))),
  y = seq(10, 1, length.out = 20),
  region = c(rep("S", length(plants_S)), rep("NS", length(plants_NS)))
)


animals <- data.frame(
  name = c(animals_S, animals_NS),
  x = c(rep(2, length(animals_S)), rep(3, length(animals_NS))),
  y = seq(10, 1, length.out = length(c(animals_S, animals_NS))),
  region = c(rep("S", length(animals_S)), rep("NS", length(animals_NS))),
  shared = FALSE
)

# Interactions: only within soil type
set.seed(123)
interactions_S <- expand.grid(plant = plants_S, animal = animals_S) %>%
  mutate(interact = rbinom(n(), 1, 0.1)) %>%
  filter(interact == 1)

interactions_NS <- expand.grid(plant = plants_NS, animal = animals_NS) %>%
  mutate(interact = rbinom(n(), 1, 0.1)) %>%
  filter(interact == 1)


interactions <- bind_rows(interactions_S, interactions_NS)


interactions$type <- case_when(
  interactions$plant %in% plants_S ~ "S",
  interactions$plant %in% plants_NS ~ "NS"
)


interactions <- interactions %>%
  left_join(plants, by = c("plant" = "name")) %>%
  rename(xp = x, yp = y) %>%
  left_join(animals, by = c("animal" = "name")) %>%
  rename(xa = x, ya = y)


cols <- c("S" = "#1f78b4",       
          "NS" = "#ffcc00",      
          "spillover" = "#984ea3")  


p <- ggplot() +
  
  geom_segment(
    data = interactions,
    aes(x = xp, xend = xa, y = yp, yend = ya, color = type),
    alpha = 0.6
  ) +
  # Plants
  geom_text(
    data = plants,
    aes(x, y, label = name, color = ifelse(region == "S", "S", "NS")),
    size = 4
  ) +
  # Animals
  geom_text(
    data = animals,
    aes(x, y, label = name, color = ifelse(region == "S", "S", "NS")),
    size = 4
  ) +

  geom_vline(xintercept = 2.5, linetype = "dashed", color = "gray40") +
  annotate("text", x = 1.5, y = 13, label = "Serpentine (S)", size = 6, color = "#1f78b4", vjust = 0) +
  annotate("text", x = 3.5, y = 13, label = "Non-serpentine (NS)", size = 6, color = "#ffcc00", vjust = 0) +
  annotate("text", x = 2.5, y = -2, label = "Full", size = 6, color = "#984ea3", vjust = 1) +

  scale_color_manual(values = cols, guide = "none") +
  theme_void() +
  theme(
    plot.margin = margin(40, 40, 40, 40),
    panel.border = element_rect(color = "#984ea3", fill = NA, linewidth = 1.5)
  )


ggsave("interaction_diagram_no_overlap.pdf", plot = p,
       width = 10, height = 6, units = "in", dpi = 600, device = cairo_pdf)
