# effect of road density on speed and area
# margaret mercer
# october 24, 2024

# packages
library(ggplot2)

# clear workspace
rm(list=ls())

# load results
results <- read.csv("results/results.csv")

# road density and home range area ####
ggplot(results, aes(x = road_density_all, y = area_sq_km)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, col = "grey", fill = "lightgrey", lwd = 3) +
  geom_point(size = 6, alpha = 0.75, col = "black") +
  labs(title = "Effect of Road Density on Bobcat Home Range Size",
       x = "Road Density (km/km²)",
       y = "Home Range Size (km²)") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 40, face = "bold", hjust = 0.5), 
    axis.title.x = element_text(size = 30, face = "bold"),      
    axis.title.y = element_text(size = 30, face = "bold"),
    axis.text = element_text(size = 30, face = "bold") 
  )

# road density and average speed ####
results$speed_kmh <- results$speed / 24

results_trimmed <- subset(results, !is.na(results$speed_kmh) & speed != Inf)
results_trimmed <- subset(results_trimmed, !is.na(results_trimmed$road_density_all))
ggplot(results_trimmed, aes(x = road_density_all, y = speed_kmh)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, col = "grey", fill = "lightgrey", lwd = 3) +
  geom_point(size = 6, alpha = 0.75) +
  labs(title = "Effect of Road Density on Bobcat Speed",
       x = "Road Density (km/km²)",
       y = "Speed (kmh)") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 40, face = "bold", hjust = 0.5), 
    axis.title.x = element_text(size = 30, face = "bold"),      
    axis.title.y = element_text(size = 30, face = "bold"),
    axis.text = element_text(size = 30, face = "bold") 
  )

