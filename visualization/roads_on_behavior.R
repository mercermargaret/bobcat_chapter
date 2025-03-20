# roads on behavior graphs
# margaret mercer
# october 24, 2024

# packages
library(ggplot2)
library(tidyverse)
library(gridExtra)

# clear workspace
rm(list=ls())

# load results
results <- read.csv("results/results.csv")

# List all CSV files in the directory
file_list <- list.files(path = "results/Dist_from_Road_and_Speed", pattern = "*.csv", full.names = TRUE)
# Read and combine all the files into one data frame
points <- do.call(rbind, lapply(file_list, function(file) {
  data <- read.csv(file)
  data$Individual_ID <- tools::file_path_sans_ext(basename(file))  # Extract filename without extension
  return(data)
}))

# distance to roads and instantaneous speed ####
# get rid of unwanted detail in name column
points$Individual_ID <- sub("_.*", "", points$Individual_ID)

# trim out rows for which speed couldn't be calculated
points_new <- points %>%
  filter(is.finite(Speed))

# trim outlier ("Jack")
points_new <- points_new %>% 
  filter(Individual_ID != "Jack")

# plot(Speed ~ log(Distance) + (log(Distance)), data = points_new)

# binned boxplot
points_new$bins <- cut(points_new$Distance, breaks = c(0, 25, 50, 100, 200, 400, 800, 1600, 3200), include.lowest = TRUE)

# convert speed to miles/hour
points_new$speed_kmh <- points_new$Speed * 3.6

# Create binned boxplots
dis <- ggplot(points_new, aes(x = bins, y = speed_kmh)) +
  geom_boxplot(outlier.alpha = 0.2, outlier.size = 3, lwd = 2) +
  labs( # title = "A",
       x = "Distance to Nearest Road (m)", 
       y = "Instantaneous Speed (km/hr)") +
  scale_x_discrete(labels = c("<25", "25-50", "50-100", "100-200", "200-400", "400-800", "800-1600", "1600<")) +
  theme_minimal() +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 40, face = "bold", hjust = -0.05), 
    axis.title.x = element_text(size = 30, face = "bold"),  
    axis.text.x = element_text(size = 25, face = "bold",),    
    axis.title.y = element_text(size = 30, face = "bold"),
    axis.text.y = element_text(size = 30, face = "bold"),
    legend.position = 'none'
  )
dis


# road density and home range area ####
dhr <- ggplot(results, aes(x = road_density_all, y = area_sq_km)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, col = "grey", fill = "lightgrey", lwd = 3) +
  geom_point(size = 6, alpha = 0.75, col = "black") +
  labs(# title = "B",
       x = "Road Density (km/km²)",
       y = "Home Range Size (km²)") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 40, face = "bold", hjust = -0.05), 
    axis.title.x = element_text(size = 30, face = "bold"),      
    axis.title.y = element_text(size = 30, face = "bold"),
    axis.text = element_text(size = 30, face = "bold") 
  )
dhr

# road density and average speed ####
results$speed_kmh <- results$speed / 24

results_trimmed <- subset(results, !is.na(results$speed) & speed != Inf)
results_trimmed <- subset(results_trimmed, !is.na(results_trimmed$road_density_all))
das <- ggplot(results_trimmed, aes(x = road_density_all, y = speed)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, col = "grey", fill = "lightgrey", lwd = 3) +
  geom_point(size = 6, alpha = 0.75) +
  labs(# title = "C",
       x = "Road Density (km/km²)",
       y = "Daily Distance Traveled (km)") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 40, face = "bold", hjust = -0.05), 
    axis.title.x = element_text(size = 30, face = "bold"),      
    axis.title.y = element_text(size = 30, face = "bold"),
    axis.text = element_text(size = 30, face = "bold") 
  )
das

# grid.arrange(dis, dhr, das, ncol = 1)
