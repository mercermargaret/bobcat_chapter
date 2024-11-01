# effect of distance to road on instentaneous speed
# margaret mercer
# october 24, 2024

# packages
library(ggplot2)
library(tidyverse)

# clear workspace
rm(list=ls())

# List all CSV files in the directory
file_list <- list.files(path = "results/Dist_from_Road_and_Speed", pattern = "*.csv", full.names = TRUE)

# Read and combine all the files into one data frame
points <- do.call(rbind, lapply(file_list, function(file) {
  data <- read.csv(file)
  data$Individual_ID <- tools::file_path_sans_ext(basename(file))  # Extract filename without extension
  return(data)
}))
# get rid of unwanted detail in name column
points$Individual_ID <- sub("_.*", "", points$Individual_ID)

# trim out rows for which speed couldn't be calculated
points_new <- points %>%
  filter(is.finite(Speed))

# plot(Speed ~ log(Distance) + (log(Distance)), data = points_new)

# binned boxplot
points_new$bins <- cut(points_new$Distance, breaks = c(0, 25, 50, 100, 200, 400, 800, 1600, 3200), include.lowest = TRUE)

# convert speed to miles/hour
points_new$speed_mph <- points_new$Speed * 2.23694

# Create binned boxplots
ggplot(points_new, aes(x = bins, y = speed_mph)) +
  geom_boxplot(outlier.alpha = 0.2, outlier.size = 1,
               ) +
  labs(title = "Effect of Proximity to Roads on Bobcat Speed", 
       x = "Distance to Nearest Road (m)", 
       y = "Instantaneous Speed (mph)") +
  scale_x_discrete(labels = c("<25", "25-50", "50-100", "100-200", "200-400", "400-800", "1600-3200", "3200<")) +
  theme_minimal() +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5), 
    axis.title.x = element_text(face = "bold", color = "black", size = 14),  
    axis.text.x = element_text(size = 10),    
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = 'none'
  )


