# summaries of individual bobcats
# margaret mercer
# august 2, 2024

library(tidyverse)
library(lubridate)
library(ggplot2)

# clear workspace
rm(list=ls())

# load data
data <- read.csv("data/bobcat_locs_all.csv")

summary(data)

unique <- unique(data$individual.local.identifier)

# create results dataframe
results <- matrix(NA, ncol = 3, nrow = length(unique(data$individual.local.identifier)))
colnames(results) <- c("identity", "sampling_interval_hours", "sampling_period_days")
results <- as.data.frame(results)

for (i in 1:length(unique(data$individual.local.identifier))) {
  name = unique[i]
  individual_gps <- data[data$individual.local.identifier == name, ]
  # summary(individual_gps)
  
  # making it a telemetry object
  individual <- as.telemetry(individual_gps)
  summary <- summary(individual)
  
  # store results in a dataframe
  results[i, 1] <- summary$identity
  results[i, 2] <- summary$`sampling interval (hours)`
  results[i, 3] <- (max(as.Date(individual_gps$timestamp)) - min(as.Date(individual_gps$timestamp)))
  
}

summary(results)

data_telemetry <- as.telemetry(data)

# plot
plot(data,
     error = 2 ,
     level.UD = 0.50)

ggplot(data = data, aes(x = location.long, y = location.lat, color = individual.local.identifier)) +
  geom_point(alpha = 0.5, size = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_fixed()
