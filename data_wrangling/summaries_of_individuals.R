# summaries of individual bobcats
# margaret mercer
# august 2, 2024

library(tidyverse)
library(lubridate)
library(ggplot2)
library(ctmm)

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

# # plot
# plot(data,
#      error = 2 ,
#      level.UD = 0.50)
# 
# ggplot(data = data, aes(x = location.long, y = location.lat, color = individual.local.identifier)) +
#   geom_point(alpha = 0.5, size = 1) +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   coord_fixed()

# troubleshooting speed
bob_speeds <- read.csv("results/movement_info.csv")
number_obs <- as.data.frame(table(data$individual.local.identifier))

# merge all these together
number_obs$name <- c("Shannan", "Minnie", "Margaret", "Catherine", "Bunny", "Sweetwater", 
                     "Jack", "Jonathan", "Val", "D2", "Morgan", "Elsie", "Steve", "Sadie", 
                     "Wyatt", "Avery", "Sylvia", "Hal", "Lisa", "Danielle", "BobbiJo", "Dave", 
                     "Braeden", "Beverly", "Cynthia", "Luna", "EmmaClaire", "Cassidy", "Tippy", 
                     "Daphne", "Carrie", "Michele", "Rocky", "Teddy", "Charlie", "Nala", "Karen", 
                     "Ben", "Cooper")
results$name <- c("Shannan", "Minnie", "Margaret", "Catherine", "Bunny", "Sweetwater", 
                     "Jack", "Jonathan", "Val", "D2", "Morgan", "Elsie", "Steve", "Sadie", 
                     "Wyatt", "Avery", "Sylvia", "Hal", "Lisa", "Danielle", "BobbiJo", "Dave", 
                     "Braeden", "Beverly", "Cynthia", "Luna", "EmmaClaire", "Cassidy", "Tippy", 
                     "Daphne", "Carrie", "Michele", "Rocky", "Teddy", "Charlie", "Nala", "Karen", 
                     "Ben", "Cooper")
joined <- left_join(bob_speeds, number_obs)
joined <- left_join(joined, results)
summary <- select(joined, -identity, -Var1)
summary$number_observations <- summary$Freq
summary$speed_error <- c(1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
summary$speed_error <- as.character(summary$speed_error)

ggplot(summary, aes(x = sampling_interval_hours, y = number_observations, color = speed_error)) +
  geom_point(size = 3) +
  labs(x = "Sampling Interval (Hours)", y = "Number of Observations", color = "Speed_Error?") +
  theme_minimal()

ggplot(summary, aes(x = sampling_period_days, y = number_observations, color = speed_error)) +
  geom_point(size = 3) +
  labs(x = "Sampling Period (Days)", y = "Number of Observations", color = "Speed_Error?") +
  theme_minimal()

# ggplot(summary, aes(x = sampling_interval_hours, y = sampling_period_days, color = speed_error)) + 
#    geom_point(size = 3) +
#    labs(x = "Sampling Interval (Hours)", y = "Sampling Period (Days)", color = "Speed_Error?") +
#    theme_minimal()

# if they're OU then I can't estimate speed!!!

model <- load("results/Model_Fit_Results/Jack_rr.Rda")
summary(fits)
# check for potential biological correlates!! ugh
# the nonrr bobcats won't get speed either because they're BM!!

