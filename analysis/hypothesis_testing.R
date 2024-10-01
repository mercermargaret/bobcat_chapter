# testing our three main hypotheses
# margaret mercer
# Sept 30, 2024

# clear workspace and load packages and data ####

# clear workspace
rm(list=ls())

# load packages
library(tidyverse)
library(lme4)

# load both range resident and non range resident data
results_rr <- read.csv("results/results_rr.csv")
results_non_rr <- read.csv("results/results_non_rr.csv")

# standardize column names and add columns for non rr bobcats so you can merge
results_rr <- results_rr %>%
  rename(road_density = road_density.)
results_non_rr$speed <- c(NA, NA)
results_non_rr$area_sq_km <- c(NA, NA)
results_non_rr$total_road_length_km <- c(NA, NA)
results_non_rr$road_density <- c(NA, NA)

# merge into one dataframe
results <- rbind(results_rr, results_non_rr)

# hypothesis 1: Do bobcats cross roads less frequently than would be expected from random chance? ####
# visualize the data
real <- results$numb_real_crossings
simulated <- results$numb_simulated_crossings
hist(real)
hist(simulated)

# wilcox test
wilcoxon_result <- wilcox.test(real, simulated, paired = TRUE, na.rm = TRUE)
print(wilcoxon_result)

# paired t test
t_result <- t.test(real, simulated, paired = TRUE, na.rm = TRUE)
print(t_result)

# log transform them
log_real <- log(real)
log_sim <- log(simulated)
t_result <- t.test(log_real, log_sim, paired = TRUE, na.rm = TRUE)
print(t_result)

# sqrt transform
sqrt_real <- (sqrt(real))
sqrt_sim <- (sqrt(simulated))
t_result <- t.test(sqrt_real, sqrt_sim, paired = TRUE, na.rm = TRUE)
print(t_result)
# cross roads 11% less frequently than expected by chance

# Jesse: split roads up into different categories. Major roads, minor roads. And then do this again. !!!

# hypothesis 2: Does bobcat movement behavior change when they are closer to roads? ####
# test for relationship between distance of each point from road and instantaneous speed
# load in point data
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
points_new<- points %>%
  filter(is.finite(Speed))

# random effect of individual for just intercept
model1 <- lmer(Speed ~ log(Distance) + (1 | Individual_ID), data = points_new)
summary(model1)
# random effect of individual for slope and intercept
model2 <- lmer(Speed ~ log(Distance) + (log(Distance) | Individual_ID), data = points_new)
# Model failed to converge! ^^
summary(model2)

# log transform distance because distance from road is likely to only matter when distance is SMALL
# use random effect for slope AND intercept (if I can) since both are likely to vary by individual

# test for relationship between road density and home range size
density <- results$road_density
area <- results$area_sq_km
plot(area ~ density)
glm(area ~ density)

# test for relationship between road density and average speed
speed <- results$speed
plot(speed ~ density)

# hm this is interesting. if speed and area covary, could have impact on analysis
plot(speed ~ area)


# hypothesis 3: Do bobcats use culverts to cross roads? ####

# get median distance of road crossing
# compare percentage of crossings near a structure between real and simulated data
