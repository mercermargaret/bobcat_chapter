# testing our three main hypotheses
# margaret mercer
# Sept 30, 2024


# clear workspace
rm(list=ls())

# load packages
library(tidyverse)
library(lme4)
library(lmerTest)


# load results
results <- read.csv("results/results.csv")

# mortality ####

# mort as a function of crossings per day
crossings_per_day_mort <- (results$crossings_per_day_all[results$mortality == 1])
crossings_per_day_no_mort <- (results$crossings_per_day_all[results$mortality == 0])
t1_mort <- t.test(crossings_per_day_no_mort, crossings_per_day_mort, na.rm = TRUE)
print(t1_mort)
# no diff, even when only using major road crossings per day

glm1_mort <- glm(mortality ~ crossings_per_day_maj, data = results, family = binomial)
summary(glm1_mort)
# no effect, even when only using major road crossings per day

# mort as function of road density
density_mort <- (results$road_density[results$mortality == 1])
density_no_mort <- (results$road_density[results$mortality == 0])
t2_mort <- t.test(density_no_mort, density_mort, na.rm = TRUE)
print(t2_mort)
# no effect

glm2_mort <- glm(mortality ~ road_density, data = results, family = binomial)
summary(glm2_mort)
# no effect

# sex ####

# crossings as function of sex
crossings_male <- (results$crossings_per_day_all[results$SEX == "M"])
crossings_female <- (results$crossings_per_day_all[results$SEX == "F"])
t1_sex <- t.test(crossings_male, crossings_female, na.rm = TRUE)
print(t1_sex)
# no correlation, either with all roads or just major roads
# glm3 <- glm(SEX ~ crossings_per_day_maj, data = results, family = binomial)
# summary(glm3)

# road density as function of sex
density_male <- (results$road_density[results$SEX == "M"])
density_female <- (results$road_density[results$SEX == "F"])
t2_sex <- t.test(density_male, density_female, na.rm = TRUE)
print(t2_sex)
# no correlation

# home range size as function of sex
area_male <- (results$area_sq_km[results$SEX == "M"])
area_female <- (results$area_sq_km[results$SEX == "F"])
t3_sex <- t.test(area_male, area_female, na.rm = TRUE)
print(t3_sex)
# so this makes sense; home ranges are MUCH smaller for females
# 17.375084 - 6.776367 = 10.59872
# t = 6.1203, df = 17.507, p-value = 9.958e-06

# hypothesis 1: Do bobcats cross roads less frequently than would be expected from random chance? ####
# visualize the data
real <- results$real_crossings_all
simulated <- results$simulated_crossings_all
hist(real)
hist(simulated)

# # wilcox test
# wilcoxon_result <- wilcox.test(real, simulated, paired = TRUE, na.rm = TRUE)
# print(wilcoxon_result)
# 
# # paired t test
# t_result <- t.test(real, simulated, paired = TRUE, na.rm = TRUE)
# print(t_result)

# # log transform them
# log_real <- log(real)
# log_sim <- log(simulated)
# t_result <- t.test(log_real, log_sim, paired = TRUE, na.rm = TRUE)
# print(t_result)

# sqrt transform
sqrt_real <- (sqrt(real))
sqrt_sim <- (sqrt(simulated))
t_sim <- t.test(sqrt_real, sqrt_sim, paired = TRUE, na.rm = TRUE)
print(t_sim)
# p-value = 0.01589

mean(real)
mean(simulated)
mean(simulated) - mean(real)
(mean(simulated) - mean(real))/mean(simulated)
# crossed all roads 12% less frequently than expected by chance

real_maj <- results$real_crossings_maj
simulated_maj <- results$simulated_crossings_maj
hist(real_maj)
hist(simulated_maj)

sqrt_real <- (sqrt(real_maj))
sqrt_sim <- (sqrt(simulated_maj))
t_sim_maj <- t.test(sqrt_real, sqrt_sim, paired = TRUE, na.rm = TRUE)
print(t_sim_maj)

mean(real_maj)
mean(simulated_maj)
mean(simulated_maj) - mean(real_maj)
(mean(simulated_maj) - mean(real_maj))/mean(simulated_maj)
# crossed major roads 26% less frequently than expected




# hypothesis 2: Do bobcats use culverts to cross roads? ####

# get median distance of road crossing for major
# pull and merge crossing info
file_list <- list.files(path = "results/Crossing_Info_Major_Roads", pattern = "*.csv", full.names = TRUE)
crossings_maj <- do.call(rbind, lapply(file_list, function(file) {
  data <- read.csv(file)
  data$Individual_ID <- tools::file_path_sans_ext(basename(file))  # Extract filename without extension
  return(data)
}))
# get rid of unwanted detail in name column
crossings_maj$Individual_ID <- sub("_.*", "", crossings_maj$Individual_ID)
mean <- mean(crossings_maj$Passage_Distances, na.rm = TRUE)
# get standard error
SE <- sd(crossings_maj$Passage_Distances) / sqrt(length(crossings_maj$Passage_Distances))
# get CIs
CI <- SE * 1.96
upper <- mean + CI
lower <- mean - CI
hist(crossings_maj$Passage_Distances)
mean

# minor
file_list <- list.files(path = "results/Crossing_Info_Minor_Roads", pattern = "*.csv", full.names = TRUE)
crossings_min <- do.call(rbind, lapply(file_list, function(file) {
  data <- read.csv(file)
  data$Individual_ID <- tools::file_path_sans_ext(basename(file))  # Extract filename without extension
  return(data)
}))
# get rid of unwanted detail in name column
crossings_min$Individual_ID <- sub("_.*", "", crossings_min$Individual_ID)
mean <- mean(crossings_min$Passage_Distances, na.rm = TRUE)
# get standard error
SE <- sd(crossings_min$Passage_Distances) / sqrt(length(crossings_min$Passage_Distances))
# get CIs
CI <- SE * 1.96
upper <- mean + CI
lower <- mean - CI
hist(crossings_min$Passage_Distances)

# and all roads
crossings_maj$Road_Type <- "major"
crossings_min$Road_Type <- "minor"
crossings_all <- bind_rows(crossings_maj, crossings_min)
mean <- mean(crossings_all$Passage_Distances, na.rm = TRUE)
# get standard error
SE <- sd(crossings_all$Passage_Distances) / sqrt(length(crossings_all$Passage_Distances))
# get CIs
CI <- SE * 1.96
upper <- mean + CI
lower <- mean - CI
hist(crossings_all$Passage_Distances)

# simulated distances
file_list <- list.files(path = "results/Simulation_Results", pattern = "*.csv", full.names = TRUE)
sim_crossings <- do.call(rbind, lapply(file_list, function(file) {
  data <- read.csv(file)
  return(data)
}))
sim_crossings$Road_Crossings_All <- sim_crossings$Road_Crossings_Min + sim_crossings$Road_Crossings_Maj
sim_crossings$Road_Crossings_Near_Structure_All <- sim_crossings$Numb_Crossings_Near_Structure_Min + sim_crossings$Numb_Crossings_Near_Structure_Maj
sim_crossings$Average_Distance_From_Crossing_Structure_All <- ((((sim_crossings$Average_Distance_From_Crossing_Structure_Min * sim_crossings$Road_Crossings_Min)
                                                                   + (sim_crossings$Average_Distance_From_Crossing_Structure_Maj * sim_crossings$Road_Crossings_Maj))
                                                               / (sim_crossings$Road_Crossings_Min + sim_crossings$Road_Crossings_Maj)))
mean <- mean(sim_crossings$Average_Distance_From_Crossing_Structure_All, na.rm = TRUE)
SE <- sd(sim_crossings$Average_Distance_From_Crossing_Structure_All, na.rm = TRUE) / sqrt(length(sim_crossings$Average_Distance_From_Crossing_Structure_All))
# get CIs
CI <- SE * 1.96
upper <- mean + CI
lower <- mean - CI

# major roads
mean <- mean(sim_crossings$Average_Distance_From_Crossing_Structure_Maj, na.rm = TRUE)
SE <- sd(sim_crossings$Average_Distance_From_Crossing_Structure_Maj, na.rm = TRUE) / sqrt(length(sim_crossings$Average_Distance_From_Crossing_Structure_Maj))
# get CIs
CI <- SE * 1.96
upper <- mean + CI
lower <- mean - CI

# take distance from each crossing,
# then multiply that value by the number of crossings, 
# then add all those together and divide by the total averaged number of crossings

# how tf do I do this????
# 
# sim_crossings %>%
#   group_by(ID) %>% 
#   summarize(mean_distance = mean(Average_Distance_From_Crossing_Structure_All, na.rm = TRUE))

# number crossings near structure
mean(results$crossings_near_structure_all, na.rm = TRUE)
median(results$crossings_near_structure_all, na.rm = TRUE)
results$percent_crossings_near_structure_all <- (results$crossings_near_structure_all/results$real_crossings_all)
median(results$percent_crossings_near_structure_all, na.rm = TRUE)
mean <- mean(results$percent_crossings_near_structure_all, na.rm = TRUE)
min(results$percent_crossings_near_structure_all, na.rm = TRUE)
max(results$percent_crossings_near_structure_all, na.rm = TRUE)
sd(results$percent_crossings_near_structure_all, na.rm = TRUE)
# get CIs
SE <- sd(results$percent_crossings_near_structure_all, na.rm = TRUE) / sqrt(length(results$percent_crossings_near_structure_all))
CI <- SE * 1.96
upper <- mean + CI
lower <- mean - CI
hist(results$percent_crossings_near_structure_all)
crossings_near_structure_all <- sum(results$crossings_near_structure_all, na.rm = TRUE)
total_crossings_all <- sum(results$real_crossings_all, na.rm = TRUE)
total_percent_crossings_near_structure <- crossings_near_structure_all/total_crossings_all
total_percent_crossings_near_structure

# get simulated crossings per structure
mean(results$sim_crossings_near_structure_all, na.rm = TRUE)
median(results$sim_crossings_near_structure_all, na.rm = TRUE)
results$sim_percent_crossings_near_structure_all <- (results$sim_crossings_near_structure_all/results$simulated_crossings_all)
median(results$sim_percent_crossings_near_structure_all, na.rm = TRUE)
mean <- mean(results$sim_percent_crossings_near_structure_all, na.rm = TRUE)
min(results$sim_percent_crossings_near_structure_all, na.rm = TRUE)
max(results$sim_percent_crossings_near_structure_all, na.rm = TRUE)
sd(results$sim_percent_crossings_near_structure_all, na.rm = TRUE)
# get CIs
SE <- sd(results$sim_percent_crossings_near_structure_all, na.rm = TRUE) / sqrt(length(results$sim_percent_crossings_near_structure_all))
CI <- SE * 1.96
upper <- mean + CI
lower <- mean - CI
hist(results$sim_percent_crossings_near_structure_all)
sim_crossings_near_structure_all <- sum(results$sim_crossings_near_structure_all, na.rm = TRUE)
sim_total_crossings_all <- sum(results$simulated_crossings_all, na.rm = TRUE)
sim_total_percent_crossings_near_structure <- sim_crossings_near_structure_all/sim_total_crossings_all
sim_total_percent_crossings_near_structure

# is total number of crossings near a structure different between real and simulated?
crossings_real <- results$crossings_near_structure_all
crossings_sim <- results$sim_crossings_near_structure_all
t_cross <- t.test(crossings_real, crossings_sim, paired = TRUE, na.rm = TRUE)
print(t_cross)
# no (difference of 2.7; p = 0.17)

# is the percent of crossings near a structure different between real and simulated?
percent_crossings_real <- results$percent_crossings_near_structure_all
percent_crossings_sim <- results$sim_percent_crossings_near_structure_all
t_cross_perc <- t.test(percent_crossings_real, percent_crossings_sim, paired = TRUE, na.rm = TRUE)
print(t_cross_perc)
# not really (difference of only 1%; p = 0.48)

# do all this for major roads only
# number crossings near structure
mean(results$crossings_near_structure_maj, na.rm = TRUE)
median(results$crossings_near_structure_maj, na.rm = TRUE)
results$percent_crossings_near_structure_maj <- (results$crossings_near_structure_maj/results$real_crossings_maj)
median(results$percent_crossings_near_structure_maj, na.rm = TRUE)
mean <- mean(results$percent_crossings_near_structure_maj, na.rm = TRUE)
min(results$percent_crossings_near_structure_maj, na.rm = TRUE)
max(results$percent_crossings_near_structure_maj, na.rm = TRUE)
sd(results$percent_crossings_near_structure_maj, na.rm = TRUE)
# get CIs
SE <- sd(results$percent_crossings_near_structure_maj, na.rm = TRUE) / sqrt(length(results$percent_crossings_near_structure_maj))
CI <- SE * 1.96
upper <- mean + CI
lower <- mean - CI
hist(results$percent_crossings_near_structure_maj)
crossings_near_structure_maj <- sum(results$crossings_near_structure_maj, na.rm = TRUE)
total_crossings_maj <- sum(results$real_crossings_all, na.rm = TRUE)
total_percent_crossings_near_structure <- crossings_near_structure_maj/total_crossings_maj
total_percent_crossings_near_structure


# absolutes for major roads
crossings_real_maj <- results$crossings_near_structure_maj
crossings_sim_maj <- results$sim_crossings_near_structure_maj
t_cross_maj <- t.test(crossings_real_maj, crossings_sim_maj, paired = TRUE, na.rm = TRUE)
print(t_cross_maj)
# no difference (difference of 1.4; p = 0.35)

# percent crossings of major roads
results$percent_crossings_near_structure_maj <- (results$crossings_near_structure_maj/results$real_crossings_maj)
results$sim_percent_crossings_near_structure_maj <- (results$sim_crossings_near_structure_maj/results$simulated_crossings_maj)
percent_crossings_real_maj <- results$percent_crossings_near_structure_maj
percent_crossings_sim_maj <- results$sim_percent_crossings_near_structure_maj
t_cross_perc_maj <- t.test(percent_crossings_real_maj, percent_crossings_sim_maj, paired = TRUE, na.rm = TRUE)
print(t_cross_perc_maj)
# some difference (difference of 2.3%; p = 0.03)

# what are individuals doing

# hypothesis 3: Does bobcat movement behavior change when they are closer to roads? ####
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
ggplot(data = points_new, aes(x = log(Distance), y = Speed)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
# # random effect of individual for slope and intercept
# model2 <- lmer(Speed ~ log(Distance) + (log(Distance) | Individual_ID), data = points_new)
# # Model failed to converge! ^^
# summary(model2)

# log transform distance because distance from road is likely to only matter when distance is SMALL
# use random effect for slope AND intercept (if I can) since both are likely to vary by individual

# test for relationship between road density and home range size
density <- results$road_density
area <- results$area_sq_km
plot(area ~ density)
glm(area ~ density)
summary(glm(area ~ density))
ggplot(results, aes(x = road_density, y = area_sq_km)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# test for relationship between road density and average speed
results_trimmed <- subset(results, !is.na(speed) & speed != Inf)
results_trimmed <- subset(results_trimmed, !is.na(density))
speed <- results_trimmed$speed
area <- results_trimmed$area
density <- results_trimmed$road_density
summary(glm(speed ~ density))
ggplot(results_trimmed, aes(x = road_density, y = speed)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# hm this is interesting. if speed and area covary, could have impact on analysis
ggplot(results_trimmed, aes(x = area, y = speed)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# bobcats with highest and lowest areas

mean(results_trimmed$road_density)

high_area <- results_trimmed[results_trimmed$road_density > 14, ]
high <- mean(high_area$speed)

low_area <- results_trimmed[results_trimmed$road_density < 14, ]
low <- mean(low_area$speed)

hist(high_area$speed)
hist(low_area$speed)

t_area <- t.test(low_area$speed, high_area$speed, paired = FALSE, na.rm = TRUE)
print(t_area)

# test for relationship between density and number crossings per day
ggplot(results_trimmed, aes(x = density, y = crossings_per_day_all)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
summary(glm(crossings_per_day_all ~ density, data = results_trimmed))
# ok so there's a clear effect of density on number of crossings per day

ggplot(results_trimmed, aes(x = density, y = crossings_per_day_maj)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
summary(glm(crossings_per_day_maj ~ density, data = results_trimmed))
# and an effect, although smaller, of density on major road crossings per day (this makes sense)


