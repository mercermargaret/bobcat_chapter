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
# crossed all roads 11% less frequently than expected by chance

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

real_min <- results$real_crossings_min
simulated_min <- results$simulated_crossings_min
hist(real_min)
hist(simulated_min)
sqrt_real <- (sqrt(real_min))
sqrt_sim <- (sqrt(simulated_min))
t_sim_min <- t.test(sqrt_real, sqrt_sim, paired = TRUE, na.rm = TRUE)
print(t_sim_min)
mean(real_min)
mean(simulated_min)
mean(simulated_min) - mean(real_min)
(mean(simulated_min) - mean(real_min))/mean(simulated_min)
# p of 0.1; crossed minor roads 8% less frequently than expected

# load in simulations
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

# see how simulation number of crossings differ from actual crossings
x <- vector(mode = "character", length = length(results$name))
for (i in 1:length(results$name)) {
  name <- results$name[i]
  subset <- sim_crossings[sim_crossings$ID == name,]
  sim_numb_crossings <- subset$Road_Crossings_All
  # hist(sim_numb_crossings)
  lower_bound <- quantile(sim_numb_crossings, 0.025)
  upper_bound <- quantile(sim_numb_crossings, 0.975)
  # pull out my value for that individual from the results dataframe
  real_crossings <- results$real_crossings_all[results$name == name]
  
  x[i] <- ifelse(real_crossings > upper_bound, "higher",
                 ifelse(real_crossings < lower_bound, "lower",
                        "within"))
}

results$real_crossings_vs_simulated <- x

plot(results$real_crossings_all ~ results$simulated_crossings_all, ylim = c(0,12000), xlim = c(0,12000))
abline(0,1)



# hypothesis 2: Do bobcats use culverts to cross roads? ####

# load major roads crossing info
# pull and merge crossing info
file_list <- list.files(path = "results/Crossing_Info_Major_Roads", pattern = "*.csv", full.names = TRUE)
crossings_maj <- do.call(rbind, lapply(file_list, function(file) {
  data <- read.csv(file)
  data$Individual_ID <- tools::file_path_sans_ext(basename(file))  # Extract filename without extension
  return(data)
}))
# get rid of unwanted detail in name column
crossings_maj$Individual_ID <- sub("_.*", "", crossings_maj$Individual_ID)
# mean <- mean(crossings_maj$Passage_Distances, na.rm = TRUE)
# # get standard error
# SE <- sd(crossings_maj$Passage_Distances) / sqrt(length(crossings_maj$Passage_Distances))
# # get CIs
# CI <- SE * 1.96
# upper <- mean + CI
# lower <- mean - CI
# hist(crossings_maj$Passage_Distances)
# mean

# load minor roads crossing info
file_list <- list.files(path = "results/Crossing_Info_Minor_Roads", pattern = "*.csv", full.names = TRUE)
crossings_min <- do.call(rbind, lapply(file_list, function(file) {
  data <- read.csv(file)
  data$Individual_ID <- tools::file_path_sans_ext(basename(file))  # Extract filename without extension
  return(data)
}))
# get rid of unwanted detail in name column
crossings_min$Individual_ID <- sub("_.*", "", crossings_min$Individual_ID)
# mean <- mean(crossings_min$Passage_Distances, na.rm = TRUE)
# # get standard error
# SE <- sd(crossings_min$Passage_Distances) / sqrt(length(crossings_min$Passage_Distances))
# # get CIs
# CI <- SE * 1.96
# upper <- mean + CI
# lower <- mean - CI
# hist(crossings_min$Passage_Distances)

# merge to get crossing info for all roads
crossings_maj$Road_Type <- "major"
crossings_min$Road_Type <- "minor"
crossings_all <- bind_rows(crossings_maj, crossings_min)
mean <- mean(crossings_all$Passage_Distances, na.rm = TRUE)
mean
# get standard error
SE <- sd(crossings_all$Passage_Distances) / sqrt(length(crossings_all$Passage_Distances))
# get CIs
CI <- SE * 1.96
upper <- mean + CI
lower <- mean - CI
hist(crossings_all$Passage_Distances)

# get average simulated distance from crossing structure
mean <- mean(sim_crossings$Average_Distance_From_Crossing_Structure_All, na.rm = TRUE)
SE <- sd(sim_crossings$Average_Distance_From_Crossing_Structure_All, na.rm = TRUE) / sqrt(length(sim_crossings$Average_Distance_From_Crossing_Structure_All))
# get CIs
CI <- SE * 1.96
upper <- mean + CI
lower <- mean - CI

# number crossings near structure for all roads
results$percent_crossings_near_structure_all <- (results$crossings_near_structure_all/results$real_crossings_all)
median(results$percent_crossings_near_structure_all, na.rm = TRUE)
mean <- mean(results$percent_crossings_near_structure_all, na.rm = TRUE)
mean
min(results$percent_crossings_near_structure_all, na.rm = TRUE)
max(results$percent_crossings_near_structure_all, na.rm = TRUE)
sd(results$percent_crossings_near_structure_all, na.rm = TRUE)
# get CIs
SE <- sd(results$percent_crossings_near_structure_all, na.rm = TRUE) / sqrt(length(results$percent_crossings_near_structure_all))
CI <- SE * 1.96
upper <- mean + CI
lower <- mean - CI

# number crossings near structure for all roads
results$percent_crossings_near_structure_maj <- (results$crossings_near_structure_maj/results$real_crossings_maj)
median(results$percent_crossings_near_structure_maj, na.rm = TRUE)
mean <- mean(results$percent_crossings_near_structure_maj, na.rm = TRUE)
mean
min(results$percent_crossings_near_structure_maj, na.rm = TRUE)
max(results$percent_crossings_near_structure_maj, na.rm = TRUE)
sd(results$percent_crossings_near_structure_maj, na.rm = TRUE)
# get CIs
SE <- sd(results$percent_crossings_near_structure_maj, na.rm = TRUE) / sqrt(length(results$percent_crossings_near_structure_maj))
CI <- SE * 1.96
upper <- mean + CI
lower <- mean - CI



# number simulated crossings near structure for all roads
results$sim_percent_crossings_near_structure_all <- (results$sim_crossings_near_structure_all/results$simulated_crossings_all)
median(results$sim_percent_crossings_near_structure_all, na.rm = TRUE)
mean <- mean(results$sim_percent_crossings_near_structure_all, na.rm = TRUE)
mean
min(results$sim_percent_crossings_near_structure_all, na.rm = TRUE)
max(results$sim_percent_crossings_near_structure_all, na.rm = TRUE)
sd(results$sim_percent_crossings_near_structure_all, na.rm = TRUE)
# get CIs
SE <- sd(results$sim_percent_crossings_near_structure_all, na.rm = TRUE) / sqrt(length(results$sim_percent_crossings_near_structure_all))
CI <- SE * 1.96
upper <- mean + CI
lower <- mean - CI

# pull out distribution of percentages for each individual and compare the actual number to that distribution
sim_crossings$Percent_Crossings_Near_Structure_All <- sim_crossings$Road_Crossings_Near_Structure_All/sim_crossings$Road_Crossings_All

x <- vector(mode = "character", length = length(results$name))
for (i in 1:length(results$name)) {
  name <- results$name[i]
  subset <- sim_crossings[sim_crossings$ID == name,]
  sim_percent_crossing_near_structure <- subset$Percent_Crossings_Near_Structure_All
  # hist(sim_percent_crossing_near_structure)
  lower_bound <- quantile(sim_percent_crossing_near_structure, 0.025, na.rm = TRUE)
  upper_bound <- quantile(sim_percent_crossing_near_structure, 0.975, na.rm = TRUE)
  # pull out my value for that individual from the results dataframe
  real_percent_crossing_near_structure <- results$percent_crossings_near_structure_all[results$name == name]
  
  x[i] <- ifelse(real_percent_crossing_near_structure >= upper_bound, "higher",
                 ifelse(real_percent_crossing_near_structure <= lower_bound, "lower",
                        "within"))
}

results$real_crossings_near_structure_vs_simulated <- x
table(results$real_crossings_near_structure_vs_simulated)
# there werent enough major road crossings to do this for major roads

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
points_new <- points %>%
  filter(is.finite(Speed))

# random effect of individual for just intercept
model1 <- lmer(Speed ~ log(Distance) + (1 | Individual_ID), data = points_new)
summary(model1)
ggplot(data = points_new, aes(x = log(Distance), y = Speed)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
# random effect of individual for slope and intercept
model2 <- lmer(Speed ~ log(Distance) + (log(Distance) | Individual_ID), data = points_new)
summary(model2)
plot(Speed ~ log(Distance) + (log(Distance)), data = points_new)

# log transform distance because distance from road is likely to only matter when distance is SMALL
# use random effect for slope AND intercept (if I can) since both are likely to vary by individual

# test for relationship between road density and home range size
density <- results$road_density
area <- results$area_sq_km
plot(area ~ density)
glm(area ~ density)
summary(glm(area ~ density))


# test for relationship between road density and average speed
results_trimmed <- subset(results, !is.na(results$speed) & speed != Inf)
results_trimmed <- subset(results_trimmed, !is.na(results_trimmed$road_density))
speed <- results_trimmed$speed
area <- results_trimmed$area
density <- results_trimmed$road_density
summary(glm(speed ~ density))

# hm this is interesting. if speed and area covary, could have impact on analysis
ggplot(results_trimmed, aes(x = area, y = speed)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# bobcats with highest and lowest areas

mean <- mean(results_trimmed$road_density)

high_area <- results_trimmed[results_trimmed$road_density > mean, ]
high <- mean(high_area$speed)

low_area <- results_trimmed[results_trimmed$road_density < mean, ]
low <- mean(low_area$speed)

# hist(high_area$speed)
# hist(low_area$speed)

t_area <- t.test(low_area$speed, high_area$speed, paired = FALSE, na.rm = TRUE)
print(t_area)
(high - low)/low

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


# mortality ####
# mort as function of road density
density_mort <- (results$road_density[results$mortality == 1])
density_no_mort <- (results$road_density[results$mortality == 0])
t2_mort <- t.test(density_no_mort, density_mort, na.rm = TRUE)
print(t2_mort)
# no effect

glm2_mort <- glm(mortality ~ road_density, data = results, family = binomial)
summary(glm2_mort)
# no effect

# plot density
hist(results$road_density, 
     main = "Road Density with Mortality As Dots", 
     xlab = "Values", 
     ylab = "Frequency", 
     breaks = 20, 
     col = "lightgray")
# add dots for mortalities
points(results$road_density[results$mortality == 1], 
       rep(0, sum(results$mortality == 1)),  
       pch = 19, col = "red")
# there doesn't really seem to be a pattern


# sex ####


# home range size as function of sex
area_male <- (results$area_sq_km[results$SEX == "M"])
area_female <- (results$area_sq_km[results$SEX == "F"])
t1_sex <- t.test(area_male, area_female, na.rm = TRUE)
print(t1_sex)
mm <- mean(area_male, na.rm = TRUE)
mf <- mean(area_female, na.rm = TRUE)
md <- mm-mf
md/mf
# so this makes sense; home ranges are MUCH smaller for females
# 17.375084 - 6.776367 = 10.59872
# t = 6.1203, df = 17.507, p-value = 9.958e-06
# plot home range size
hist(results$area_sq_km, 
     main = "Home Range With Males as Dots", 
     xlab = "Values", 
     ylab = "Frequency", 
     breaks = 20, 
     col = "lightgray")
# add dots for males
points(results$area_sq_km[results$SEX == "M"], 
       rep(0, sum(results$SEX == "M")),  
       pch = 19, col = "red")
# males have larger home ranges

# crossings as function of sex
crossings_male <- (results$crossings_per_day_all[results$SEX == "M"])
crossings_female <- (results$crossings_per_day_all[results$SEX == "F"])
t2_sex <- t.test(crossings_male, crossings_female, na.rm = TRUE)
print(t2_sex)
# plot crossings
hist(results$crossings_per_day_all, 
     main = "Crossings with Males as Dots", 
     xlab = "Values", 
     ylab = "Frequency", 
     breaks = 20, 
     col = "lightgray")
# add dots for males
points(results$crossings_per_day_all[results$SEX == "M"], 
       rep(0, sum(results$SEX == "M")),  
       pch = 19, col = "red")
# no strong pattern

# road density as function of sex
density_male <- (results$road_density[results$SEX == "M"])
density_female <- (results$road_density[results$SEX == "F"])
t3_sex <- t.test(density_male, density_female, na.rm = TRUE)
print(t3_sex)
# no correlation
# plot density
hist(results$road_density, 
     main = "Road Density with Males as Dots", 
     xlab = "Values", 
     ylab = "Frequency", 
     breaks = 20, 
     col = "lightgray")
# add dots for males
points(results$road_density[results$SEX == "M"], 
       rep(0, sum(results$SEX == "M")),  
       pch = 19, col = "red")
# no clear pattern

# crossings near structure as function of sex
crossing_structure_male <- (results$percent_crossings_near_structure_all[results$SEX == "M"])
crossing_structure_female <- (results$percent_crossings_near_structure_all[results$SEX == "F"])
t4_sex <- t.test(crossing_structure_male, crossing_structure_female, na.rm = TRUE)
print(t4_sex)
mean(crossing_structure_male, na.rm = TRUE) - mean(crossing_structure_female, na.rm = TRUE)
# nonsignificant and only a difference of 0.2%... (females sliiightly higher use of crossing structure)
# plot crossings near structure
hist(log(results$percent_crossings_near_structure_all), 
     main = "Crossings Near Structure with Males as Dots", 
     xlab = "Values", 
     ylab = "Frequency", 
     breaks = 20, 
     col = "lightgray")
# add dots for males
points(log(results$percent_crossings_near_structure_all[results$SEX == "M"]), 
       rep(0, sum(results$SEX == "M")),  
       pch = 19, col = "red")
# very 0 inflated so i log transformed it; no clear pattern

# speed as a function of sex
results_new <- results %>%
  filter(is.finite(speed))
speed_male <- (results_new$speed[results_new$SEX == "M"])
speed_female <- (results_new$speed[results_new$SEX == "F"])
t5_sex <- t.test(speed_male, speed_female, na.rm = TRUE)
print(t5_sex)
mean(speed_male, na.rm = TRUE) - mean(speed_female, na.rm = TRUE)
# no significant effect
# plot speed
hist(log(results$speed), 
     main = "Speed with Males as Dots", 
     xlab = "Values", 
     ylab = "Frequency", 
     breaks = 20, 
     col = "lightgray")
# add dots for males
points(log(results$speed[results$SEX == "M"]), 
       rep(0, sum(results$SEX == "M")),  
       pch = 19, col = "red")
# no clear pattern

# save modified results df ####

write.csv(results, "results/results.csv", row.names = FALSE)
