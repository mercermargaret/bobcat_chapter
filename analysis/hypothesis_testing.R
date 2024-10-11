# testing our three main hypotheses
# margaret mercer
# Sept 30, 2024

# clear workspace and load packages and data ####

# clear workspace
rm(list=ls())

# load packages
library(tidyverse)
library(lme4)
library(lmerTest)

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
# calculate crossings/day
results$crossings_per_day <- results$numb_real_crossings/results$numb_days
# add mortality and sex columns
results$mortality <- ifelse(results$name == "Hal" | 
                              results$name ==  "Cooper" | 
                              results$name == "Minnie" | 
                              results$name == "EmmaClaire" | 
                              results$name == "Cassidy" |
                              results$name == "Michele", 1, 0)
# results$sex <- 

# mort as a function of crossings per day
crossings_per_day_mort <- (results$crossings_per_day[results$mortality == 1])
crossings_per_day_no_mort <- (results$crossings_per_day[results$mortality == 0])
t1 <- t.test(crossings_per_day_no_mort, crossings_per_day_mort, na.rm = TRUE)
print(t1)

# mort as function of road density
density_mort <- (results$road_density[results$mortality == 1])
density_no_mort <- (results$road_density[results$mortality == 0])
t2 <- t.test(density_no_mort, density_mort, na.rm = TRUE)
print(t2)

# mort as function of types of roads crossed
laksdfasknd

# hypothesis 1: Do bobcats cross roads less frequently than would be expected from random chance? ####
# visualize the data
real <- results$numb_real_crossings
simulated <- results$numb_simulated_crossings
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
# cross roads 11% less frequently than expected by chance

# visualization
# # reshape
# long_results <- results %>%
#   pivot_longer(cols = c(numb_real_crossings, numb_simulated_crossings), 
#                names_to = "Type", 
#                values_to = "Value")
# long_results <- select(long_results, name, Type, Value)
# 
# # Create the grouped bar chart
# ggplot(long_results, aes(x = name, y = Value, fill = Type)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
#   labs(title = "Grouped Bar Chart: Real vs Simulated Values",
#        x = "Individual",
#        y = "Value") +
#   theme_minimal() +
#   scale_fill_manual(values = c("numb_real_crossings" = "blue", "numb_simulated_crossings" = "orange"), 
#                     name = "Type")

# lollipop chart

results$difference <- results$numb_real_crossings - results$numb_simulated_crossings

# Define custom colors
my_colors <- c("Increase" = "#0B5401", 
               "Slight Increase" = "#77A87C", 
               "No Change" = "steelblue", "Slight Decrease" = "#C67976", 
               "Decrease" = "#8B0000", 
               "White" = "white")

# Create the lollipop chart with legend title removed and custom colors
lol <- ggplot(results, aes(x = name, y = difference,
                        fill = ifelse(difference > 0, "Increase",
                                                    ifelse(difference < 0, "Decrease", "No Change")),
                        color = ifelse(difference > 0, "Increase",
                                                     ifelse(difference < 0, "Decrease", "No Change"))
)) +
  geom_segment(aes(xend = name, yend = 0)) +
  geom_point(shape = 21, size = 3) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c((min((results$difference)) - 0.15), 
                                (max((results$difference)) + 0.1)),) +
  coord_flip() +
  theme_classic () +
  theme(axis.title.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Helvetica", size = 15)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  labs(x = NULL, 
       y = "Difference Between Real and Simulated Crossings", 
       main = "Difference Between Real and Simulated Crossings") +
  scale_color_manual(values = my_colors) +  # Set custom colors
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
  scale_fill_manual(values = my_colors) +
  geom_text(aes(x = name, 
                y = (min((difference)) - 0.14), 
                label = name), 
            hjust = 0, 
            vjust = 0.5, 
            color = "black")
lol



# Jesse: split roads up into different categories. Major roads, minor roads. And then do this again. !!!


# hypothesis 2: Do bobcats use culverts to cross roads? ####

# get median distance of road crossing
# compare percentage of crossings near a structure between real and simulated data
# pull and merge crossing info
file_list <- list.files(path = "results/Crossing_Info", pattern = "*.csv", full.names = TRUE)
crossings <- do.call(rbind, lapply(file_list, function(file) {
  data <- read.csv(file)
  data$Individual_ID <- tools::file_path_sans_ext(basename(file))  # Extract filename without extension
  return(data)
}))
# get rid of unwanted detail in name column
crossings$Individual_ID <- sub("_.*", "", crossings$Individual_ID)

mean <- mean(crossings$Passage_Distances, na.rm = TRUE)

# get standard error
SE <- sd(crossings$Passage_Distances) / sqrt(length(crossings$Passage_Distances))

# get CIs
CI <- SE * 1.96

upper <- mean + CI
lower <- mean - CI

# PROBLEM!!!! if we're just getting the nearest crossing structure, the nearest could be BEHIND the bobcat, or NOT ON THE ROAD IT'S CROSSING
# ????how would we deal with that??
hist(crossings$Passage_Distances)

mean(results$numb_crossings_near_structure, na.rm = TRUE)
median(results$numb_crossings_near_structure, na.rm = TRUE)

results$percent_crossings_near_structure <- (results$numb_crossings_near_structure/results$numb_real_crossings)
median(results$percent_crossings_near_structure)
mean <- mean(results$percent_crossings_near_structure)
min(results$percent_crossings_near_structure)
max(results$percent_crossings_near_structure)
sd(results$percent_crossings_near_structure)
# get CIs
SE <- sd(results$percent_crossings_near_structure) / sqrt(length(results$percent_crossings_near_structure))
CI <- SE * 1.96
upper <- mean + CI
lower <- mean - CI

hist(results$percent_crossings_near_structure)

crossings_near_structure <- sum(results$numb_crossings_near_structure)
total_crossings <- sum(results$numb_real_crossings)
crossings_near_structure/total_crossings


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
ggplot(results_trimmed, aes(x = density, y = crossings_per_day)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
summary(glm(crossings_per_day ~ density))
