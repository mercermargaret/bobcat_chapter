# wrangle results and merge with individual bobcat info
# margaret mercer
# october 17, 2024


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
bobcats <- read.csv("data/BOBCATS IN TUCSON BOBCAT CAPTURE MASTER.xlsx - Sheet1.csv")

# combine individual information with results ####

# add columns for non rr bobcats so you can merge
results_non_rr$speed <- c(NA, NA)
results_non_rr$area_sq_km <- c(NA, NA)
results_non_rr$total_road_length_km_all <- c(NA, NA)
results_non_rr$total_road_length_km_maj <- c(NA, NA)
results_non_rr$road_density_all <- c(NA, NA)
results_non_rr$road_density_maj <- c(NA, NA)
# fix wyatt's name
results_rr[33,1] <- "Wyatt"

# merge into one dataframe
results <- rbind(results_rr, results_non_rr)
# create columns for all crossings
results$real_crossings_all <- results$real_crossings_maj + results$real_crossings_min
results$simulated_crossings_all <- results$simulated_crossings_maj + results$simulated_crossings_min
results$crossings_near_structure_all <- results$crossings_near_structure_maj + results$crossings_near_structure_min
results$sim_crossings_near_structure_all <- results$sim_crossings_near_structure_maj + results$sim_crossings_near_structure_min
# calculate crossings/day
results$crossings_per_day_all <- results$real_crossings_all/results$days
results$crossings_per_day_maj <- results$real_crossings_maj/results$days
# add mortality and sex columns
results$mortality <- ifelse(results$name == "Hal" | 
                              results$name ==  "Cooper" | 
                              results$name == "Minnie" | 
                              results$name == "EmmaClaire" | 
                              results$name == "Cassidy" |
                              results$name == "Michele", 1, 0)

# standardize columns with both upper and lowercase answers
bobcats$SEX <- toupper(bobcats$SEX)
bobcats$REPRODUCTIVE_CONDITION <- toupper(bobcats$REPRODUCTIVE_CONDITION)
bobcats$COLLARED <- toupper(bobcats$COLLARED)
bobcats$BLOOD <- toupper(bobcats$BLOOD)
bobcats$DNA_SWAB <- toupper(bobcats$DNA_SWAB)
bobcats$FECAL <- toupper(bobcats$FECAL)
bobcats$HAIR <- toupper(bobcats$HAIR)
bobcats$ECTOPARASITES <- toupper(bobcats$ECTOPARASITES)

# replace ? and empty with NA
bobcats <- bobcats %>%
  mutate(across(where(is.character), ~ na_if(., "?")))  %>% 
  mutate(across(where(is.character), ~ na_if(., "")))

# replace "inf" with NA
results$speed[is.infinite(results$speed)] <- NA

# create bobcat_info dataframe and merge with results
bobcat_info <- select(bobcats, SEX, NAME, ACTUAL_WEIGHT_LB)
bobcat_info <- bobcat_info %>%
  filter(!is.na(SEX) & !is.na(NAME) & SEX != "" & NAME != "" & SEX != "U") 
bobcat_info <- bobcat_info %>%
  distinct(NAME, .keep_all = TRUE)
bobcat_info <- rename(bobcat_info, name = NAME)
results <- inner_join(results, bobcat_info, by = "name")

results$diff_all <- results$real_crossings_all - results$simulated_crossings_all
results$diff_maj <- results$real_crossings_maj - results$simulated_crossings_maj


# compare number of total crossings real vs simulated  ####

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

# number crossings near structure ####
results$percent_crossings_near_structure_all <- (results$crossings_near_structure_all/results$real_crossings_all)
results$percent_crossings_near_structure_maj <- (results$crossings_near_structure_maj/results$real_crossings_maj)
results$sim_percent_crossings_near_structure_all <- (results$sim_crossings_near_structure_all/results$simulated_crossings_all)


# compare crossings near structure real vs simulated ####
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

# print results to csv ####
write.csv(results, "results/results.csv", row.names = FALSE)
