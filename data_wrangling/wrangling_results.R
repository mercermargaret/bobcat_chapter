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

# wrangle data ####

# add columns for non rr bobcats so you can merge
results_non_rr$speed <- c(NA, NA)
results_non_rr$area_sq_km <- c(NA, NA)
results_non_rr$total_road_length_km <- c(NA, NA)
results_non_rr$road_density <- c(NA, NA)

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


# print results to csv ####
write.csv(results, "results/results.csv")
