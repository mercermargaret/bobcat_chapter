# wrangle bobcat gps data into individual bobcats: remove outliers, 
  # check for range residency, and write csv
# margaret mercer
# Sept 2, 2024

# load packages
library(ctmm)
library(tidyverse)

# clear workspace
rm(list=ls())

# loading in gps data as csv
data <- read.csv("data/bobcat_locs_all.csv")

names_list <- unique(data$individual.local.identifier)
name <- names_list[4] # change this number, 1 through however many individuals you have

# subset to one individual
individual_gps <- data[data$individual.local.identifier == name, ]
summary(individual_gps)

# making it a telemetry object so ctmm recognizes it
individual_tel <- as.telemetry(individual_gps)

uere(individual_tel) <- 7

# plotting!
plot(individual_tel, error = 2, level.UD = 0.50)

# ID outliers
outliers <- outlie(individual_tel)
plot(outliers)

# get rid of outliers in individual_tel
outlier_t <- outliers$t[outliers$distance >= 6000] # change the number here so it trims outliers away
# don't cut outliers unless they look super crazy!
individual_tel <- individual_tel[!individual_tel$t %in% outlier_t, ]
# replot data
plot(individual_tel, error = 2, level.UD = 0.50)
outliers <- outlie(individual_tel)
plot(outliers)

# make a variogram
vg <- variogram(individual_tel) 
plot(vg) # is it range resident? if so, proceed
# if not: when do ctmm select, only run non-range resident models!
# can't do a home range estimation! Jesse sent me an email with instructions.
# can't do rsf on it. speed estimation should work
# in methods: say which analyses I excluded non-range resident individuals from!

# turn back into a gps-style dataframe
individual_gps_new <- data.frame(
  individual.identifier = individual_gps$individual.local.identifier[1:25],
  timestamp = individual_tel$timestamp,
  location.lat = individual_tel$latitude,
  location.long = individual_tel$longitude
)

# now write csv

write.csv(individual_gps, "data/Bobcat_Individuals/wyatt.csv")


