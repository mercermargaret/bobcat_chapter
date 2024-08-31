# speed debugging example
# margaret mercer
# Aug 26, 2024

# load packages
library(ctmm)
library(tidyverse)

# clear workspace
rm(list=ls())

# loading in gps data as csv
bobcat <- read.csv("bobcat_example.csv")

# making it a telemetry object so ctmm recognizes it
bobcat <- as.telemetry(bobcat)

uere(bobcat) <- 7 # make sure this worked

# load model fits I ran earlier (it took ~24 hours)
load('data/margaret_ctmm.Rda')

# estimate average speed
speed <- speed(bobcat, fits, fast=TRUE) # this is where it breaks. Loads for ~ 5 minutes and then says:
# "Error in sqrt(diff(data$x)^2 + diff(data$y)^2)/DT/SPD : 
  # non-numeric argument to binary operator"
