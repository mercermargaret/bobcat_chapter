# just the model fitting portion of ctmm analysis (for NON range resident individuals)
# margaret mercer
# September 20, 2024

# load packages
library(ctmm)
library(tidyverse)
library(data.table)

ind_file <- commandArgs(trailingOnly = TRUE)
print(ind_file)

individual_gps <- fread(ind_file)

t(paste0("Data loaded at ", Sys.time()))

# making bobcat a telemetry object so ctmm recognizes it
individual <- as.telemetry(individual_gps)

# right here we want to make the name so it doesn't have spaces
name <- gsub(" ", "_", individual_gps$individual.identifier)

# add individual id to telemetry object
individual$identity <- name

slot(individual, "info")$identity <- name[1]

uere(individual) <- 7

id <- individual@info$identity[1]
print(id)

assign(paste0("t_",id), individual)
t_name <- paste0("t_",id)

print(paste0("Telemetry object created at ",Sys.time()))

vg <- variogram(individual)

# Guesstimate the model to obtain initial parameter values
guess <- ctmm.guess(individual,
                    variogram = vg,
                    CTMM=ctmm(range=FALSE),
                    interactive = FALSE)
guess$error <- TRUE

fits <- ctmm.select(individual, CTMM = guess)

assign(paste0("ctmm_", id), fits)
ctmm_name <- paste0("ctmm_", id)

print(paste0("CTMM fit at ", Sys.time()))
print(summary(fits))

save(fits, individual, list = c(ctmm_name, t_name),
     file = paste0("data/Model_Fit_Results/", id, "_nonrr.Rda"))

print(paste0("Done at ",Sys.time()))

