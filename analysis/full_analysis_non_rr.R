# full analysis of non rr bobcats 
# (speed not included because can't calculate speed with BM model)
# margaret mercer
# sept 20, 2024

# load packages
library(ctmm)
library(proj4)
library(foreach)
library(doParallel)
library(sf)
library(raster)
library(adehabitatHR)
library(tidyverse)
library(geosphere)

# intro stuff ####
# 
# # test driving smaller subset of data
# load("results/Model_Fit_Results/Danielle_nonrr.Rda")
# individual_gps <- read.csv("data/Bobcat_Individuals/non_range_resident/danielle_NOT_RR.csv")
# individual_gps <- individual_gps[1:50,]
# individual <- as.telemetry(individual_gps)
# individual$identity <- individual_gps$individual.identifier
# slot(individual, "info")$identity <- individual_gps$individual.identifier[1]
# uere(individual) <- 7

# load data
ind_file <- commandArgs(trailingOnly = TRUE)
print(ind_file)

load(ind_file)

t(paste0("Data loaded at ", Sys.time()))

# now we have "fits" (model fit), "hr" (home range), and "individual" (telemetry object). 
# Each of these is duplicated and the copy is named after the bobcat

roads <- st_read("data/Roadmap_Wrangled")
t(paste0("Roads loaded at ", Sys.time()))

# Reproject the roads to match the tracking data
roads <- st_transform(roads, crs("epsg:4326"))

# estimate number of road crossings (Noonan 2021)  ####
# Estimate the most likely path based on the fitted movement model
path <- predict(individual, fits, dt = 60, complete = TRUE) # this takes like 10-15 minutes to run!
t(paste0("Predicted path generated at ", Sys.time()))

# Convert to the right format for counting road crossings
path <- SpatialPointsDataFrame.telemetry(path)
path <- spTransform(path, crs("epsg:4326"))
path_2 <- lapply(split(path, path$identity),
                 function(x) Lines(list(Line(coordinates(x))), path$identity[1L]))
crs_roads <- st_crs(roads)$proj4string
path_2 <- SpatialLines(path_2, proj4string = CRS(crs_roads))

# How many times does it cross the road
path_sf <- st_as_sf(path_2)

road_crossings <- st_intersection(path_sf, roads)
t(paste0("Road crossings generated at ", Sys.time()))

# turn everything into multipoints so we can convert to points
crossings_multi <- st_cast((road_crossings), to = "MULTIPOINT")
crossings_new <- st_cast((crossings_multi), to = "POINT")


# get number of crossings
numb_real_crossings <- length(crossings_new$geometry)
numb_real_crossings

# transform road crossings to sp
road_crossings_sp <- as(crossings_new, "Spatial")
# Find times it crossed roads
cross_times <- vector()
for(i in 1:nrow(road_crossings_sp@coords)){
  # Find which point in the path is closest to the crossing location
  dists <- geosphere::distHaversine(road_crossings_sp@coords[i,], path@coords)
  cross_times[i] <- path@data[which(dists == min(dists)),"timestamp"]
} # takes about an hour to run

# head(cross_times)

# see whether they crossed roads more or less frequently than expected at random (Noonan 2021) ####
# Set up the paralellisation
# Reg. multiple cores for DoParallel
nCores <- 6
registerDoParallel(nCores)
# Check that it's setup correctly
getDoParWorkers()
# A character string for reprojections
LatLon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#Number of simulated replicates per animal
nReps <- 1000
# Run the simulations for this animal

x <- foreach(j = 1:nReps) %dopar% {
  
  # Simulate data from the fitted movement model
  sims <- simulate(fits, t = individual$t, complete = TRUE)
  
  # Convert to the right format for counting road crossings
  path_sim <- SpatialPointsDataFrame.telemetry(sims)
  path_sim <- spTransform(path_sim, crs("epsg:4326"))
  path_sim_2 <- lapply(split(path_sim, path_sim$identity),
                       function(x) Lines(list(Line(coordinates(x))), path_sim$identity[1L]))
  crs_roads <- st_crs(roads)$proj4string
  path_sim_2 <- SpatialLines(path_sim_2, proj4string = CRS(crs_roads))
  
  # How many times does it cross the road
  path_sim_sf <- st_as_sf(path_sim_2)
  sim_road_crossings <- st_intersection(path_sim_sf, roads)
  
  sim_crossings_multi <- st_cast((sim_road_crossings), to = "MULTIPOINT")
  sim_crossings_new <- st_cast((sim_crossings_multi), to = "POINT")
  
  Sim_Road_Cross_Count <- length(sim_crossings_new$LINEARID)
  
  
  # Crossing times for the simulated animal
  # Requires conversion to lat long for the distHaversine function
  sim_road_crossings_sp <- as(sim_crossings_new, "Spatial")
  # road_crossings_latlong <- spTransform(sim_road_crossings, LatLon)
  path_sim_latlong <- spTransform(path_sim, LatLon)
  # Find times it crossed the road
  sim_road_cross_times <- vector()
  for(i in 1:nrow(sim_road_crossings_sp@coords)){
    # Find which point in the path is closest to the crossing location
    dists <- geosphere::distHaversine(sim_road_crossings_sp@coords[i,], path_sim_latlong@coords)
    sim_road_cross_times [i] <- path_sim@data[which(dists == min(dists)),"timestamp"]
  }
  
  
  # list of results to return
  list(fits@info$identity,
       # sim_road_cross_times,
       Sim_Road_Cross_Count)
  
  # # use this to troubleshoot
  # plot(path_sim, col = "NA")
  # lines(roads, col = "#FF0000")
  # lines(path_sim_sf, col = "#046C9A")
  # coords <- st_coordinates(sim_crossings_new)  # Extract coordinates
  # points(coords[, 1], coords[, 2], col = "black", pch = 16)  # Plot the points
  
  
}

t(paste0("Crossing simulations finished at ", Sys.time()))


# 10 sims takes 96 seconds
# 100 sims takes 24 minutes
# 1000 sims should take 3ish hours

# Clean up results
sim_results <- data.frame("ID" = unlist(lapply(x, function (x) x[1])),
                          "Road_Crossings" = unlist(lapply(x, function (x) x[2])))
numb_simulated_crossings <- mean(sim_results$Road_Crossings)
name <- sim_results[1,1]
name


# now crossing structures ugh
bridges <- st_read("data/Bridges_As_Lines")
t(paste0("Bridge data loaded at ", Sys.time()))

# Measuring distance of crossings from passages
# convert to get distance in m
crossings_utc <- st_transform(crossings_new, crs = 32633)
bridges_utc <- st_transform(bridges, crs = 32633)
# Empty vector to store results
pass_dists <- vector("numeric", length = length(crossings_utc))
for(i in 1:length(crossings_utc$LINEARID)){
  crossing_point <- crossings_utc[i, ]
  # Find which point in the path is closest to the crossing location
  dists <- st_distance(crossing_point, bridges_utc)
  pass_dists[i] <- min(dists)
}

head(pass_dists)

t(paste0("Pass distances generated at ", Sys.time()))

# Which crossings were within 20m (replace with my own median gps error!!!) of a road passage
numb_crossings_near_structure <- length(which(pass_dists <= 7))

# merge crossings_new, cross_times, and pass_dists into one dataframe
crossing_info <- as.data.frame(crossings_new)
crossing_info$Cross_Times <- cross_times
crossing_info$Passage_Distances <- pass_dists # check to be sure this works!

# Vector of results to return
x <- data.frame(name, numb_real_crossings, numb_simulated_crossings, numb_crossings_near_structure)
# Store results in data.frame
write.table(x, 'results/results_non_rr.csv', append=TRUE, row.names=FALSE, col.names=FALSE, sep=',')

# save full simulation results
write.csv(sim_results,
          file = paste0("results/Number_of_Simulated_Crossings/", name, "_sim_cross_non_rr.csv"))

# save crossing info dataframe
write.csv(crossing_info,
          file = paste0("results/Crossing_Info/", name, "_crossing_info_non_rr.csv"))
