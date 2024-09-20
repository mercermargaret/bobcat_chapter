# road crossing number hypothesis
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

# load data
ind_file <- commandArgs(trailingOnly = TRUE)
print(ind_file)

load(ind_file)

t(paste0("Data loaded at ", Sys.time()))

# now we have "fits" (model fit), "hr" (home range), and "individual" (telemetry object). 
# Each of these is duplicated and the copy is named after the bobcat

# import roads and create home range ####
# calculate the AKDE based on the best fit model
individual_akde <- akde(individual, fits)
#Return the basic statistics on the HR area
summary(individual_akde)

roads <- st_read("data/Roadmap_Wrangled")

# Reproject the roads to match the tracking data
roads <- st_transform(roads, crs("epsg:4326"))

# create and reproject home range contour
# Extract the 95% home range contour
home_range_polygon <- SpatialPolygonsDataFrame.UD(individual_akde)

# Convert SpatialPolygonsDataFrame to an sf object
home_range_sf <- st_as_sf(home_range_polygon)

# tranform to proper crs
home_range <- st_transform(home_range_sf, crs("epsg:4326"))

# Get the roads that fall within home range
roads_within_range <- st_intersection(home_range, roads)


# estimate number of road crossings (Noonan 2021)  ####
# Estimate the most likely path based on the fitted movement model
path <- predict(individual, fits, dt = 60, complete = TRUE) # this takes like 10-15 minutes to run!

# Convert to the right format for counting road crossings
path <- SpatialPointsDataFrame.telemetry(path)
path <- spTransform(path, crs("epsg:4326"))
path_2 <- lapply(split(path, path$identity),
                 function(x) Lines(list(Line(coordinates(x))), path$identity[1L]))
crs_roads <- st_crs(roads)$proj4string
path_2 <- SpatialLines(path_2, proj4string = CRS(crs_roads))

# How many times does it cross the road
path_sf <- st_as_sf(path_2)

road_crossings <- st_intersection(path_sf, roads) # this takes looong time

# turn everything into multipoints so we can convert to points
crossings_multi <- st_cast((road_crossings), to = "MULTIPOINT")
crossings_new <- st_cast((crossings_multi), to = "POINT")


# get number of crossings
real_crossings <- length(crossings_new$geometry)
real_crossings

# Plot of the most likely path and the road
plot(path_2, col = "NA")
lines(roads, col = "#FF0000")
lines(path_2, col = "#046C9A")
coords <- st_coordinates(crossings_new)  # Extract coordinates
points(coords[, 1], coords[, 2], col = "black", pch = 16)  # Plot the points
title(main = paste(individual@info$identity, " - Crossings: ", real_crossings),
      family = "serif",
      font.main = 1,
      cex.main = 0.85)

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
nReps <- 10
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


# 10 sims takes 96 seconds
# 100 sims takes 24 minutes
# 1000 sims should take 3ish hours

# Clean up results
sim_results <- data.frame("ID" = unlist(lapply(x, function (x) x[1])),
                          "Road_Crossings" = unlist(lapply(x, function (x) x[2])))
mean_sim_cross <- mean(sim_results$Road_Crossings)
name <- sim_results[1,1]

# so I want to pull out the mean of the simulated results and compare it to the actual number (real_crossings)
# I want a dataframe with three columns: one for bobcat name, one for number of crossings, one for stimulated crossings
# Vector of results to return
x <- data.frame(name, real_crossings, mean_sim_cross)
# Store results in data.frame
write.table(x, 'results/road_crossings.csv', append=TRUE, row.names=FALSE, col.names=FALSE, sep=',')

# also I should probably save the full simulation results

save(sim_results,
     file = paste0("results/Number_of_Simulated_Crossings/", name, "_sim_cross.csv"))
