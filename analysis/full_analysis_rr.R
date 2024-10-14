# full analysis ctmm
# margaret mercer
# Sept 27, 2024

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

# load data
ind_file <- commandArgs(trailingOnly = TRUE)
print(ind_file)

load(ind_file)

t(paste0("Data loaded at ", Sys.time()))

# # test driving smaller subset of data
# load("results/Model_Fit_Results/Ben_rr.Rda")
# individual_gps <- read.csv("data/Bobcat_Individuals/range_resident/ben.csv")
# individual_gps <- individual_gps[1:50,]
# individual <- as.telemetry(individual_gps)
# individual$identity <- individual_gps$individual.identifier
# slot(individual, "info")$identity <- individual_gps$individual.identifier[1]
# uere(individual) <- 7

# import roads and create home range ####
# calculate the AKDE based on the best fit model
individual_akde <- akde(individual, fits)
#Return the basic statistics on the HR area
summary(individual_akde)

major_roads <- st_read("data/Major_Roads")
minor_roads <- st_read("data/Minor_Roads")
all_roads <- st_read("data/All_Roads")
t(paste0("Roads loaded at ", Sys.time()))

# Reproject the roads to match the tracking data
major_roads <- st_transform(major_roads, crs("epsg:4326"))
minor_roads <- st_transform(minor_roads, crs("epsg:4326"))
all_roads <- st_transform(minor_roads, crs("epsg:4326"))

# create and reproject home range contour
# Extract the 95% home range contour
home_range_polygon <- SpatialPolygonsDataFrame.UD(individual_akde)

# Convert SpatialPolygonsDataFrame to an sf object
home_range_sf <- st_as_sf(home_range_polygon)

# tranform to proper crs
home_range <- st_transform(home_range_sf, crs("epsg:4326"))

# estimate number of road crossings (Noonan 2021)  ####
# Estimate the most likely path based on the fitted movement model
path <- predict(individual, fits, dt = 60, complete = TRUE) # this takes like 10-15 minutes to run!
t(paste0("Predicted path generated at ", Sys.time()))

# Convert to the right format for counting road crossings
path <- SpatialPointsDataFrame.telemetry(path)
path <- spTransform(path, crs("epsg:4326"))
path_2 <- lapply(split(path, path$identity),
                 function(x) Lines(list(Line(coordinates(x))), path$identity[1L]))
crs_maj_roads <- st_crs(major_roads)$proj4string
path_2 <- SpatialLines(path_2, proj4string = CRS(crs_maj_roads))

# How many times does it cross the road
path_sf <- st_as_sf(path_2)

road_crossings_maj <- st_intersection(path_sf, major_roads)
road_crossings_min <- st_intersection(path_sf, minor_roads)
t(paste0("Road crossings generated at ", Sys.time()))

# turn everything into multipoints so we can convert to points
crossings_multi_maj <- st_cast((road_crossings_maj), to = "MULTIPOINT")
crossings_new_maj <- st_cast((crossings_multi_maj), to = "POINT")

crossings_multi_min <- st_cast((road_crossings_min), to = "MULTIPOINT")
crossings_new_min <- st_cast((crossings_multi_min), to = "POINT")


# get number of crossings for major roads
real_crossings_maj <- length(crossings_new_maj$geometry)
real_crossings_maj

# transform road crossings to sp
road_crossings_sp <- as(crossings_new_maj, "Spatial")
# Find times it crossed roads
cross_times_maj <- vector()
for(i in 1:nrow(road_crossings_sp@coords)){
  # Find which point in the path is closest to the crossing location
  dists <- geosphere::distHaversine(road_crossings_sp@coords[i,], path@coords)
  cross_times_maj[i] <- path@data[which(dists == min(dists)),"timestamp"]
} # takes about an hour to run

head(cross_times_maj)

# crossing structures maj  ####
bridges <- st_read("data/Bridges_As_Lines")
t(paste0("Bridge data loaded at ", Sys.time()))

# Measuring distance of crossings from passages
# convert to get distance in m
crossings_utc_maj <- st_transform(crossings_new_maj, crs = 32633)
bridges_utc <- st_transform(bridges, crs = 32633)
# Empty vector to store results
pass_dists_maj <- vector("numeric", length = length(crossings_utc_maj$OBJECTID))
for(i in 1:length(pass_dists_maj)){
  crossing_point <- crossings_utc_maj[i, ]
  # Find which point in the path is closest to the crossing location
  dists <- st_distance(crossing_point, bridges_utc)
  pass_dists_maj[i] <- min(dists)
}

head(pass_dists_maj)

t(paste0("Pass distances maj generated at ", Sys.time()))

# Which crossings were within 20m (replace with my own median gps error!!!) of a road passage
crossings_near_structure_maj <- length(which(pass_dists_maj <= 7))

# merge crossings_new, cross_times, and pass_dists into one dataframe
crossing_info_maj <- as.data.frame(crossings_new_maj)
crossing_info_maj$Cross_Times <- cross_times_maj
crossing_info_maj$Passage_Distances <- pass_dists_maj

# transform road crossings to sp
road_crossings_sp <- as(crossings_new_min, "Spatial")
# Find times it crossed roads
cross_times_min <- vector()
for(i in 1:nrow(road_crossings_sp@coords)){
  # Find which point in the path is closest to the crossing location
  dists <- geosphere::distHaversine(road_crossings_sp@coords[i,], path@coords)
  cross_times_min[i] <- path@data[which(dists == min(dists)),"timestamp"]
} # takes about an hour to run

# head(cross_times)

# crossing structures min  ####

# get number crossings minor roads
real_crossings_min <- length(crossings_new_min$geometry)
real_crossings_min

# Measuring distance of crossings from passages
# Empty vector to store results
crossings_utc_min <- st_transform(crossings_new_min, crs = 32633)
pass_dists_min <- vector("numeric", length = length(crossings_utc_min$OBJECTID))
for(i in 1:length(pass_dists_min)){
  crossing_point <- crossings_utc_min[i, ]
  # Find which point in the path is closest to the crossing location
  dists <- st_distance(crossing_point, bridges_utc)
  pass_dists_min[i] <- min(dists)
}

head(pass_dists_min)

t(paste0("Pass distances min generated at ", Sys.time()))

# Which crossings were within 20m (replace with my own median gps error!!!) of a road passage
crossings_near_structure_min <- length(which(pass_dists_min <= 7))

# merge crossings_new, cross_times, and pass_dists into one dataframe
crossing_info_min <- as.data.frame(crossings_new_min)
crossing_info_min$Cross_Times <- cross_times_min
crossing_info_min$Passage_Distances <- pass_dists_min

# see how number of crossings and distance to crossing structures differs in simulations ####
# Set up the paralellisation
# Reg. multiple cores for DoParallel
nCores <- 6
registerDoParallel(nCores)
# Check that it's setup correctly
getDoParWorkers()
# A character string for reprojections
LatLon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# Number of simulated replicates per animal
# nReps <- 5 # when running tests
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
  crs_roads <- st_crs(major_roads)$proj4string
  path_sim_2 <- SpatialLines(path_sim_2, proj4string = CRS(crs_roads))
  
  # How many times does it cross major roads
  path_sim_sf <- st_as_sf(path_sim_2)
  sim_road_crossings_maj <- st_intersection(path_sim_sf, major_roads)
  
  sim_crossings_multi_maj <- st_cast((sim_road_crossings_maj), to = "MULTIPOINT")
  sim_crossings_new_maj <- st_cast((sim_crossings_multi_maj), to = "POINT")
  
  Sim_Road_Cross_Count_Maj <- length(sim_crossings_new_maj$OBJECTID)
  Sim_Road_Cross_Count_Maj
  
  sim_crossings_utc_maj <- st_transform(sim_crossings_new_maj, crs = 32633)
  
  if (Sim_Road_Cross_Count_Maj > 0) {
    # create empty vector to store results
    sim_pass_dists_maj <- vector("numeric", length = length(sim_crossings_utc_maj$OBJECTID))
    for(i in 1:length(sim_crossings_utc_maj$OBJECTID)){
      sim_crossing_point_maj <- sim_crossings_utc_maj[i, ]
      # Find which point in the path is closest to the crossing location
      sim_dists_maj <- st_distance(sim_crossing_point_maj, bridges_utc)
      sim_pass_dists_maj[i] <- min(sim_dists_maj)
    }
    
    # Average distance to nearest crossing structure
    sim_average_dist_maj <- mean(sim_pass_dists_maj)
    
    # Number of crossings near crossing structure
    sim_crossings_near_structure_maj <- as.numeric(length(which(sim_pass_dists_maj <= 7)))
      
  } else {
    sim_average_dist_maj <- NA
    sim_crossings_near_structure_maj <- 0
  }
  
  
  # how many times does it cross minor roads
  sim_road_crossings_min <- st_intersection(path_sim_sf, minor_roads)
  sim_crossings_multi_min <- st_cast((sim_road_crossings_min), to = "MULTIPOINT")
  sim_crossings_new_min <- st_cast((sim_crossings_multi_min), to = "POINT")
  Sim_Road_Cross_Count_Min <- length(sim_crossings_new_min$OBJECTID)
  sim_crossings_utc_min <- st_transform(sim_crossings_new_min, crs = 32633)
  
  if (Sim_Road_Cross_Count_Min > 0) {
    sim_pass_dists_min <- vector("numeric", length = length(sim_crossings_utc_min$OBJECTID))
    for(i in 1:length(sim_crossings_utc_min$OBJECTID)){
      sim_crossing_point_min <- sim_crossings_utc_min[i, ]
      sim_dists_min <- st_distance(sim_crossing_point_min, bridges_utc)
      sim_pass_dists_min[i] <- min(sim_dists_min)
    }
    sim_average_dist_min <- mean(sim_pass_dists_min)
    sim_crossings_near_structure_min <- as.numeric(length(which(sim_pass_dists_min <= 7)))
    
  } else {
    sim_average_dist_min <- NA
    sim_crossings_near_structure_min <- 0
  }
  
  # list of results to return
  list(fits@info$identity,
       Sim_Road_Cross_Count_Maj,
       sim_average_dist_maj,
       sim_crossings_near_structure_maj,
       Sim_Road_Cross_Count_Min,
       sim_average_dist_min,
       sim_crossings_near_structure_min)
  
  
}

t(paste0("Crossing simulations finished at ", Sys.time()))

# Clean up results
sim_results <- data.frame("ID" = unlist(lapply(x, function (x) x[1])),
                          "Road_Crossings_Maj" = unlist(lapply(x, function (x) x[2])),
                          "Average_Distance_From_Crossing_Structure_Maj" = unlist(lapply(x, function (x) x[3])),
                          "Numb_Crossings_Near_Structure_Maj" = unlist(lapply(x, function (x) x[4])),
                          "Road_Crossings_Min" = unlist(lapply(x, function (x) x[5])),
                          "Average_Distance_From_Crossing_Structure_Min" = unlist(lapply(x, function (x) x[6])),
                          "Numb_Crossings_Near_Structure_Min" = unlist(lapply(x, function (x) x[7])))
# sim_results
simulated_crossings_maj <- mean(sim_results$Road_Crossings_Maj)
simulated_crossings_min <- mean(sim_results$Road_Crossings_Min)
sim_crossings_near_structure_maj <- mean(sim_results$Numb_Crossings_Near_Structure_Maj)
sim_crossings_near_structure_min <- mean(sim_results$Numb_Crossings_Near_Structure_Min)
name <- sim_results[1,1]
name

# estimate average speed ####
speed <- speed(individual, fits, fast=TRUE, robust=TRUE) 
speed <- speed$CI[1, "est"]

t(paste0("Speed function finished at ", Sys.time()))

# estimate instantaneous speeds ####
speeds <- speeds(individual, fits)
# its in meters/second

# import roads and create home range ####
# calculate the AKDE based on the best fit model
individual_akde <- akde(individual, fits)
#Return the basic statistics on the HR area
summary(individual_akde)

# create and reproject home range contour
# Extract the 95% home range contour
home_range_polygon <- SpatialPolygonsDataFrame.UD(individual_akde)

# Convert SpatialPolygonsDataFrame to an sf object
home_range_sf <- st_as_sf(home_range_polygon)

# tranform to proper crs
home_range <- st_transform(home_range_sf, crs("epsg:4326"))

# Get the areas that fall on either side of the road
roads_within_range <- st_intersection(home_range, all_roads)


# calculate distance of each point to nearest road (Noonan 2021) ####
# First I need to get instantaneous speed at each point as a function of the distance from that point to the road
# so we'll need to get out individual (as spatial) and roads, get the distances, and then get instantaneous speed from each point too

# get distances:
# make individual a spatial object
individual_df <- as.data.frame(individual)
# simplify dataframe
individual_df <- individual_df[ , 2:3]
individual_sf <- st_as_sf(individual_df,
                          coords = c("longitude", "latitude"),
                          crs = st_crs(crs("epsg:4326")))

# make empty dataframe to store results
results_road_dist <- data.frame(Distance = rep(NA, length(individual$timestamp)), stringsAsFactors = FALSE)
# and add "timestamp" column
results_road_dist$Timestamp <- individual$timestamp

# then get distance to nearest road (for loop to run through all points)
for(i in 1:length(individual$timestamp)){
  
  # select i
  point_i <- individual_sf[i, ]
  
  # Find the nearest road
  nearest_road_idx <- st_nearest_feature(point_i, all_roads)
  
  # Get the nearest road geometry
  nearest_road <- all_roads[nearest_road_idx, ]
  
  # Calculate the distance
  distance <- st_distance(point_i, nearest_road)
  
  # add distance to empty results dataframe
  results_road_dist[i, 1] <- distance
  
}

t(paste0("Distance to nearest road calculated at ", Sys.time()))
results_dist_and_speed <- results_road_dist

# make dataframe with one row per point and a column with instantaneous speed and a column with distance
results_dist_and_speed$Speed <- speeds$est

# now calculate road density and home range size ####
# grab home range size
summary <- summary(individual_akde)
areas <- as.data.frame(summary$CI)
area_sq_km <- areas$est

# Calculate total road length in meters
total_road_length <- st_length(roads_within_range)
total_road_length_m <- sum(total_road_length)
total_road_length_km <- as.numeric(total_road_length_m/1000)

# Compute road density (meters of road per square meter of home range)
road_density <- total_road_length_km / area_sq_km

t(paste0("Road density calculated at ", Sys.time()))

# print outputs ####
# get number of days collar was deployed
times <- (individual$timestamp)
days <- as.numeric(difftime(max(times), min(times), units = "days"))

# Vector of results to return
x <- data.frame(name, 
                days, 
                real_crossings_maj, 
                crossings_near_structure_maj,
                real_crossings_min,
                crossings_near_structure_min,
                simulated_crossings_maj, 
                sim_crossings_near_structure_maj,
                simulated_crossings_min,
                sim_crossings_near_structure_min,
                speed, 
                area_sq_km, 
                total_road_length_km, 
                road_density)

# Store results in data.frame
write.table(x, 'results/results_rr.csv', append=TRUE, row.names=FALSE, col.names=FALSE, sep=',')

# save full simulation results
write.csv(sim_results,
          file = paste0("results/Simulation_Results/", name, "_sim_cross.csv"))

# save crossing info dataframes
write.csv(crossing_info_maj,
          file = paste0("results/Crossing_Info_Major_Roads/", name, "_crossing_info_maj.csv"))
write.csv(crossing_info_min,
          file = paste0("results/Crossing_Info_Minor_Roads/", name, "_crossing_info_min.csv"))


write.csv(results_dist_and_speed,
          file = paste0("results/Dist_from_Road_and_Speed/", name, "_dist_and_speed.csv"))


