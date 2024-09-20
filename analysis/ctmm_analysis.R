# ctmm analysis
# margaret mercer
# july 30, 2024
# I put this together by looking at the code from Noonan 2019 and Noonan 2021 simultaneously
    # and using what applied to me
    # see: https://static-content.springer.com/esm/art%3A10.1186%2Fs40462-019-0177-1/MediaObjects/40462_2019_177_MOESM2_ESM.pdf
    # and: https://zslpublications.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Facv.12728&file=acv12728-sup-0002-AppendixS2.pdf

# introductory stuff ####

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

# clear workspace
rm(list=ls())

# loading in gps data as csv
individual_gps <- read.csv("data/Bobcat_Individuals/range_resident/margaret.csv")

# making bobcat a telemetry object so ctmm recognizes it
individual <- as.telemetry(individual_gps)

# add individual id to telemetry object
individual$identity <- individual_gps$individual.identifier

slot(individual, "info")$identity <- individual_gps$individual.identifier[1]

uere(individual) <- 7

# make variogram
vg <- variogram(individual)

# Guesstimate the model to obtain initial parameter values
guess <- ctmm.guess(individual,
                        variogram = vg,
                        interactive = FALSE)
guess$error <- TRUE

# load model
load('data/Model_Fit_Results/Margaret_rr.Rda')

# return a summary of the fitted models
summary(fits) # this should list a model $name, $DOF, and $CI

# plot variogram and model
plot(vg, CTMM = fits)

# make variogram
vg <- variogram(individual)

# THEN, with model and data in hand, do the other stuff...

# estimate average speed ####
speed <- speed(individual, fits, fast=TRUE, robust=TRUE) # might take a while
# still breaking. wtf.


# estimate instantaneous speeds ####
speeds <- speeds(individual, fits)
# its in meters/second


# Estimating daily movement distance over a study period ####
# First identify how many days the individual was tracked for
individual$day <- cut(individual$timestamp, breaks = "day")
days <- unique(individual$day)

# An empty list to fill with the results
results_daily_distance <- list()

# Loop over the number of days
for (i in 1:length(days)) {
  message ("Estimating distance travelled on day", i,": ", days[i])
  # Select data for the day in question
  data_subset <- individual[which(individual$day == days[i]), ]
  # Calculate the duration of the sampling period (in seconds )
  samp_time <- diff (c(data_subset$t[1],
                         data_subset$t[nrow(data_subset)]))

  # Guesstimate the model for initial parameter values
  guess <- ctmm.guess(individual,
                          variogram = variogram(individual) ,
                          interactive = FALSE)
  # Turn error on
  guess$error <- TRUE
  # fits the movement model to the day’s data
  fit <- ctmm.fit(data_subset,
                     CTMM = guess)
  # Calculate speed in m/s
  ctmm_speed <- speed(object = data_subset,
                         CTMM = fit,
                         units = FALSE,
                          robust=TRUE)
  # Multiply speed (in m/s) by the sample time (in s)
  # to get the estimated distance travelled (in m)
  ctmm_dist <- ctmm_speed * samp_time
  # Re-name the variable
  rownames(ctmm_dist) <- "distance(meters)"

  # And store the results in the list
  x <- c(i, #The day
         ctmm_dist[2], #The ML distance estimate
         ctmm_dist[1], #Min CI
         ctmm_dist[3]) #Max CI
  names(x) <- c("date", "dist.ML", "dist.Min", "dist.Max")
  results[[i]] <- x
}

# even when speed works (on 100 or 1000 rows), this doesn't:
# Error in ctmm_speed * samp_time : non-numeric argument to binary operator
# In addition: Warning messages:
#   1: In cov.loglike(DIFF$hessian, grad) :
#   MLE is near a boundary or optimizer failed.
# 2: In speed.ctmm(CTMM, data = object, t = t, level = level, robust = robust,  :
#                    Movement model is fractal.

# Finally bind results together as a data frame
results_daily_distance <- as.data.frame(do.call(rbind, results_daily_distance))
results_daily_distance$date <- as.Date(days)
head(results_daily_distance)



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

# Get the areas that fall on either side of the road
roads_within_range <- st_intersection(home_range, roads)


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
  cat("Starting point ",i,"\n") # tells you how far along you are
  
  # Find the nearest road
  nearest_road_idx <- st_nearest_feature(point_i, roads)
  
  # Get the nearest road geometry
  nearest_road <- roads[nearest_road_idx, ]
  
  # Calculate the distance
  distance <- st_distance(point_i, nearest_road)
  
  # add distance to empty results dataframe
  results_road_dist[i, 1] <- distance
  
}

results_dist_and_speed <- results_road_dist

# plot to be sure this worked right
ggplot() +
  # Plot the points from individual_sf
  geom_sf(data = individual_sf, color = "blue", size = 3) +
  # Overlay the roads
  geom_sf(data = roads, color = "red", alpha = 0.5) +
  # Set the limits to match the extent of individual_sf
  coord_sf(crs = st_crs(individual_sf), xlim = st_bbox(individual_sf)[c("xmin", "xmax")],
      ylim = st_bbox(individual_sf)[c("ymin", "ymax")]) +
  # Add titles and labels if needed
  ggtitle("Bobcat's Range with Roads Overlay") +
  theme_minimal()

# make dataframe with one row per point and a column with instantaneous speed and a column with distance
results_dist_and_speed$Speed <- speeds$est

# run analysis
plot(results_dist_and_speed$Speed ~ results_dist_and_speed$Distance)
# not really a relationship here

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

# pull daily movement speed (from calculations above)
# !!! using estimates from model rn because the speed function won't work

# analyze daily movement speed as a function of road density
# !!! this has to happen after ALL bobcats are analyzed!


# estimate road crossings (Noonan 2021)  ####
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
Num_Crossings <- length(crossings_new$geometry)
Num_Crossings

# Plot of the most likely path and the road
plot(path_2, col = "NA")
lines(roads, col = "#FF0000")
lines(path_2, col = "#046C9A")
coords <- st_coordinates(crossings_new)  # Extract coordinates
points(coords[, 1], coords[, 2], col = "black", pch = 16)  # Plot the points
title(main = paste(individual@info$identity, " - Crossings: ", Num_Crossings),
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
nReps <- 1000
# Run the simulations for this animal
tic()
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
toc()

# 10 sims takes 96 seconds
# 100 sims takes 24 minutes
# 1000 sims should take 3ish hours

# Clean up results
sim_results <- data.frame("ID" = unlist(lapply(x, function (x) x[1])),
                      "Road_Crossings" = unlist(lapply(x, function (x) x[2])))
mean(sim_results$Road_Crossings)

# t test? make a list of simulated road crossings and then actual road crossings for all
# individuals and then t test the means?


# did they use crossing structures (Noonan 2021) ####
bridges <- st_read("data/Bridges_As_Lines")
# Measuring distance of crossings from passages
# convert to get distance in m
crossings_utc <- st_transform(crossings_new, crs = 32633)
bridges_utc <- st_transform(bridges, crs = 32633)
# Empty vector to store results
pass_dists <- vector("numeric", length = length(crossings_utc))
for(i in 1:length(crossings_utc)){
  crossing_point <- crossings_utc[i, ]
 # Find which point in the path is closest to the crossing location
  dists <- st_distance(crossing_point, bridges_utc)
 pass_dists[i] <- min(dists)
}

head(pass_dists)

t_test <- t.test(pass_dists) # t testing whether true mean can be = to 0
t_test

# Which crossings were within 20m (replace with my own median gps error!!!) of a road passage
which(pass_dists <= 20)
# ok but I can SEE that she uses a crossing structure on st marys road

# Plot of the most likely path, the road, and the passage structures
plot(path_2, col = "NA")
lines(roads, col = "darkgray")
lines(path_2, col = "#046C9A")
lines(bridges, pch = 16, lwd = 3, col = "red")
title(main = paste(individual@info$identity, " - Crossings: ", Num_Crossings),
      family = "serif",
      font.main = 1,
      cex.main = 0.85)

plot(individual, col = "NA")
lines(roads, col = "darkgray")
plot(individual, col = "#046C9A")
lines(bridges, pch = 16, lwd = 3, col = "red")



# outputs ####
fits
# model
        # one model per bobcat (best fit movement model)
speed 
# value
        # one value per bobcat (average speed of bobcat)
# speeds # unnecessary since we merge this into "results_dist_and_speed"
# # dataframe
#         # one row per datapoint with:
#             # instantaneous speed at that point
results_daily_distance # (check this once i get speed working)
# dataframe
        # One row per date with:
           # average daily movement distance
           # lower CI
           # upper CI
home_range
# sf object (polygon)
        # one object per bobcat with:
            # 95% home range estimate
# results_road_dist # unnecessary since we merge this into "results_dist_and_speed"
# # dataframe
#         # one row per datapoint with:
#             # distance from each point to the nearest road
#             # timestamp of each point
results_dist_and_speed 
# dataframe
        # combined results_road_dist and speeds
        # one row per datapoint with:
            # distance from each point to nearest road
            # instantaneous speed at that point
            # timestamp of each point
area_sq_km 
# value
        # one value per bobcat (area of home range in km)
total_road_length_km 
# value
        # one value per bobcat (total length of all road in range in km)
road_density 
# value
        # calculate from area and road 
        # one value per bobcat (road density of home range)
crossings_new 
# spatial object
        # one point per crossing event (locations of all road crossings)
Num_Crossings 
# value
        # one value per bobcat (number of times bobcat's path crossed road)
cross_times 
# vector
        # one list item per crossing event
sim_results
# dataframe
        # one row per simulation with:
          # number of road crossings in that simulation
          # timestamp? (check this!!)
pass_dists 
# vector
        # one list item per crossing event (distance from crossing event to nearest crossing structure)

# !!!! We still need:
# traffic volume?

