# ctmm analysis
# margaret mercer
# july 30, 2024
# I put this together by looking at the code from Noonan 2019 and Noonan 2021 simultaneously
    # and using what applied to me
    # see: https://static-content.springer.com/esm/art%3A10.1186%2Fs40462-019-0177-1/MediaObjects/40462_2019_177_MOESM2_ESM.pdf
    # and: https://zslpublications.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Facv.12728&file=acv12728-sup-0002-AppendixS2.pdf

# install.packages("proj4")
# install.packages("foreach")
# install.packages("doParallel")
# install.packages("sf")
# install.packages("raster")
# install.packages("adehabitatHR")
# install.packages("tidyverse")

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

# rerun 41 and 304!!

# clear workspace
rm(list=ls())

# loading in gps data as csv
data <- read.csv("data/bobcat_locs_all.csv")

# data prep from Noonan 2019 ####
margaret_gps <- data[data$individual.local.identifier == "BC #12 Margaret", ]
summary(margaret_gps)

# making margaret a telemetry object so ctmm recognizes it
margaret <- as.telemetry(margaret_gps)

uere(margaret) <- 7 # make sure this worked

# subset for preliminary analysis
margaret <- margaret[1:1000,]
summary(margaret)

# plotting!
# plot(margaret ,
      # error = 2 ,
      # level.UD = 0.50) # this doesn't work great because we don't know the error yet. thats ok.

# ID outliers
outliers <- outlie(margaret)
# plot(outliers)

# get rid of outliers in margaret
outlier_t <- outliers$t[outliers$speed >= .40] # this requires visual assessment!! how do this when running all of them at once?
margaret <- margaret[!margaret$t %in% outlier_t, ]

# replot data
# plot(margaret,
       # error = 2 ,
       # level.UD = 0.50)
outliers <- outlie(margaret)
# plot(outliers)

# make a variogram
vg <- variogram(margaret)

# Guesstimate the model to obtain initial parameter values
guess <- ctmm.guess(margaret,
                        variogram = vg,
                        interactive = FALSE)
guess$error <- TRUE

# only run the following code chunk if running on the hpc
fit <- ctmm.select(margaret, CTMM = guess, trace = 2) # let this run for a while! it takes like 24 hours!
# run this on the hpc!!
# this ran and spit output into margaret_ctmm.Rda
# save(fit, file = "margaret_ctmm.Rda")

# # # only run the following code chunk if running in R studio
# load('data/margaret_ctmm.Rda')

# return a summary of the fitted models
summary(fit) # this should list a model $name, $DOF, and $CI


# plot variogram and model
plot(vg, CTMM = fit)

# THEN, with model and data in had, do the other stuff...

# estimate average speed
speed <- speed(margaret, fit, fast=TRUE)
# Error in sqrt(diff(data$x)^2 + diff(data$y)^2)/DT/SPD: non-numeric argument to binary operator
# Error also shows up when I run it on the hpc
# speed <- speed(margaret, fit, cores = -1, fast = TRUE)
# just loads forever and doesn't make progress :')
# doesn't error when I subset data to 100



# estimate instantaneous speeds
speeds <- speeds(margaret, fit)
# its in meters/second

# Estimating daily movement distance over a study period
# First identify how many days the individual was tracked for
margaret$day <- cut(margaret$timestamp, breaks = "day")
days <- unique(margaret$day)

# An empty list to fill with the results
results <- list()

# Loop over the number of days
for (i in 1:length(days)) {
  message ("Estimating distance travelled on day", i,": ", days[i])
  # Select data for the day in question
  data_subset <- margaret[which(margaret$day == days[i]), ]
  # Calculate the duration of the sampling period (in seconds )
  samp_time <- diff (c(data_subset$t[1],
                         data_subset$t[nrow(data_subset)]))

  # Guesstimate the model for initial parameter values
  guess <- ctmm.guess(margaret,
                          variogram = variogram(margaret) ,
                          interactive = FALSE)
  # Turn error on
  guess$error <- TRUE
  # Fit the movement model to the day’s data
  fits <- ctmm.fit(data_subset,
                     CTMM = guess)
  # Calculate speed in m/s
  ctmm_speed <- speed(object = data_subset,
                         CTMM = fits,
                         units = FALSE)
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
results <- as.data.frame(do.call(rbind, results))
results$date <- as.Date(days)
head(results)


# assess whether there's a range shift (Noonan 2021) ####
# calculating home range
# calculate the AKDE based on the best fit model
margaret_akde <- akde(margaret, fit)
#Return the basic statistics on the HR area
summary(margaret_akde)

# Create a function that scales colours between red and blue
rbPal <- colorRampPalette(c('#FF0000','#046C9A'))
# Then create a variable that scales from red to blue between the two times
margaret$Col <- rbPal(nrow(margaret))[as.numeric(cut(margaret$t, breaks = nrow(margaret)))]
# Plot the AKDE range estimate, with the relocation data, coloured by time
plot(margaret,
     UD = margaret_akde,
     col.grid = NA,
     family = "serif",
     pch = 20,
     cex = 0.2,
     col.DF = "#669543",
     col = margaret$Col,
     labels = FALSE)

# visually assess whether there's a range shift going on over the course of the study period

# road permeability (Noonan 2021) *not sure if relevant to us* ####
# are they willing to establish home ranges on either side of the roads?

# Load the road data
roads <- st_read("data/Roadmap_Wrangled")

# Reproject the roads to match the tracking data
roads <- st_transform(roads, crs("epsg:4326"))

# create and reproject home range contour
# Extract the 95% home range contour
home_range_polygon <- SpatialPolygonsDataFrame.UD(margaret_akde)

# Convert SpatialPolygonsDataFrame to an sf object
home_range_sf <- st_as_sf(home_range_polygon)

# tranform to proper crs
home_range <- st_transform(home_range_sf, crs("epsg:4326"))

# Get the areas that fall on either side of the road
roads_within_range <- st_intersection(home_range, roads)
# blpi <- st_buffer(roads_within_range, dist = 0.000001)
# dpi <- st_difference(home_range, blpi)
# Side_1 <- SpatialPolygons(list(Polygons(list(dpi@polygons[[1]]@Polygons[[1]]), "1")))
# Side_2 <- SpatialPolygons(list(Polygons(list(dpi@polygons[[1]]@Polygons[[2]]), "2")))
# 
# # Plot the split HR and the road
# plot(home_range)
# plot(Side_1, col = "lightgreen", add = TRUE)
# plot(Side_2, col = "lightblue", add = TRUE)
# lines(roads, col = "red")
# 
# # Area on each side
# Side_1 <- raster::area(Side_1)
# Side_2 <- raster::area(Side_2)
# # Ratio of roads on each side
# ratio <- min(Side_1, Side_2)/max(Side_1, Side_2)
# round(ratio, 3)

# calculate distance of home range to nearest road (Noonan 2021) *will need to change* ####
# First I need to get instantaneous speed at each point as a function of the distance from that point to the road
# so we'll need to get out margaret (as spatial) and roads, get the distances, and then get instantaneous speed from each point too

# get distances:
# make margaret a spatial object
margaret_df <- as.data.frame(margaret)
# simplify dataframe
margaret_df <- margaret_df[ , 2:3]
margaret_sf <- st_as_sf(margaret_df,
                        coords = c("longitude", "latitude"),
                        crs = st_crs(crs("epsg:4326")))

# make empty dataframe to store results
results <- data.frame(Distance = rep(NA, length(margaret$timestamp)), stringsAsFactors = FALSE)
# and add "timestamp" column
results$Timestamp <- margaret$timestamp

# then get distance to nearest road (for loop to run through all points)
for(i in 1:length(margaret$timestamp)){
  
  # select i
  point_i <- margaret_sf[i, ]
  cat("Starting point ",i,"\n") # tells you how far along you are
  
  # Find the nearest road
  nearest_road_idx <- st_nearest_feature(point_i, roads)
  
  # Get the nearest road geometry
  nearest_road <- roads[nearest_road_idx, ]
  
  # Calculate the distance
  distance <- st_distance(point_i, nearest_road)
  
  # add distance to empty results dataframe
  results[i, 1] <- distance
  
}

copy_results <- results

# plot to be sure this worked right
ggplot() +
  # Plot the points from margaret_sf
  geom_sf(data = margaret_sf, color = "blue", size = 3) +
  # Overlay the roads
  geom_sf(data = roads, color = "red", alpha = 0.5) +
  # Set the limits to match the extent of margaret_sf
  coord_sf(crs = st_crs(margaret_sf), xlim = st_bbox(margaret_sf)[c("xmin", "xmax")],
      ylim = st_bbox(margaret_sf)[c("ymin", "ymax")]) +
  # Add titles and labels if needed
  ggtitle("Bobcat's Range with Roads Overlay") +
  theme_minimal()

# make dataframe with one row per point and a column with instantaneous speed and a column with distance
results$Speed <- speeds$est

# run analysis
plot(results$Speed ~ results$Distance)
# not really a relationship here

# now calculate road density and home range size
# grab home range size
summary <- summary(margaret_akde)
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
path <- predict(margaret, fit, dt = 60, complete = TRUE) # this takes like 10-15 minutes to run!

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

# note: simplifying the original roadmap DOESN"T work because it messes up the lines. 
# Trimming is also not good for this purpose because it doesn't quite include ALL the roads in the range, just most of them!

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
title(main = paste(margaret@info$identity, " - Crossings: ", Num_Crossings),
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
  sims <- simulate(fit, t = margaret$t, complete = TRUE)
  
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
  #Find times it crossed the road
  sim_road_cross_times <- vector()
  for(i in 1:nrow(sim_road_crossings_sp@coords)){
    # Find which point in the path is closest to the crossing location
    dists <- geosphere::distHaversine(sim_road_crossings_sp@coords[i,], path_sim_latlong@coords)
    sim_road_cross_times [i] <- path_sim@data[which(dists == min(dists)),"timestamp"]
  }

  
  # list of results to return
  list(fit@info$identity,
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
title(main = paste(margaret@info$identity, " - Crossings: ", Num_Crossings),
      family = "serif",
      font.main = 1,
      cex.main = 0.85)

plot(margaret, col = "NA")
lines(roads, col = "darkgray")
plot(margaret, col = "#046C9A")
lines(bridges, pch = 16, lwd = 3, col = "red")
