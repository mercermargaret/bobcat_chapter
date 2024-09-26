# movement as function of distance to roads
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
# load data
ind_file <- commandArgs(trailingOnly = TRUE)
print(ind_file)

load(ind_file)

t(paste0("Data loaded at ", Sys.time()))

# estimate average speed ####
speed <- speed(individual, fits, fast=TRUE, robust=TRUE) # might take a while
# still breaking. wtf.

t(paste0("Speed function finished at ", Sys.time()))

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

t(paste0("Daily speeds calculated at ", Sys.time()))

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
t(paste0("Roads loaded at ", Sys.time()))
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
t(paste0("Distance to nearest road calculated at ", Sys.time()))
results_dist_and_speed <- results_road_dist

# # plot to be sure this worked right
# ggplot() +
#   # Plot the points from individual_sf
#   geom_sf(data = individual_sf, color = "blue", size = 3) +
#   # Overlay the roads
#   geom_sf(data = roads, color = "red", alpha = 0.5) +
#   # Set the limits to match the extent of individual_sf
#   coord_sf(crs = st_crs(individual_sf), xlim = st_bbox(individual_sf)[c("xmin", "xmax")],
#            ylim = st_bbox(individual_sf)[c("ymin", "ymax")]) +
#   # Add titles and labels if needed
#   ggtitle("Bobcat's Range with Roads Overlay") +
#   theme_minimal()

# make dataframe with one row per point and a column with instantaneous speed and a column with distance
results_dist_and_speed$Speed <- speeds$est

# # run analysis
# plot(results_dist_and_speed$Speed ~ results_dist_and_speed$Distance)
# # not really a relationship here

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

# Vector of results to return
x <- data.frame(name, speed, area_sq_km, total_road_length_km, road_density)
# Store results in data.frame
write.table(x, 'results/movement_info.csv', append=TRUE, row.names=FALSE, col.names=FALSE, sep=',')

save(results_dist_and_speed,
     file = paste0("results/Dist_from_Road_and_Speed/", name, "_dist_and_speed.csv"))

save(results_daily_distance,
     file = paste0("results/Daily_Distance/", name, "_daily_distance.csv"))


