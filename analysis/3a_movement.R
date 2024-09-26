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
speed <- speed(individual, fits, fast=TRUE, robust=TRUE) 

t(paste0("Speed function finished at ", Sys.time()))

# estimate instantaneous speeds ####
speeds <- speeds(individual, fits)
# its in meters/second

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

# Vector of results to return
x <- data.frame(name, speed, area_sq_km, total_road_length_km, road_density)
# Store results in data.frame
write.table(x, 'results/movement_info.csv', append=TRUE, row.names=FALSE, col.names=FALSE, sep=',')

save(results_dist_and_speed,
     file = paste0("results/Dist_from_Road_and_Speed/", name, "_dist_and_speed.csv"))


