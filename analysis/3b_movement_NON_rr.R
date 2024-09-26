# movement as function of distance to roads for non range resident bobcats
# margaret mercer
# sept 25, 2024

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

# estimate instantaneous speeds ####
speeds <- speeds(individual, fits)
# its in meters/second

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

# Vector of results to return
x <- data.frame(name, speed)
# Store results in data.frame
write.table(x, 'results/movement_info_non_rr.csv', append=TRUE, row.names=FALSE, col.names=FALSE, sep=',')

write.csv(results_dist_and_speed,
     file = paste0("results/Dist_from_Road_and_Speed/", name, "_dist_and_speed.csv"))



