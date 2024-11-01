# range map
# margaret mercer
# Oct 29, 2024

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

files <- list.files("results/Model_Fit_Results")

major_roads <- st_read("data/Major_Roads")
minor_roads <- st_read("data/Minor_Roads")
all_roads <- st_read("data/All_Roads")
t(paste0("Roads loaded at ", Sys.time()))

# Reproject the roads to match the tracking data
major_roads <- st_transform(major_roads, crs("epsg:4326"))
minor_roads <- st_transform(minor_roads, crs("epsg:4326"))
all_roads <- st_transform(all_roads, crs("epsg:4326"))

for (i in 1:length(files)) {
  
  # Construct the full file path
  file_path <- file.path("results/Model_Fit_Results", files[i])
  
  # Load the file
  load(file_path)
  
  # print name of individual it's on
  print(file_path)
  
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
  
  # road_crossings_maj <- st_intersection(path_sf, major_roads)
  # road_crossings_min <- st_intersection(path_sf, minor_roads)
  # t(paste0("Road crossings generated at ", Sys.time()))
  # 
  # # turn everything into multipoints so we can convert to points
  # crossings_multi_maj <- st_cast((road_crossings_maj), to = "MULTIPOINT")
  # crossings_new_maj <- st_cast((crossings_multi_maj), to = "POINT")
  # 
  # crossings_multi_min <- st_cast((road_crossings_min), to = "MULTIPOINT")
  # crossings_new_min <- st_cast((crossings_multi_min), to = "POINT")
  # 
  # # load bridges data
  # bridges <- st_read("data/Bridges_As_Lines")
  # t(paste0("Bridge data loaded at ", Sys.time()))
  # # convert to get distance in m
  # bridges_utc <- st_transform(bridges, crs = 32633)
  # 
  # plot map of path and roads ####
  pdf(paste0("results/Individual_Path_Maps/", individual@info$identity, ".pdf"))
  
  # save ####
  plot(path_2, col = "NA")
  lines(path_2, col = rgb(0, 0.4, 0.6, alpha = 0.3))
  lines(all_roads, col = "#FF0000")
  # coords_maj <- st_coordinates(crossings_new_maj)  # Extract coordinates for major roads
  # points(coords_maj[, 1], coords_maj[, 2], col = "black", pch = 16)  # Plot the major roads
  # coords_min <- st_coordinates(crossings_new_min)  # Extract coordinates for minor roads
  # points(coords_maj[, 1], coords_maj[, 2], col = "black", pch = 16)  # Plot the minor roads
  title(main = paste(individual@info$identity, "Movement Path"),
        family = "serif",
        font.main = 1,
        cex.main = 0.85)
  
  dev.off()

  
}

# map crossings also (for nala and charlie) (10 and 26)
# Construct the full file path
file_path <- file.path("results/Model_Fit_Results", files[i])

# Load the file
load(file_path)

# print name of individual it's on
print(file_path)

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

# load bridges data
bridges <- st_read("data/Bridges_As_Lines")
t(paste0("Bridge data loaded at ", Sys.time()))
# convert to get distance in m
bridges_utc <- st_transform(bridges, crs = 32633)

# plot map of path and roads ####
pdf(paste0("results/Individual_Path_Maps/", individual@info$identity, "crossings.pdf"))

# save ####
plot(path_2, col = "NA")
lines(path_2, col = rgb(0, 0.4, 0.6, alpha = 0.3))
lines(all_roads, col = "#FF0000")
coords_maj <- st_coordinates(crossings_new_maj)  # Extract coordinates for major roads
points(coords_maj[, 1], coords_maj[, 2], col = "black", pch = 16)  # Plot the major roads
coords_min <- st_coordinates(crossings_new_min)  # Extract coordinates for minor roads
points(coords_maj[, 1], coords_maj[, 2], col = "black", pch = 16)  # Plot the minor roads

title(main = paste(individual@info$identity, "Movement Path with Crossings"),
      family = "serif",
      font.main = 1,
      cex.main = 0.85)

dev.off()
