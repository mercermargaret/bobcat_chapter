# map of individual path (plus roads and crossings and bridges) of SIMULATED path
# margaret mercer
# Dec 26, 2024

# load stuff ####

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
bridges <- st_read("data/Bridges_As_Lines")
bridges_points_df <- read.csv("data/Bridges - final.csv")

# format coordinates correctly
bridges_points_df <- separate(bridges_points_df, coordinates, into = c("lat", "long"), sep = ",")
bridges_points <- bridges_points_df %>% 
  st_as_sf(coords = c('long', 'lat')) %>%
  st_set_crs(4326)

# Reproject the roads and bridges to match the tracking data
major_roads <- st_transform(major_roads, crs("epsg:4326"))
minor_roads <- st_transform(minor_roads, crs("epsg:4326"))
all_roads <- st_transform(all_roads, crs("epsg:4326"))
bridges <- st_transform(bridges, crs("epsg:4326"))

# select Dave
i = 14

file_path <- file.path("results/Model_Fit_Results", files[i])

# Load the file
load(file_path)

# print name of individual it's on
print(file_path)

# wrangle stuff  ####
# Estimate the most likely path based on the fitted movement model
# simulate a path
sims <- simulate(fits, t = individual$t, complete = TRUE)

plot(sims)

path <- predict(sims, fits, dt = 60, complete = TRUE) # this takes like 10-15 minutes to run!

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


# turn everything into multipoints so we can convert to points
crossings_multi_maj <- st_cast((road_crossings_maj), to = "MULTIPOINT")
crossings_new_maj <- st_cast((crossings_multi_maj), to = "POINT")

crossings_multi_min <- st_cast((road_crossings_min), to = "MULTIPOINT")
crossings_new_min <- st_cast((crossings_multi_min), to = "POINT")

coords_maj <- st_coordinates(crossings_new_maj)  # Extract coordinates for major roads
coords_min <- st_coordinates(crossings_new_min)  # Extract coordinates for minor roads

sum(length(crossings_new_maj$OBJECTID), length(crossings_new_min$OBJECTID))

# Get the bounding box of path_2 (extent)

# coords <- list(c(-111.05434, 32.20235), c(-110.98385, 32.20235), c(-110.98385, 32.23172), c(-111.05434, 32.23172)) # set extent of the REAL path so you can compare them

coords <- list(c(-111.06303, 32.20235), c(-110.96250, 32.20235), c(-110.96250, 32.23172), c(-111.06303, 32.23172)) # extent of max value for real and simulated

# Convert list to a matrix of coordinates
coords_matrix <- matrix(unlist(coords), ncol = 2, byrow = TRUE)

# Add the first coordinate again at the end to close the polygon
coords_matrix <- rbind(coords_matrix, coords_matrix[1,])

# Create the polygon object using st_polygon
polygon <- st_sfc(st_polygon(list(coords_matrix)))

# Convert to a spatial object (sf)
polygon_sf <- st_sf(geometry = polygon)

st_crs(polygon_sf) <- crs(minor_roads)

plot(polygon_sf)

# Crop minor and major roads to the extent of path_2
minor_roads_cropped <- st_intersection(minor_roads, polygon_sf)
major_roads_cropped <- st_intersection(major_roads, polygon_sf)
bridges_points_cropped <- st_intersection(bridges_points, polygon_sf)

# # plot just roads ####
# geom_sf() +
#   geom_sf(data = minor_roads_cropped, color = "black", 
#           linewidth = 0.75, inherit.aes = FALSE) +
#   
#   geom_sf(data = major_roads_cropped, color = "black", 
#           linewidth = 2, inherit.aes = FALSE, show.legend = FALSE) + 
#   
#   # Customize plot appearance
#   theme_minimal() +
#   theme(
#     title = element_text("Simulated Path"), 
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank(),
#     legend.position = "none"
#   )


# plot path and roads ####

# Create the combined plot
ggplot() +
  # Plot path_2 first
  geom_sf(data = path_sf, color = "#7390EE", linewidth = 1.5, alpha = 0.6) +
  
  geom_sf(data = minor_roads_cropped, color = "black", 
          linewidth = 0.75, inherit.aes = FALSE) +
  
  geom_sf(data = major_roads_cropped, color = "black", 
          linewidth = 2, inherit.aes = FALSE, show.legend = FALSE) + 
  
  # Customize plot appearance
  theme_minimal() +
  theme(
    title = element_text("Simulated Path"), 
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )


# plot path, roads, and crossings ####

# Create the combined plot
ggplot() +
  # Plot path_2 first
  geom_sf(data = path_sf, color = "#7390EE", linewidth = 1.5, alpha = 0.6) +
  
  # Plot minor roads in dark grey
  geom_sf(data = minor_roads_cropped, color = "black", 
          linewidth = 0.75, inherit.aes = FALSE) +
  
  # Plot major roads in black
  geom_sf(data = major_roads_cropped, color = "black", 
          linewidth = 2, inherit.aes = FALSE, show.legend = FALSE) + 
  
  geom_sf(data = crossings_new_min, color = "blue",
          size = 4, inherit.aes = FALSE) +
  
  geom_sf(data = crossings_new_maj, color = "blue",
          size = 4, inherit.aes = FALSE) +
  
  # Customize plot appearance
  theme_minimal() +
  theme(
    title = element_text("Simulated Path"), 
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

# plot path, roads, crossings, and bridges ####

# Create the combined plot
ggplot() +
  # Plot path_2 first
  geom_sf(data = path_sf, color = "#7390EE", size = 1, alpha = 0.6) +
  
  # Plot minor roads in dark grey
  geom_sf(data = minor_roads_cropped, color = "black",
          linewidth = 0.75, inherit.aes = FALSE) +
  
  # Plot major roads in black
  geom_sf(data = major_roads_cropped, color = "black",
          linewidth = 2, inherit.aes = FALSE, show.legend = FALSE) +
  
  geom_sf(data = crossings_new_min, color = "blue",
          size = 2, inherit.aes = FALSE) +
  
  geom_sf(data = crossings_new_maj, color = "blue",
          size = 2, inherit.aes = FALSE) +
  
  geom_sf(data = bridges_points_cropped, color = "red", shape = 4, size = 8, stroke = 4, alpha = 1,
          linewidth = 10, inherit.aes = FALSE) +
  
  # Customize plot appearance
  theme_minimal() +
  theme(
    title = element_text("Simulated Path"), 
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )
