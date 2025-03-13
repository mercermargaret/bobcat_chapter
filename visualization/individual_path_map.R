# map of individual path (plus roads and crossings and bridges)
# margaret mercer
# Oct 29, 2024

# load stuff ####

# load packages
library(ctmm)
library(proj4)
library(foreach)
library(doParallel)
library(sf)
library(raster)
# library(adehabitatHR)
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

# select individual
i = 26

file_path <- file.path("results/Model_Fit_Results", files[i])

# Load the file
load(file_path)

# print name of individual it's on
print(file_path)

# trim to make path show up better (optional)
individual <- individual[1:300,]

# wrangle stuff  ####
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

coords_maj <- st_coordinates(crossings_new_maj)  # Extract coordinates for major roads
coords_min <- st_coordinates(crossings_new_min)  # Extract coordinates for minor roads

# Get the bounding box of path_2 (extent)
bbox <- st_bbox(path_2)

# Extract coordinates from bbox
coords <- list(c(bbox["xmin"], bbox["ymin"]), 
               c(bbox["xmax"], bbox["ymin"]), 
               c(bbox["xmax"], bbox["ymax"]), 
               c(bbox["xmin"], bbox["ymax"]))

# Print the coordinates to verify
print(coords)


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
bridges_cropped <- st_intersection(bridges, polygon_sf)
bridges_points_cropped <- st_intersection(bridges_points, polygon_sf)

# plot just roads ####
ggplot() +
  geom_sf(data = minor_roads_cropped, color = "black", 
          linewidth = 0.75, inherit.aes = FALSE) +
  
  geom_sf(data = major_roads_cropped, color = "black", 
          linewidth = 2, inherit.aes = FALSE, show.legend = FALSE) + 
  
  # Customize plot appearance
  theme_minimal() +
  theme(
    title = element_text("Real Path"), 
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

# plot path and roads ####

# Create the combined plot
ggplot() +
  # Plot path_2 first with legend
  geom_sf(data = path_sf, aes(color = "Bobcat Path"), linewidth = 1, alpha = 1, show.legend = TRUE) +
  
  # Plot minor roads without legend
  geom_sf(data = minor_roads_cropped, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  
  # Plot major roads with legend
  geom_sf(data = major_roads_cropped, aes(color = "Roads"), linewidth = 2, inherit.aes = FALSE, show.legend = TRUE) +
  scale_color_manual(values = c("Bobcat Path" = "aquamarine3", "Roads" = "black")) +  # Set color for Path and Major Roads
  # Customize plot appearance
  labs(title = "Bobcat #56 Movement Path") + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5, vjust = 0.5),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size = 20)
  ) +
  guides(color = guide_legend(title = NULL))




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
    title = element_text("Real Path"), 
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

# plot path, roads, crossings, and bridges ####

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
  
  geom_sf(data = bridges_points_cropped, color = "red", shape = 4, size = 8, stroke = 4, alpha = 1,
          linewidth = 10, inherit.aes = FALSE) +
  
  # Customize plot appearance
  theme_minimal() +
    theme(
      title = element_text("Real Path"), 
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )
# 
# # example "crossing and structure" ####
# # Load necessary library
# library(ggplot2)
# 
# # Create some data for the line and points
# line_data <- data.frame(x = c(0, 10), y = c(0, 10))  # coordinates for the line
# point_data <- data.frame(x = 5, y = 5)  # coordinates for the red point
# x_shape_data <- data.frame(x = 6, y = 6)  # coordinates for the 'X' shape
# 
# # Create the plot
# ggplot() +
#   geom_line(data = line_data, aes(x = x, y = y), color = "black", size = 1) +
#   geom_point(data = point_data, aes(x = x, y = y), color = "blue", size = 3) +
#   geom_point(data = x_shape_data, aes(x = x, y = y), color = "red", shape = 4, size = 4, stroke = 3, alpha = 1, inherit.aes = FALSE) +
#   # Customize the theme
#   theme_minimal() +
#   theme(
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank(),
#     legend.position = "none"
#   )
# 


# for loop to make plot for all individuals: ####
# for (i in 1:length(files)) {
#   
#   # Construct the full file path
#   file_path <- file.path("results/Model_Fit_Results", files[i])
#   
#   # Load the file
#   load(file_path)
#   
#   # print name of individual it's on
#   print(file_path)
#   
#   # estimate number of road crossings (Noonan 2021) 
#   # Estimate the most likely path based on the fitted movement model
#   path <- predict(individual, fits, dt = 60, complete = TRUE) # this takes like 10-15 minutes to run!
#   t(paste0("Predicted path generated at ", Sys.time()))
#   
#   # Convert to the right format for counting road crossings
#   path <- SpatialPointsDataFrame.telemetry(path)
#   path <- spTransform(path, crs("epsg:4326"))
#   path_2 <- lapply(split(path, path$identity),
#                    function(x) Lines(list(Line(coordinates(x))), path$identity[1L]))
#   crs_maj_roads <- st_crs(major_roads)$proj4string
#   path_2 <- SpatialLines(path_2, proj4string = CRS(crs_maj_roads))
#   
#   # How many times does it cross the road
#   path_sf <- st_as_sf(path_2)
#   
#   road_crossings_maj <- st_intersection(path_sf, major_roads)
#   road_crossings_min <- st_intersection(path_sf, minor_roads)
#   t(paste0("Road crossings generated at ", Sys.time()))
# 
#   # turn everything into multipoints so we can convert to points
#   crossings_multi_maj <- st_cast((road_crossings_maj), to = "MULTIPOINT")
#   crossings_new_maj <- st_cast((crossings_multi_maj), to = "POINT")
# 
#   crossings_multi_min <- st_cast((road_crossings_min), to = "MULTIPOINT")
#   crossings_new_min <- st_cast((crossings_multi_min), to = "POINT")
# 
#   # load bridges data
#   bridges <- st_read("data/Bridges_As_Lines")
#   t(paste0("Bridge data loaded at ", Sys.time()))
#   # convert to get distance in m
#   bridges_utc <- st_transform(bridges, crs = 32633)
# 
#   # plot map of path and roads
#   pdf(paste0("results/Individual_Path_Maps/", individual@info$identity, ".pdf"))
#   
#   # save 
#   plot(path_2, col = "NA")
#   lines(path_2, col = rgb(0, 0.4, 0.6, alpha = 0.3))
#   lines(all_roads, col = "#FF0000")
#   # coords_maj <- st_coordinates(crossings_new_maj)  # Extract coordinates for major roads
#   # points(coords_maj[, 1], coords_maj[, 2], col = "black", pch = 16)  # Plot the major roads
#   # coords_min <- st_coordinates(crossings_new_min)  # Extract coordinates for minor roads
#   # points(coords_maj[, 1], coords_maj[, 2], col = "black", pch = 16)  # Plot the minor roads
#   title(main = paste(individual@info$identity, "Movement Path"),
#         family = "serif",
#         font.main = 1,
#         cex.main = 0.85)
#   
#   # dev.off()
# 
#   
# }
