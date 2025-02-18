# different range road density examples
# margaret mercer
# december 26, 2024

# we want the home ranges of Carrie (7) and Jack (18) to show the contrast. just the home range polygon? and road map underlay

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
major_roads <- st_read("data/Major_Roads")
minor_roads <- st_read("data/Minor_Roads")


files <- list.files("results/Model_Fit_Results")
i = 18 # grab individual

file_path <- file.path("results/Model_Fit_Results", files[i])

# Load the file
load(file_path)

shapefile_polygons <- as.sf(hr, level.UD=0.95, level=0.95)

middle_polygon <- shapefile_polygons[2,]  #selects the second row which is the 95% est middle polygon

crs <- crs(minor_roads)
final_polygon <- st_transform(middle_polygon, crs = crs) # match crs to base data so everything is aligned 

# Get the bounding box of path_2 (extent)
bbox <- (st_bbox(final_polygon))

coords <- list(c(bbox$xmin, bbox$ymin), c(bbox$xmin, bbox$ymax), c(bbox$xmax, bbox$ymax), c(bbox$xmax, bbox$ymin))

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

ggplot() +
  geom_sf(data = minor_roads_cropped, color = "black", 
          linewidth = 0.75, inherit.aes = FALSE) +
  
  geom_sf(data = major_roads_cropped, color = "black", 
          linewidth = 2, inherit.aes = FALSE, show.legend = FALSE) + 
  
  geom_sf(data = final_polygon, color = NA, fill = "blue", alpha = 0.5) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

