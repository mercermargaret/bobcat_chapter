# import streets data and experiment 
# margaret mercer
# august 15, 2024

# import packages
library(tidyverse)
library(sp)
library(sf)
library(adehabitatHR)
library(raster)

# clear workspace
rm(list=ls())

# import data
# first roadmap
streets <- st_read("../data_too_big/Raw_Roads_AZ/Street_Roads/TIGER_Streets")
# then bobcats
bobcats <- read.csv("data/bobcat_locs_all.csv")

# this is a lot
# lets trim down to only within study area

bobcats_new <- bobcats[, c("individual.local.identifier", "location.long", "location.lat")] 

# replace individual ids with "id" because right now we just care about the entire study area
bobcats_all <- bobcats_new[,2:3]
bobcats_all$id <- "id"
bobcats_all_sp <- rename(bobcats_all,
                         "x" = "location.long",
                         "y" = "location.lat")

coordinates(bobcats_all_sp) <- c("x", "y")

str(bobcats_all_sp) # check structure to be sure it worked

proj4string(bobcats_all_sp) <- CRS("+proj=longlat +datum=WGS84") # Use longlat projection for latitude and longitude

bobcats_all_mcp <- mcp(bobcats_all_sp[, "id"], percent = 100) # calculate mcp

plot(bobcats_all_sp, col = as.factor(bobcats_all_sp@data$id), pch = 16) # map to be sure it worked
plot(bobcats_all_mcp, col = alpha(1:5, 0.5), add = TRUE)

str(bobcats_all_mcp)

# convert to meters and add buffer
# make spatial
bobcats_sf <- st_as_sf(bobcats_all_mcp)

# Transform to a projected CRS (e.g., UTM zone 10N) to use meters
bobcats_sf <- st_transform(bobcats_sf, crs = 32612) # UTM zone 10N

# Add a 1000-meter buffer
buffered_sf <- st_buffer(bobcats_sf, dist = 10000)

# Plot the original polygon and the buffered polygon
plot(st_geometry(bobcats_sf), col = "lightblue", main = "Polygon with 1000m Buffer")
plot(st_geometry(buffered_sf), add = TRUE, border = "red")

# figure out which roads are within polygon
study_area <- buffered_sf

# set crs of road map
streets <- st_transform(streets, crs(study_area))

# find the intersection between roadmaps and range polygon
streets_inside <- st_intersection(streets, study_area)

ggplot(data = streets_inside) +
  geom_sf() +
  theme_classic()

# YESSS IT WORKED!!!

# now I want to save streets_inside as a spatial object, to pull into my ctmm analysis
st_write(streets_inside, "data/Roadmap_Wrangled/roadmap.shp")
