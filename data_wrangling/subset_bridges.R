# subsetting arizona bridges
# margaret mercer
# july 31, 2024

# load packages
library(tidyverse)
library(sp)
library(sf)
library(adehabitatHR)


# load data
bridges <- read.csv("data/AZ_bridges.csv")
bobcats <- read.csv("data/bobcat_locs_all.csv")

# get min and max lat and long for bobcats
min_lat <- min(bobcats$location.lat)
max_lat <- max(bobcats$location.lat)
min_long <- min(bobcats$location.long)
max_long <- max(bobcats$location.long)
bridges <- rename(bridges, long = x)
bridges <- rename(bridges, lat = y)

trimmed <- bridges[which(bridges$long < max_long),]
trimmed <- trimmed[which(trimmed$long > min_long),]
trimmed <- trimmed[which(trimmed$lat < max_lat),]
trimmed <- trimmed[which(trimmed$lat > min_lat),]

# now how do we figure out which bridges are crossing structures?
# first let's map all the bridges
trimmed$coordinates <- paste(trimmed$lat, trimmed$long, sep = ", ")

plot(trimmed$long, trimmed$lat)

coordinates <- trimmed[,c("long","lat")]

bridges_as_sf <- st_as_sf(coordinates, coords = c("long", "lat"), crs = 4326)

plot(bridges_as_sf)

# some notes: there are locations that are named the same thing ("santa cruz river br" 
# for example) that are DIFFERENT bridges, at DIFFERENT locations!

length(unique(trimmed$coordinates))





# that's great. now let's subset even more, because a lot of the bridges aren't even in the bobcat area

# Calculate 100% MCP to subset only bridges within the bobcats range! Then buffer to 50 or 100m
# see here: https://jamesepaterson.github.io/jamespatersonblog/03_trackingworkshop_homeranges
# 
bobcats_new <- bobcats[, c("individual.local.identifier", "location.long", "location.lat")] 
# bobcats_sp <- rename(bobcats_new,
#                      "x" = "location.long",
#                      "y" = "location.lat",
#                      "id" = "individual.local.identifier")
#
# # Create a SpatialPointsDataFrame by defining the coordinates
# coordinates(bobcats_sp) <- c("x", "y")
# 
# str(bobcats_sp)
# 
# # pull out individuals with fewer than 5 points
# counts <- table(bobcats_sp$id)
# print(counts)
# valid_ids <- names(counts[counts >= 5])
# bobcats_sp <- bobcats_sp[bobcats_sp$id %in% valid_ids, ]
# 
# proj4string(bobcats_sp) <- CRS("+proj=longlat +datum=WGS84") # Use longlat projection for latitude and longitude
# 
# plot(bobcats_sp, pch = 16)
# 
# bobcats_mcp <- mcp(bobcats_sp[, "id"], percent = 100) # calculate mcp # calculate mcp
# 
# bobcats_mcp
# 
# plot(bobcats_sp, col = as.factor(bobcats_sp@data$id), pch = 16)
# plot(bobcats_mcp, col = alpha(1:5, 0.5), add = TRUE)

# cool, so this works.

# replace individual ids with "id" because right now we just care about the entire study area
bobcats_all <- bobcats_new[,2:3]
bobcats_all$id <- "id"
bobcats_all_sp <- rename(bobcats_all,
                     "x" = "location.long",
                     "y" = "location.lat")

coordinates(bobcats_all_sp) <- c("x", "y")

str(bobcats_all_sp) # check strucutre to be sure it worked

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

# Add a 100-meter buffer
buffered_sf <- st_buffer(bobcats_sf, dist = 100)

# Plot the original polygon and the buffered polygon
plot(st_geometry(bobcats_sf), col = "lightblue", main = "Polygon with 100m Buffer")
plot(st_geometry(buffered_sf), add = TRUE, border = "red")

# do point in polygon test to see which bridges are within the polygon
study_area <- buffered_sf

# set crs of bridges_as_sf
bridges_as_sf <- st_transform(bridges_as_sf, crs = 32612)

# Perform the point-in-polygon test
inside <- st_within(bridges_as_sf, study_area, sparse = FALSE)

# Extract rows from df that are inside the polygon
bridges_in_range <- trimmed[which(inside[,1]),]

coordinates <- bridges_in_range[,c("long","lat")]

bridges_inside_as_sf <- st_as_sf(coordinates, coords = c("long", "lat"), crs = 4326)

plot(bridges_inside_as_sf)

# wrangle data
library(dplyr)

# Select specified columns from the dataframe
bridges_in_range <- bridges_in_range %>%
  dplyr::select(OBJECTID, Route.ID, Structure.Name, Functional.Class, long, lat, coordinates)

write.csv(bridges_in_range, "data/subset_bridges")

# visually assess all bridges in google maps, mark ends of bridges, delete duplicates and errors, and 
# mark new bridges found in the process

bridges <- read.csv("data/Bridges - final.csv")

