# divide roads into major vs minor vs all
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
# first roadmaps
all_streets <- st_read("../data_too_big/Dissolved_Street_Network/") # this is all public streets
fha_categories <- st_read("../data_too_big/Federal_Highway_Administration_Functional_Categories/") # this is category information for most roads but not small ones
speed_limits <- st_read("../data_too_big/Speed_Limits/") # this has all the same roads as fha but with speed limit information
lanes <- st_read("../data_too_big/Number_of_Lanes/") # this has the fewest number of roads, and only the more major ones

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

# plot(bobcats_all_sp, col = as.factor(bobcats_all_sp@data$id), pch = 16) # map to be sure it worked
# plot(bobcats_all_mcp, col = alpha(1:5, 0.5), add = TRUE)

str(bobcats_all_mcp)

# convert to meters and add buffer
# make spatial
bobcats_sf <- st_as_sf(bobcats_all_mcp)

# Transform to a projected CRS (e.g., UTM zone 10N) to use meters
bobcats_sf <- st_transform(bobcats_sf, crs = 32612) # UTM zone 10N

# Add a 1000-meter buffer
buffered_sf <- st_buffer(bobcats_sf, dist = 10000)

# Plot the original polygon and the buffered polygon
plot(st_geometry(bobcats_sf), col = "lightblue", main = "Polygon with 10000m Buffer")
plot(st_geometry(buffered_sf), add = TRUE, border = "red")

# figure out which roads are within polygon
study_area <- buffered_sf

# set crs of road map
all_streets <- st_transform(all_streets, crs(study_area))
# find the intersection between roadmaps and range polygon
all_streets <- st_intersection(all_streets, study_area)
ggplot(data = all_streets) +
  geom_sf() +
  theme_classic()


# do same for other maps
fha_categories <- st_transform(fha_categories, crs(study_area))
fha_categories <- st_intersection(fha_categories, study_area)
ggplot(data = fha_categories) +
  geom_sf() +
  theme_classic()

speed_limits <- st_transform(speed_limits, crs(study_area))
speed_limits <- st_intersection(speed_limits, study_area)
ggplot(data = speed_limits) +
  geom_sf() +
  theme_classic()

lanes <- st_transform(lanes, crs(study_area))
lanes <- st_intersection(lanes, study_area)
ggplot(data = lanes) +
  geom_sf() +
  theme_classic()

table(fha_categories$FUNCCAT_DS)

# rur_inter <- subset(fha_categories, FUNCCAT_DS == "Rural Interstate")
# urb_int <- subset(fha_categories, FUNCCAT_DS == "Urban Interstate")
# urb_free <- subset(fha_categories, FUNCCAT_DS == "Urban Other Freeway")
# urb_princ_art <- subset(fha_categories, FUNCCAT_DS == "Urban Principal Arterial")
# urb_min_art <- subset(fha_categories, FUNCCAT_DS == "Urban Minor Arterial")
# rur_min_art <- subset(fha_categories, FUNCCAT_DS == "Rural Minor Arterial")
# rur_maj_coll <- subset(fha_categories, FUNCCAT_DS == "Rural Major Collector")
# rur_min_coll <- subset(fha_categories, FUNCCAT_DS == "Rural Minor Collector")
# urb_coll <- subset(fha_categories, FUNCCAT_DS == "Urban Collector")
# urb_min_coll <- subset(fha_categories, FUNCCAT_DS == "Urban Minor Collector")
# urb_loc <- subset(fha_categories, FUNCCAT_DS == "Urban Local")
# 
# high <- subset(fha_categories, FUNCCAT_DS == "Rural Interstate" | 
#                  FUNCCAT_DS == "Urban Interstate" | 
#                  FUNCCAT_DS == "Urban Other Freeway" |
#                  FUNCCAT_DS == "Urban Principal Arterial" |
#                  FUNCCAT_DS == "Urban Minor Arterial" |
#                  FUNCCAT_DS == "Rural Minor Arterial")
# medium <- subset(fha_categories, FUNCCAT_DS == "Rural Major Collector" | 
#                    FUNCCAT_DS == "Rural Minor Collector" | 
#                    FUNCCAT_DS == "Urban Collector" |
#                    FUNCCAT_DS == "Urban Minor Collector")
# low <- subset(fha_categories, FUNCCAT_DS == "Urban Local")
# 
# 
# 
# ggplot() +
#   geom_sf(data = low, fill = "lightblue", color = "lightblue") +
#   geom_sf(data = medium, fill = "yellow", color = "yellow") +
#   geom_sf(data = high, fill = "red", color = "red") +
#   theme_classic() +
#   labs(title = "Colored by Functional Category")


# now lets split up the all_streets data
front <- subset(all_streets, ROADCAT_DS == "Frontage road")
interstate <- subset(all_streets, ROADCAT_DS == "Interstate highway")
inter_ramp <- subset(all_streets, ROADCAT_DS == "Interstate ramp")
maj_road <- subset(all_streets, ROADCAT_DS == "Major local road")
maj_ramp <- subset(all_streets, ROADCAT_DS == "Major road ramp")
min_road <- subset(all_streets, ROADCAT_DS == "Minor local road")
state <- subset(all_streets, ROADCAT_DS == "State route")
state_ramp <- subset(all_streets, ROADCAT_DS == "State route ramp")

high <- subset(all_streets, ROADCAT_DS == "Interstate highway" | 
                 ROADCAT_DS == "Interstate ramp" | 
                 ROADCAT_DS == "State route" |
                 ROADCAT_DS == "State route ramp" |
                 ROADCAT_DS == "Frontage road" | 
                 ROADCAT_DS == "Major local road" | 
                 ROADCAT_DS == "Major road ramp")
low <- subset(all_streets, ROADCAT_DS == "Minor local road")

ggplot() +
  geom_sf(data = low, fill = "lightblue", color = "lightblue") +
  geom_sf(data = high, fill = "red", color = "red") +
  theme_classic() +
  labs(title = "Colored by Functional Category")

# now I want to save streets_inside as a spatial object, to pull into my ctmm analysis
st_write(high, "data/Major_Roads/major.shp")
st_write(low, "data/Minor_Roads/minor.shp")
st_write(all_streets, "data/All_Roads/roads.shp")

