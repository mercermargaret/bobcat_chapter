# map crossing structures
# margaret mercer
# jan 22, 2025

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
library(ggplot2)

# clear workspace
rm(list=ls())

# crop and map major vs minor roads ####

# load full roads data
all_streets <- st_read("../data_too_big/Dissolved_Street_Network/") # this is all public streets
all_streets <- st_transform(all_streets, crs = 4326)

# load bobcat points and figure out what extent to crop to

# then bobcats
bobcats <- read.csv("data/bobcat_locs_all.csv")

# get extent
min_lat <- min(bobcats$location.lat)
max_lat <- max(bobcats$location.lat)
min_lon <- min(bobcats$location.long - 0.05) # add a bit for a more aesthetically pleasing map
max_lon <- max(bobcats$location.long + 0.05) # add a bit for a more aesthetically pleasing map

# Define the bounding box as a matrix of xmin, xmax, ymin, ymax
bbox <- c(xmin = min_lon, xmax = max_lon, ymin = min_lat, ymax = max_lat)

# Crop the sf object using the bounding box
all_streets <- st_crop(all_streets, bbox)

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

# read in and plot bridges ####
# I want to load both so I can choose whether I like bridges as lines or points better
# bridges_lines <- st_read("data/Bridges_As_Lines/bridges_as_lines.shp") # I like points better
bridges_points_df <- read.csv("data/Bridges - final.csv")

# format coordinates correctly
bridges_points_df <- separate(bridges_points_df, coordinates, into = c("lat", "long"), sep = ",")
bridges_points <- bridges_points_df %>% 
  st_as_sf(coords = c('long', 'lat')) %>%
  st_set_crs(4326)

# plot(st_geometry(bridges_lines)) # plot bridges as lines
plot(bridges_points$geometry) # plot bridges as points

# get rid of point furthest to north
# make polygon out of extent of roadmap
# read in bobcats for reference
bobcats <- read.csv("data/bobcat_locs_all.csv")

# get extent
min_lat <- min(bobcats$location.lat)
max_lat <- max(bobcats$location.lat)
min_lon <- min(bobcats$location.long)
max_lon <- max(bobcats$location.long)

# Define the bounding box as a matrix of xmin, xmax, ymin, ymax
bbox <- c(xmin = min_lon, xmax = max_lon, ymin = min_lat, ymax = max_lat)

# Crop the sf object using the bounding box
bridges_points <- st_crop(bridges_points, bbox)

# plot roads and bridges together ####
ggplot() +
  geom_sf(data = low, aes(color = "Minor Roads"), linewidth = 0.75, 
          show.legend = "line", inherit.aes = F) +  # Minor roads in dark grey
  geom_sf(data = high, aes(color = "Major Roads"), linewidth = 2, 
          show.legend = "line", inherit.aes = F) +     # Major roads in black
  geom_sf(data = bridges_points, color = "darkorange", shape = 4, size = 4, stroke = 3, alpha = 0.75) +
  scale_color_manual(values = c("Minor Roads" = "darkgrey", "Major Roads" = "black")) +  # Manual color scale
  labs(
    color = "Road Type"  # Legend title
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.key.size = unit(3, "cm")
  )

