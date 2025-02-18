# show whole study area with major versus minor roads
# margaret mercer
# december 23, 2024

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


library(ggplot2)
library(sf)

# Assuming 'low' and 'high' are your sf objects for minor and major roads

ggplot() +
  geom_sf(data = low, aes(color = "Minor Roads"), linewidth = 0.75, 
          show.legend = "line", inherit.aes = F) +  # Minor roads in dark grey
  geom_sf(data = high, aes(color = "Major Roads"), linewidth = 2, 
          show.legend = "line", inherit.aes = F) +     # Major roads in black
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

