# subsetting arizona bridges
# margaret mercer
# july 31, 2024

# load packages
library(tidyverse)
library(sp)
library(sf)

# load dsf# load data
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

