# connect start and end points of bridges to make them lines then turn into shape file
# margaret mercer
# august 19, 2024

# clear workspace
rm(list=ls())

# load packages
library(sf)
library(tidyverse)

# import bridge data
bridges <- read.csv("data/Bridges - final.csv")

# cut out bridges where I can't tell where they start and end
bridges <- bridges[bridges$start_coords != "can't tell", ]

# separate coordinates
bridges <- separate(bridges, start_coords, into = c("start_lat", "start_long"), sep = ",")
bridges <- separate(bridges, end_coords, into = c("end_lat", "end_long"), sep = ",")

# turn coordinates into numbers
bridges$start_lat <- as.numeric(bridges$start_lat)
bridges$start_long <- as.numeric(bridges$start_long)
bridges$end_lat <- as.numeric(bridges$end_lat)
bridges$end_long <- as.numeric(bridges$end_long)


# from chatgpt
# Function to create a linestring from two points
create_linestring <- function(start_lat, start_long, end_lat, end_long) {
  st_linestring(matrix(c(start_long, end_long, start_lat, end_lat), ncol = 2))
}

# Apply the function to each row to create an sfc object
bridges$geometry <- mapply(create_linestring, 
                      bridges$start_lat, bridges$start_long, 
                      bridges$end_lat, bridges$end_long, 
                      SIMPLIFY = FALSE)

bridges_sf <- st_sf(bridges, crs = 4326)

plot(st_geometry(bridges_sf)) # plot to be sure it worked

# write shapefile
st_write(bridges_sf, "data/Bridges_As_Lines/bridges_as_lines.shp")
