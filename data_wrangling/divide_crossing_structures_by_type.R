# divide crossing structures by type
# margaret mercer
# april 18 2025

library(sf)
library(ggplot2)
library(raster)
library(tidyverse)

# clear workspace
rm(list=ls())

# import
bridges <- st_read("data/Bridges_As_Lines")
summary(bridges$wht_gs_)

wash_crossing <- subset(bridges, wht_gs_ == "riverbed")
road_crossing <- subset(bridges, wht_gs_ == "road" | wht_gs_ == "railroad")
culvert_crossing <- subset(bridges, wht_gs_ == "culvert")

st_write(wash_crossing, "data/Wash_Crossings/wash.shp")
st_write(road_crossing, "data/Road_Crossings/road.shp")
st_write(culvert_crossing, "data/Culvert_Crossings/culvert.shp")
