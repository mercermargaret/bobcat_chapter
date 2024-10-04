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

# Okkkkkk so let's look at primary versus secondary
primary <- st_read("../data_too_big/Raw_Roads_AZ/Primary_Roads/TIGER_Primary")
secondary <- st_read("../data_too_big/Raw_Roads_AZ/Secondary_Roads/TIGER_Secondary")

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
plot(st_geometry(bobcats_sf), col = "lightblue", main = "Polygon with 10000m Buffer")
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

# do same for primary ...
primary <- st_transform(primary, crs(study_area))
primary_inside <- st_intersection(primary, study_area)
ggplot(data = primary_inside) +
  geom_sf() +
  theme_classic()
# primary is JUST the highway

# ... and secondary
secondary <- st_transform(secondary, crs(study_area))
secondary_inside <- st_intersection(secondary, study_area)
ggplot(data = secondary_inside) +
  geom_sf() +
  theme_classic()
# secondary is highway plus a couple major roads

# # what if we try named vs unnamed roads?
# named_streets <- subset(streets_inside, !is.na(FULLNAME))
# unnamed_streets <- subset(streets_inside, is.na(FULLNAME))
# ggplot(data = named_streets) +
#   geom_sf() +
#   theme_classic()
# ggplot(data = unnamed_streets) +
#   geom_sf() +
#   theme_classic()
# # nope, there are hardly any unnamed roads

# what about splitting it up by RTTYP?
# county <- subset(streets_inside, RTTYP == "C") # only one tiny road segment here
interstate <- subset(streets_inside, RTTYP == "I")
common <- subset(streets_inside, RTTYP == "M")
other <- subset(streets_inside, RTTYP == "O" | is.na(RTTYP))
state <- subset(streets_inside, RTTYP == "S")
# us <- subset(streets_inside, RTTYP == "U") # nothing here within study area so no need


# ggplot(data = us) +
#   geom_sf() +
#   theme_classic()
ggplot(data = interstate) +
  geom_sf() +
  theme_classic()
ggplot(data = state) +
  geom_sf() +
  theme_classic()
# ggplot(data = county) +
#   geom_sf() +
#   theme_classic()
ggplot(data = common) +
  geom_sf() +
  theme_classic()
ggplot(data = other) +
  geom_sf() +
  theme_classic()

# or plot all at once with different colors
ggplot() +
  geom_sf(data = common, fill = "white", color = "white") +
  geom_sf(data = common, fill = "lightblue", color = "lightblue") +
  geom_sf(data = state, fill = "green", color = "green") +
  geom_sf(data = interstate, fill = "red", color = "red") +
  geom_sf(data = other, fill = "orange", color = "orange") +
  theme_classic() +
  labs(title = "All Streets Colored By RTTYP")


# split by MTFCC
S1100 <- subset(streets_inside, MTFCC == "S1100")
S1200 <- subset(streets_inside, MTFCC == "S1200")
S1400 <- subset(streets_inside, MTFCC == "S1400")
S1500 <- subset(streets_inside, MTFCC == "S1500")
S1630 <- subset(streets_inside, MTFCC == "S1630")
S1640 <- subset(streets_inside, MTFCC == "S1640")
S1710 <- subset(streets_inside, MTFCC == "S1710")
S1730 <- subset(streets_inside, MTFCC == "S1730")
S1740 <- subset(streets_inside, MTFCC == "S1740")
S1750 <- subset(streets_inside, MTFCC == "S1750")
S1780 <- subset(streets_inside, MTFCC == "S1780")
S1820 <- subset(streets_inside, MTFCC == "S1820")

ggplot() +
  geom_sf(data = S1400, fill = "lightblue", color = "lightblue") +
  geom_sf(data = S1630, fill = "yellow", color = "yellow") +
  geom_sf(data = S1780, fill = "orange", color = "orange") +
  geom_sf(data = S1740, fill = "blue", color = "blue") +
  geom_sf(data = S1100, fill = "purple", color = "purple") +
  geom_sf(data = S1200, fill = "green", color = "green") +
  geom_sf(data = S1500, fill = "red", color = "red") +
  geom_sf(data = S1640, fill = "red", color = "red") +
  geom_sf(data = S1710, fill = "red", color = "red") +
  geom_sf(data = S1730, fill = "red", color = "red") +
  geom_sf(data = S1750, fill = "red", color = "red") +
  geom_sf(data = S1820, fill = "red", color = "red") +
  theme_classic() +
  labs(title = "All Streets Colored By MTFCC")


# now I want to save streets_inside as a spatial object, to pull into my ctmm analysis
# st_write(streets_inside, "data/Roadmap_Wrangled/roadmap.shp")

