# preliminary data exploration for bobcat gps data
# margaret mercer
# july 30, 2024
# I put this together by looking at the code from Noonan 2019 and Noonan 2021 simultaneously
    # and using what applied to me
    # see: https://static-content.springer.com/esm/art%3A10.1186%2Fs40462-019-0177-1/MediaObjects/40462_2019_177_MOESM2_ESM.pdf
    # and: https://zslpublications.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Facv.12728&file=acv12728-sup-0002-AppendixS2.pdf

# load packages
library(ctmm)
library(proj4)
library(rgdal)
library(foreach)
library(doParallel)

# loading in gps data as csv
data <- read.csv("data/bobcat_locs_all.csv")

# all this from Noonan 2019 ####
margaret_gps <- data[data$individual.local.identifier == "BC #12 Margaret", ]
summary(margaret_gps)

# making margaret a telemetry object so ctmm recognizes it
margaret <- as.telemetry(margaret_gps)
summary(margaret)

# plotting!
# plot(margaret ,
      # error = 2 ,
      # level.UD = 0.50) # this doesn't work great because we don't know the error yet. thats ok.

# ID outliers
outliers <- outlie(margaret)
# plot(outliers)

# get rid of outliers in margaret
outlier_t <- outliers$t[outliers$speed >= .40]
margaret <- margaret[!margaret$t %in% outlier_t, ]

# replot data
# plot(margaret,
       # error = 2 ,
       # level.UD = 0.50)
outliers <- outlie(margaret)
# plot(outliers)

# make a variogram
vg <- variogram(margaret)

# Guesstimate the model to obtain initial parameter values
guess <- ctmm.guess(margaret,
                        variogram = vg,
                        interactive = FALSE)
guess$error <- TRUE

fits <- ctmm.select(margaret, CTMM = guess) # let this run for a while! it takes like 24 hours!
# run this on the hpc!!

# # results of Margaret: $name
# [1] "OUF anisotropic error"
# 
# $DOF
# mean        area   diffusion       speed 
# 841.0483495 478.8594468  66.9396815   0.2145813 
# 
# $CI
#                                           low        est         high
# area (square kilometers)          8.614783e+00   9.441605 1.030577e+01
# τ[position] (hours)               1.045637e+01  11.556108 1.277151e+01
# τ[velocity] (minutes)             1.176433e-06  12.133369 1.251398e+08
# speed (kilometers/day)            8.089032e-03  24.957827 6.759088e+01
# diffusion (square kilometers/day) 1.580028e+00   2.039031 2.555674e+00
# error all (meters)                3.563701e-03 111.682002 3.183893e+02
# 

# return a summary of the fitted models
summary(fits)

save(fits, file = "margaret_ctmm.Rda")

# plot variogram and model

"margaret_ctmm.Rda"

plot(vg, CTMM = fits)


# THEN, with model and data in had, do the other stuff...

# estimate average speed
speed(margaret, fits)

# estimate instantaneous speeds
speeds <- speeds (margaret, fits)

# Estimating daily movement distance over a study period
# First identify how many days the individual was tracked for
margaret$day <- cut(margaret$timestamp, breaks = "day")
days <- unique (margaret$day)

#An empty list to fill with the results
results <- list()

# Loop over the number of days
for (i in 1:length(days)) {
  message ("Estimating distance travelled on day", i,": ", days[i])
  # Select data for the day in question
  data_subset <- margaret[which(margaret$day == days[i]), ]
  # Calculate the duration of the sampling period (in seconds )
  samp_time <- diff (c(data_subset$t[1],
                         data_subset$t[nrow(data_subset)]))
  
  # Guesstimate the model for initial parameter values
  guess <- ctmm.guess(margaret,
                          variogram = variogram (margaret) ,
                          interactive = FALSE )
  # Turn error on
  guess$error <- TRUE
  # Fit the movement model to the day’s data
  fits <- ctmm.fit(data_subset,
                     CTMM = guess)
  # Calculate speed in m/s
  ctmm_speed <- speed(object = data_subset,
                         CTMM = fits,
                         units = FALSE)
  # Multiply speed (in m/s) by the sample time (in s)
  # to get the estimated distance travelled (in m)
  ctmm_dist <- ctmm_speed * samp_time
  # Re-name the variable
  rownames(ctmm_dist) <- "distance(meters)"
  
  # And store the results in the list
  x <- c(i, #The day
         ctmm_dist[2], #The ML distance estimate
         ctmm_dist[1], #Min CI
         ctmm_dist[3]) #Max CI
  names(x) <- c("date", "dist.ML", "dist.Min", "dist.Max")
  results[[i]] <- x
}

# Finally bind results together as a data frame
results <- as.data.frame(do.call(rbind, results))
results$date <- as.Date (days)
head(results)


# all this from Noonan 2021 ####
# calculating home range
#calculate the AKDE based on the best fit model
margaret_akde <- akde(margaret, fits)
#Return the basic statistics on the HR area
summary(margaret_akde)

# Create a function that scales colours between red and blue
rbPal <- colorRampPalette(c('#FF0000','#046C9A'))
# Then create a variable that scales from red to blue between the two times
margaret$Col <- rbPal(nrow(margaret))[as.numeric(cut(margaret$t,breaks = nrow(margaret)))]
# Plot the AKDE range estimate, with the relocation data, coloured by time
plot(margaret,
     UD = margaret_akde,
     col.grid = NA,
     family = "serif",
     pch = 20,
     cex = 0.2,
     col.DF = "#669543",
     col = margaret$Col,
     labels = FALSE)

# visually assess whether there's a range shift going on over the course of the study period

# road permeability
# are they willing to establish home ranges on either side of the roads?

# Load the road data
# Is this just one road? Will we be loading in all roads here?
roads <- readOGR(dsn = "~/Dropbox (Personal)/UBC/Side_Projects/Arnaud_Anteaters/Scripts/Roads/BR262")
## OGR data source with driver: ESRI Shapefile
## Source: "/Users/michaelnoonan/Dropbox (Personal)/UBC/Side_Projects/Arnaud_Anteaters/Scripts/Roads/BR262", layer: "BR262"
## with 1 features
## It has 11 fields
## Integer64 fields read as strings: tessellate extrude visibility drawOrder

# Reproject the roads to match the tracking data
bobcat_proj <- margaret@info$projection
roads <- spTransform(roads, bobcat_proj)

# Reproject the HR contour
HR_contour <- spTransform(HR_contour, bobcat_proj)

# Get the areas that fall on either side of the road
lpi <- gIntersection(HR_contour, roads)
blpi <- gBuffer(lpi, width = 0.000001)
dpi <- gDifference(HR_contour, blpi)
Side_1 <- SpatialPolygons(list(Polygons(list(dpi@polygons[[1]]@Polygons[[1]]), "1")))
Side_2 <- SpatialPolygons(list(Polygons(list(dpi@polygons[[1]]@Polygons[[2]]), "2")))

# Plot the split HR and the road
plot(HR_contour)
plot(Side_1, col = "lightgreen", add = TRUE)
plot(Side_2, col = "lightblue", add = TRUE)
lines(roads, col = "red")

# Area on each side
Side_1 <- raster::area(Side_1)
Side_2 <- raster::area(Side_2)
# Ratio of roads on each side
ratio <- min(Side_1, Side_2)/max(Side_1, Side_2)
round(ratio,3)

# distance of home range to nearest road
# Extract coordinates of home range center and carry out some reprojections
pj <- proj4::project(fits$mu,
                     fits@info$projection,
                     inverse = TRUE)
mu <- data.frame(lat = pj[,1],
                 lon = pj[,2])
mu <- SpatialPointsDataFrame(coords = mu,
                             data = mu,
                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
mu_proj <- spTransform(mu, CRS("+proj=utm +zone=21 +south +ellps=WGS72 +units=m +no_defs"))
# Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO", prefer_proj
# = prefer_proj): Discarded datum Unknown based on WGS 72 ellipsoid in Proj4
# definition
ROAD_proj <- spTransform(roads, CRS("+proj=utm +zone=21 +south +ellps=WGS72 +units=m +no_defs"))
# Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO", prefer_proj
# = prefer_proj): Discarded datum Unknown based on WGS 72 ellipsoid in Proj4
# definition
# Calculate distance to nearest road
gDistance(mu_proj, ROAD_proj)/1000

# Plot everything to make sure projections are correct
HR_contour_proj <- spTransform(HR_contour, CRS("+proj=utm +zone=21 +south +ellps=WGS72 +units=m +
no_defs"))
# Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO", prefer_proj
# = prefer_proj): Discarded datum Unknown based on WGS 72 ellipsoid in Proj4
# definition
plot(HR_contour_proj)
plot(ROAD_proj, add = TRUE, col = "red")
plot(mu_proj, add = TRUE, pch = 16, col = "#046C9A")

# estimate road crossings 
# Estimate the most likely path based on the fitted movement model
mlp <- predict(margaret, fits, dt = 60, complete = TRUE)
# Warning in if (axes == "z") {: the condition has length > 1 and only the first
# element will be used

# Convert to the right format for counting road crossings
MLP <- SpatialPointsDataFrame.telemetry(mlp)
MLP <- spTransform(MLP, bobcat_proj)
MLP_2 <- lapply(split(MLP, MLP$identity),
                function(x) Lines(list(Line(coordinates(x))), MLP$identity[1L]))
MLP_2 <- SpatialLines(MLP_2, proj4string = roads@proj4string)

# How many times does it cross the paved road
BR262_crossings <- rgeos::gIntersection(MLP_2, roads)
Num_Crossings <- length(BR262_crossings)
Num_Crossings

# Plot of the most likely path and the road
plot(MLP_2, col = "NA")
lines(roads, col = "#FF0000")
lines(MLP_2, col = "#046C9A")
title(main = paste(margaret@info$identity, " - Crossings: ", Num_Crossings),
      family = "serif",
      font.main = 1,
      cex.main = 0.85)

# From these calculations, we see that this animal crossed highway BR262 a total of 46 times. In addition to estimating the
# number of crossings, we quantified the times that each of these crossings occurred.

# Requires conversion to lat long for the distHaversine function
roads_crossings_latlong <- spTransform(roads_crossings,"+proj=longlat +datum=WGS84 +no_defs +ell
ps=WGS84 +towgs84=0,0,0")
# Warning in validityMethod(object): duplicate rownames are interpreted by rgeos
# as MultiPoints; use SpatialMultiPoints to define these; in future sp versions
# this warning will become an error
MLP_latlong <- spTransform(MLP,"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
")
# Find times it crossed roads
cross_times <- vector()
for(i in 1:nrow(BR262_crossings@coords)){
  # Find which point in the mlp is closest to the crossing location
  dists <- geosphere::distHaversine(roads_crossings_latlong@coords[i,], MLP_latlong@coords)
  cross_times[i] <- MLP@data[which(dists == min(dists)),"timestamp"]
}

head(cross_times)

# here they did stream crossings; we'll instead do modeling and see whether they crossed roads more or less frequently than expected at random

# crossing structures 
# Load in the locations of the crossing passages
passes <- readOGR(dsn = "~/Dropbox (Personal)/UBC/Side_Projects/Arnaud_Anteaters/Scripts/Roads/P
assages")
# OGR data source with driver: ESRI Shapefile
# Source: "/Users/michaelnoonan/Dropbox (Personal)/UBC/Side_Projects/Arnaud_Anteaters/Scripts/Roads/Passages", layer: "PassagesRoads"
# with 29 features
# It has 6 fields
# Integer64 fields read as strings: ID
passes <- spTransform(passes, bobcat_proj)
# Empty vector to store results
pass_dists <- vector("numeric", length = length(roads_crossings))
# Measuring distance of crossings from passages
for(i in 1:nrow(roads_crossings@coords)){
 # Find which point in the mlp is clsoest to the crossing location
  dists <- raster::pointDistance(roads_crossings@coords[i,], passes@coords, lonlat = FALSE)
 pass_dists[i] <- min(dists)
}

head(pass_dists)

res <- t.test(pass_dists)
res

# Which crossings were within 20m (replace with my own median gps error!!!) of a road passage
which(pass_dists <= 20)

# Plot of the most likely path, the road, and the passage structures
plot(MLP_2, col = "NA")
lines(roads, col = "#FF0000")
lines(MLP_2, col = "#046C9A")
points(passes, pch = 16)
title(main = paste(margaret@info$identity, " - Crossings: ", Num_Crossings),
      family = "serif",
      font.main = 1,
      cex.main = 0.85)

# They did parallelization, but maybe we just do it on the hpc?
# Reg. multiple cores for DoParallel
nCores <- 8
registerDoParallel(nCores)

# Check that it's setup correctly
getDoParWorkers()

# A character string for reprojections
LatLon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# Number of simulated replicates per animal
nReps <- 1000
# Run the simulations for this animal
x <- foreach(j = 1:nReps) %dopar% {
  
  # Simulate data from the fitted movement model
  
  SIM <- simulate(fits, t = margaret$t, complete = TRUE)
  
  # Convert to the right format/projection for identifying road crossings using rgeos
  MLP <- SpatialPointsDataFrame.telemetry(SIM)
  MLP <- spTransform(MLP, margaret@info$projection)
  MLP_2 <- lapply(split(MLP, MLP$identity),
                  function(x) Lines(list(Line(coordinates(x))), MLP$identity[1L]))
  MLP_2 <- SpatialLines(MLP_2, proj4string = roads@proj4string)

  # The number of crossings for the simulated animal
  
  # How many times does it cross the road
  Sim_Road_Cross <- rgeos::gIntersection(MLP_2, roads)
  Sim_Road_Cross_Count <- length(Sim_Road_Cross)
  
  # How many times does it cross streams
  Sim_Stream_Cross <- rgeos::gIntersection(MLP_2, streams)
  Sim_Stream_Cross_Count <- length(Sim_Stream_Cross)

  # Crossing times for the simulated animal
  
  # Requires conversion to lat long for the distHaversine function
  Road_crossings_latlong <- spTransform(Sim_Road_Cross,LatLon)
  Sim_Stream_Cross_latlong <- spTransform(Sim_Stream_Cross,LatLon)
  
  MLP_latlong <- spTransform(MLP,LatLon)
  
  # Find times it crossed the road
  road_cross_times <- vector()
  for(i in 1:nrow(roads_crossings@coords)){
    # Find which point in the mlp is closest to the crossing location
    dists <- geosphere::distHaversine(roads_crossings_latlong@coords[i,], MLP_latlong@coords)
    road_cross_times[i] <- MLP@data[which(dists == min(dists)),"timestamp"]
  }
  
  # Find times it crossed the stream
  stream_cross_times <- vector()
  for(i in 1:nrow(Sim_Stream_Cross@coords)){
    # Find which point in the mlp is closest to the crossing location
    dists <- geosphere::distHaversine(Sim_Stream_Cross_latlong@coords[i,], MLP_latlong@coords)
    stream_cross_times[i] <- MLP@data[which(dists == min(dists)),"timestamp"]
  }
  
  # list of results to return
  list(fits@info$identity,
       Sim_Road_Cross_Count,
       Sim_Stream_Cross_Count,
       road_cross_times,
       stream_cross_times)
}

# Clean up results
results <- data.frame("ID" = unlist(lapply(x, function (x) x[1])),
                      "Road_Crossings" = unlist(lapply(x, function (x) x[2])),
                      "Stream_Crossings"= unlist(lapply(x, function (x) x[3])))
head(results)

# Mean road and stream crossings
mean(results$Road_Crossings); mean(results$Stream_Crossings)

