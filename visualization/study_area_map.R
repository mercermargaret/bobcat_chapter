# study area map
# margaret mercer
# march 4 2025

# load packages
library(tidyverse)
library(ggplot2)
library(sf)
library(sp)
library(lubridate)
library(terra)

# load data ####

# load capture locations
capture_locs_df <- read.csv("data/BOBCATS IN TUCSON BOBCAT CAPTURE MASTER.xlsx - Sheet1.csv")
# load streets
all_streets <- st_read("../data_too_big/Dissolved_Street_Network/")
# load rivers
rivers <- st_read("data/az_hydro_routesNAD83/")

# wrangle data ####

# wrangle capture data
# fix Carrie's wrong datapoint
capture_locs_df_new <- capture_locs_df %>%
  mutate(CAPTURE_LOCATION_LONG = ifelse(row_number() == 58, -111.0168, CAPTURE_LOCATION_LONG),
         CAPTURE_LOCATION_LAT = ifelse(row_number() == 58, 32.25371, CAPTURE_LOCATION_LAT))

# fix steve's wrong date
capture_locs_df_new <- capture_locs_df_new %>% 
  mutate(CAPTURE_DATE = ifelse(row_number() == 24, "11/20/2021", CAPTURE_DATE))

# trim to only initial capture locations
capture_locs_df_new <- capture_locs_df_new %>%
  filter(INITIAL_OR_RECAPTURE == "Initial" | INITIAL_OR_RECAPTURE == "Initail")

# create year column
capture_locs_df_new$Year <- as.character(year(mdy(capture_locs_df_new$CAPTURE_DATE)))

# and add color column by year
# Manually assign colors to each year
year_to_color <- c(
  "2020" = "red",       
  "2021" = "blue",  
  "2022" = "green",      
  "2023" = "purple"
)
# Create a new column 'year_color' in the dataframe with the assigned colors
capture_locs_df_new$year_color <- year_to_color[as.character(capture_locs_df_new$Year)]



# turn to shapefile
capture_locs <- capture_locs_df_new %>% 
  st_as_sf(coords = c('CAPTURE_LOCATION_LONG', 'CAPTURE_LOCATION_LAT')) %>%
  st_set_crs(4326)


# wrangle streets data
# set crs of streets
all_streets <- st_transform(all_streets, crs("epsg:4326"))
# trim all_streets down
bbox <- st_bbox(capture_locs)
coords <- list(c((bbox["xmin"] - 0.1), (bbox["ymin"] - 0.1)), # Extract coordinates from bbox
               c((bbox["xmax"] + 0.1), (bbox["ymin"] - 0.1)), 
               c((bbox["xmax"] + 0.1), (bbox["ymax"] + 0.1)), 
               c((bbox["xmin"] - 0.1), (bbox["ymax"] + 0.1)))
coords_matrix <- matrix(unlist(coords), ncol = 2, byrow = TRUE) # Convert list to a matrix of coordinates
coords_matrix <- rbind(coords_matrix, coords_matrix[1,]) # Add the first coordinate again at the end to close the polygon
polygon <- st_sfc(st_polygon(list(coords_matrix))) # Create the polygon object using st_polygon
polygon_sf <- st_sf(geometry = polygon) # Convert to a spatial object (sf)
st_crs(polygon_sf) <- crs(all_streets) # set crs
all_streets <- st_intersection(all_streets, polygon_sf)
major_roads <- subset(all_streets, ROADCAT_DS == "Interstate highway" | 
                 ROADCAT_DS == "Interstate ramp" | 
                 ROADCAT_DS == "State route" |
                 ROADCAT_DS == "State route ramp" |
                 ROADCAT_DS == "Frontage road" | 
                 ROADCAT_DS == "Major local road" | 
                 ROADCAT_DS == "Major road ramp")
minor_roads <- subset(all_streets, ROADCAT_DS == "Minor local road")

# wrangle streams data
rivers <- st_zm(rivers) # get into right geometry
rivers <- st_transform(rivers, crs("epsg:4326")) # transform
rivers <- st_intersection(rivers, polygon_sf) # trim to study area only

# plot ####
ggplot() +
  geom_sf(data = rivers, color = "blue",
          linewidth = 1, inherit.aes = FALSE) +
  geom_sf(data = minor_roads, color = "black",
          linewidth = 0.5, inherit.aes = FALSE, alpha = 0.5) +
  geom_sf(data = major_roads, color = "black", 
          linewidth = 1, inherit.aes = FALSE, show.legend = FALSE) + 
  geom_sf(data = capture_locs, aes(color = Year),
          size = 6, inherit.aes = FALSE) +
  labs(title = "Study Area", color = "Capture 
Locations") +
  scale_color_manual(values=c("#D81B60", "#F18527", "#FFC107", "yellow")) +
  # scale_color_manual(values=c("#FB0896", "#1E88E5", "#FFC107", "#004D40")) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5, vjust = 0.5), 
    title = element_text(size = 20, face = "bold"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size = 15)
  )

