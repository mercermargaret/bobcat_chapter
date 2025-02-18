# graph of distance to crossing structure
# margaret mercer
# january 27, 2025

# load packages
library(tidyverse)
library(ggplot2)

# load crossing info data for each individual
minor_files <- list.files(path = "results/Crossing_Info_Minor_Roads", pattern = "*.csv", full.names = TRUE)
crossings_min <- do.call(rbind, lapply(minor_files, function(file) {
  data <- read.csv(file)
  data$Individual_ID <- tools::file_path_sans_ext(basename(file))  # Extract filename without extension
  return(data)
}))

major_files <- list.files(path = "results/Crossing_Info_Major_Roads", pattern = "*.csv", full.names = TRUE)
crossings_maj <- do.call(rbind, lapply(major_files, function(file) {
  data <- read.csv(file)
  data$Individual_ID <- tools::file_path_sans_ext(basename(file))  # Extract filename without extension
  return(data)
}))

# merge major and minor roads crossing info
crossings <- rbind(crossings_min, crossings_maj)

# make histogram of crossing distances
hist(crossings$Passage_Distances, breaks = 100)

# OK i don't like the way this looks...it doesn't show what I want it to show
# SOOOO lets try something different

# plotting how many crossings of each individual fall within 7m of a crossing structure ####
# for that we'll need results$percent_crossings_near_structure_all

results <- read.csv("results/results.csv")
crossings_near_structure <- results[c(1, 29)]
crossings_near_structure$percent_crossings_near_structure_all <- crossings_near_structure$percent_crossings_near_structure_all * 100

hist(crossings_near_structure$percent_crossings_near_structure_all)

ggplot(crossings_near_structure, aes(x = percent_crossings_near_structure_all)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(x = "Percent Crossings Near Structure", y = "Frequency") +
  theme_minimal() +
  theme(
    axis.ticks = element_line(),
    # axis.text.y = element_blank(),
    # axis.title.y = element_blank(),
    # panel.grid = element_blank(),
    legend.position = "none"
  )

# hmmm maybe. but might be better in a table?
write.csv(crossings_near_structure, "results/percent_crossings_near_structure.csv")
