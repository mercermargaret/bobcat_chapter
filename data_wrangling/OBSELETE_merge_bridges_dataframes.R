# merges bridges dataframes and output as spreadsheet
# margaret mercer
# august 3, 2024
# i'll never have to do this again!!! this is just to subset the csv with all my notes to only bridges within study area

# load packages
library(tidyverse)
library(xlsx)
library(clipr)

# import data
old <- read.csv("../../../Downloads/Bridges - Sheet1.csv")
new <- read.csv("data/subset_bridges")

# now only include rows of "old" that correspond to coordinates in "new"
result <- semi_join(old, new, by = "coordinates")

# copy onto clipboard to paste in google sheets
write_clip(result)
