# rename bobcats to be simpler
# margaret mercer
# sept 20 2024

# clear workspace
rm(list=ls())

bobcat <- read.csv("data/Bobcat_Individuals/non_range_resident/tippy_NOT_RR.csv")

bobcat$individual.identifier <- "Tippy"

write.csv(bobcat, "data/Bobcat_Individuals/non_range_resident/tippy_NOT_RR.csv")
