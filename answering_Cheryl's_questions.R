# these are cheryl's questions
# margaret mercer
# nov 1, 2024

# load in data
results <- read.csv("results/results.csv")

# the two groups in question are: the River Cats (Charlie and Nala) and the Sweetwater Cats (Wyatt, Danielle, Cassidy, Michele)

# compare number of major road crossings
Charlie_crossings <- results$real_crossings_maj[4]
Nala_crossings <- results$real_crossings_maj[22]
Wyatt_crossings <- results$real_crossings_maj[33]
Danielle_crossings <- results$real_crossings_maj[35]
Cassidy_crossings <- results$real_crossings_maj[3]
Michele_crossings <- results$real_crossings_maj[19]

# and crossings per day
Charlie_daily_rossings <- results$crossings_per_day_maj[4]
Nala_daily_crossings <- results$crossings_per_day_maj[22]
Wyatt_daily_crossings <- results$crossings_per_day_maj[33]
Danielle_daily_crossings <- results$crossings_per_day_maj[35]
Cassidy_daily_crossings <- results$crossings_per_day_maj[3]
Michele_daily_crossings <- results$crossings_per_day_maj[19]

# compare percent of major road crossings near a structure
Charlie_structure_crossings <- results$percent_crossings_near_structure_maj[4]
Nala_structure_crossings <- results$percent_crossings_near_structure_maj[22]
Wyatt_structure_crossings <- results$percent_crossings_near_structure_maj[33]
Danielle_structure_crossings <- results$percent_crossings_near_structure_maj[35]
Cassidy_structure_crossings <- results$percent_crossings_near_structure_maj[3]
Michele_structure_crossings <- results$percent_crossings_near_structure_maj[19]

# actually there's a better way to do this
new <- results[c(4, 22, 33, 35, 3, 19),]
new <- new[,c(1, 3, 20, 29)]
new$crossings_per_day_maj <- round(new$crossings_per_day_maj, 1)
new$percent_crossings_near_structure_maj <- round(new$percent_crossings_near_structure_maj, 2)
new$group <- c("River", "River", "Wetland", "Wetland", "Wetland", "Wetland")
