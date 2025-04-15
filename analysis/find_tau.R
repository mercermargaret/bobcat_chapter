# find tau for reference
# mm
# april 9 2025
# List all files in the directory
files <- list.files("results/Model_Fit_Results", full.names = TRUE)

# Get the length of files
n_files <- length(files)

# Create empty dataframe
taus <- data.frame(
  position = numeric(n_files),  # Adjust length based on number of files
  velocity = numeric(n_files)
)

# Loop through all files
for(i in 1:n_files){
  # Load the .rda file
  load(files[i])  # Load the specific file in the loop
  
  # Assuming that fits is the object loaded from the .rda file
  taus$position[i] <- fits$tau[1]
  taus$velocity[i] <- fits$tau[2]
}

# Now, `taus` will contain the extracted values from all files

# write csv
write.csv(taus, "results/taus.csv", row.names = FALSE)
