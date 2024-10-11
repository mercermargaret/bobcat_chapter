#!/bin/bash
#SBATCH --job-name=full                 # name of the job (make it short!)
#SBATCH --partition=standard              # partition to be used (standard OR windfall)
#SBATCH --account=jmalston                # hpc group name! (always jmalston)
#SBATCH --time=24:00:00                    # walltime (up to 10-00:00:00(240:00:00))
#SBATCH --nodes=1                          # number of nodes
#SBATCH --ntasks-per-node=1                # number of tasks (i.e. parallel processes) to be started
#SBATCH --cpus-per-task=4                  # number of cpus required to run the script
#SBATCH --mem-per-cpu=32G                 # memory required for process
#SBATCH --array=0-33%34                      # set number of total simulations and number that can run simultaneously (0-33%34)

ml R gdal/3.8.5

cd /home/u15/mmercer3/proj/bobcat_chapter  # where executable and data is located

# Create a list of files
list=(/home/u15/mmercer3/proj/bobcat_chapter/results/Model_Fit_Results/*_rr.Rda)

# Print date and script initiation message
date
echo "Initiating script"

# Check if the results file exists
if [ -f results/results_rr.csv ]; then
    echo "Results file already exists! continuing..."
else
    echo "Creating results file results_rr.csv"
    echo "name, days, real_crossings_maj, crossings_near_structure_maj, real_crossings_min, crossings_near_structure_min, simulated_crossings_maj,sim_crossings_near_structure_maj, simulated_crossings_min, sim_crossings_near_structure_min, speed, area_sq_km, total_road_length_km, road_density" > results/results_rr.csv
fi

# Run the R script with the specified input from the array
Rscript analysis/full_analysis_rr.R "${list[SLURM_ARRAY_TASK_ID]}"

# Print completion message and date
echo "Script complete"
date
