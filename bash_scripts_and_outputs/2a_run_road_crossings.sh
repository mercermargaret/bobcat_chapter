#!/bin/bash
#SBATCH --job-name=roads                 # name of the job (make it short!)
#SBATCH --partition=standard              # partition to be used (standard OR windfall)
#SBATCH --account=jmalston                # hpc group name! (always jmalston)
#SBATCH --time=5:00:00                    # walltime (up to 10-00:00:00(240:00:00))
#SBATCH --nodes=1                          # number of nodes
#SBATCH --ntasks-per-node=1                # number of tasks (i.e. parallel processes) to be started
#SBATCH --cpus-per-task=1                  # number of cpus required to run the script
#SBATCH --mem-per-cpu=32G                 # memory required for process
#SBATCH --array=0-2%3                      # set number of total simulations and number that can run simultaneously (0-33%34)

ml R gdal/3.8.5

cd /home/u15/mmercer3/proj/bobcat_chapter  # where executable and data is located

# Create a list of files
list=(/home/u15/mmercer3/proj/bobcat_chapter/results/Model_Fit_Results/*_rr.Rda)

# Print date and script initiation message
date
echo "Initiating script"

# Check if the results file exists
if [ -f results/road_crossings.csv ]; then
    echo "Results file already exists! continuing..."
else
    echo "Creating results file road_crossings.csv"
    echo "name,real_crossings,mean_sim_cross" > results/road_crossings.csv
fi

# Run the R script with the specified input from the array
Rscript analysis/2a_road_crossings.R "${list[SLURM_ARRAY_TASK_ID]}"

# Print completion message and date
echo "Script complete"
date

#SBATCH --job-name=roads         # name of the job (make it short!)
#SBATCH --partition=standard           # partition to be used (standard OR windfall)
#SBATCH --account=jmalston          # hpc group name! (always jmalston)
#SBATCH --time=5:00:00            # walltime (up to 10-00:00:00(240:00:00))
#SBATCH --nodes=1                  # number of nodes
#SBATCH --ntasks-per-node=1        # number of tasks (i.e. parallel processes) to be started
#SBATCH --cpus-per-task=1          # number of cpus required to run the script
#SBATCH --mem-per-cpu=32G         # memory required for process
#SBATCH --array=0-2%3    	   # set number of total simulations and number that can run simultaneously (0-33%34) 

ml R gdal/3.8.5

cd /home/u15/mmercer3/proj/bobcat_chapter  # where executable and data is located

# Create a list of files
list=(/home/u15/mmercer3/proj/bobcat_chapter/results/Model_Fit_Results/*_rr.Rda)

# Print date and script initiation message
date
echo "Initiating script"

# Check if the results file exists
if [ -f results/road_crossings.csv ]; then
	echo "Results file already exists! continuing..."
else
	echo "Creating results file road_crossings.csv"
	echo "name, real_crossings, mean_sim_cross" > results/road_crossings.csv
fi

# Run the R script with the specified input from the array
Rscript analysis/2a_road_crossings.R "${list[SLURM_ARRAY_TASK_ID]}"

# Print completion message and date
echo "Script complete"
date