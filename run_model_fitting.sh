#!/bin/bash
#SBATCH --job-name=model         # name of the job (make it short!)
#SBATCH --partition=standard           # partition to be used (standard OR windfall)
#SBATCH --account=jmalston          # hpc group name! (always jmalston)
#SBATCH --time=48:00:00            # walltime (up to 10-00:00:00(240:00:00))
#SBATCH --nodes=1                  # number of nodes
#SBATCH --ntasks-per-node=1        # number of tasks (i.e. parallel processes) to be started
#SBATCH --cpus-per-task=1          # number of cpus required to run the script
#SBATCH --mem-per-cpu=32G         # memory required for process
#SBATCH --array=0-2%3    	   # set number of total simulations and number that can run simultaneously (will be 0-33%34 for real run through)

ml R gdal/3.8.5

cd /home/u15/mmercer3/proj/bobcat_chapter   # where executable and data is located

list=(/home/u15/mmercer3/proj/bobcat_chapter/data/Bobcat_Individuals/range_resident/*.csv)

date
echo "Initiating script"

Rscript analysis/model_fitting.R ${list[SLURM_ARRAY_TASK_ID]} # name of script
echo "Script complete"
date
