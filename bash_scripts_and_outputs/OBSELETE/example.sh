#!/bin/bash
#SBATCH --job-name=de         # name of the job (make it short!)
#SBATCH --partition=standard           # partition to be used (standard OR windfall)
#SBATCH --account=jmalston          # hpc group name! (always jmalston)
#SBATCH --time=96:00:00            # walltime (up to 10-00:00:00(240:00:00))
#SBATCH --nodes=1                  # number of nodes
#SBATCH --ntasks-per-node=1        # number of tasks (i.e. parallel processes) to be started
#SBATCH --cpus-per-task=1          # number of cpus required to run the script
#SBATCH --mem-per-cpu=32G         # memory required for process


ml R gdal

cd /home/u15/mmercer3/proj/bobcat_chapter   # where executable and data is located

date
echo "Initiating script"

Rscript data_wrangling/data_exploration.R  # name of script
echo "Script complete"
date
