#!/bin/bash
#SBATCH --job-name=penalized
#SBATCH --output=../output/%x_%j.out
#SBATCH --error=../output/%x_%j.err
#SBATCH --mail-user=talyigasg@gmail.com
#SBATCH --mail-type=FAIL,END
#SBATCH --partition=cpu-medium,cpu-long,gpu-medium,gpu-long
#SBATCH --time=18:59:59
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=20
#SBATCH --mem=1000M

#
# loading the R module
#
module load R/3.6.2-fosscuda-2019b

#
# the actual job commands
#
echo "#### Running R serial test"

# just to illustrate some native slurm environment variables
echo "This is $SLURM_JOB_USER and this job has the ID $SLURM_JOB_ID"
echo "This job was submitted from $SLURM_SUBMIT_DIR"
echo "This job runs on $SLURMD_NODENAME"
# get the current working directory
CWD=$(pwd)
echo "I am currently in $CWD"
# get the current time and date
DATE=$(date)
echo "It is now $DATE"

# Run the file
echo "[$SHELL] Run script"
Rscript penalized_A.R
echo "[$SHELL] Script finished"

echo "#### Finished R script"