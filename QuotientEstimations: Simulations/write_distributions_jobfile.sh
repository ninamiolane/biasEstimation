#!/bin/bash
#
#all commands that start with SBATCH contain commands that are just used by SLURM for scheduling
#################
#set a job name
#SBATCH --job-name=write-distributions
#################
#################
#email user
#SBATCH --mail-user=nmiolane@sherlock.stanford.edu
#SBATCH --mail-type=ALL
#################
# a file for errors from the job
#SBATCH --error=write-distributions.err
#################
#time you think you need; default is one hour
#in minutes in this case
#SBATCH --time=720:00
#################
#quality of service; think of it as job priority
#SBATCH --qos=normal
#################
#number of nodes you are requesting
#SBATCH --nodes=1
#################
#memory per node; default is 4000 MB per CPU
#SBATCH --mem=1000MB
#you could use --mem-per-cpu; they mean what we are calling cores
#################
#tasks to run per node; a "task" is usually mapped to a MPI processes.
# for local parallelism (OpenMP or threads), use "--ntasks-per-node=1 --cpus-per-tasks=16" instead
#SBATCH --ntasks-per-node=1
#################

#now run normal batch commands
module load R/3.2.0

#run querying for some subset of ids
Rscript query-distributions-data.R ${estname} ${n} ${xi}

