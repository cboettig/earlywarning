e the current working directory
#$ -cwd
## use bash commands
#$ -S /bin/bash
## combine error and output files
#$ -j y
## Parallel for openmp:
##$ -pe threaded 16
## Launch parallel mpi threads
#$ -pe mpi 50
## Output file name
#$ -o full_prosecutor.out
## Name for queue job
#$ -N full_prosecutor

module load openmpi R Rmpi
R -f simulation.R 


