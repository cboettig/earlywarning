e the current working directory
#$ -cwd
## use bash commands
#$ -S /bin/bash
## combine error and output files
#$ -j y
## Parallel for openmp:
##$ -pe threaded 16
## Launch parallel mpi threads
#$ -pe mpi 60
## Output file name
#$ -o fallacy 
## Name for queue job
#$ -N fallacy

module load gcc 
module load openmpi 
module load R 
module load Rmpi
R -f prosecutorSims.R 


