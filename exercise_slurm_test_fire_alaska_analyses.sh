#!/bin/bash
#SBATCH --job-name=slurm_test_spatial_array_test    # Job name
#SBATCH --mail-type=ALL                          # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=bparmentier@sesync.org       # Where to send mail
#SBATCH	--array=1-4                              # job array, four jobs corresponding to four tiles in Alaska
#SBATCH --ntasks=1                               # each array task gets a single CPU core
#SBATCH --time=00:50:00                          # Time limit hrs:min:sec
#SBATCH --output=console_%A_%a.out   # Standard output from console
#SBATCH --error=log_%A_%a.err   # Error log 
#SBATCH --partition=sesynctest        # queue name, this is for debugging and testing

pwd; hostname; date
 
echo "Running exercise 2 workshop on a single CPU core"
 
Rscript --vanilla /nfs/bparmentier-data/Data/SLURM_test/scripts/exercise_slurm_test_fire_alaska_analyses_01252018.R
