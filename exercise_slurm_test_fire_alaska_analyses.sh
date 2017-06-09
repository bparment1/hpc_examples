#!/bin/bash
#SBATCH --job-name=bp_workshop_ex2_array_test    # Job name
#SBATCH --mail-type=ALL                          # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=bparmentier@sesync.org       # Where to send mail
#SBATCH --time=00:50:00                          # Time limit hrs:min:sec
#SBATCH --output=bp_workshop_ex2_array_test_%j.out   # Standard output from console
#SBATCH --error=bp_workshop_ex2_array_test_%j.err   # Error log 
#SBATCH --partition=sesynctest        # queue name, this is for debugging and testing

pwd; hostname; date
 
echo "Running exercise 2 workshop on a single CPU core"
 
Rscript --vanilla /nfs/bparmentier-data/Data/SLURM_test/scripts/exercise_slurm_test_fire_alaska_analyses_06092017.R
