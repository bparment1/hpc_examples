#!/bin/bash
#SBATCH --job-name=benoit_workshop_ex2_test    # Job name
#SBATCH --mail-type=ALL               # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=bparmentier@sesync.org   # Where to send mail	
#SBATCH --ntasks=1                    # Run on a single CPU
#SBATCH --time=00:25:00               # Time limit hrs:min:sec
#SBATCH --output=serial_test_%j.out   # Standard output and error log
#SBATCH --partition=sesynctest        # queue name, this is for debugging and testing

pwd; hostname; date
 
echo "Running exercise 2 workshop on a single CPU core"
 
Rscript --vanilla exercise2_fire_alaska_analyses_05112017.R