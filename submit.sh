#!/bin/bash
#
#SBATCH --array=0-2
#SBATCH --job-name=slurm_test3
#SBATCH --output=slurm_%a.out

#SBATCH --partition=sesyncshared

#SBATCH --mail-type=ALL

#SBATCH --mail-user=bparmentier@sesync.org

#SBATCH --time=00:25:00

#SBATCH --output=console_%A_%a.out

#SBATCH --error=log_%A_%a.err
/usr/lib/R/bin/Rscript --vanilla slurm_run.R
