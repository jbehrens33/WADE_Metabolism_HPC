#!/bin/sh
#SBATCH -A cli185
#SBATCH -J trialRpackageload
#SBATCH -o %x-%j.out
#SBATCH -t 0:15:00
#SBATCH -p batch
#SBATCH -N 1
#SBATCH --mem=8GB

module load r
module load gcc
module load openblas

Rscript package_download.R
