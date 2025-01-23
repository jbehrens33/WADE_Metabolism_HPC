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

export all_proxy=socks://proxy.ccs.ornl.gov:3128/
export ftp_proxy=ftp://proxy.ccs.ornl.gov:3128/
export http_proxy=http://proxy.ccs.ornl.gov:3128/
export https_proxy=http://proxy.ccs.ornl.gov:3128/
export no_proxy='localhost,127.0.0.0/8,*.ccs.ornl.gov' 

Rscript package_download.R
