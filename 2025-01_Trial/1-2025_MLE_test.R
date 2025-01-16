# Set Up

# Preparing Data for Modeling
## Step 1

scripts <- list.files('code/', 
                      pattern = '*.R', 
                      full.names = TRUE)

# source them in 
sapply(scripts, source, .GlobalEnv)

## Step 2

## this fun installs streamMetabolizer (and dependencies) and rstan (and dependencies) from github
prep_bayes_model()

# if this returns an error, running this below will suffice
# install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# remotes::install_github("https://github.com/appling/unitted")
# remotes::install_github("https://github.com/DOI-USGS/streamMetabolizer")

# Load in the remaining packages
library('tidyverse')
library('rstan')
library('StanHeaders')
library('unitted')
library('streamMetabolizer')
library(StreamPULSE)

#install.packages(c('cli', 'fansi', 'vctrs'))
lapply(c('cli', 'fansi', 'vctrs'), library, character.only = TRUE)

# Model in MLE
## Step 4

# the user will have to define which years (calendar) they are interested in
model_mle(years = 2021)

# evaluate the MLE outputs
mle_eval()

