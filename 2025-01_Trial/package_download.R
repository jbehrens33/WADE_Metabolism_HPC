#options(repos = "https://cran.rstudio.com")

#install.packages("remotes")
# Feel free to update below depending on the version of R loaded on CADES
#install_version("groundhog", version = "3.1.2", repos = "https://cran.rstudio.com")

# Libraries
library("remotes")
#library("groundhog")

remotes::install_github('appling/unitted')

install.packages("rstan", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))

install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

remotes::install_github(
  "DOI-USGS/streamMetabolizer", # soon to be "DOI-USGS/streamMetabolizer"
  build_vignettes = TRUE)

# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))


# This will download libraries based on the version available a month after
# the release of the version of R that is loaded into CADES
# packages<-c("tidyverse")#, 
#             #"unitted", "StanHeaders", "rstan")
# 
# groundhog.library(packages, "2023-04-22")