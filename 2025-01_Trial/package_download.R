options(repos = "https://cran.rstudio.com")

#library("remotes")
library("groundhog")
# Feel free to update below depending on the version of R loaded on CADES
#install_version("groundhog", version = "3.1.2", repos = "https://cran.rstudio.com")

# This will download libraries based on the version available a month after
# the release of the version of R that is loaded into CADES
packages<-c(#"tidyverse", 
            "unitted", "StanHeaders", "rstan")

groundhog.library(packages, "2023-04-22")