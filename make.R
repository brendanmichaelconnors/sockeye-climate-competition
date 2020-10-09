## Reproduce the project
## Source this file to reproduce the project
##
## ** make.R last run on 2020-10-9 **

rm(list = ls())

## remove reproducible directories
unlink("./figures", recursive = TRUE)
unlink("./output", recursive = TRUE)
unlink("./pub", recursive = TRUE)


time.start <- proc.time()

source("load.R")
# source("data_download.R")  ## only needs to be run if data have changed
source("sock_data_clean.R")
source("sst_import_process.R")
source("sst_explore.R")
source("detrend_covars.R")
source("sock_covariates.R")
source("sock_explore.R")
source("single_stock.R")
source("hbm_fit.R")
source("pub.R")
source("hatch_effects.R")

time.run <- proc.time() - time.start
round(time.run[3] / 60, 4)
