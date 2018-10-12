## run the cohort model on this mac (as cluster busy!)

library(foreach)
library(doParallel)
library(R.devices)



# don't fill all cores
no_cores <- detectCores() - 1


#### SOCKETS
cl<-makeCluster(no_cores)

registerDoParallel(cl)

source("~/Documents/LTBI_MDR/code/run_cohort_ltbi_mdr_inf_priors_parallel.R")

save1 <- lapply(86:138, run_cohort_parallel)


stopCluster(cl)


#### FORKING - quicker?

source('~/Documents/LTBI_MDR/code/run_cohort_ltbi_mdr_inf_priors_parallel.R', echo=TRUE)

save1 <- mclapply(1:10, run_cohort_parallel,mc.cores = no_cores)