## run the cohort model on this mac (as cluster busy!)

library(foreach)
library(doParallel)

# don't fill all cores
no_cores <- detectCores() - 1
cl<-makeCluster(no_cores)

registerDoParallel(cl)

source("~/Documents/LTBI_MDR/code/run_cohort_ltbi_mdr_inf_priors_parallel.R")

save1 <- lapply(1:138, run_cohort_parallel)


stopCluster(cl)