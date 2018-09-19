### Code to fit linear model to all countries

#load libraries
library('rstan')
library('dplyr')
library('ggmcmc')
library('loo')

# PRINT? 
pp <- 0

# where
setwd("~/Documents/LTBI_MDR/code")

# Country list and WHO data
who0 <- as.data.frame(read.csv("~/Dropbox/MDR/new_who_edited_sub.csv")[,-1])
uu <- unique(who0$iso3) # 107
luu <- length(unique(who0$iso3)) # 107

luu <- 5 ##### DO JUST FOR FIRST FIVE! *******************************************************************************************

# Add in sigma to data
who0$mdr_new <- who0$new_mdr_prop
who0$sigma <- (who0$mhi - who0$mdr_new)/1.96 # 

# if sigma = 0, set to minimum range from rest of data
w<-which(who0$sigma == 0)


pred_samples_lin <- list()
pred_samples_quad <- list()
pred_samples_quadc <- list()
cc<-c() # compare outputs

# Cycle through all countries
for(ii in 1:luu) {
  # country for this fit 
  country <- uu[ii]
  print(country)
  
  who_l <- who0 %>%
    filter(iso3==country) %>%
    select(year, mdr_new, mlo, mhi, sigma)
  
  years_to_predict <- seq(1970, 2018) - 1970
  
  m.lin <- stan(file="linear.stan",
                data = list(N=nrow(who_l), 
                            N2=length(years_to_predict), 
                            q=who_l$mdr_new, 
                            years_obs=who_l$year-1970, 
                            sigma=who_l$sigma, 
                            years=years_to_predict),
                pars=c("a", "b", "p_pred","log_likelihood"),
                chains=2, iter=20000, warmup=10000, thin=10)
  
  m.quad <- stan(file="quadratic.stan",
                 data = list(N=nrow(who_l), 
                             N2=length(years_to_predict), 
                             q=who_l$mdr_new, 
                             years_obs=who_l$year-1970,
                             years_obs2=(who_l$year-1970)^2,
                             sigma=who_l$sigma, 
                             years=years_to_predict,
                             years2=(years_to_predict)^2),
                 pars=c("a", "b","p_pred","log_likelihood"),
                 chains=2, iter=20000, warmup=10000, thin=10)
  
  m.quadc <- stan(file="quadratic_cgreatzero.stan",
                  data = list(N=nrow(who_l), 
                              N2=length(years_to_predict), 
                              q=who_l$mdr_new, 
                              years_obs=who_l$year-1970,
                              years_obs2=(who_l$year-1970)^2,
                              sigma=who_l$sigma, 
                              years=years_to_predict,
                              years2=(years_to_predict)^2),
                  pars=c("a", "b","p_pred","log_likelihood"),
                  chains=2, iter=20000, warmup=10000, thin=10)
  
  # From https://www.weirdfishes.blog/blog/fitting-bayesian-models-with-stan-and-r/#whats-stan-and-why-use-it
  log_lik_lin <- extract_log_lik(m.lin, parameter_name = "log_likelihood")
  log_lik_quad <- extract_log_lik(m.quad, parameter_name = "log_likelihood")
  log_lik_quadc <- extract_log_lik(m.quadc, parameter_name = "log_likelihood")
  lin_loo <- loo::loo(log_lik_lin)
  quad_loo <- loo::loo(log_lik_quad)
  quadc_loo <- loo::loo(log_lik_quadc)
  compare(lin_loo, quad_loo)
  compare(quad_loo, quadc_loo)
  c <- compare(lin_loo, quad_loo, quadc_loo)
  # if elpd_diff is positive the second model is preferred. If it’s negative, the first model is preferred
  cc <- rbind(cc,cbind(c,uu[ii]))
  
  waic1 <- waic(log_lik_lin)
  waic2 <- waic(log_lik_quad)
  compare(waic1, waic2)
  # if elpd_diff is positive the second model is preferred. If it’s negative, the first model is preferred
  
  ## Linear
  posterior.lin<-As.mcmc.list(m.lin,pars=c("a", "b"))
  if(pp > 0){ggmcmc(ggs(posterior.lin), file=paste0("~/Dropbox/MDR/output/",country, "lin_mcmc.pdf"))}
  samples.lin <- rstan::extract(m.lin, pars="p_pred")[[1]][1001:2000, ]
  
  mcmc.samples.lin <- data.frame(samples.lin) %>%
    tbl_df %>%
    dplyr::mutate(sample=1:n()) %>%
    gather(col, prediction, starts_with("X")) %>%
    dplyr::mutate(year=as.integer(sub("^X", "", col)) + 1969L) %>%
    select(-col)
  
  annual.lin <- mcmc.samples.lin %>%
    group_by(year) %>%
    dplyr::summarise(mean=mean(prediction), min=quantile(prediction, 0.025), max=quantile(prediction, 0.975)) %>%
    group_by(n=1:n()) %>%
    dplyr::mutate(mean=max(mean, 0), min=max(min, 0), max=max(max, 0)) %>%
    ungroup %>%
    select(-n)
  
  if(pp > 0){
  g <- ggplot(annual.lin, aes(x=year)) +
    geom_line(aes(y=mean)) +
    geom_ribbon(aes(ymin=min, ymax=max), alpha=0.3, fill = "red") +
    geom_point(data=who_l, aes(y=mdr_new)) +
    geom_errorbar(data=who_l, aes(ymin=mlo, ymax=mhi)) +
    geom_vline(xintercept=2014, linetype="dashed")
  ggsave(paste0("~/Dropbox/MDR/output/",country, "lin_mcmc_fit.pdf"))}

  pred_samples_lin[[country]] <- mcmc.samples.lin %>%
    mutate(country = country)
  
  ## Quadratic
  posterior.quad<-As.mcmc.list(m.quad,pars=c("a", "b"))
  if(pp > 0){ggmcmc(ggs(posterior.quad), file=paste0("~/Dropbox/MDR/output/",country, "quad_mcmc.pdf"))}
  samples.quad <- rstan::extract(m.quad, pars="p_pred")[[1]][1001:2000, ]
  
  mcmc.samples.quad <- data.frame(samples.quad) %>%
    tbl_df %>%
    dplyr::mutate(sample=1:n()) %>%
    gather(col, prediction, starts_with("X")) %>%
    dplyr::mutate(year=as.integer(sub("^X", "", col)) + 1969L) %>%
    select(-col)
  
  annual.quad <- mcmc.samples.quad %>%
    group_by(year) %>%
    dplyr::summarise(mean=mean(prediction), min=quantile(prediction, 0.025), max=quantile(prediction, 0.975)) %>%
    group_by(n=1:n()) %>%
    dplyr::mutate(mean=max(mean, 0), min=max(min, 0), max=max(max, 0)) %>%
    ungroup %>%
    select(-n)
  
  if(pp > 0){
  g <- ggplot(annual.quad, aes(x=year)) +
    geom_line(aes(y=mean)) +
    geom_ribbon(aes(ymin=min, ymax=max), alpha=0.3, fill = "red") +
    geom_point(data=who_l, aes(y=mdr_new)) +
    geom_errorbar(data=who_l, aes(ymin=mlo, ymax=mhi)) +
    geom_vline(xintercept=2014, linetype="dashed")
  ggsave(paste0("~/Dropbox/MDR/output/",country, "quad_mcmc_fit.pdf"))}
  
  pred_samples_quad[[country]] <- mcmc.samples.quad %>%
    mutate(country = country)
  
  ## Quadratic with c > 0
  posterior.quadc<-As.mcmc.list(m.quadc,pars=c("a", "b"))
  if(pp > 0){ggmcmc(ggs(posterior.quadc), file=paste0("~/Dropbox/MDR/output/",country, "quadc_mcmc.pdf"))}
  samples.quadc <- rstan::extract(m.quadc, pars="p_pred")[[1]][1001:2000, ]
  
  mcmc.samples.quadc <- data.frame(samples.quadc) %>%
    tbl_df %>%
    dplyr::mutate(sample=1:n()) %>%
    gather(col, prediction, starts_with("X")) %>%
    dplyr::mutate(year=as.integer(sub("^X", "", col)) + 1969L) %>%
    select(-col)
  
  annual.quadc <- mcmc.samples.quadc %>%
    group_by(year) %>%
    dplyr::summarise(mean=mean(prediction), min=quantile(prediction, 0.025), max=quantile(prediction, 0.975)) %>%
    group_by(n=1:n()) %>%
    dplyr::mutate(mean=max(mean, 0), min=max(min, 0), max=max(max, 0)) %>%
    ungroup %>%
    select(-n)
  
  if(pp > 0){
  g <- ggplot(annual.quadc, aes(x=year)) +
    geom_line(aes(y=mean)) +
    geom_ribbon(aes(ymin=min, ymax=max), alpha=0.3, fill = "red") +
    geom_point(data=who_l, aes(y=mdr_new)) +
    geom_errorbar(data=who_l, aes(ymin=mlo, ymax=mhi)) +
    geom_vline(xintercept=2014, linetype="dashed")
  ggsave(paste0("~/Dropbox/MDR/output/",country, "quadc_mcmc_fit.pdf"))}
  
  pred_samples_quadc[[country]] <- mcmc.samples.quadc %>%
    mutate(country = country)
}

all_samples_lin <- pred_samples_lin %>% 
  bind_rows

all_samples_quad <- pred_samples_quad %>% 
  bind_rows

all_samples_quadc <- pred_samples_quadc %>% 
  bind_rows

write.csv(pred_samples_lin,"../output/pred_samples_lin.csv")
write.csv(all_samples_lin,"../output/all_samples_lin.csv")

write.csv(pred_samples_quad,"../output/pred_samples_quad.csv")
write.csv(all_samples_quad,"../output/all_samples_quad.csv")

write.csv(pred_samples_quadc,"../output/pred_samples_quadc.csv")
write.csv(all_samples_quadc,"../output/all_samples_quadc.csv")

write.csv(cc, "../output/compare_models_elpd.csv")


##### make correct for adding to other data
# and merge
load("~/Dropbox/MDR/output/rundata_ari_1000.Rdata")
rundatar$ari <- exp(rundatar$lari)

for(ii in 1:3){
  print(ii)
  
  if (ii == 1){ all_samples0 <- all_samples_lin}
  if (ii == 2){ all_samples0 <- all_samples_quad}
  if (ii == 3){ all_samples0 <- all_samples_quadc}
  
  # Rename
  all_samples <- all_samples0 %>%
    mutate(replicate = sample, iso3 = country) %>%
    dplyr::select(-sample) %>% dplyr::select(-country)
  # Set negative to 0
  all_samples[which(all_samples$prediction < 0), "prediction"] <- 0
  
  # Add in 0 before 1970
  ns <- max(all_samples0$sample)
  sample_v <- seq(1,ns,1)
  years <- unique(rundatar$year) 
  years_v <- years[which(years < 1970)]
  ny <- length(years_v)
  nc <- length(final_list_cn)
  pre_1970_mdr <- as.data.frame(cbind(rep(sample_v,ny*nc),rep(years_v, each = ns*nc)))
  colnames(pre_1970_mdr) <- c("replicate", "year")   
  pre_1970_mdr$iso3 <-rep(final_list_cn, each = ns)
  pre_1970_mdr$prediction <- 0
  
  all_samples <- rbind(all_samples, pre_1970_mdr)
  
  all_samples_1970_2014 <-all_samples
  if (ii == 1){ write.csv(all_samples_1970_2014, "~/Dropbox/MDR/output/all_samples_lin_1970_2014.csv") }
  if (ii == 2){ write.csv(all_samples_1970_2014, "~/Dropbox/MDR/output/all_samples_quad_1970_2014.csv") }
  if (ii == 3){ write.csv(all_samples_1970_2014, "~/Dropbox/MDR/output/all_samples_quadc_1970_2014.csv") }
  
  ## MERGE DS AND MDR
  all0 <- merge(all_samples, rundatar, by = c("year","iso3","replicate"))
  # MDR-ARI
  all0$mdr_ari <- all0$prediction * all0$ari
  
  # TOTAL AND MDR
  all0$ds_ari <- all0$ari - all0$mdr_ari # ds ARI is the remainder of the ari
  
  if (ii == 1){ save(all0,file="~/Dropbox/MDR/output/all0_lin_ds_mdr.Rdata") }
  if (ii == 2){ save(all0,file="~/Dropbox/MDR/output/all0_quad_ds_mdr.Rdata") }
  if (ii == 3){ save(all0,file="~/Dropbox/MDR/output/all0_quadc_ds_mdr.Rdata") }
  
  
}





