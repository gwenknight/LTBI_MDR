### Code to fit linear model to all countries

#load libraries
library('rstan')
library('dplyr')
library('ggmcmc')
library("boot")

# PRINT? 
pp <- 1

# Country list and WHO data
who0 <- as.data.frame(read.csv("~/Dropbox/MDR/new_who_edited_sub.csv")[,-1])
who0$year <- who0$year_new
uu <- unique(who0$iso3) # 138 now
luu <- length(unique(who0$iso3)) # 138

# WHILE ERROR WITH SINGLE DATAPOINT
uu <- read.csv("~/Dropbox/MDR/138_final_list_included_countries.csv",stringsAsFactors = FALSE)[,-1]
luu <- length(unique(who0$iso3)) # 138

#luu <- 5 ##### DO JUST FOR FIRST FIVE! *******************************************************************************************

# Add in sigma to data
who0$mdr_new <- who0$new_mdr_prop
who0$sigma <- (who0$mhi - who0$mdr_new)/1.96 # 

# if sigma = 0, set to minimum range from rest of data
w<-which(who0$sigma == 0)

pred_samples_p <- list()

# Cycle through all countries
for(ii in 1:luu) {
  # country for this fit 
  country <- uu[ii]
  
  
  who_l <- who0 %>% filter(iso3==country) 
  who_l <- who_l[,c("year", "mdr_new", "mlo", "mhi", "sigma")]
  
  years_to_predict <- seq(1970, 2018)
  print(c(ii,as.character(country),nrow(who_l)))
  
  
  if(nrow(who_l) == 1){ 
    m.p <- stan(file="quadratic_priors_years.stan",
                data = list(N=nrow(who_l), 
                            N2=length(years_to_predict), 
                            q=as.array(who_l$mdr_new),
                            years_obs=as.array(who_l$year), 
                            sigma=as.array(who_l$sigma), 
                            years=years_to_predict),
                control = list(adapt_delta = 0.99),
                chains=2, iter=20000, warmup=10000, thin=10)
    
  } else {
    m.p <- stan(file="quadratic_priors_years.stan",
                data = list(N=nrow(who_l), 
                            N2=length(years_to_predict), 
                            q=who_l$mdr_new, 
                            years_obs=who_l$year, 
                            sigma=who_l$sigma, 
                            years=years_to_predict),
                control = list(adapt_delta = 0.99),
                chains=2, iter=20000, warmup=10000, thin=10)
  }
  
  ## With informative Priors
  posterior.p<-As.mcmc.list(m.p,pars=c("b", "t_m","rho"))
  if(pp > 0){ggmcmc(ggs(posterior.p), file=paste0("~/Dropbox/MDR/output/",country, "_inforprior_mcmc.pdf"))}
  if(pp > 0){ #plot posterior against priors
    pp <- as.data.frame(m.p,pars=c("b", "t_m","rho"))[1001:2000,]
    nsamples <- 1000000
    priors <- cbind(rlnorm(nsamples, meanlog = log(0.0008), sdlog = 0.8), #b
                    rnorm(nsamples,1985,9), #t_m
                    rnorm(nsamples,15,5)) # rho
    colnames(priors) <- colnames(pp)
    pp_p <- rbind(pp, priors)
    pp_p$prior <- c(matrix(0,1000,1), matrix(1,nsamples,1))
    pp_p <- melt(pp_p, id.vars = "prior")
    ggplot(pp_p,aes(x=value, group = prior)) + geom_density(alpha = 0.3,aes(fill = factor(prior))) + 
      facet_wrap(~variable, scale="free") + scale_fill_discrete("Dist",labels = c("Posterior","Prior"))
    ggsave(paste0("~/Dropbox/MDR/output/",country, "_inforprior_post_vs_prior.pdf"),width = 20, height = 10)
  }
  
  # 200 samples for predicted levels 
  samples.p.p <- rstan::extract(m.p, pars="p_pred", permuted = FALSE)[801:1000,1,] # pick first chain
  
  # # 200 samples for time mdr = 0
  # samples.p.t <- rstan::extract(m.p, pars="t_m", permuted = FALSE)[801:1000,1,] # pick first chain
  # 
  # ## remove inverse logit - need to? this is what is fitted
  # #samples.p.p <- logit(samples.p.p)
  # samples.p.yr <- as.data.frame(matrix(years_to_predict,200,length(years_to_predict)))
  # #for(i in 1:200){samples.p.yr[i,] <- 1970 + years_to_predict - samples.p.t[i]}
  # samples.p.yr$t <- samples.p.t
  
  colnames(samples.p.p) <-NULL
  mcmc.samples.p.p <- data.frame(samples.p.p) %>%
    tbl_df %>%
    dplyr::mutate(sample=1:n()) %>%
    gather(col, prediction, starts_with("X")) %>%
    dplyr::mutate(year=as.integer(sub("^X", "", col)) + 1969) 
  
  # colnames(samples.p.yr) <-NULL
  # mcmc.samples.p.yr <- data.frame(samples.p.yr) %>%
  #   tbl_df %>%
  #   dplyr::mutate(sample=1:n()) %>%
  #   gather(col, year, starts_with("X")) 
  # 
  # mcmc.samples <- merge(mcmc.samples.p.p, mcmc.samples.p.yr, by = c("sample","col"))
  # 
  annual.p <- mcmc.samples.p.p %>%
    group_by(year) %>%
    dplyr::summarise(mean=mean(prediction), min=quantile(prediction, 0.025), max=quantile(prediction, 0.975)) %>%
    group_by(n=1:n()) %>%
    dplyr::mutate(mean=max(mean, 0), min=max(min, 0), max=max(max, 0)) %>%
    ungroup 
  
  if(pp > 0){
    g <- ggplot(annual.p, aes(x=year)) +
      geom_line(aes(y=mean)) +
      geom_ribbon(aes(ymin=min, ymax=max), alpha=0.3, fill = "red") +
      geom_point(data=who_l, aes(y=mdr_new)) +
      geom_errorbar(data=who_l, aes(ymin=mlo, ymax=mhi)) +
      geom_vline(xintercept=2014, linetype="dashed")
    ggsave(paste0("~/Dropbox/MDR/output/",country, "_inforprior_mcmc_fit.pdf"))
    
    g <- ggplot(mcmc.samples.p.p, aes(x=year, group = sample)) +
      geom_line(aes(y=prediction)) +
      geom_point(data=who_l, aes(y=mdr_new, group = year), col = "red") +
      geom_errorbar(data=who_l, aes(ymin=mlo, ymax=mhi, group = year), col = "red") +
      geom_vline(xintercept=2014, linetype="dashed") + scale_y_continuous(limits = c(0, NA), expand = c(0,0))
    ggsave(paste0("~/Dropbox/MDR/output/",country, "_inforprior_mcmc_fit_all_curves.pdf"))
  }
  
  pred_samples_p[[country]] <- mcmc.samples.p.p %>%
    mutate(country = country)
  
}

all_samples_p <- pred_samples_p[] %>% bind_rows

write.csv(pred_samples_p,"~/Dropbox/MDR/output/pred_samples_p.csv")
write.csv(all_samples_p,"~/Dropbox/MDR/output/all_samples_p.csv")


##### make correct for adding to other data
# and merge
load("~/Dropbox/MDR/output/rundata_ari_1000.Rdata")
rundatar$ari <- exp(rundatar$lari)

final_list_cn <- read.csv("~/Dropbox/MDR/138_final_list_included_countries.csv")[,-1]

# Rename
all_samples <- all_samples_p %>%
  mutate(replicate = sample, iso3 = country) %>%
  dplyr::select(-sample) %>% dplyr::select(-country)
# Set negative to 0
all_samples[which(all_samples$prediction < 0), "prediction"] <- 0

# Set greater than 1 to 1! Data on proportion (0-1) so cannot go above 1. 
all_samples[which(all_samples$prediction > 1), "prediction"] <- 1

# Add in 0 before 1970
ns <- max(all_samples_p$sample)
sample_v <- seq(1,ns,1)
years <- unique(rundatar$year) 
years_v <- years[which(years < 1970)]
ny <- length(years_v)
nc <- length(final_list_cn)
pre_1970_mdr <- as.data.frame(cbind(rep(sample_v,ny*nc),rep(years_v, each = ns*nc)))
colnames(pre_1970_mdr) <- c("replicate", "year")   
pre_1970_mdr$iso3 <-rep(final_list_cn, each = ns)
pre_1970_mdr$prediction <- 0

all_samples <- rbind(all_samples[,c("replicate","year","iso3","prediction")], pre_1970_mdr)

all_samples_1970_2014 <-all_samples
write.csv(all_samples_1970_2014, "~/Dropbox/MDR/output/all_samples_p_1970_2014.csv") 

## MERGE DS AND MDR
all0 <- merge(all_samples, rundatar, by = c("year","iso3","replicate"))
# MDR-ARI
all0$mdr_ari <- all0$prediction * all0$ari

# TOTAL AND MDR
all0$ds_ari <- all0$ari - all0$mdr_ari # ds ARI is the remainder of the ari

save(all0,file="~/Dropbox/MDR/output/all0_p_ds_mdr.Rdata") 




