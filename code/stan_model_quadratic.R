### Code to fit linear model to all countries

#load libraries
library('rstan')
library('dplyr')
library('ggmcmc')

# where
setwd("~/Documents/LTBI_MDR/code")

# Country list and WHO data
who0 <- as.data.frame(read.csv("~/Dropbox/MDR/new_who_edited_sub.csv")[,-1])
uu <- unique(who0$iso3) # 107
luu <- length(unique(who0$iso3)) # 107

# Add in sigma to data
who0$mdr_new <- who0$new_mdr_prop
who0$sigma <- (who0$mhi - who0$mdr_new)/1.96 # 

# if sigma = 0, set to minimum range from rest of data
w<-which(who0$sigma == 0)


pred_samples <- list()

# Cycle through all countries
for(ii in 1:luu) {
  # country for this fit 
  country <- uu[ii]
  print(country)
  
  who_l <- who0 %>%
    filter(iso3==country) %>%
    dplyr::select(year, mdr_new, mlo, mhi, sigma)
  
  years_to_predict <- seq(1970, 2018) - 1970
  
  m <- stan(file="quadratic.stan",
            data = list(N=nrow(who_l), 
                        N2=length(years_to_predict), 
                        q=who_l$mdr_new, 
                        years_obs=who_l$year-1970,
                        years_obs2=(who_l$year-1970)^2,
                        sigma=who_l$sigma, 
                        years=years_to_predict,
                        years2=(years_to_predict)^2),
            pars=c("a", "b","p_pred"),
            chains=2, iter=20000, warmup=10000, thin=10)
  
  posterior<-As.mcmc.list(m,pars=c("a", "b"))
  
  ggmcmc(ggs(posterior), file=paste0("~/Dropbox/MDR/output/",country, "_mcmc_quad.pdf"))
  
  samples <- rstan::extract(m, pars="p_pred")[[1]][1001:2000, ]
  
  mcmc.samples <- data.frame(samples) %>%
    tbl_df %>%
    dplyr::mutate(sample=1:n()) %>%
    gather(col, prediction, starts_with("X")) %>%
    dplyr::mutate(year=as.integer(sub("^X", "", col)) + 1969L) %>%
    dplyr::select(-col)
  
  annual <- mcmc.samples %>%
    group_by(year) %>%
    dplyr::summarise(mean=mean(prediction), min=quantile(prediction, 0.025), max=quantile(prediction, 0.975)) %>%
    group_by(n=1:n()) %>%
    dplyr::mutate(mean=max(mean, 0), min=max(min, 0), max=max(max, 0)) %>%
    ungroup %>%
    dplyr::select(-n)
  
  g <- ggplot(annual, aes(x=year)) +
    geom_line(aes(y=mean)) +
    geom_ribbon(aes(ymin=min, ymax=max), alpha=0.3, fill = "red") +
    geom_point(data=who_l, aes(y=mdr_new)) +
    geom_errorbar(data=who_l, aes(ymin=mlo, ymax=mhi)) +
    geom_vline(xintercept=2014, linetype="dashed")
  ggsave(paste0("~/Dropbox/MDR/output/",country, "_mcmc_quad_fit.pdf"))
  
  pred_samples[[country]] <- mcmc.samples %>%
    mutate(country = country)
}

all_samples <- pred_samples %>% 
  bind_rows

write.csv(pred_samples,"../output/pred_samples_quad.csv")
write.csv(all_samples,"../output/all_samples_quad.csv")

# CHECKS
ggplot(who_l, aes(x=year,y=mdr_new)) + geom_errorbar(aes(ymin = mdr_new - sigma, ymax = mdr_new + sigma))
