#load libraries
library('rstan')
library('dplyr')

country <- "ZWE"

who_l <- who0 %>%
  filter(iso3==country) %>%
  select(year, mdr_new=new_mdr_prop, mhi)

who_l$sigma <- (who_l$mhi - who_l$mdr_new)/1.96 # 

years_to_predict <- seq(1970, 2018) - 1970

m <- stan(file="linear.stan",
     data = list(N=nrow(who_l), 
                 N2=length(years_to_fit), 
                 q=who_l$mdr_new, 
                 years_obs=who_l$year-1970, 
                 sigma=who_l$sigma, 
                 years=years_to_predict),
     pars=c("a", "b", "p_pred"),
     chains=2, iter=20000, warmup=10000, thin=10)

posterior<-As.mcmc.list(m,pars=c("a", "b"))

ggmcmc(ggs(posterior), file=paste0(country, "_mcmc.pdf"))

samples <- rstan::extract(m, pars="p_pred")[[1]][1001:2000, ]

mcmc.samples <- data.frame(samples) %>%
  tbl_df %>%
  mutate(sample=1:n()) %>%
  gather(col, prediction, starts_with("X")) %>%
  mutate(year=as.integer(sub("^X", "", col)) + 1969L) %>%
  select(-col)

annual <- mcmc.samples %>%
  group_by(year) %>%
  summarise(mean=mean(prediction), min=quantile(prediction, 0.025), max=quantile(prediction, 0.975)) %>%
  group_by(n=1:n()) %>%
  mutate(mean=max(mean, 0), min=max(min, 0), max=max(max, 0)) %>%
  ungroup %>%
  select(-n)

ggplot(annual, aes(x=year)) +
  geom_line(aes(y=mean)) +
  geom_ribbon(aes(ymin=min, ymax=max), alpha=0.3) +
  geom_point(data=who_l, aes(y=mdr_new)) +
  geom_errorbar(data=who_l, aes(ymin=mdr_new-sigma, ymax=mdr_new+sigma)) +
  geom_vline(xintercept=2014, linetype="dashed")

pred <- annual %>%
  filter(year==2014)

pred_samples <- mcmc.samples %>%
  filter(year==2014)
