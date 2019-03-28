### Code to fit y = bt - ct^2 to WHO data for all countries

###********** Libraries and ggplot theme ************************************************************************************************#######
library('rstan')
library('dplyr')
library('ggmcmc')
library("boot")
library("reshape")
library("ggforce")
library("splines")
theme_set(theme_bw(base_size = 24))

###********** Home ************************************************************************************************#######
home <- "~/Documents/LTBI_MDR/"
setwd(home)

###********** Load code and data ************************************************************************************************#######
# Country list and WHO data
## NOT SUPPLIED WITH REPOSITORY
who0 <- as.data.frame(read.csv("data/new_who_edited_sub.csv")[,-1]) 
#who0$year <- who0$year_new

# Add in sigma (standard deviation) to data
who0$mdr_new <- who0$new_mdr_prop
who0$sigma <- (who0$mhi - who0$mdr_new)/1.96  

## JUST doing for 3 countries now: India / China / USA
#uu <- read.csv("data_final/138_final_list_included_countries.csv",stringsAsFactors = FALSE)[,-1]
uu <- read.csv("data/3_spline_included_countries.csv",stringsAsFactors = FALSE)[,-1]
luu <- 3 #length(unique(who0$iso3)) # 138

# Store
pred_samples_p <- list()

pp <- "original"

## Needed for splines
X <- seq(from=-5, to=5, by=.1) # generating inputs
B <- t(bs(X, knots=seq(-5,5,1), degree=3, intercept = TRUE)) # creating the B-splines
num_data <- length(X); num_basis <- nrow(B)
a0 <- 0.2 # intercept
a <- rnorm(num_basis, 0, 1) # coefficients of B-splines
Y_true <- as.vector(a0*X + a%*%B) # generating the output
Y <- Y_true + rnorm(length(X),0,.2) # adding noise
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
sm<-stan_model("code/splines.stan")
fit<-sampling(sm,iter=500,control = list(adapt_delta=0.95))

## plot
ff<-rstan::extract(fit)
Y_hat_med <- array(NA, length(Y))
Y_hat_ub <- array(NA, length(Y))
Y_hat_lb <- array(NA, length(Y))
for (i in 1:length(Y)) {
  Y_hat_med[i] <- median(ff$Y_hat[,i]);
  Y_hat_lb[i] <- quantile(ff$Y_hat[,i],probs = 0.25)
  Y_hat_ub[i] <- quantile(ff$Y_hat[,i],probs = 0.75)
}
plot(X,Y, col="azure4")
polygon(c(rev(X), X), c(rev(Y_hat_lb), Y_hat_ub), col = 'grey80', border = NA)
lines(X, Y_hat_med, col="Red", lw=2)
lines(X, Y_true, col="blue",lw=2)

##OR:

set.seed(1234)
num_knots <- 10 # true number of knots
spline_degree <- 3
num_basis <- num_knots + spline_degree - 1
X <- seq(from=-10, to=10, by=.1)
knots <- unname(quantile(X,probs=seq(from=0, to=1, length.out = num_knots)))
num_data <- length(X)
a0 <- 0.2
a <- rnorm(num_basis, 0, 1)
B_true <- t(bs(X, df=num_basis, degree=spline_degree, intercept = TRUE))
Y_true <- as.vector(a0*X + a%*%B_true)
Y <- Y_true + rnorm(length(X), 0, 0.2)

num_knots <- 100; # number of knots for fitting
num_basis <- num_knots + spline_degree - 1
knots <- unname(quantile(X,probs=seq(from=0, to=1, length.out = num_knots)))
rstan_options(auto_write = TRUE);
options(mc.cores = parallel::detectCores());
spline_model<-stan_model("code/splines.stan")
fit_spline_penalized<-sampling(spline_model,iter=500,control = list(adapt_delta=0.95))



## TRY FOR CHINA
who_chn <- subset(who0, iso3 == "CHN")
X <- c(1970,1971,1972,1973,1974,who_chn$year_new) #seq(from=-5, to=5, by=.1) # generating inputs
B <- t(bs(X, knots=seq(1970,2014,1), degree=1, intercept = TRUE, Boundary.knots = c(1970, 2014))) # creating the B-splines
num_data <- length(X); num_basis <- nrow(B)
a0 <- 0 # intercept
a <- rnorm(num_basis, 0, 1) # coefficients of B-splines
Y_true <- c(0,0,0,0,0,who_chn$mdr_new) #as.vector(a0*X + a%*%B) # generating the output
Y <- Y_true #+ rnorm(length(X),0,.2) # adding noise
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
sm<-stan_model("code/splines.stan")
fit<-sampling(sm,iter=500,control = list(adapt_delta=0.95))

## plot
ff<-rstan::extract(fit)
Y_hat_med <- array(NA, length(Y))
Y_hat_ub <- array(NA, length(Y))
Y_hat_lb <- array(NA, length(Y))
for (i in 1:length(Y)) {
  Y_hat_med[i] <- median(ff$Y_hat[,i]);
  Y_hat_lb[i] <- quantile(ff$Y_hat[,i],probs = 0.25)
  Y_hat_ub[i] <- quantile(ff$Y_hat[,i],probs = 0.75)
}
plot(X,Y, col="azure4")
polygon(c(rev(X), X), c(rev(Y_hat_lb), Y_hat_ub), col = 'grey80', border = NA)
lines(X, Y_hat_med, col="Red", lw=2)
lines(X, Y_true, col="blue",lw=2)



predictions <- who_chn %>% 
  mutate(predicted = predict(fit_smooth1)[,"Estimate"])

lines(who_chn, col = "blue")
ss10 <- smooth.spline(cars[,"speed"], cars[,"dist"], df = 10)
lines(ss10, lty = 2, col = "red")


# Cycle through all countries
for(ii in 1:luu) {
  # country for this fit 
  country <- uu[ii]
  
  # data for this country
  who_l <- who0 %>% filter(iso3==country) 
  who_l <- who_l[,c("year", "mdr_new", "mlo", "mhi", "sigma")]
  
  years_to_predict <- seq(1970, 2018)
  print(c(ii,as.character(country),nrow(who_l)))
  
  # if only one data point - have to make some inputs to stan "as.array"s
  if(nrow(who_l) == 1){ 
    # model fit in stan
    m.p <- stan(file="code/model_trend.stan",
                data = list(N=nrow(who_l), 
                            N2=length(years_to_predict), 
                            q=as.array(who_l$mdr_new),
                            years_obs=as.array(who_l$year), 
                            sigma=as.array(who_l$sigma), 
                            years=years_to_predict),
                control = list(adapt_delta = 0.99),
                chains=2, iter=20000, warmup=10000, thin=10)
    
  } else {
    m.p <- stan(file="code/quadratic_priors_years.stan",
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
  ggmcmc(ggs(posterior.p), file=paste0("output_spline/",country, "_inforprior_mcmc.pdf"))
  
  # 200 samples for predicted levels 
  samples.p.p <- rstan::extract(m.p, pars="p_pred", permuted = FALSE)[801:1000,1,] # pick first chain
  
  colnames(samples.p.p) <-NULL
  mcmc.samples.p.p <- data.frame(samples.p.p) %>%
    tbl_df %>%
    dplyr::mutate(sample=1:n()) %>%
    gather(col, prediction, starts_with("X")) %>%
    dplyr::mutate(year=as.integer(sub("^X", "", col)) + 1969) 
  
  pred_samples_p[[country]] <- mcmc.samples.p.p %>%
    mutate(country = country)
  
}

all_samples_p <- pred_samples_p[] %>% bind_rows

write.csv(pred_samples_p,"output_spline/pred_samples_p.csv")
write.csv(all_samples_p,"output_spline/all_samples_p.csv")

##### make correct for adding to other data
# and merge
load("../MDR-LTBI-paper/data_final/rundata_ari_1000.Rdata") # From running the code in Houben & Dodd in 2016.
rundatar$ari <- exp(rundatar$lari)

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
nc <- llu
pre_1970_mdr <- as.data.frame(cbind(rep(sample_v,ny*nc),rep(years_v, each = ns*nc)))
colnames(pre_1970_mdr) <- c("replicate", "year")   
pre_1970_mdr$iso3 <-rep(uu, each = ns)
pre_1970_mdr$prediction <- 0

all_samples <- rbind(all_samples[,c("replicate","year","iso3","prediction")], pre_1970_mdr)

all_samples_1970_2014 <-all_samples
write.csv(all_samples_1970_2014, "output_spline/all_samples_p_1970_2014.csv") 

## MERGE DS AND MDR
all0 <- merge(all_samples, rundatar, by = c("year","iso3","replicate"))
# MDR-ARI
all0$mdr_ari <- all0$prediction * all0$ari

# TOTAL AND MDR
all0$ds_ari <- all0$ari - all0$mdr_ari # ds ARI is the remainder of the ari

save(all0,file="output_spline/all0_p_ds_mdr.Rdata") 


### PLOT
# READ IN
#load("output_spline/all0_p_ds_mdr.Rdata")
theme_set(theme_bw(base_size = 24))
for(cci in 1:length(uu)){
  print(uu[cci])
  ### WHO data
  d <-subset(who0, iso3 == as.character(uu[cci]) )
  
  ### ARI for both DS and mDR in all0
  rdata <- all0[which(all0$iso3 == as.character(uu[cci])),]
  
  a1 <- ggplot(d, aes(x=year_new, y = new_mdr_prop),col="red",pch = 10) + # points won't plot over lines unless do points first?!
    geom_point() +
    geom_line(data = rdata, aes(x=year, y = prediction, group = factor(replicate)),alpha = 0.2) +
    scale_y_continuous("Prop. new with MDR") + scale_x_continuous("Year",lim=c(1970,2016)) +
    geom_point(data = d, aes(x=year_new, y = new_mdr_prop),col="red",pch = 10, size = 3) + geom_errorbar(data = d, aes(ymin = mlo, ymax = mhi), col = "red")
  ggsave(paste0("output_spline/",uu[cci],"_mdr_trends_with_data_",pp,".pdf"),width=11, height=11)
  
  a2 <- ggplot(rdata, aes(x=year, y = mdr_ari, group = factor(replicate))) + geom_line(alpha = 0.2) +
    scale_y_continuous("MDR ARI") + scale_x_continuous("Year",lim=c(1970,2015))
  ggsave(paste0("output_spline/",uu[cci],"_mdr_ari_",pp,".pdf"),width=11, height=11)
  
}  

