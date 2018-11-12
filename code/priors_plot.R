### Priors_plot

library(ggplot2)
library(dplyr)
library(magrittr)
library("truncnorm")
theme_set(theme_bw(base_size = 24))


# Country list and WHO data
who0 <- as.data.frame(read.csv("~/Dropbox/MDR/new_who_edited_sub.csv")[,-1])
who0$year <- who0$year_new

who0.r.95 <- who0 %>%
  group_by(year) %>%
  dplyr::summarise(median=median(av_mdr_new_pcnt), min=quantile(av_mdr_new_pcnt, 0.025), max=quantile(av_mdr_new_pcnt, 0.975)) %>%
  group_by(n=1:n()) %>% dplyr::mutate(median=max(median, 0), min=max(min, 0), max=max(max, 0)) %>% ungroup 

who0.r.80 <- who0 %>%
  group_by(year) %>%
  dplyr::summarise(median=median(av_mdr_new_pcnt), min=quantile(av_mdr_new_pcnt, 0.1), max=quantile(av_mdr_new_pcnt, 0.9)) %>%
  group_by(n=1:n()) %>% dplyr::mutate(median=max(median, 0), min=max(min, 0), max=max(max, 0)) %>% ungroup 

who0.r.50 <- who0 %>%
  group_by(year) %>%
  dplyr::summarise(median=median(av_mdr_new_pcnt), min=quantile(av_mdr_new_pcnt, 0.25), max=quantile(av_mdr_new_pcnt, 0.75)) %>%
  group_by(n=1:n()) %>% dplyr::mutate(median=max(median, 0), min=max(min, 0), max=max(max, 0)) %>% ungroup 

####### Priors:
### tm  ****######################################################################
# original: tm <- normal(1980,5)
#tseq <- rnorm(100,mean = 1980, sd = 5)
tprop <- rnorm(10000,mean = 1985, sd = 9)
plot(density(tprop), col = "red")
quantile(tprop,probs=c(0.05, 0.95))

tseq <- 1980

### b  ****######################################################################
# original: b <- uniform(0,0.05) 
bseq <- runif(30,min = 0, max = 0.05)

# mean = exp(mu + sd^2/2): want it at 1980 => log(1980) - sd^2 / 2 = mu
bseq <- rlnorm(10000000, meanlog = -5.5, sdlog = 0.7)
hist(bseq)
mean(bseq)
plot(density(bseq))
quantile(bseq) # DATA: #25-75: 0.0003 - 0.001: this is close: 0.0004 - 0.001
quantile(bseq, prob = c(0.05, 0.5,0.95)) # DATA: #25-75: 0.0003 - 0.001: this is close: 0.0004 - 0.001 WANT -15\% to 21\% to match Cohen 2014
plot(quantile(bseq)[1:4]) 
max(bseq) # hits 0.05

### rho ****######################################################################
# original: rho  <- uniform(0,1)
rhoseq <- rnorm(100000,mean = 5, sd = 15)

plot(density(rhoseq))
quantile(rhoseq, prob = c(0.05, 0.95)) # DATA: #25-75: 0.0003 - 0.001: this is close: 0.0004 - 0.001
max(rhoseq)



###*** PLOT ***######################################################################################################################################################################
###### UNIVARIATE *****#####################################################################################################################################
# Variation in t
tseq <- seq(1970,2014,5)
bseq <- 0.01 #seq(0,0.05,0.01)
rhoseq <- 5 #seq(-10,36,3)

curves <- c()
rep <- 0

for(i in 1:length(tseq)){for(j in 1:length(bseq)){for(k in 1:length(rhoseq)){
  
  t_mdr <- tseq[i]
  b <- bseq[j]
  rho <- rhoseq[k]
  
  c <- rho * b / t_mdr
  
  x <- seq(1960, 2020,1)
  x <- x - t_mdr
  
  y <- b*(x) - c*x^2
  
  rep = rep + 1
  print(rep)
  
  year =  seq(1960, 2020,1)
  curves <- rbind(curves, cbind(rep,x, y, t_mdr,b,rho,c,year))
}}}

curves <- as.data.frame(curves)

ggplot(curves, aes(x=year, y = y, group = rep, colour = factor(t_mdr))) + geom_line(lwd = 2) + scale_y_continuous("Proportion of new TB cases\nthat are MDR-TB",limits = c(0,0.6)) + 
  geom_vline(xintercept = 2014) + scale_color_discrete("Time") 
ggsave("~/Dropbox/MDR/output/prior_curves_t.pdf")

# Variation in b
tseq <- 1970 #seq(1970,2014,5)
bseq <- seq(0,0.01,0.001)#,seq(0.01,0.2,0.05))
rhoseq <- 5 #seq(-10,36,3)

curves <- c()
rep <- 0

for(i in 1:length(tseq)){for(j in 1:length(bseq)){for(k in 1:length(rhoseq)){
  
  t_mdr <- tseq[i]
  b <- bseq[j]
  rho <- rhoseq[k]
  
  c <- rho * b / t_mdr
  
  x <- seq(1960, 2020,1)
  x <- x - t_mdr
  
  y <- b*(x) - c*x^2
  
  rep = rep + 1
  print(rep)
  
  year =  seq(1960, 2020,1)
  curves <- rbind(curves, cbind(rep,x, y, t_mdr,b,rho,c,year))
}}}

curves <- as.data.frame(curves)

ggplot(curves, aes(x=year, y = y, group = rep, colour = factor(b))) + geom_line(lwd = 2) + scale_y_continuous("Proportion of new TB cases\nthat are MDR-TB",limits = c(0,0.6)) + 
  geom_vline(xintercept = 2014) + scale_color_discrete("b") 
ggsave("~/Dropbox/MDR/output/prior_curves_b.pdf")

# Variation in rho
tseq <- 1970 #seq(1970,2014,5)
bseq <- 0.01 #seq(0,0.05,0.01)
rhoseq <- seq(-20,36,3)

curves <- c()
rep <- 0

for(i in 1:length(tseq)){for(j in 1:length(bseq)){for(k in 1:length(rhoseq)){
  
  t_mdr <- tseq[i]
  b <- bseq[j]
  rho <- rhoseq[k]
  
  c <- rho * b / t_mdr
  
  x <- seq(1960, 2020,1)
  x <- x - t_mdr
  
  y <- b*(x) - c*x^2
  
  rep = rep + 1
  print(rep)
  
  year =  seq(1960, 2020,1)
  curves <- rbind(curves, cbind(rep,x, y, t_mdr,b,rho,c,year))
}}}

curves <- as.data.frame(curves)

ggplot(curves, aes(x=year, y = y, group = rep, colour = factor(rho))) + geom_line(lwd = 2) + scale_y_continuous("Proportion of new TB cases\nthat are MDR-TB",limits = c(0,0.6)) + 
  geom_vline(xintercept = 2014) + scale_color_discrete("r") 
ggsave("~/Dropbox/MDR/output/prior_curves_r.pdf", width = 12, height = 10)



###### MULTIVARIATE *****#####################################################################################################################################
nsamples <- 100000

years = seq(1950,2020,1)
ny <- length(years)

rhoseq <- rtruncnorm(nsamples, a=-Inf, b=36, mean = 5, sd = 15) #rnorm(nsamples,mean = 5, sd = 15)
bseq <- rlnorm(nsamples,-5.5, 0.7);

mm <- cbind(rep(rhoseq, each = ny),rep(bseq, each = ny))
colnames(mm)<- c("r","b")
mm <- as.data.frame(mm)

mm$year <- years
mm$years <- years - tseq
mm$rep <- rep(1:nsamples,each = ny)

###**** different t = 1975 **##################################################################################################################################
tseq <- 1975

mm$c <- mm$r*mm$b / tseq
mm$y <- mm$b * mm$years - mm$c * mm$years * mm$years

curves <- as.data.frame(mm)

curves.r.95 <- curves %>%
  group_by(year) %>%
  dplyr::summarise(median=median(y), min=quantile(y, 0.025), max=quantile(y, 0.975)) %>%
  group_by(n=1:n()) %>% dplyr::mutate(median=max(median, 0), min=max(min, 0), max=max(max, 0)) %>% ungroup 

curves.r.80 <- curves %>%
  group_by(year) %>%
  dplyr::summarise(median=median(y), min=quantile(y, 0.1), max=quantile(y, 0.9)) %>%
  group_by(n=1:n()) %>% dplyr::mutate(median=max(median, 0), min=max(min, 0), max=max(max, 0)) %>% ungroup 

curves.r.50 <- curves %>%
  group_by(year) %>%
  dplyr::summarise(median=median(y), min=quantile(y, 0.25), max=quantile(y, 0.75)) %>%
  group_by(n=1:n()) %>% dplyr::mutate(median=max(median, 0), min=max(min, 0), max=max(max, 0)) %>% ungroup 

ggplot(curves.r.95, aes(x=year)) +
  geom_line(aes(y=median)) +
  geom_ribbon(aes(ymin=min, ymax=max), alpha=0.3, fill = "red") +
  geom_vline(xintercept=2014, linetype="dashed") +
  geom_ribbon(data = curves.r.50,aes(ymin=min, ymax=max), alpha=0.3, fill = "blue")  + 
  geom_ribbon(data = curves.r.80,aes(ymin=min, ymax=max), alpha=0.3, fill = "green") +
  expand_limits(x = 1950, y = 0.8) + 
  scale_y_continuous("Proportion of new TB cases\nthat are MDR-TB")
ggsave("~/Dropbox/MDR/output/priors_rhotrunc5_1975.pdf")

ggplot(curves[1:(50*ny),], aes(x = year, y = y, group = rep)) + geom_line() +
  geom_vline(xintercept=2014, linetype="dashed") +
  expand_limits(x = 1950, y = 0) + 
  scale_y_continuous(limits = c(0,1),"Proportion of new TB cases\nthat are MDR-TB")
ggsave("~/Dropbox/MDR/output/priors_examples_rhotrunc5_1975.pdf")

###**** different t = 1985 **##################################################################################################################################
mm <- as.data.frame(mm)
tseq <- 1985
mm$years <- mm$year - tseq

rhoseq <- rtruncnorm(nsamples, a=-Inf, b=36, mean = 5, sd = 25) #rnorm(nsamples,mean = 5, sd = 15)
bseq <- rlnorm(nsamples,-6.5, 0.6);

mm$r <- rep(rhoseq, each = ny)
mm$b <- rep(bseq, each = ny)

mm$c <- mm$r*mm$b / tseq
mm$y <- mm$b * mm$years - mm$c * mm$years * mm$years

curves <- as.data.frame(mm)

curves.r.95 <- curves %>%
  group_by(year) %>%
  dplyr::summarise(median=median(y), min=quantile(y, 0.025), max=quantile(y, 0.975)) %>%
  group_by(n=1:n()) %>% dplyr::mutate(median=max(median, 0), min=max(min, 0), max=max(max, 0)) %>% ungroup 

curves.r.80 <- curves %>%
  group_by(year) %>%
  dplyr::summarise(median=median(y), min=quantile(y, 0.1), max=quantile(y, 0.9)) %>%
  group_by(n=1:n()) %>% dplyr::mutate(median=max(median, 0), min=max(min, 0), max=max(max, 0)) %>% ungroup 

curves.r.50 <- curves %>%
  group_by(year) %>%
  dplyr::summarise(median=median(y), min=quantile(y, 0.25), max=quantile(y, 0.75)) %>%
  group_by(n=1:n()) %>% dplyr::mutate(median=max(median, 0), min=max(min, 0), max=max(max, 0)) %>% ungroup 

ggplot(curves.r.95, aes(x=year)) +
  geom_line(aes(y=median)) +
  geom_ribbon(aes(ymin=min, ymax=max), alpha=0.3, fill = "red") +
  geom_vline(xintercept=2014, linetype="dashed") +
  geom_ribbon(data = curves.r.50,aes(ymin=min, ymax=max), alpha=0.3, fill = "blue")  + 
  geom_ribbon(data = curves.r.80,aes(ymin=min, ymax=max), alpha=0.3, fill = "green") +
  expand_limits(x = 1950, y = 0.8) + 
  scale_y_continuous("Proportion of new TB cases\nthat are MDR-TB")
ggsave("~/Dropbox/MDR/output/priors_rhotrunc5_1985.pdf")





# DATA
ggplot(who0.r.95, aes(x=year)) +
  geom_line(aes(y=median/100)) +
  geom_ribbon(aes(ymin=min/100, ymax=max/100), alpha=0.3, fill = "red") +
  geom_vline(xintercept=2014, linetype="dashed") +
  geom_ribbon(data = who0.r.50,aes(ymin=min/100, ymax=max/100), alpha=0.3, fill = "blue")  + 
  geom_ribbon(data = who0.r.80,aes(ymin=min/100, ymax=max/100), alpha=0.3, fill = "green") + 
  expand_limits(x = 1950, y = 0.8) + 
  scale_y_continuous("Proportion of new TB cases\nthat are MDR-TB")
ggsave("~/Dropbox/MDR/output/WHO_data_ranges.pdf")

