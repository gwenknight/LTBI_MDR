### Priors_plot

####### Priors:
### tm  ****######################################################################
# original: tm <- normal(1980,5)
#tseq <- rnorm(100,mean = 1980, sd = 5)
tprop <- rnorm(10000,mean = 1985, sd = 9)
plot(density(tprop))
quantile(tprop,probs=c(0.05, 0.95))


tseq <- 1980

### b  ****######################################################################
# original: b <- uniform(0,0.05) 
bseq <- runif(30,min = 0, max = 0.05)

# mean = exp(mu + sd^2/2): want it at 1980 => log(1980) - sd^2 / 2 = mu
bseq <- rlnorm(10000000, meanlog = log(0.0008), sdlog = 0.8)
hist(bseq)
mean(bseq)
plot(density(bseq))
quantile(bseq) # DATA: #25-75: 0.0003 - 0.001: this is close: 0.0004 - 0.001
quantile(bseq, prob = c(0.05, 0.95)) # DATA: #25-75: 0.0003 - 0.001: this is close: 0.0004 - 0.001
plot(quantile(bseq)[1:4]) 
max(bseq) # hits 0.05

### rho ****######################################################################
# original: rho  <- uniform(0,1)
rhoseq <- rnorm(100000,mean = 15, sd = 5)
plot(density(rhoseq))
quantile(rhoseq, prob = c(0.05, 0.95)) # DATA: #25-75: 0.0003 - 0.001: this is close: 0.0004 - 0.001
max(rhoseq)

curves <- c()
rep <- 0

for(i in 1:length(tseq)){
  for(j in 1:length(bseq)){
    for(k in 1:length(rhoseq)){
      
      t_mdr <- tseq[i]
      
      b <- bseq[j]
      
      rho <- rhoseq[k]
      
      c <- rho * b / t_mdr
      
      x <- seq(1950, 2020,1)
      x <- x - t_mdr
      
      y <- b*(x) - c*x^2
      yi <- inv.logit(b*(x) - c*x^2) - 0.5
      
      rep = rep + 1
      print(rep)
      
      year = seq(1950,2020,1)
      curves <- rbind(curves, cbind(rep,x, y, yi, t_mdr,b,rho,c,year))
      
    }
  }
}

curves <- as.data.frame(curves)

ggplot(curves, aes(x=year, y = y, group = rep, colour = rep)) + geom_line() 

ggplot(curves, aes(x=year, y = y, group = rep, colour = rep)) + geom_line() + scale_y_continuous(lim = c(0,1))

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
    geom_point(data=who_l, aes(y=mdr_new)) +
    geom_errorbar(data=who_l, aes(ymin=mlo, ymax=mhi)) +
    geom_vline(xintercept=2014, linetype="dashed") +
  geom_ribbon(data = curves.r.50,aes(ymin=min, ymax=max), alpha=0.3, fill = "blue")  + 
  geom_ribbon(data = curves.r.80,aes(ymin=min, ymax=max), alpha=0.3, fill = "green") 

ggplot(curves, aes(rho)) + geom_histogram(binwidth = 0.1)
ggplot(curves, aes(c)) + geom_histogram()
