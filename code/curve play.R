#### Curve play

library(ggplot2)
library(boot)
x <- seq(0,34, 1)

# Time when MDR > 0
t_mdr <- 0
# SD
#24 - (1.96)(s) = 14 # 1970 earliest
#24 + (1.96)(s) = 34 # 1990 latest

s_mdr <- (24-14)/(1.96) # 5.1 

# Slope of linear
b <- 0

# Slope of quadratic
c <- 0

# rho = t_mdr*c / b

curves <- c()

for(i in 1:100){
  
  t_mdr <- rnorm(1, 10, s_mdr) # sample time to MDR > 0
  
  b <- rnorm(1, 0,0.5)
  
  rho <- runif(1, 0,1)  
  
  c <- b * rho / t_mdr
  
  y <- b*x - c*x^2
  
  curves <- rbind(curves, cbind(i,x, y,t_mdr,b,rho,c))
  
}

curves <- as.data.frame(curves)

ggplot(curves, aes(x=x, y = y, group = i, colour = i)) + geom_line()


#### sequence
tseq <- 34 #seq(5,24,5)
bseq <- seq(0,0.05,0.01)
rhoseq <- seq(0,1,0.1)

curves <- c()
rep <- 0

for(i in 1:length(tseq)){
  for(j in 1:length(bseq)){
    for(k in 1:length(rhoseq)){
      
      t_mdr <- tseq[i]
      
      b <- bseq[j]
      
      rho <- rhoseq[k]
      
      c <- rho * b / t_mdr
      
      x <- matrix(0,1,45 - t_mdr)
      x <- c(x,seq(1,t_mdr,1))
       
        y <- b*(x) - c*x^2 
      
        rep = rep + 1
        
        year = seq(1970,2014,1)
        curves <- rbind(curves, cbind(rep,x, y,t_mdr,b,rho,c,year))
      
    }
  }
}

curves <- as.data.frame(curves)

ggplot(curves, aes(x=x, y = y, group = rep, colour = rep)) + geom_line() 
ggplot(curves, aes(x=year, y = y, group = rep, colour = rep)) + geom_line() 
ggsave("~/Dropbox/MDR/output/prior_curves_34.pdf")

#### sequence
tseq <- seq(1970,2014,5)
bseq <- seq(0,0.05,0.01)
rhoseq <- seq(0,1,0.1)

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

ggplot(curves, aes(x=x, y = y, group = rep, colour = rep)) + geom_line() 
ggplot(curves, aes(x=year, y = y, group = rep, colour = rep)) + geom_line() 
ggplot(curves, aes(x=year, y = yi, group = rep, colour = rep)) + geom_line() +ggtitle("INVlog")
ggplot(curves, aes(x=x, y = yi, group = rep, colour = rep)) + geom_line() +ggtitle("INVlog")
ggsave("~/Dropbox/MDR/output/prior_curves_more.pdf")
