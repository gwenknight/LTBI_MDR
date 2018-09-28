#### Curve play


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
tseq <- 10 #seq(0,20,1)
bseq <- seq(0,100,1)
cseq <- seq(0,10,1)
curves <- c()
rep <- 0
for(i in 1:length(tseq)){
  for(j in 1:length(bseq)){
    for(k in 1:length(cseq)){
      
      t_mdr <- tseq[i]
      
      b <- bseq[j]
      
      c <- cseq[k]
      
      if(c > (b-0.05)/2){
        y <- b*(x-t_mdr) - c*(x-t_mdr)^2 
        
        rep = rep + 1
        curves <- rbind(curves, cbind(rep,x, y,t_mdr,b,rho,c))
      }
    }
  }
}

curves <- as.data.frame(curves)

ggplot(curves, aes(x=x, y = y, group = rep, colour = rep)) + geom_line() + scale_y_continuous(lim=c(0,30))




