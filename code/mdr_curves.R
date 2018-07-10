### Generate the trends in MDR to use
# stores as csv to be read in for others
# Linear
# Sigmoid
# Quadratic

library(ggplot2)

#### Linear curves to use *****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####
lin <- function(jump, max){c(rep(0,jump-1934),seq(0,max,length=(2014-(jump-1))))}
save_lin <- c()
index = 1
jump_point <- seq(1970,2010,5)

for(i in 1:length(jump_point)){
  plot(  seq(1934,2014,1),lin(jump_point[i],0.02) )
  save_lin <- rbind(save_lin,
                    cbind(index, i, jump_point[i],seq(1934,2014,1),lin(jump_point[i],0.02)))
  index = index + 1
}

save_lin <- as.data.frame(save_lin)
colnames(save_lin) <-c("index","i","jump","year","out")
save_lin$tilt <- 0
save_lin$type <- 0

#### Quadratic curves to use*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****##
quad <- function(x,maxv){ 
  ll <-length(x)
  if(ll < 81){
    ff <- maxv/(ll^2)*(x^2)
    ff2 <- c(matrix(0,81-ll,1),ff)
  }else{ff2 <- maxv/(ll^2)*(x^2)}
  # 
  # 
  # w<-which( x < when)
  # if(length(w)>0){
  #   print("here")
  #   ff <- matrix(0,length(w),1)
  #   ff2 <-c(ff,max/((80-when)^2)*(x[w]^2))
  # } else{ ff2 <-max/((80-when)^2)*(x^2) }
  return(ff2)
} 


plot( seq(1934,2014,1),quad(seq(0,50,1),0.02))

# when increase after 1934? 1975, 1980, 1985, 1990, 1995, 2000
lengthx = seq(5,40,1)

save_quads <- c()
index = 1
for(i in 1:length(lengthx)){
  plot(  seq(1934,2014,1),quad(seq(0,lengthx[i],1),0.02))
  
  if(quad(seq(0,lengthx[i],1),0.02)[37] < 0.0002){
    if(quad(seq(0,lengthx[i],1),0.02)[81] > (0.02-0.002)){
      save_quads <- rbind(save_quads,
                          cbind(index,i,seq(1934,2014,1),quad(seq(0,lengthx[i],1),0.02)))
      index = index + 1
    }
  }
  
}

save_quads <- as.data.frame(save_quads)
colnames(save_quads) <-c("index","i","year","out")

ggplot(save_quads, aes(x=year,y=out,group=index, colour=index)) +  
  geom_line() +  geom_vline(xintercept = 1970)

#### Sigmoid curves to use *****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####
sigm <- function(x,delta,max,B=1){ max/(1+exp(-B*(x-delta)))} 

# when increase after 1934? 1975, 1980, 1985, 1990, 1995, 2000
jump_point <- seq(41,76,1) #c(41,46,51,56,61,66,71,76)
# tilt-how much of a gradient?
tilt <-seq(0.01,2,0.05)

save_jps <- c()
index = 1
for(i in 1:length(jump_point)){
  plot(  seq(1934,2014,1),sigm(seq(0,80,1),jump_point[i],0.02,1))
  
  for(j in 1:length(tilt)){
    points(seq(1934,2014,1),sigm(seq(0,80,1),jump_point[i],0.02,tilt[j]),type="l")
    abline(v = 1970,lty="dashed")
    if(sigm(seq(0,80,1),jump_point[i],0.02,tilt[j])[37] < 0.0002){
      if(sigm(seq(0,80,1),jump_point[i],0.02,tilt[j])[81] > (0.02-0.002)){
        save_jps <- rbind(save_jps,
                          cbind(index, i,j,seq(1934,2014,1),sigm(seq(0,80,1),jump_point[i],0.02,tilt[j])))
        index = index + 1
      }
    }
    
  }
}
save_jps <- as.data.frame(save_jps)
colnames(save_jps) <-c("index","jump","tilt","year","out")

ggplot(save_jps, aes(x=year,y=out,group=index,colour = factor(tilt))) + 
  geom_line() + facet_wrap(~jump) + geom_vline(xintercept = 1970)

ggplot(save_jps, aes(x=year,y=out,group=index,colour = factor(tilt))) + 
  geom_line() +  geom_vline(xintercept = 1970)



#### Plateuing sigmoid curves*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####*****####
## fdsig-base(t)=1 / 1+exp(−a′1(t−t′mid1))11+exp(−a′2(t−t′mid2)). From "Sicegar" package
# I(t)=(Imax−Iinite−a1(t−tmid1)+1+Iinit)(1−Ifinalea2(t−tmid2)+1+Ifinal)
sigm_plateau <- function(x, delta1, delta2, max, final, A = 1, B = 1){ (max / (1 + exp(-A*(x-delta1))) ) * (final + (1 - final)/(1 + exp(B*(x-delta2)) ))} 
inv_sigm <- function(x, delta2, B, final){ (final + (1 - final)/(1 + exp(B*(x-delta2)) ))}

plot(  seq(1934,2014,1),sigm_plateau(seq(0,80,1),jump_point[1], (15 + jump_point[1]),0.05, 0.02 / 0.05, 0.5,0.5), type = "l")
abline(v = 1970,lty="dashed")

# variables = gap in when drop happens // max height (up to 10 times higher?) // tilt for each side

# when increase after 1934? 1975, 1980, 1985, 1990, 1995, 2000
jump_point <- c(41,46,51,56,61,66,71,76) # do every 5 years
# tilt-how much of a gradient?
tilt <-seq(0.01,2,0.2) # halve the gap
# gap
gap <- seq(15,25,5) # less than 15 is not a real jump
# height
heig <- seq(2,4,1)


save_jps_plat <- c()
index = 1
for(i in 1:length(jump_point)){
  #  plot(  seq(1934,2014,1),sigm_plateau(seq(0,80,1),jump_point[1], (10 + jump_point[1]),0.05, 0.02 / 0.05, 5,5))
  #  abline(v = 1970,lty="dashed")
  
  for(j in 1:length(tilt)){
    for(k in 1:length(tilt)){
      for(g in 1:length(gap)){
        for(h in 1:length(heig)){
          sg <- sigm_plateau(seq(0,80,1),jump_point[i], (gap[g] + jump_point[i]),heig[h] * 0.02, 0.02 / (heig[h]*0.02), tilt[j],tilt[k])
          # points(seq(1934,2014,1),sg,type="l")
          
          if(sg[37] < 0.0002){
            if(sg[81] > (0.02-0.002)){
              if(sg[81] < (0.02+0.002)){
                save_jps_plat <- rbind(save_jps_plat,
                                  cbind(index, i,j,k,g,h,seq(1934,2014,1),sg))
                index = index + 1
              }
            }
          }
        }
      }
    }
  }
}
save_jps_plat <- as.data.frame(save_jps_plat)
colnames(save_jps_plat) <-c("index","jump","tilt1","tilt2","dist_up","max_up","year","out")

ggplot(save_jps_plat, aes(x=year,y=out,group=index,colour = interaction(factor(tilt1), factor(tilt2)))) + 
  geom_line() + facet_wrap(~jump) + geom_vline(xintercept = 1970)

ggplot(save_jps_plat, aes(x=year,y=out,group=index,colour = factor(tilt1))) + 
  geom_line() +  geom_vline(xintercept = 1970)

##*********** Together ************************************************************************************************************************************************************######
## Save
save_quads$tilt = 0
save_quads$jump = 0
save_jps$i = 0
save_jps$type = 1
save_quads$type = 2
save_quads$tilt1 = 0
save_quads$tilt2 = 0
save_quads$dist_up = 0
save_quads$max_up = 0

save_jps$tilt1 = 0
save_jps$tilt2 = 0
save_jps$dist_up = 0
save_jps$max_up = 0

save_lin$tilt1 = 0
save_lin$tilt2 = 0
save_lin$dist_up = 0
save_lin$max_up = 0

save_jps_plat$tilt = 0
save_jps_plat$i = 0
save_jps_plat$type = 3

save_curves <- rbind(save_lin, save_jps, save_quads, save_jps_plat)

setwd("~/Dropbox/MRC SD Fellowship/Research/MDR/Latent MDR/Data/")
write.csv(save_curves,"lin_sig_quad_sigd_curves.csv")


ggplot(save_curves, aes(x=year, y = out, group = index)) + geom_line(aes(col=factor(type))) + 
  facet_wrap(~type) + geom_vline(xintercept = 1970)
ggsave("mdr_curves_lin_sig_quad_sigd.pdf")



