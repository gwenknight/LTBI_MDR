### Generate the trends in MDR to use

library(ggplot2)

#### Linear curves to use
ari_store[82:162,"mdr"] <-c(rep(0,1970-1934),seq(0,mdr_last0,length=(2014-1969))) * rr$Av


#### Sigmoid curves to use

library(ggplot2); library(mc2d)

home <- "~/Documents/LTBI_MDR/"
output <- "~/Documents/LTBI_MDR/output/"

## Function
sigm <- function(x,delta,max,B=1){ max/(1+exp(-B*(x-delta)))} 

# when increase after 1934? 1975, 1980, 1985, 1990, 1995, 2000
jump_point <- c(41,46,51,56,61,66,71,76)
# tilt-how much of a gradient?
tilt <-seq(0.01,1,0.05)

save_jps <- c()
index = 1
for(i in 1:length(jump_point)){
  plot(  seq(1934,2014,1),sigm(seq(0,80,1),jump_point[i],0.02))
  
  for(j in 1:length(tilt)){
    points(seq(1934,2014,1),sigm(seq(0,80,1),jump_point[i],0.02,tilt[j]),type="l")
    abline(v = 1970,lty="dashed")
    if(sigm(seq(0,80,1),jump_point[i],0.02,tilt[j])[37] < 0.0002){ # 1970 ~ 0
      if(sigm(seq(0,80,1),jump_point[i],0.02,tilt[j])[81] > 0.02 - 0.0002){ # 2014 ~ 0.02
        save_jps <- rbind(save_jps,
                          cbind(index, i,j,seq(1934,2014,1),sigm(seq(0,80,1),jump_point[i],0.02,tilt[j])))
        index = index + 1
      }
    }
  }
}
save_jps <- as.data.frame(save_jps)
colnames(save_jps) <-c("index","jump","tilt","year","out")

setwd(output)
ggplot(save_jps, aes(x=year,y=out,group=index,colour = factor(tilt))) + 
  geom_line() + facet_wrap(~jump) + geom_vline(xintercept = 1970)

ggplot(save_jps, aes(x=year,y=out,group=index,colour = factor(tilt))) + 
  geom_line() +  geom_vline(xintercept = 1970)
ggsave("sigmoid_curves_include.pdf")

#### Quadratic curves to use
quad <- function(x,maxv){ 
  ll <-length(x)
  if(ll < 81){
    ff <- maxv/(ll^2)*(x^2)
    ff2 <- c(matrix(0,81-ll,1),ff)
  }else{ff2 <- maxv/(ll^2)*(x^2)}
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



##### ALSO ADD IN TRIANGULAR CDF: https://en.wikipedia.org/wiki/Triangular_distribution
#### Triangular curves to use
tri <- function(x,mini,modi,maxi){ 
  ll <-length(x)
  if(ll < 81){
    ff  <- suppressWarnings(ptriang(x, min=mini, mode=modi, max=maxi))
    ff2 <- c(matrix(0,81-ll,1),ff)
    if(any(is.na(ff))){ff2 <- NaN}
  }else{ff2 <- suppressWarnings(ptriang(x, min=mini, mode=modi, max=maxi))}
  return(ff2)
} 

mmax <- 0.02
plot( seq(1934,2014,1),mmax*tri(seq(0,45,1),1,12,30))
lines( seq(1934,2014,1),mmax*tri(seq(0,45,1),1,5,30))

# when increase after 1970? 1975, 1980, 1985, 1990, 1995, 2000
tri_start <- seq(44,5,-5)
min_start <- seq(0,50,5)
mod_start <- seq(0,50,5)
max_start <- seq(0,50,5)

save_tjps <- c()
index = 1
for(i in 1:length(tri_start)){
  for(ij in 1:length(min_start)){
    for(ik in 1:length(mod_start)){
      for(il in 1:length(max_start)){
        
        tt <- tri(seq(0,lin_start[i],1),min_start[ij],mod_start[ik],max_start[il])
        
        if(!is.na(tt) && sum(tt) > 0){
          
          #  plot(seq(1934,2014,1),0.02*tri(seq(0,lin_start[i],1),min_start[ij],mod_start[ik],max_start[il]))
          
          if(sum(tt[1:40]) > 0 && !any(tt[1:72]>0.98)){ # remove those greater than 0 before 1970 and greater than 0.98 before 2005
            
            save_tjps <- rbind(save_tjps,
                               cbind(index, i,ij,ik,il,seq(1934,2014,1),tt))
          }
        }
        
        index = index + 1
        print(index)
      }
    }
  }
  
}

save_quads <- as.data.frame(save_quads)
colnames(save_quads) <-c("index","i","year","out")

ggplot(save_quads, aes(x=year,y=out,group=index, colour=index)) +  
  geom_line() +  geom_vline(xintercept = 1970)

## Together
ggplot(save_jps, aes(x=year,y=out,group=index,colour = factor(tilt))) + 
  geom_line() +  geom_vline(xintercept = 1970) + 
  geom_line(data = save_quads,aes(x=year,y=out,group=index, colour=factor(index)))

## Save
save_quads$tilt = 0
save_quads$jump = 0
save_jps$i = 0
save_jps$type = 1
save_quads$type = 2

save_curves <- rbind(save_jps, save_quads)
setwd("~/Dropbox/MRC SD Fellowship/Research/MDR/Latent MDR/Data/")
write.csv(save_curves,"sig_quad_curves.csv")

  save_tjps <- as.data.frame(save_tjps)
colnames(save_tjps) <-c("index","start","mini","modi","maxi","year","out")

ggplot(save_tjps, aes(x=year,y=out,group=index,colour = factor(start))) + 
  geom_line() +  geom_vline(xintercept = 1970)

ggplot(save_tjps, aes(x=year,y=out,group=index,colour = factor(maxi))) + 
  geom_line() +  geom_vline(xintercept = 1970) + facet_wrap(start ~ mini)
ggsave("lin_curves_include.pdf")

w<-intersect(which(save_tjps$start == 1),which(save_tjps$mini == 2))
ggplot(save_tjps[w,], aes(x=year,y=out,group=index,colour = factor(start))) + 
  geom_line() +  geom_vline(xintercept = 1970) + facet_wrap(start ~ mini)

### Remove those that are > 0 before 1970
w<-which(save_tjps$year < 1972)
w1<-which(save_tjps[w,"out"] > 0)
nsave_tjps <- save_tjps[-w1,]
ggplot(nsave_tjps, aes(x=year,y=out,group=index,colour = factor(start))) + 
  geom_line() +  geom_vline(xintercept = 1970)
### Remove those that are 0.02 too soon
w<-which(nsave_tjps$year < 2000)
w1<-which(nsave_tjps[w,"out"] > 0.98)
nsave_tjps <- nsave_tjps[-w1,]
