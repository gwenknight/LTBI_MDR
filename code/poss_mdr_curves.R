### Function for curve generation

ari_mdr <- function(coun, rr){
  ### cn = country
  ### rr = ARI for DS, average over those from Dodd&Houben
  
  ### mdr levels
  # For now use this as what is at 2018 but could change
  mdr_last_v <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/data/mdr_last.csv")[,-1]
  mdr_last <- mdr_last_v[which(mdr_last_v$iso3 == coun),]
  mdr_last0 <- mdr_last$mdr_new_pcnt / 100 # convert to proportion
  
  # How many curves?
  nari = 130
  ari_store <- as.data.frame(matrix(0,nari*81,2))
  colnames(ari_store) <- c("ds","mdr")
  
  ### ARI MDR
  ## 1. no MDR
  ari_store[1:81,"ds"] <- rr$Av
  ari_store[1:81,"mdr"] <- 0
  
  ## Read in 120 others 
  save_curves <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/Latent MDR/Data/lin_sig_quad_curves.csv")[-1]
  # modify peak mdr to that of country
  save_curves$out <- mdr_last0 * save_curves$out/0.02 # 0.02 used to generate save_curves
  ari_store[82:(81+81*129),"ds"] = rep(rr$Av, 129)
  ari_store[82:(81+81*129),"mdr"] = save_curves$out * rr$Av
  
  ari_store$time <- rep(seq(1934, 2014, 1),130)
  ari_store$rep <- rep(seq(1,130,1),each = 81)
  ari_store$type <- rep(c(1,rep(2,each = 9),rep(3,each = 98),rep(4,each = 22)),each = 81) # 1 = no mdr, 2 = linear increase, 3 = sigma, 4 = quadratic
  
  return(ari_store)
}

## Check
#ari_s <- ari_mdr("IND",rr)

#ggplot(ari_s, aes(x=time, y = mdr, group = rep)) + geom_line(aes(col=factor(type)))

