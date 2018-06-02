#### Cohort model for MDRLTBI

cohort_ltbi <- function(ari,pop){
  ### inputs 
  # ari: 2 columns
  # pop: population in 2014
  
  #### Protection from re-infecton
  ## Andrews: 0.79 .7-.86 
  pm <- 0.79                      # Mean        #0.5 #CHANGE HERE!
  pv <- (0.86-0.7)^2/3.92^2
  apb <- pm*(1-pm)/pv-1
  # Shape parameters
  pa <- pm*apb                            #77.88
  pb <- (1-pm)*apb                        #20.70
  
  ### Random sample of level of protection - beta distribution
  # same for dr/ds
  alph <- rbeta(100*81,shape1=pb,shape2=pa)
  dim(alph) <-  c(81, 100)
  
  # Matrix framework
  c_last <- as.data.frame(matrix(0,100,6))
  colnames(c_last) <- c("pr_ds","pr_dr","new_ds","new_dr","rei_sr","rei_rs")
  c_now <- as.data.frame(matrix(0,100,6))
  colnames(c_now) <- c("pr_ds","pr_dr","new_ds","new_dr","rei_sr","rei_rs")
  store_c <- as.data.frame(matrix(0,100*81,6))
  colnames(store_c) <- colnames(c_last)
  
  # Number of years (1934:2014)
  nyrs <- 81
  
  ## Initial conditions
  # hazard = 
  H = seq(0,99,1) * as.numeric(ari[1,"ds"]) # constant ARI pre-1934
  c_last[,"pr_ds"] <- 1 - exp(-H)
  
  for(i in 1:nyrs){
    c_now[1,] <- 0
    c_now[2:100,] <- c_last[1:99,] # New time, 100+yos not included
    
    # Add together existing with new proportions ds / dr
    c_now$pr_ds <- c_now$pr_ds + c_now$new_ds + c_now$rei_rs 
    c_now$pr_dr <- c_now$pr_dr + c_now$new_dr + c_now$rei_sr
    # Set new to zero
    c_now[,c("new_ds","rei_rs","new_dr","rei_sr")] <- 0
    
    # New calculations
    # ARI for this year
    ari_s <- as.numeric(ari[i,"ds"])
    ari_r <- as.numeric(ari[i,"mdr"])
    # Calculations
    c_now$new_ds <- ari_s * (1 - c_now$pr_ds - c_now$pr_dr) # currently none, new infection DS
    c_now$new_dr <- ari_r * (1 - c_now$pr_ds - c_now$pr_dr - c_now$new_ds) # currently none, new infection DR
    c_now$rei_sr <- c_now$pr_ds * ari_r * alph[i,] 
    c_now$rei_rs <- c_now$pr_dr * ari_s * alph[i,]
    
    ### Store and update
    c_last <- c_now  
    store_c[((i-1)*100 + 1):(i*100),] <- c_now
  }
  
  # Add in year
  store_c$year <- rep(1934:2014,each = 100)
  
  ## Capture last one
  c_2014 <- c_last
  c_2014$pr_ds <- c_2014$pr_ds + c_2014$new_ds + c_2014$rei_rs 
  c_2014$pr_dr <- c_2014$pr_dr + c_2014$new_dr + c_2014$rei_sr
  
  #Number in each
  # group by 5 yr to 80+ 
  c_ds_age <- colwise(mean)(as.data.frame(matrix(c_2014$pr_ds, 5)))
  ds_age <- as.numeric(c(c_ds_age[1:16], mean(as.numeric(c_ds_age[17:20]))))
  c_dr_age <- colwise(mean)(as.data.frame(matrix(c_2014$pr_dr, 5)))
  dr_age <- as.numeric(c(c_dr_age[1:16], mean(as.numeric(c_dr_age[17:20]))))
  
  prop_ds <- pop * ds_age
  prop_dr <- pop * dr_age
  
  perc_ds <- 100*prop_ds / sum(pop)
  perc_dr <- 100*prop_dr / sum(pop)
  
  combs <- as.data.frame(cbind(prop_ds,prop_dr,perc_ds,perc_dr))
  colnames(combs)<- c("prop_ds","prop_dr","perc_ds","perc_dr")
  
  return(list(store_c = store_c, c_2014 = c_2014,combs = combs ))
  
}

