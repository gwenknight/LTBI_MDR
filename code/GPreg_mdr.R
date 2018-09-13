## Analysis code for Houben & Dodd 2016, distributed under CC BY 4.0 license https://creativecommons.org/licenses/by/4.0/

### GK: Gaussian Process regression to generate ARIs for countries without data and for uncertainty
## Want to find the correct properties of functions that will go through the data
## Fits GP models to data - uses Maximum Likelihood to optimise parameters

#args <- commandArgs(TRUE) # used if running on cluster... 
#jj <- as.numeric(args[1]) # counter
#print(jj)

library(MASS)
library(Matrix)

#### GK: to run on desktop reorganised the below
setwd("~/Documents/LTBI_MDR/")

####**** FUNCTIONS ********************************************************************************************************************************************************************#####
## 
getKKtonly <- function(t1,t2,k=function(x,y) exp(-abs(x-y)),Wm){
  K <- outer(t1,t2,FUN=k)
  K
}                                       #make K(X,Y) matrices

## Function to do double-backsolve inversion given cholesky decomp
## Cholesky decomposition: takes Hermitian, positive-definite matrix into a product of a 
## lower triangular matrix and its conjugate transpose (for efficiency)
bsi <- function(CH,v) backsolve(CH,backsolve(CH, v, transpose = TRUE))

## Function to generate a spare
## this is basically 1,t x I(country==j)
getHtonly <- function(t,n=1){             #n is highest power
  H <- Matrix(0,nrow=(n+1),ncol=length(t)) #3 for 1,t,t^2
  for(k in 0:n)
    H[1 + k,] <- t^k
  as(H,'sparseMatrix')
}

## Function to generate the sum of the diaganol (trace)
tr <- function(x) sum(diag(x))

## Function to give exponential of first element / half of second
## gets s_k^2, L, tscale
PRMconvert2 <- function(x) c( exp(x[1]), exp(x[2]/2))


## 
getMLnGradT <- function(x,grad=TRUE){                    # see Eqn 2.45 in R&W
  ## preliminaries
  ## -- covariances --
  a <- x[1]; b <- x[2]
  
  # outer product = row each in first, column each in second
  # here tdz = data points 
  # Is this the squared exponential kernel for the covariance? 
  K <- outer(tdz,tdz,FUN=function(x,y) exp(a-exp(-b)*(x-y)^2) )
  K <- Matrix(K)                      #kxx
  K2 <- outer(tdz,tdz,FUN=function(x,y) (x-y)^2*exp(a-exp(-b)*(x-y)^2) )
  K2 <- Matrix(K2)                     #dK/da
  ## -- derived matrices (as used above) --
  ## new version
  cvy <- K + sigz
  U <- chol(cvy)
  Uy <- backsolve(U, y, transpose = TRUE)
  ky <- backsolve(U, Uy) 
  hky <- H %*% ky
  AM <- symmpart(H %*% bsi(U,t(H)))
  V <- chol(AM)
  Vy <- backsolve(V, hky, transpose = TRUE)
  ## -- marginal loglikelihood --
  LML <- -sum(Uy^2) + sum(Vy^2) #data likelihood
  LML <- LML - 2*sum(log(diag(U)))
  LML <- LML - 2*sum(log(diag(V)))
  LML <- LML/2
  ## extras for dLML
  VVy <- backsolve(V, Vy)
  ympy <- bsi(U, t(H) %*% VVy)       #K^{-1} x ...
  dLML <- NULL
  if(grad){
    ## -- gradient --
    ## --- gradient helper function ---
    dHelp <- function(dK){              #takes the local vars from up-level
      dML <- t(ky) %*% dK %*% ky + t(ympy) %*% dK %*% ympy
      dML <- dML - 2*t(ympy) %*% dK %*% ky
      tmp <- bsi(U,dK)        #K^{-1}dK
      dML <- dML - tr(tmp)
      tmp <- bsi(V,H%*%tmp)
      tmp <- bsi(U, t(H)%*%tmp)
      dML <- dML + tr(tmp)  
      return(as.numeric(dML/2))
    }
    ## --- get gradient ---
    dLML <- c( dHelp(K), dHelp(K2) )
  }
  if(LML>0){LML <- -1e3; dLML <- -1e3*rep(1,2)}
  ## return
  return(list(LML=LML,dLML=dLML))
}


### Generates the prediction? 
# inputs exp of predicted parameters / datatimes / times to extrapolate /  All$lr_ari / square of error
getPredztonly <- function(x,tdz,tez,y,Vz){
  ## from here
  usek <- function(i,j)x[1]*exp(-abs(i-j)^2/x[2]^2)
  ## H
  H <- getHtonly(tdz,n=lin)
  Hs <- getHtonly(tez,n=lin)
  ## make matrices
  kxx <- getKKtonly(tdz,tdz,k=usek)
  kxxs <- getKKtonly(tdz,tez,k=usek)
  kxsx <- t(kxxs)
  kxsxs <- getKKtonly(tez,tez,k=usek)
  sigz <- Diagonal(x=Vz)                         #noise
  covy <- kxx + sigz
  U <- chol(covy)
  ## regress
  reg <- 1                                #flag
  HKH <- H %*% bsi(U,t(H))
  V <- chol(symmpart(HKH))                       
  R <- Hs - H %*% bsi(U,kxxs)
  mn <- 0
  y <- y-mn
  ## mean/covar
  mf <- kxsx %*% bsi(U,y)          #mean prediction
  cf <- kxsxs  - kxsx %*% bsi(U,kxxs)
  ## mean stuff
  bbar <- bsi(V,(H %*% bsi(U,y)))
  mg <- mf + reg * t(R) %*% bbar
  if(nrow(V)>1)
    cg <- cf + reg * t(R) %*% bsi(V,R)
  else
    cg <- cf + reg * t(R) %*% (R/V[1,1])
  ## return
  return(list(mg=mg,cg=cg))
}



####**** Load data ********************************************************************************************************************************************************************#####
load('datar/All_mdr.Rdata')
All0 <- Allm[Allm$lr_ari!=-Inf,]
lin <- 1                           #linear (1) or constant (0)  --- CHANGE HERE! 

final_list_cn <- read.csv("~/Dropbox/MDR/107_final_list_included_countries.csv", stringsAsFactors = FALSE)[,-1]
#uu <- unique(as.character(All0$iso3)) # number of unique countries
uu <- uu[-is.na(uu)] 
uu <- final_list_cn
length(uu) # 107 = final_list
skp <- c()

## For each country in turn
for(jj in 1:length(uu)){
  
  print(jj)
  
  ####**** Load data ********************************************************************************************************************************************************************#####
  cn <- uu[jj] # unique country for this run
  print(cn)
  All <- All0[All0$iso3 %in% cn,]
  ### For MDR - can't have 15...? 
  if(dim(All)[1] < 5){print(paste0("dim(All) = ", dim(All)," skip ", cn, " ", jj));skp <- c(skp, jj)} 
  if(dim(All)[1] < 5) next  ## New uu: skip 33 BES & 156 TLS (OLD: 15 cutoff in supp: Removes 40 "BES", 66 "CUW", 189 "SXM", 195 "TKL", 197 "TLS")
  All$year <- as.numeric(All$year)
  ## ============== work ====================
  
  ## time/country vectors for data/extrapolation
  fyear <- 1933 # start date
  # td = data times, te = all extrapolation times
  tdz <- All$year-fyear  # units of time to extrapolate that have ARI values for for this country
  tez <- 1934:2014 - fyear                  #extrapolation times
  
  ## H as a global
  ## dcnz, tdz, sigz, y as globals
  ## H
  H <- getHtonly(tdz,n=lin)  # Matrix of 1s
  Hs <- getHtonly(tez,n=lin) # Matrix of 1s
  y <- All$lr_ari   # grab log ari from data *** MDR!! 
  vz <- (All$E)^2  # var is square of error
  sigz <- Diagonal(x=vz)                         #noise, on diagonal
  
  ## Normal priors on the hyperparamers (loga(σ_k) and loga(l)
  ## means of 0.5 / 2 for the overall kernel scale / smoothing time scale, respectively
  ## standard deviations of 100
  mz <- c(log(.5),2*1.5*log(2)); ### why 2*1.5 for second? 
  sz <- c(1,1)*100
  
  ## Maximum likelihood function? 
  LMLfun2  <- function(x) -(getMLnGradT(x,grad=FALSE)$LML - sum(.5*(x-mz)^2/sz^2))
  dLMLfun2 <- function(x) -(getMLnGradT(x)$dLML - (x-mz)/sz^2)
  x02 <- mz
  
  ## optimize
  # input prior means, function for max liklihood and gradient?
  system.time({                           #<1s
    testo2 <- optim(par=x02,fn = LMLfun2,gr = dLMLfun2)
  })
  pab <- testo2$par
  ab <- PRMconvert2(pab) # exponential of first element / half of second
  ## ab
  print(ab);
  ## NB trade-off flexibility vs growing noise going back
  xx <- ab
  tot <- getPredztonly(xx,tdz,tez,y,vz)
  scf <- as.numeric(sqrt(diag(tot$cg)))
  erw <- data.frame(year=tez+fyear,iso3=as.character(unique(All$iso3)),
                    lari=as.numeric(tot$mg),
                    upper=as.numeric(tot$mg) + 1.96*scf,
                    lower=as.numeric(tot$mg) - 1.96*scf)
  
  if(lin > 0){save(erw,file=paste0('datar/',cn,'.Rdata'))}
  if(lin == 0){save(erw,file=paste0('datar_const/',cn,'.Rdata'))}
  
  
  runs <- mvrnorm(n=1e3,mu=as.numeric(tot$mg),Sigma=as.matrix(symmpart(tot$cg))) # CHANGED to 1,000 from 2e2
  runsdf <- data.frame(year=tez+fyear,iso3=as.character(unique(All$iso3)),
                       lari=c(t(runs)),replicate=rep(1:nrow(runs),each=ncol(runs)))
  
  if(lin > 0){save(runsdf,file=paste0('datar/zz_',cn,'.Rdata')); print(paste0('datar/zz_',cn,'.Rdata'))}
  if(lin == 0){save(runsdf,file=paste0('datar_const/zz_',cn,'.Rdata')); print(paste0('datar/zz_',cn,'.Rdata'))}
  
}

uu <- uu[-skp]
save(uu,file='datar/uu.Rdata')
