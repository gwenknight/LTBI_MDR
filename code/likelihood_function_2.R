### Fit to data using mcmc

# MFIDD fitR package
library('fitR')
library('coda')

# Function to take in a, b and generate y = a + b*x and calculates likelihood for this line

likelihoodab <- function(theta){
  # theta = c(a,b): parameters of curve to fit

  # DATA
  n = dim(who_l)[1] # number of data points
  
  # Sigma and mu from data, from 95% CI
  who_l <- as.data.frame(who_l)
  who_l$sigma <- (who_l$mhi - who_l$mdr_new)/1.96 # assuming normal distribution
  # who_l$sigma2 <- (who_l$sigma)^2
  
  # PARAMETERS
  #log.prior.a <- dunif(theta[["a"]], min = -I, max = 0, log = TRUE)
  #log.prior.b <- dunif(theta[["b"]], min = 0, max = 100, log = TRUE)
  log.prior <- 0 # log.prior.a + log.prior.b
  
  # CURVE
  year <- seq(1970, 2018,1)
  out <- theta[["a"]] + theta[["b"]]*(year - 1969) # exponential b if input a log
  curve_l <- as.data.frame(cbind(year, out))
  
  # LIKELIHOOD
  curves_use <- as.data.frame(filter(curve_l, year %in% who_l$year))
  if(length(curves_use$out) < length(who_l$mdr_new)){print("ERROR")}
  # Likelihood
  #who_l$L <- 1/(who_l$sigma*sqrt(2*pi)) * exp(-(who_l$mdr_new - curves_use$out)^2/(2*(who_l$sigma2)))
  # Log likelihood
  who_l$L <- dnorm(who_l$mdr_new, curves_use$out, who_l$sigma, log=TRUE)
  # - log(who_l$sigma) - (1/2) * log(2*pi) - (who_l$mdr_new - curves_use$out)^2/(2*(who_l$sigma2))
  
  # POSTERIOR
  log.posterior <- log.prior + sum(who_l$L)
  
  return(list(log.density=log.posterior, trace=theta))
}

# This is a function that takes four arguments:
# - target: the target distribution, a function that takes one argument
#           (a number) and returns the (logged) value of the
#           distribution of interest
# - init.theta: the initial value of theta, the argument for `target`
# - proposal.sd: the standard deviation of the (Gaussian) proposal distribution
# - n.iterations: the number of iterations
# The function should return a vector of samples of theta from the target
# distribution
my_mcmcMH <- function(target, init.theta, proposal.sd, n.iterations) {
  
  # evaluate the function "target" at "init.theta", and assign to
  # a variable called target.theta.current.
  target.theta.current <- target(init.theta)
  
  # initialise variables to store the current value of theta, the
  # vector of samples, and the number of accepted runs
  theta.current <- init.theta
  samples <- theta.current
  accepted <- 0
  
  # run MCMC for n.iteration interations
  for (i.iteration in seq_len(n.iterations)) {
    
    # draw a new theta from the (Gaussian) proposal distribution
    # and assign to a variable called theta.proposed.  
    # See "?rnorm for more information
    # Note that this step is vectorized for any arbitratry theta 
    # which will be useful when we will sample from a multivariate
    # target distribution
    theta.proposed <- rnorm(n = length(theta.current),
                            mean = theta.current,
                            sd = proposal.sd)
    
    # Note that 'rnorm' returns an unnamed vector, but the functions of
    # 'fitmodel' need a named parameter vector. We therefore set
    # the names of theta.proposed to be the same as the names of
    # theta.current
    names(theta.proposed) <- names(theta.current)
    
    # evaluate the function target at the proposed theta and
    # assign to a variable called target.theta.proposed
    target.theta.proposed <- target(theta.proposed)
    
    # compute Metropolis-Hastings ratio (acceptance probability). Since
    # the multivariate Gaussian is symmetric, we don't need to consider
    # the proposal distribution here
    # if very big positive difference => exp(log.acceptance) = infinity = accept proposed
    # if likelihood of proposed much less than that of current => exp(-neg number) = 0-1 = threshold for probability
    log.acceptance <- target.theta.proposed - target.theta.current
    
    # draw random number number between 0 and 1 using "runif" and assign to
    # a variable called r.
    r <- runif(1)
    
    # test acceptance by comparing the random number to the
    # Metropolis-Hastings ratio (acceptance probability) (using
    # "exp" because we calculated the logarithm of the
    # Metropolis-Hastings ratio before)
    if (r < exp(log.acceptance)) {
      
      # if accepted:
      # change the current value of theta to the proposed theta
      theta.current <- theta.proposed
      
      # updated the current value of the target
      target.theta.current <- target.theta.proposed
      
      # update number of accepted proposals
      accepted <- accepted + 1
    }
    
    # add the current theta to the vector of samples
    # Note that we use `rbind` in order to deal with multivariate 
    # target. So if `theta` is a vector then `samples` is a matrix.
    samples <- rbind(samples, theta.current, deparse.level=0)
    
    # print current state of chain and acceptance rate
    # use paste() to deal with the case where `theta` is a vector
    message("iteration: ", i.iteration, ", chain:", paste(theta.current, collapse=" "),
            ", acceptance rate:", accepted / i.iteration)
    
  }
  
  # return the trace of the chain (i.e., the vector of samples)
  return(samples)
}


theta0 <- c(a = -0.1, b = 0.1)
who_l <- who0[which(who0$iso3 == "SWZ"),c("year","mdr_new","mhi")]

trace <- my_mcmcMH(likelihoodab, theta0, c(0.01,0.004), 10000)
mcmc.trace <- mcmc(trace)
summary(mcmc.trace)
plot(mcmc.trace)

mcmc.trace.burned <- burnAndThin(mcmc.trace, burn = 1000)
plot(mcmc.trace.burned)

who_l <- who0[which(who0$iso3 == "ARM"),c("year","mdr_new","mhi")]
trace <- mcmcMH(likelihoodab, theta0, c(0.01,0.004), 10000)
mcmc.trace <- mcmc(trace)
summary(mcmc.trace)
mcmc.trace.burned <- burnAndThin(mcmc.trace, burn = 2500)
plot(mcmc.trace.burned)



trace <- mcmcMH(target=likelihoodab, init.theta=theta0, 
                n.iterations=10000, 
                adapt.size.start=100,
                adapt.shape.start=200,
                adapt.size.cooling=0.99,
                limits = list(lower=c(a = -Inf, b = 0), upper = c(a = 0, b = Inf)))
mcmc.trace <- mcmc(trace$trace)
mcmc.trace.burned <- burnAndThin(mcmc.trace, burn = 5000)
plot(mcmc.trace.burned)
ggmcmc(ggs(mcmc.trace.burned))



mcmcMH(likelihoodab, c(a = 0.01, b = 0.01), 100, proposal.sd = c(0.001,0.003))


effectiveSize(mcmc.trace)
hist(trace, freq = FALSE)


autocorr.plot(mcmc.trace.burned)


# CHECKS
likelihoodab(c(a = -0.021, b = -7.24))

x <- seq(1,2018-1970,1)
plot(x,mean(trace[,1]) + mean(trace[,2]) * x, ylim = c(0,0.2))
lines(who_l$year - 1969, who_l$mdr_new)




LL_gk = function(alpha, beta){
  
  n = length(sigma)
  LL_v = matrix(0,1,n)
  for(i in 1:n){
    LL_v[i] = 1/(sigma_v[i]*sqrt(2*pi))*exp(-(mean_v[i] - alpha - beta*year_v[i])/(2*(sigma_v[i]^2)))
  }
 return(prod(LL_v))
}

sigma_v = c(0.1,0.2,0.1)
year_v = c(0.5,2,3)
mean_v = 0.1 + 0.5*year_v

LL_gk(0.1,0.5)

library(stats4)
mle(LL_gk, start = list(alpha = 0.1, beta=0.5))
