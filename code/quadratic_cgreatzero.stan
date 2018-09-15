data {
    int N; // number of observations
    int N2; // number of generated observations
    real q[N]; // observations
    vector[N] years_obs; // years of observations
    vector[N] years_obs2; // years of observations squared
    real sigma[N]; // standard deviations
    vector[N2] years; // years to predict
    vector[N2] years2; // years to predict squared
}
parameters {
  real a; // slope x
  real<upper=0> b; // slope x2
  real<upper=0> c; // intercept
}
transformed parameters {
  vector[N] quadpred;
  quadpred = a * years_obs + b * years_obs2 + c;
}
model {  
  q ~ normal(quadpred, sigma);
}
generated quantities {
  vector[N2] p_pred;
  vector[N] log_likelihood;
  p_pred = a * years + b * years2 + c; // observations predicted by the model
  
  for (i in 1:N) {

   log_likelihood[i] = normal_lpdf(q | quadpred, sigma);
    
  }
}
