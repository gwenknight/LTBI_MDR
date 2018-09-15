data {
    int N; // number of observations
    int N2; // number of generated observations
    real q[N]; // observations
    vector[N] years_obs; // years of observations
    real sigma[N]; // standard deviations
    vector[N2] years; // years to predict
}
parameters {
  real<upper=0> a; // intercept
  real b; // slope
}
transformed parameters {
  vector[N] linpred;
  linpred = a + b * years_obs;
}
model {  
  q ~ normal(linpred, sigma);
}
generated quantities {
  vector[N2] p_pred;
  vector[N] log_likelihood;
  p_pred = a + b * years; // observations predicted by the model
  
   for (i in 1:N) {

   log_likelihood[i] = normal_lpdf(q | linpred, sigma);
    
  }
}
