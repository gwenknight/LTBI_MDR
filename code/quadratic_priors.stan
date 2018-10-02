data {
    int N; // number of observations
    int N2; // number of generated observations
    vector[N] q; // observations
    vector[N] years_obs; // years of observations
    vector[N] sigma; // standard deviations
    vector[N2] years; // years to predict
}
parameters {
  real <lower=0> t_m; // time when mdr arose
  real <lower=0,upper=0.05> b; // slope
  real <lower=0,upper=1> rho; // dummy parameter
}
transformed parameters {
  real <lower=0> c;
  vector[N] quadpred;
  
  c = rho * b / t_m;
  
  for (i in 1:N)
    quadpred[i] = b * (years_obs[i] - t_m) - c * pow((years_obs[i]-t_m),2);
   
}
model { 
  q ~ normal(quadpred, sigma);
  t_m ~ normal(34, 5);
}
generated quantities {
  vector[N2] p_pred;
  vector[N] log_likelihood;
  
  for (i in 1:N2)
    p_pred[i] = b * (years[i] - t_m) - c * pow((years[i]-t_m),2); // observations predicted by the model
  
  for (i in 1:N) {
   log_likelihood[i] = normal_lpdf(q | quadpred, sigma);
  }
}
