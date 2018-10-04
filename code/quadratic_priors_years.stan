data {
    int N; // number of observations
    int N2; // number of generated observations
    vector[N] q; // observations
    vector[N] years_obs; // years of observations
    vector<lower=0>[N] sigma; // standard deviations
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
  vector[N] yrs_fromtm;

  c = rho * b / t_m;
  yrs_fromtm = years_obs - rep_vector(t_m,N);
  
  for(i in 1:N)
    quadpred[i] = inv_logit(b * yrs_fromtm[i] - c * pow(yrs_fromtm[i],2)) - 0.5;
    
    //quadpred = inv_logit(quadpred);
}
model { 
 // q ~ normal(quadpred, sigma);
  q ~ normal(quadpred, sigma);
  t_m ~ normal(1980, 5);

}
generated quantities {
  vector[N2] p_pred;
  vector[N2] yrsp_fromtm;
  
  yrsp_fromtm = years - rep_vector(t_m,N2);
  
  for(i in 1:N2)
    //p_pred[i] = b * yrsp_fromtm[i] - c * pow(yrsp_fromtm[i],2); //observations predicted by the model
    p_pred[i] = inv_logit(b * yrsp_fromtm[i] - c * pow(yrsp_fromtm[i],2)) - 0.5; //observations predicted by the model
}
