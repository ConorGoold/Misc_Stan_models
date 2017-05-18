data{
  int<lower=0> N;
  int<lower=1> P;
  vector<lower=0>[N] y;
  matrix[N, P] X;
}

parameters{
  real a;
  real a_hurdle;
  vector[P] B;
  vector[P] B_hurdle;
  real<lower=0> shape;
}

transformed parameters{
  vector[N] eta;
  vector[N] eta_hurdle;
  eta = a + X * B;
  eta_hurdle = a_hurdle + X * B_hurdle;
}

model{
  a ~ normal(0, 1);
  a_hurdle ~ normal(0, 1);
  B ~ normal(0, 1);
  B_hurdle ~ normal(0, 1);
  shape ~ cauchy(0, 2);

  for(n in 1:N){
    if(y[n] == 0)
      target += log( inv_logit(eta_hurdle[n]) );
    else
      target += log1m( inv_logit(eta_hurdle[n]) ) + gamma_lpdf( y[n] | shape , shape/exp(eta[n]) );
}
}
