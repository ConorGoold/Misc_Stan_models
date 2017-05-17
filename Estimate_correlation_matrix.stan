data {
int<lower=0> N;            // data length
int<lower=1> P;            // number of variables
vector[P] Y[N];             // data matrix
}

transformed data{
vector[P] scaled_Y[N];
for( n in 1:N ) {
  for(p in 1:P ){
  scaled_Y[n, p] = ( Y[n, p] - mean(Y[,p]) )/sd(Y[,p]);
    }
  }
}

parameters{
cholesky_factor_corr[P] L_Rho;
vector[P] mu;
vector<lower=0>[P] sigmas;
}

transformed parameters{
matrix[P, P] L_Sigma;
L_Sigma = diag_pre_multiply(sigmas, L_Rho);
}

model{
mu ~ normal(0, 1);
sigmas ~ cauchy(0, 2);
L_Rho ~ lkj_corr_cholesky(2);
scaled_Y ~ multi_normal_cholesky( mu , L_Sigma );
}

generated quantities{
matrix[P, P] Rho;
matrix[P, P] inv_Rho;
Rho = L_Rho * L_Rho';
}
