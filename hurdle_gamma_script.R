# ============================================
# Hurdle gamma example in Stan
# ============================================

# load libraries

library(rstan)
library(rethinking)
library(ggplot2)

# set working directory!

# ============================================
# Simulate fake data

set.seed(12345)
N = 1000; shape = 1; a = 5; a_hurdle = -1; b = 0.5; b_hurdle = 0.25  

x = rnorm(N, 0, 1)   # continuous covariate

lin_pred_hurdle <- a_hurdle + b_hurdle * x_Z        # hurdle model
lin_pred_gamma <- a + b * x_Z                       # gamma model
                
bernoulli <- rbinom(N, 1, 1 - inv_logit(lin_pred_hurdle)) # zero values
y <- bernoulli * rgamma(N, shape=shape, rate=shape/exp(lin_pred_gamma))  # combined y vector

hist(y, breaks=30)

# create a design matrix for covariates. We only have one here, but it's useful to generate a design
# matrix so that the fitting process extends easily to more than one covariate
X_mat <- as.matrix( model.matrix( ~ x_Z )[,-1] )

# arrange the data for Stan
stan_data <- list(N = length(y), P = ncol(X_mat), X = X_mat)

# Stan preliminaries - change accordingly
n_chains = 4; n_cores = 4; n_iterations = 2000; n_warmup = n_iterations/2

# ========= fit the Stan model. See the separate model file for details ====================
fit_stan <- stan(file = "hurdle_gamma.stan", data = stan_data, 
                 chains = n_chains, cores = n_cores, 
                 iter = n_iterations, warmup = n_warmup)

# NB: Stan throws some messages here, but in my experience they are harmless. 

print(fit_stan, pars = c("a","B","shape","a_hurdle","B_hurdle"))

# ========= plot the model against the data ====================

# extract the samples
post_samples <- as.data.frame(fit_stan)

# quickly simulate from the posterior mean estimates 

sims <- rbinom(N, 1, 1-inv_logit(mean(post_samples$a_hurdle)) ) * 
    rgamma(N, 
           shape = mean( post_samples$shape ), 
           rate = (mean(post_samples$"shape")/exp(mean(post_samples$a)) ))

par(mfrow=c(1,2))
hist(y, main = "y")
hist(sims, main="simulated y")
              




