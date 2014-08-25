#######################################
### Robust Linear Models with RStan ###
#######################################

# If you don't have Stan for R in your computer, uncomment and run the following lines:
#source('http://mc-stan.org/rstan/install.R', echo = TRUE, max.deparse.length = 2000)
#install_rstan()

# Load necessary packages
library(rstan)  # Stan interface for R

# Data set
data(cars)

# Model

m1 <- '
data {                          
int<lower=0> N;                  # number of observations
vector[N] distance;              # setting the dependent variable
vector[N] speed;                 # independent variable
}
parameters {
vector[2] beta;                  # one beta for the intercept, another for the independent variables
real<lower=0, upper=100> sigma;  # error scale with a uniform prior (0,100)
real<lower=1> nu;                # degrees of freedom constant nu
}
model {
beta[1] ~ normal(0,100);         # normal priors for both betas
beta[2] ~ normal(0,100);
nu ~ gamma(2,0.1);               # prior cited by Vehtari and Gelman (2014) http://bit.ly/1tvulWj
distance ~ student_t(nu, beta[1] + beta[2] * speed, sigma); #formula
}
'

# Prepare the data list
data.list <- list(N = nrow(cars), distance = cars$dist, speed = cars$speed)
str(data.list)

# Estimate the model
fit <- stan(model_code = m1, data = data.list, iter = 500, chains = 4)
print(fit, digits = 3)
