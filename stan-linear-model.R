#########################################
### (Robust) Linear Models with RStan ###
#########################################

# If you don't have Stan for R in your computer, uncomment and run the following lines:
# source('http://mc-stan.org/rstan/install.R', echo = TRUE, max.deparse.length = 2000)
# install_rstan()

# Load necessary packages
library(rstan)  # Stan interface for R

# Data set
library(Zelig)
data(macro)
str(macro)

# Model

model1 <- '
data {
int<lower=0> N;              # number of observations
vector[N] gdp;               # dependent variable - gdp in US$1000
vector[N] unem;              # independent variable - unemployment
vector[N] capmob;            # independent variable - capital mobility
vector[N] trade;             # independent variable - trade (gdp %)
}
parameters {
real alpha;                  # intercept
real coef_unem;              # coefficient for unemployment
real coef_capmob;            # coefficient for capital mobility
real coef_trade;             # coefficient for trade
real<lower=0> sigma;         # residual standard error
}
model {
alpha ~ normal(0,100);       # weakly informative prior for the intercept
coef_unem ~ normal(-2,1);    # informative prior 
coef_capmob ~ normal(0,100); # weakly informative prior
coef_trade ~ normal(.2, .1); # informative prior

gdp ~ normal(alpha + coef_unem * unem + coef_capmob * capmob + coef_trade * trade, sigma); # model
}
'
# If you want to estimate a robust regression, say, a t-distribution with 4 degrees of freedom,
# just change the sampling distribution to:
# gdp ~ student_t(4, alpha + coef_unem * unem + coef_capmob * capmob + coef_trade * trade, sigma);

# Prepare the data list
data.list <- list(N = nrow(macro), gdp = macro$gdp, unem = macro$unem,
                  capmob = macro$capmob, trade = macro$trade)
str(data.list)

# Estimate the model
fit <- stan(model_code = model1, data = data.list, iter = 1000, chains = 4)
print(fit, digits = 2)

# Compare with frequentist estimation
summary(lm(gdp ~ unem + capmob + trade, macro))

# Extract Stan object
f1 <- extract(fit)
str(f1)
plot(density(f1$coef_capmob), main = "Posterior -- Capital Mobility")  # draw posterior
quantile(f1$coef_capmob, c(.025, .975))                                # 95% interval

# Load coda and ggmcmc to plot graphs
library(coda)
library(ggmcmc)

# Plotting some graphs and assessing convergence with the coda package.
# We can convert a fit object to coda with the following function
# (http://jeromyanglim.tumblr.com/post/91434443911/how-to-convert-a-stan-fit-object-to-work-with-coda-and)

stan2coda <- function(fit) {
        mcmc.list(lapply(1:ncol(fit), function(x) mcmc(as.array(fit)[,x,])))
}

fit.mcmc <- stan2coda(fit)

# I personally like the codamenu() function. It has all tests and plots one may
# want. Just type:
codamenu()
2 # Use an mcmc object
fit.mcmc

# And follow the menu instructions. A Stan fit object can also be transformed into
# a ggmcmc object with ggs(). We can also change the parameters' labels.
P <- data.frame(Parameter = c("alpha", "coef_unem", "coef_capmob", "coef_trade", "sigma"),
                Label = c("Intercept", "Unemployment", "Capital Mobility", "Trade", "Residual Standard Error"))
fit.ggmcmc <- ggs(fit, par_labels = P)

# Some plots
ggs_traceplot(fit.ggmcmc) + ggtitle("Trace Plots") + theme_bw()
ggs_density(fit.ggmcmc) + ggtitle("GDP per capita (US$ 1000)") + 
        xlab("Estimate") + ylab("Density") + theme_bw()
ggs_caterpillar(fit.ggmcmc) + ggtitle("Coefficient Plot") +
        xlab("HPD") + ylab("Parameter") + theme_bw()
