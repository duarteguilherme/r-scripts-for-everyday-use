# How to use simulation to represent uncertainty in regression coefficients:
# An informal Bayesian approach (Gelman & Hill, 2007, p. 140)

# Load necessary packages
library(Zelig) # data sets
library(arm)   # simulations

# Load data set 
data(turnout)

# Model
m1 <- bayesglm(vote ~ educate + income + as.factor(race) + income*race + age + I(age^2),
               binomial("logit"), turnout)
display(m1)

# Simulate
sim1 <- sim(m1)
str(sim1)

# Quantities of interest
educate_sim1 <- sim1@coef[,2]
mean(educate_sim1)                   # mean estimate
hist(educate_sim1)
quantile(educate_sim1, c(.025,.975)) # 95% interval

# Simulations also make interactions easy to understand and interpret 
interaction_inc_race <- sim1@coef[,5]
mean(interaction_inc_race)
hist(interaction_inc_race)
quantile(interaction_inc_race, c(.025,.975))

# Plots
plot(jitter(turnout$educate, 2), jitter(turnout$vote, .05), pch=20,
     cex = .6, xlab = "Education Years", 
     ylab = "Probability of Voting", main = "Predicted Values", axes=FALSE, 
     xlim=c(0,20), ylim=c(0,1))

axis(1) # adds x axis
axis(2) # adds y axis

# Add curves
for(i in 1:100){
        curve(invlogit(sim1@coef[i,1] + sim1@coef[1,2]*x), col="gray",
               add=TRUE)}
curve(invlogit(m1$coef[1] + m1$coef[2]*x), col="dark red", lwd=4, add=TRUE)
