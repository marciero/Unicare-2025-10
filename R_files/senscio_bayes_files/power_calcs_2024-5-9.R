# pwr packaage
library(pwr)

## Using Brenton's google doc. Exported to excel, rounding
pwr.2p.test(h = ES.h(p1 = 0.25, p2 = 0.20),
            sig.level = 0.05, power = .80)

pwr.2p.test(h = ES.h(p1 = 0.8, p2 = 0.75),
            sig.level = 0.05, power = .80, alternative = "greater")

## For means, eg, number of visits. 2.2 vs 1.5

power.t.test(n = NULL, delta = 0.1, sd = sqrt(0.35), sig.level = 0.05,
             power = 0.80,
             type = "two.sample",
             strict = FALSE,
             alternative =  "two.sided")

## total admissions ~ N(n mu, n sigma^2)
n_all <- 1571
sqrt(n_all)*sqrt(1.48)

n_treat <- 209
sqrt(n_treat)*sqrt(1.48)

### Check manually; two sided test. From Devore.
pwr_n <- function(p1,p2, alpha, beta){
    q1 <- 1-p1
    q2 <- 1-p2
    num1 <- qnorm(1 - alpha/2)*sqrt((p1 + p2)*(q1 + q2)/2)
    num2 <- qnorm(beta)*sqrt(p1*q1 + p2*q2)
    (num1 + num2)^2/(p1 - p2)^2
}


pois_power <- function(m1, m2, alpha = 0.05, beta = 0.8, two.sided = TRUE){
    if(two.sided == TRUE) {
        gamma <- alpha/2
    } else {
        gamma <- alpha
    }
    (m1 + m2)*((qnorm(1- gamma) + qnorm(1- beta))/(m2 - m1))^2
}

pois_power <- function(m1, m2, alpha = 0.05, beta = 0.8, two.sided = TRUE){
    if(two.sided == TRUE) {
        gamma <- alpha/2
    } else {
        gamma <- alpha
    }
    (m1 + m2)*((qnorm(1- gamma) + qnorm(1- beta))/(m2 - m1))^2
}

pois_power(.45,.35)

library(WebPower)

wp.poisson(exp0 = 0.45, exp1 = 0.8, alpha = 0.05,  power = 0.8, family = "Bernoulli",
           parameter = 0.5,
           alternative = "two.sided")

wp.logistic(n = NULL, p0 = 0.25, p1 = 0.18, alpha = 0.05,
            power = 0.8, alternative = c("two.sided"),
            family = c("Bernoulli"), parameter = 0.5)

pwr_n(.2, .10, .05, .9)

pwr.2p.test(h = ES.h(p1 = 0.10, p2 = 0.20),
            sig.level = 0.05, power = .90)

power.prop.test(p1 = 0.8, p2 = 0.75,
                sig.level = 0.05, power = .80, alternative = "two.sided")


