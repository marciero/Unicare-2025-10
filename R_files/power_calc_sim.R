
## Zero inflated Poisson distribution
n <- 10000

# parameters, no predictors

theta <- 0.7  # zero-inflation probability
lambda <- 1.8  # Poisson rate

# Simulate data
y <- ifelse(runif(n) < theta, 0, rpois(n, lambda))

mean(y == 0)
mean(y[y !=0])
mean(y)

## Now add predictors

age <- round(runif(n, 60, 90))
ibis <- rbinom(n, 1, 0.5)
logit_theta <- 1.7  - (age - 70 )* 0.1 + 0.4*ibis
theta <- 1 / (1 + exp(-logit_theta))

log_lambda <- 0.75  + (age-70) * 0.05 - 0.2*ibis
lambda <- exp(log_lambda)

y <- ifelse(runif(n) < theta, 0, rpois(n, lambda))

mean(y == 0)

(mean(y[ibis == 0 & y !=0]) -
mean(y[ibis == 1 & y !=0]))/mean(y[ibis == 0 & y !=0])

(mean(y[ibis == 0]) -
        mean(y[ibis == 1]))/mean(y[ibis == 0])

plot(age,y)
mean(y[age == 90])

#####

gen_data <- function(n, theta_int = 0.3, lambda_int = -0.7,
                        theta_age = -0.05, lambda_age = .9,
                     theta_ibis = 0.2, lambda_ibis = 0.2) {
    age <- round(runif(n, 60, 90))
    ibis <- rbinom(n, 1, 0.5)
    logit_theta <- theta_int  + 0.1*(age - 70 )* theta_age + theta_ibis*ibis
    theta <- 1 / (1 + exp(-logit_theta))

    log_lambda <- lambda_int  + 0.1*(age-70) * lambda_age - lambda_ibis*ibis
    lambda <- exp(log_lambda)

    y_nz <- rpois(n, lambda)   ## for testing with non-zero inflated
    y <- ifelse(runif(n) < theta, 0, rpois(n, lambda))

    yis0 = y == 0  # for checking proportions of zero admissions
                    #  (could just use df$y == 0)
    ynot0 = y != 0
    y_nzis0 = y_nz == 0

   return(data.frame(y, yis0, y_nz, y_nzis0, ynot0, age, ibis))
}

df <- gen_data(10000)
mean(df$y[df$ibis == 0])
mean(df$y[df$ibis == 1])

mean(df$y[df$ibis == 1] == 0)
mean(df$y[df$ibis == 0] == 0)

mean(df$ynot0[df$ibis == 1])
mean(df$ynot0[df$ibis == 0])



#df <- gen_data(10000, 1.75, 0.05, 0.1, 0.05, 0.35, 0.10)
#df <- gen_data(10000, 1,    0.05, 0.1, 0.05, 0.75, 0.10)

#df <- gen_data(10000, 1.5,  0.05, 0.1, 0.05, 0.20, 0.2) ## ~20%


#df <- gen_data(1000, 1.5,  0.05,  # int theta, int lambda
#                 0.1, 0.05,    # theta age, lambda age;
#                0.17, 0.2)     # theta ibis, lambda ibis

df %>% ggplot(aes(x = y_nz)) + geom_bar() + facet_wrap(~ibis)
df %>% ggplot(aes(x = y)) + geom_bar() + facet_wrap(~ibis)

mean(df$y[df$ibis == 1] == 0)
mean(df$y[df$ibis == 0] == 0)

mean(df$y[df$ibis == 0])
mean(df$y[df$ibis == 1])

glm(y_nz ~  ibis, family = "poisson", df) %>% summary()
glm(y_nz ~  ibis, family = "poisson", df) %>% summary()

## Simulate power for proportion of admits ibis vs non-ibis
df_ibis <- df %>% filter(ibis == 1)
df_no_ibis <- df %>% filter(ibis == 0)

prop.test(c(sum(df_ibis$yis0), sum(df_no_ibis$yis0)),
          c(nrow(df_ibis), nrow(df_no_ibis)))

run_prop_test <- function(data) {
    df_ibis <- data %>% filter(ibis == 1)
    df_no_ibis <- data %>% filter(ibis == 0)

   #test <- prop.test(c(sum(df_ibis$yis0), sum(df_no_ibis$yis0)),
                #     c(nrow(df_ibis), nrow(df_no_ibis)))

    test <- prop.test(c(sum(df_ibis$ynot0), sum(df_no_ibis$ynot0)),
                      c(nrow(df_ibis), nrow(df_no_ibis)))

          return(test$p.value)
    }

run_prop_test(df)

sim <- function(n) {
    df <- gen_data(n)
    return(run_prop_test(df))
}

sim(1000)

mean(replicate(1e4, sim(1000)) < 0.05)

## Should be the same as test on ibis coef with logistic regression
run_logistic_test <- function(data) {
    test <- glm(yis0 ~ ibis, family = "binomial", data = data) %>%
        summary()
    ibis_est <- test$coefficients["ibis", 'Estimate']
    ibis_se <- test$coefficients["ibis", 'Std. Error']
    ibis_p <- test$coefficients["ibis", 'Pr(>|z|)']
    return(c(ibis_est, ibis_se, ibis_p))
}

glm(yis0 ~ ibis, family = "binomial", df) %>% summary()


sim_logistic <- function(n) {
    df <- gen_data(n)
    return(run_logistic_test(df))
}

sim_logistic(1000)

reps <- replicate(100, sim_logistic(1000))

reps_df <- data.frame(rep = 1:ncol(reps), estimate = reps[1,], se = reps[2,], p = reps[3,])
reps_df %>% ggplot(aes(rep, estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin =
                estimate - 1.96*se, ymax = estimate + 1.96*se)) +
    geom_hline(yintercept = 0, color = "red")

mean(reps_df$p < 0.05)

## Test mean of y
run_mean_test <- function(data) {
    test <- t.test(y ~ ibis, data)
    return(test$p.value)
}

sim_ttest <- function(n) {
    df <- gen_data(n)
    return(run_mean_test(df))
}

sim_ttest(1000)

mean(replicate(1e4, sim_ttest(1000))<0.05)


### Now add age coeff

run_log_coef_test <- function(data) {
    test <- glm(yis0 ~ ibis + age, data, family = "binomial") %>%
        summary()
    return(test$coefficients["ibis", "Pr(>|z|)"])
}

sim_logistic_coef <- function(n) {
    df <- gen_data(n)
    return(run_log_coef_test(df))
}

sim_logistic_coef(1000)

mean(replicate(1e4, sim_logistic_coef(1000))<0.05)

## Test with poisson regression
run_poisson_coef_test <- function(data) {
    test <- glm(y ~ ibis + age, data, family = "poisson") %>% summary()
    ibis_est <- test$coefficients["ibis", 'Estimate']
    ibis_se <- test$coefficients["ibis", 'Std. Error']
    ibis_p <- test$coefficients["ibis", 'Pr(>|z|)']
    return(c(ibis_est, ibis_se, ibis_p))
}

sim_poisson_test <- function(n) {
    df <- gen_data(n)
    return(run_poisson_coef_test(df))
}

sim_poisson_test(1000)

mean(replicate(1e4, sim_poisson_test(1000)[3])<0.05)

## Test with zero-inflated regression
# Load the pscl package
library(pscl)

test <- zeroinfl(y ~ ibis + age | ibis + age,
                 dist = "poisson",
                 data = df) %>% summary()

test$coefficients$count["ibis", 'Pr(>|z|)']

coeftest(test) ## cant be summary to do this

run_zeroinfl_test <- function(data) {
    test <- zeroinfl(y ~ ibis + age | ibis + age,
                     dist = "poisson",
                     data = data) %>% summary()
      ibis_count_p <- test$coefficients$count["ibis", 'Pr(>|z|)']
      ibis_zero_p <- test$coefficients$zero["ibis", 'Pr(>|z|)']
    return(c(ibis_count_p, ibis_zero_p))
}

df <- gen_data(1000)
run_zeroinfl_test(df)

sim_zeroinfl_test <- function(n) {
    df <- gen_data(n)
    return(run_zeroinfl_test(df))
}

sim_zeroinfl_test(2500)

results <- data.frame()

for (n in c(750, 1000, 1250, 1500, 1750, 2000, 2250, 2500)) {
 reps <- replicate(5000, sim_zeroinfl_test(n))
 count_power <- mean(reps[1,] < 0.05)
 zero_power <- mean(reps[2,] < 0.05)

 results <- rbind(results, data.frame(n = n, count_power = count_power, zero_power = zero_power))
}

df <- gen_data(1000)
zeroinfl(y ~ ibis + age | ibis + age,
         dist = "negbin",
         data = df) %>% summary()

test_negbin <- zeroinfl(y ~ ibis + age | ibis + age,
                 dist = "negbin",
                 data = df) %>% summary()

run_zeroinfl_test_negbin <- function(data) {
    test <- zeroinfl(y ~ ibis + age | ibis + age,
                     dist = "negbin",
                     data = data) %>% summary()
    ibis_count_p <- test$coefficients$count["ibis", 'Pr(>|z|)']
    ibis_zero_p <- test$coefficients$zero["ibis", 'Pr(>|z|)']
    return(c(ibis_count_p, ibis_zero_p))
}

sim_zeroinfl_test_negbin <- function(n) {
    df <- gen_data(n)
    return(run_zeroinfl_test_negbin(df))
}

mean(replicate(10, sim_zeroinfl_test_negbin(2500)[1]<0.05))

mean(replicate(1e4, sim_zeroinfl_test_negbin(2000)[1] < 0.05))
mean(replicate(1e4, sim_zeroinfl_test_negbin(2000)[2] < 0.05))




########
library(brms)
library(bayesplot)

## brms model intercept only zero inflation, poisson
## Make sure to check priors, and centering!

zero_inf_bayes <- brm(y ~ ibis + age,
                      data = df, family = zero_inflated_poisson(),
   chains = 4, iter = 5000*2, seed = 84735, sample_prior = "no"
   # control = list(adapt_delta = .9,
             #      max_treedepth = 10)
)

summary(zero_inf_bayes)
plot(conditional_effects(zero_inf_bayes), ask = FALSE)

yrep1 <- posterior_epred(zero_inf_bayes)

ppc_dens_overlay(df$y, yrep1[1:50,])

## Now add predictor to zero inflation
## note that we use bf() for multiple formulas
## Repeat twice to see if results differ

zero_inf_bayes_2 <- brm(bf(y ~ ibis + age, zi ~ ibis + age),
                      data = df, family = zero_inflated_poisson(),
                      chains = 4, iter = 5000*2, seed = 84735,
                      sample_prior = "no"
                      # control = list(adapt_delta = .9,
                      #      max_treedepth = 10)
)

summary(zero_inf_bayes_2)
plot(conditional_effects(zero_inf_bayes_2), ask = FALSE)

summary(zero_inf_bayes_2)

yrep2 <- posterior_epred(zero_inf_bayes)

ppc_dens_overlay(df$y, yrep2[1:50,])


mcmc_areas(zero_inf_bayes_2, regex_pars = c("ibis"), prob = .95)
mcmc_areas(zero_inf_bayes_2, regex_pars = c("Intercept"), prob = .95)
mcmc_intervals(zero_inf_bayes_2, regex_pars = "zi_")


## try negative binomial

zero_inf_bayes_3 <- brm(bf(y ~ ibis + age, zi ~ ibis + age),
                      data = df, family = zero_inflated_negbinomial(),
                      chains = 4, iter = 5000*2, seed = 84735,
                      sample_prior = "no"
                      # control = list(adapt_delta = .9,
                      #      max_treedepth = 10)
)

summary(zero_inf_bayes_3)
plot(conditional_effects(zero_inf_bayes_3), ask = FALSE)

mcmc_areas(zero_inf_bayes_3, regex_pars = c("ibis"), prob = .95)
mcmc_areas(zero_inf_bayes_3, regex_pars = c("Intercept"), prob = .95)

###

yrep3 <- posterior_predict(zero_inf_bayes_3)

ppc_dens_overlay(df$y, yrep3[1:50,])

ppc_stat(df$y, yrep3, stat = function(x) mean(x == 0), binwidth = 0.005)

prop_one <- function(x) mean(x == 1)
ppc_stat(df$y, yrep3, stat = function(x) mean(x == 1), binwidth = 0.005)
ppc_stat(df$y, yrep3, stat = function(x) mean(x == 5), binwidth = 0.001)
ppc_stat(df$y, yrep3, stat = function(x) mean(x == 6), binwidth = 0.001)
ppc_stat(df$y, yrep3, stat = function(x) mean(x == 10), binwidth = 0.001)
ppc_stat(df$y, yrep3, stat = function(x) mean(x > 0), binwidth = 0.001)

mcmc_areas(zero_inf_bayes_3, regex_pars = c("ibis"), prob = .95)
mcmc_areas(zero_inf_bayes_3, regex_pars = c("Intercept"), prob = .95)
mcmc_intervals(zero_inf_bayes_3, regex_pars = "zi_")


####
get_prior(y ~ ibis + age,
          data = df, family = zero_inflated_poisson())

prior_summary(zero_inf_bayes_3)
