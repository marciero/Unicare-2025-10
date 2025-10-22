## power analysis.
## The functions used in sim_plot.R are defined here. Those routines then stored data used in power_calc_zero_inf_2025-06.qmd

summary(mod_zip3)
summary(mod_zinb3)

fixef(zip3_bayes)

## Need corellated covariates.

patients_events %>%
  select(age, chf, atrial_fibrillation) %>% summarise(across(everything(), ~ mean(.x)))


cor_mat <- patients_events %>%
  select(age, chf, atrial_fibrillation) %>% cor()

############### Generate data
##############
gen_zi_data <- function(n, cormat = cor_mat,
                        theta_int = 0.3, lambda_int = -0.46,     ### adjust intercept to get right overall mean?
                        theta_age = -0.65, lambda_ibis = -0.3,
                        lambda_age = -0.28,
                        lambda_chf = 0.75,
                        lambda_afib = 0.7,
                        disp = 0.7   ## dispersion for neg binom
     )
   {

  age_raw <- round(runif(n, 60, 90))
  age <- (age_raw - 60)/10  # center at 60
  ibis <- rbinom(n, 1, 0.5)  # cohort
  cohort <- ifelse(ibis == 1, "ibis", "control")


  ## Need corellated covariates

   chf <- rbinom(n, 1, 0.2)
  afib <- rbinom(n, 1, 0.2)

   conds <- data.frame(age, chf, afib)
   nvars <- ncol(conds)

   rnorm_helper <- matrix(rnorm(nvars*n, 0, 1), ncol = nvars)

   # define a matrix of desired correlations between variables
   Q <- cormat
   L <- t(chol(Q))
   Z <- rnorm_helper %*% t(L)  ## Z has desired correlations

   raw <- as.data.frame(Z,row.names = NULL)
   names(raw) <- names(conds)

   ##could use dplyr but use for loop for explicit.
   for(name in names(raw)) {
     raw <- arrange(raw, name)
     conds <- arrange(conds, name)
     raw[,name] <- conds[,name]
   }

  logit_theta <- theta_int  + theta_age*age
  theta <- 1 / (1 + exp(-logit_theta))

  log_lambda <- lambda_int  +  lambda_ibis*ibis + lambda_age*age +
    lambda_chf*chf + lambda_afib*afib
  lambda <- exp(log_lambda)

  y <- ifelse(runif(n) < theta, 0, rnbinom(n, size = disp, mu = lambda))
  #y <- ifelse(runif(n) < theta, 0, rpois(n, lambda))

  yis0 = y == 0  # for checking proportions of zero admissions

  ynot0 = y != 0

  ## Base probabilities. 60 yo with no chronic conds
  base_zi_prob <- theta_int <- 1 / (1 + exp(-lambda_int))
  base_mean_control <- (1 - base_zi_prob)*exp(lambda_int)
  base_mean_ibis <- (1 - base_zi_prob)*exp(lambda_int + lambda_ibis)
  base_mean_diff <- base_mean_control - base_mean_ibis

  df <- data.frame(y, yis0, ynot0, cohort, age, chf, afib)

  return(list(data = df, base_zi_prob = base_zi_prob, count_int = lambda_int,
              base_mean_control = base_mean_control,
              base_mean_ibis = base_mean_ibis,
         base_mean_diff = base_mean_diff))

}

##########
########


df <- gen_zi_data(500)

df$data %>% summarise(.by = cohort, mean(y))

df$base_zi_prob
df$base_mean_diff

patients_events %>% summarize(.by = cohort, mean(inpatient_count))

### Tests

####

run_zi_poisson_coef_test <- function(data) {
  test <- zeroinfl(y ~ cohort  + age + chf + afib | age,
                   data = data, dist = "poisson")  %>% summary()
  ibis_est <- test$coefficients$count["cohortibis", "Estimate"]
  ibis_se <- test$coefficients$count["cohortibis", "Std. Error"]
  ibis_p <- test$coefficients$count["cohortibis", 'Pr(>|z|)']
  return(c(ibis_est, ibis_se, ibis_p))
}

run_zi_poisson_coef_test(df$data)



sim_zi_poisson_coef_test <- function(n, cormat) {
  df <- gen_zi_data(n, cormat)
  return(run_zi_poisson_coef_test(df$data))
}

## revised to catch na's from vcov matrix
sim_zi_poisson_coef_test <- function(n, cormat, max_tries = 5) {
  for (attempt in 1:max_tries) {
    df <- gen_zi_data(n, cormat)
    result <- run_zi_poisson_coef_test(df$data)
    if (!any(is.na(result))) {
      return(result)
    }
  }
  warning("Failed to get valid model after ", max_tries, " attempts.")
  return(c(NA, NA, NA))
}


set.seed(123)
mean(replicate(1000, sim_zi_poisson_coef_test(500, cor_mat)[3]) < .05)

results4 <- data.frame()

for (n in seq(100, 1500, by = 100)) {
  prop_sig <- replicate(100, sim_zi_poisson_coef_test(n, cor_mat)[3])
  }

results4 <- rbind(results4, data.frame(n = n, prop_sig = prop_sig))


### Table with lambda_ibis and corresponding eff. size (as factor, not raw percent)

eff_size <- data.frame(eff_size = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)) %>%
                mutate(lambda_ibis = log(1 - eff_size))


####
### intercept only logistic. Should be equiv to t test.

run_logistic_coef_test <- function(data) {
  test <- glm(yis0 ~ cohort, data, family = "binomial") %>% summary()
  ibis_est <- test$coefficients["ibis", 'Estimate']
  ibis_se <- test$coefficients["ibis", 'Std. Error']
  ibis_p <- test$coefficients["ibis", 'Pr(>|z|)']
  return(c(ibis_est, ibis_se, ibis_p))
}

sim_logistic_test <- function(n, cormat) {
  df <- gen_data(n, cormat)
  return(run_logistic_coef_test(df))
}




results3 <- data.frame()

for (n in c(750, 1000, 1250, 1500, 1750, 2000, 2250, 2500)) {
  prop_sig <- mean(replicate(5000, sim_logistic_test(n)[3]) < 0.05)

  results3 <- rbind(results3, data.frame(n = n, prop_sig = prop_sig))
}




#### more from old power calc

reps <- replicate(100, sim_logistic_test(1000))

reps_df <- data.frame(rep = 1:ncol(reps), estimate = reps[1,], se = reps[2,], p = reps[3,])
reps_df %>% ggplot(aes(rep, estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin =
                      estimate - 1.96*se, ymax = estimate + 1.96*se)) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Coefficient estimates and 95% confidence intervals, 100 reps n = 1000", x = "rep", y = "estimate")
