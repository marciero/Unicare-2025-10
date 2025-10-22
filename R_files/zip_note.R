### ZIP model check

df <- patients_events %>% filter(cohort == "ibis") %>%
    group_by(condition_count) %>%
    summarize(frequency = n(), .groups = "drop") %>%
    mutate(density = frequency / sum(frequency))

poisson_reg() %>%
    fit(condition_count ~ 1, data = patients_events %>% filter(cohort == "ibis")) %>% tidy()

zeroinfl(condition_count ~ 1, data = patients_events  %>% filter(cohort == "ibis"), dist = "poisson") %>%
    summary()

exp(-exp(1.43))

poisson_reg() %>%
    fit(condition_count ~ 1, data = patients_events) %>% tidy()

zeroinfl(condition_count ~ 1, data = patients_events, dist = "poisson") %>%
    summary()

pi <- inv_logit(-2.23)
pi + (1 - pi) * exp(-exp(1.53))

patients_events  %>% summarize(mean(condition_count == 0))


mod <- zeroinfl(condition_count ~ 1 | cohort, data = patients_events, dist = "poisson")
    summary(mod)

    ibis_df = data.frame(cohort = "ibis")
    control_df = data.frame(cohort = "control")

pi <- inv_logit(mod$coefficients$zero[1] + mod$coefficients$zero[2])
pi + (1 - pi) * exp(-exp(mod$coefficients$count[1]))

predict(mod, type = "prob", newdata = control_df)[1]

df <- patients_events[1,]

predict(mod, newdata = df, type = "response")  # overall expected count
predict(mod,  newdata = df, type = "count")     # Poisson mean (μ)
predict(mod,  newdata = df, type = "zero")      # structural zero probability (π)
predict(mod,  newdata = df, type = "prob") # probabilities that Y = k

coef(mod)[1]
coef(mod, model = "count")

predict(mod_nb, type = "response") ## mean
predict(mod_nb, type = "link")    ## linear predictor
predict(mod_nb, type = "terms")      ## linear predictor for each term
predict(mod, type = "prob", newdata = patients_events[1,]) # probability that Y = 0
## mean

mod_zinb <- zeroinfl(condition_count ~ 1 | cohort, data = patients_events, dist = "negbin")
mod_zip <- zeroinfl(condition_count ~ 1 | cohort, data = patients_events, dist = "poisson")

predict(mod_zip, type = "prob", newdata = control_df)
predict(mod_zinb, type = "prob", newdata = control_df)



inv_logit <- function(x){
    exp(x) / (1 + exp(x))
}
mu <- predict(mod_nb, type = "response")   # vector of μ_i
theta <- mod_nb$theta                      # estimated dispersion parameter

# Say you want P(Y = 0) for all observations
probs_zero <- dnbinom(0, size = theta, mu = mu)

patients_events %>% filter(cohort == "control") %>% summarize(mean(condition_count == 0))



## Now try with cohort as predictor
patients_events %>%
    group_by(cohort, condition_count) %>%
    summarize(frequency = n(), .groups = "drop") %>%
    mutate(.by = cohort, density = frequency / sum(frequency))

