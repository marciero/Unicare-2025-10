mod <- zeroinfl(inpatient_count ~ cohort | cohort, data = patients_events, dist = "poisson")


mod_nb_predict_admit <- function(data) {
    mod_nb <- MASS::glm.nb(inpatient_count ~ cohort, data = data, link = "log")
    ibis_df = data.frame(cohort = "ibis")
    control_df = data.frame(cohort = "control")

    mu_ibis_nb <- predict(mod_nb, newdata = ibis_df, type = "response")
    mu_control_nb <- predict(mod_nb, newdata = control_df, type = "response")

    theta_nb <- mod_nb$theta

    mod_nb_probs <- data.frame(inpatient_count = 0:10) %>%
        mutate(prob = dnbinom(inpatient_count, mu = mu_ibis_nb, size = theta_nb)) %>%
        mutate(cohort = "ibis") %>%
        bind_rows(data.frame(inpatient_count = 0:10) %>%
                      mutate(prob = dnbinom(inpatient_count, mu = mu_control_nb, size = theta_nb)) %>%
                      mutate(cohort = "control"))

    return(mod_nb_probs)
}

mod_nb_predict_admit(patients_events) %>%
    full_join(obs_freqs_cohort_inpatient, by = c("inpatient_count", "cohort")) %>%
    mutate(expected = prob * frequency)


chisq.test(ibis_df %>% select(frequency, expected), expected ~ frequency)


expect <- c(62, 12.44, 16.73, 17.74, 16.3, 13.58, 10.55, 7.76, 5.48, 10.43)
observed <- c(62, 16,9,19, 19,15,13,4,6,10)
chisq.test(observed, p = expect / sum(expect))

sum((expect - observed)^2/ expect)

###################

nd <- data.frame(cohort = patients_events[1,])
mod_zinb <- zeroinfl(inpatient_count ~ cohort | cohort, data = patients_events, dist = "negbin")
mod_zip <- zeroinfl(inpatient_count ~ cohort | cohort, data = patients_events, dist = "poisson")
mod_nb <- MASS::glm.nb(inpatient_count ~ cohort, data = patients_events, link = "log")


pchisq(2 * (logLik(mod_zinb) - logLik(mod_zip)), df = 1, lower.tail = FALSE)
pchisq(2 * (logLik(mod_nb) - logLik(mod_zip)), df = 1, lower.tail = FALSE)


pscl::zeroinfl(inpatient_count ~ cohort | 1, data = patients_events, dist = "poisson")
pscl::zeroinfl(inpatient_count ~ cohort   | condition_count, data = patients_events, dist = "poisson")
pscl::zeroinfl(inpatient_count ~ cohort | condition_count , data = patients_events, dist = "negbin")
pscl::zeroinfl(inpatient_count ~ cohort + age | condition_count , data = patients_events, dist = "negbin")

poisson_reg() %>%
    fit(inpatient_count ~ cohort, data = patients_events) %>% tidy()

MASS::glm.nb(inpatient_count ~ cohort, data = patients_events, link = "log") %>%
    tidy()
zeroinfl(inpatient_count ~ cohort   | condition_count, data = patients_events, dist = "poisson") %>%
    coefficients()

zeroinfl(inpatient_count ~ cohort   | condition_count, data = patients_events, dist = "negbin") %>%
    coefficients()



