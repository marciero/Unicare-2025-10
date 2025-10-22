

###############
## neg binomial model for admissions.
## used term = 270


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

mod_nb_predict_admit(patients_events) ## try one time

boots_probs_nb_admit <- function(data){
    idx <- sample(1:nrow(data), nrow(data), replace = TRUE)
    probs <- mod_nb_predict_admit(data[idx, ])
    return(probs)
}

boots_probs_nb_admit(patients_events)

boot_samples_nb_admit <- replicate(2000, boots_probs_nb_admit(patients_events), simplify = FALSE)

save(boot_samples_nb_admit, file = "boot_samples_admissions.RData")

#### ZIP for admissions

mod_zip_predict_admit <- function(data) {
    mod_zip <- pscl::zeroinfl(inpatient_count ~ cohort | 1, data = data, dist = "poisson")

    ibis_df = data.frame(cohort = "ibis")
    control_df = data.frame(cohort = "control")

    ibis_predict <- predict(mod_zip, newdata = ibis_df, type = "prob")
    control_predict <- predict(mod_zip, newdata = control_df, type = "prob")

    ibis_probs <- data.frame(inpatient_count = 0:10) %>%
        mutate(prob = ibis_predict[inpatient_count + 1]) %>%
        mutate(cohort = "ibis")

    control_probs <- data.frame(inpatient_count = 0:10) %>%
        mutate(prob = control_predict[inpatient_count + 1]) %>%
        mutate(cohort = "control")

    mod_zip_probs <- bind_rows(ibis_probs, control_probs)

    return(mod_zip_probs)
}

mod_zip_predict_admit(patients_events) ## try one time

boots_probs_zip_admit <- function(data){
    idx <- sample(1:nrow(data), nrow(data), replace = TRUE)
    probs <- mod_zip_predict_admit(data[idx, ])
    return(probs)
}

boot_samples_zip_admit <- replicate(2000, boots_probs_zip_admit(patients_events), simplify = FALSE)


save(boot_samples_nb_admit, boot_samples_zip_admit, file = "boot_samples_admissions.RData")
#####

boot_df_zip <- bind_rows(boot_samples_zip, .id = "replicate")

boot_stats3 <- boot_df_zip %>% group_by(cohort, condition_count) %>%
    summarize(mean_prob = mean(prob),
              se_prob = sd(prob))


