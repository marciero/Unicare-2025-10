
########
######. from unicare vs not



zi_table <- inpatient_coverage_table %>% group_by(cohort, inpatient_count) %>%
    summarize(count = n())

uv_table <- inpatient_coverage_table %>%
    group_by(cohort, inpatient_count) %>%
    summarize(member_count = n())

patients_events %>% group_by(cohort, inpatient_count) %>%
    summarize(member_count = n())

admityn <- patients_events %>% mutate(admit = if_else(inpatient_count == 0, 0, 1))

logistic_reg() %>% fit(as.factor(admit) ~ cohort + age + chf + atrial_fibrillation, admityn) %>% tidy()

m0 <- poisson_reg() %>% fit(inpatient_count ~ cohort  + chf + atrial_fibrillation, data = patients_events)
modzi <- pscl::zeroinfl(inpatient_count ~ cohort + age | cohort, data = patients_events, dist = "negbin")

m1 <- MASS::glm.nb(inpatient_count ~ cohort +  chf + atrial_fibrillation , data = patients_events, link = "log")
tidy(m1)

m2 <- zeroinfl(inpatient_count ~ cohort + chf + atrial_fibrillation | 1, data = patients_events, dist = "poisson")
zinf <- pscl::zeroinfl(inpatient_count ~ cohort | condition_count , data = patients_events, dist = "negbin") %>% summary()

predict(mod_nb, newdata = admityn[1,])



exp(-.6)

conditions <- admityn %>% select(c(age, condition_count:urologic_cancer)) %>%
    select(-condition_count)

X <- cor(conditions)
eigs <- eigen(X)
eigs$values

cor(conditions, admityn$admit) %>% as.data.frame() %>% arrange(desc(V1))

library(corrr)
correlate(conditions) %>%
    stretch(na.rm = TRUE) %>%             # Long format, removes redundant NA values
    filter(x != y) %>%                    # Exclude self-correlations
    arrange(desc(abs(r)))



admit_table <- admityn %>%
    select(cohort, admit) %>% table()
prop.test(admit_table[,2], c(sum(admit_table[1,]), sum(admit_table[2, ])))

mod_nb_predict <- function(data) {
    mod_nb <- MASS::glm.nb(condition_count ~ cohort + age, data = data, link = "log")
    ibis_df = data.frame(cohort = "ibis")
    control_df = data.frame(cohort = "control")

    mu_ibis_nb <- predict(mod_nb, newdata = ibis_df, type = "response")
    mu_control_nb <- predict(mod_nb, newdata = control_df, type = "response")

    theta_nb <- mod_nb$theta

    mod_nb_probs <- data.frame(condition_count = 0:10) %>%
        mutate(prob = dnbinom(condition_count, mu = mu_ibis_nb, size = theta_nb)) %>%
        mutate(cohort = "ibis") %>%
        bind_rows(data.frame(condition_count = 0:10) %>%
                      mutate(prob = dnbinom(condition_count, mu = mu_control_nb, size = theta_nb)) %>%
                      mutate(cohort = "control"))

    return(mod_nb_probs)
}
