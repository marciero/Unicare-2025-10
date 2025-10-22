
########
### Used in zero_inf_admissions.qmd and
## additional follows this below

zip_bayes <- brm(bf(inpatient_count ~ cohort + chf + atrial_fibrillation,
                         zi ~ 1),
                       data = patients_events,
                       family = zero_inflated_poisson(),

                       prior = c(
                            ## Count priors
                              prior(normal(0.5, 1), class = Intercept),
                             prior(normal(0, 0.5), class = b, coef = "cohortibis"),
                             prior(normal(0, 0.5), class = b, coef = "chf"),
                             prior(normal(0, 0.5), class = b, coef = "atrial_fibrillation"),
                            ## Zero-inflation priors
                             prior(normal(2, 0.5), class = Intercept, dpar = zi)
                            ),

                       chains = 4, iter = 5000*2, seed = 84735,
                       sample_prior = "no"
)


zinb_bayes <- brm(bf(inpatient_count ~ cohort + chf + atrial_fibrillation,
                          zi ~ 1),
                       data = patients_events,
                       family = zero_inflated_negbinomial(),
                       prior = c(
                           ## Count priors
                           prior(normal(0.5, 1), class = Intercept),
                           prior(normal(0, 0.5), class = b, coef = "cohortibis"),
                           prior(normal(0, 0.5), class = b, coef = "chf"),
                           prior(normal(0, 0.2), class = b, coef = "atrial_fibrillation"),
                           ## Zero-inflation priors
                           prior(normal(2, 0.1), class = Intercept, dpar = zi)
                       ),

                       chains = 4, iter = 5000*2, seed = 84735,
                       sample_prior = "no"
)


save(zip_bayes, zinb_bayes,
     file = "data/zif_bayes.RData")

###############

#######
### Post zero_inf_admissions-short. Bayes with age in count and zero-inflation parts
######


#### Scale age, fit and compare with zip3
patients_events_scale <- patients_events %>%
    mutate(age = (age - 60) / 10)

mod_zip3 <-
    pscl::zeroinfl(inpatient_count ~
                       cohort  + age + chf + atrial_fibrillation | age,
                   data = patients_events_scale, dist = "poisson")

summary(mod_zip3)

zip3_bayes <- brm(bf(inpatient_count ~ cohort + age + chf + atrial_fibrillation,
                     zi ~ age),
                  data = patients_events_scale,
                  family = zero_inflated_poisson(),
                  prior = c(
                      ## Count priors
                      prior(normal(0.5, 1), class = Intercept),
                      prior(normal(0, 0.5), class = b, coef = "cohortibis"),
                      prior(normal(0, 0.5), class = b, coef = "chf"),
                      prior(normal(0, 0.5), class = b, coef = "atrial_fibrillation"),
                      prior(normal(0, 0.5), class = b, coef = "age"),
                      ## Zero-inflation priors
                      prior(normal(2, 0.5), class = Intercept, dpar = zi),
                      prior(normal(0, 0.2), class = b, coef = "age", dpar = zi)
                  ),
                  chains = 4, iter = 5000*2, seed = 84735,
                  sample_prior = "no"
)


save(zip3_bayes, file = "data/zip3_bayes.RData")
fixef(zip3_bayes)

mcmc_dens(zip3_bayes, pars = c("b_Intercept", "b_cohortibis",
                               "b_age", "b_chf", "b_atrial_fibrillation", "b_zi_Intercept", "b_zi_age"),
          facet_args = list(ncol = 2)) +
    labs(title = "Posterior distributions for zero-inflated model parameters; scaled age")


mcmc_intervals(zip3_bayes, pars = vars(starts_with("b_"))) +
    labs(title = "Posterior intervals for zero-inflated model parameters")

y_rep <- posterior_predict(zip3_bayes)

y_obs <- zip3_bayes$data$inpatient_count

ppc_dens_overlay(y = y_obs, yrep = y_rep[1:100, ]) +
    labs(
        x = "Number of admissions", y = NULL) +
    theme_minimal(base_size = 12)

### Now by cohort
y_rep_ibis <- posterior_predict(zip3_bayes, newdata = patients_events_scale %>% filter(cohort == "ibis"))
y_rep_control <- posterior_predict(zip3_bayes, newdata = patients_events_scale %>% filter(cohort == "control"))

y_obs_ibis <- patients_events_scale %>% filter(cohort == "ibis") %>% pull(inpatient_count)
y_obs_control <- patients_events_scale %>% filter(cohort == "control") %>% pull(inpatient_count)

ppc_dens_overlay(y = y_obs_ibis, yrep = y_rep_ibis[1:100, ]) +
    labs(
        x = "Number of admissions", y = NULL) +
    theme_minimal(base_size = 12) +
    ggtitle("Density overlay check for IBIS cohort")

ppc_dens_overlay(y = y_obs_control, yrep = y_rep_control[1:100, ]) +
    labs(
        x = "Number of admissions", y = NULL) +
    theme_minimal(base_size = 12) +
    ggtitle("Density overlay check for control cohort")

## More Post predictive checks
### Overall

y_obs <- zip3_bayes$data$inpatient_count
y_rep <- posterior_predict(zip3_bayes)

# Function to compute proportion of a given count
make_prop_stat <- function(k) {
    function(x) mean(x == k)
}

# Loop over counts 0 through 10 and compute stats
stats_df <- map_dfr(0:5, function(k) {
    stat_fun <- make_prop_stat(k)

    tibble(
        count = k,
        yrep_stats = apply(y_rep, 1, stat_fun),   # each row is a draw
        y_obs_stat = stat_fun(y_obs)
    )
}, .id = "id") %>%
    mutate(count = as.integer(count))

# Unnest for ggplot
stats_long <- stats_df %>%
    select(count, yrep_stats, y_obs_stat) %>%
    unnest(yrep_stats = yrep_stats)

# Plot
ggplot(stats_long, aes(x = yrep_stats)) +
    geom_density(fill = "skyblue", alpha = 0.6) +
    geom_vline(aes(xintercept = y_obs_stat), color = "red", linetype = "dashed") +
    facet_wrap(~ count, scales = "free") +
    labs(
        title = "Posterior predictive distribution of proportion for each count (0–10)",
        x = "Proportion in posterior samples",
        y = "Density"
    ) +
    theme_minimal()

### Now by cohort

stats_df_cohort <- map_dfr(0:5, function(k) {
    stat_fun <- make_prop_stat(k)

    tibble(
        count = k,
        yrep_stats_ibis = apply(y_rep_ibis, 1, stat_fun),   # each row is a draw
        y_obs_stat_ibis = stat_fun(y_obs_ibis),
        yrep_stats_control = apply(y_rep_control, 1, stat_fun),
        y_obs_stat_control = stat_fun(y_obs_control)
    )
}, .id = "id") %>%
    mutate(count = as.integer(count))


stats_long_cohort <- stats_df_cohort %>%
    select(count, yrep_stats_ibis, y_obs_stat_ibis, yrep_stats_control, y_obs_stat_control) %>%
    pivot_longer(
        cols = -count,
        names_to = c(".value", "cohort"),
        names_pattern = "(yrep_stats|y_obs_stat)_(.+)"
    )


p_ibis <- stats_long_cohort %>%
    filter(cohort == "ibis") %>%
    ggplot(aes(x = yrep_stats)) +
    geom_density(fill = "blue", alpha = 0.6) +
    geom_vline(aes(xintercept = y_obs_stat), color = "red", linetype = "dashed") +
    facet_wrap(~ count, scales = "free") +
    labs(
        title = "Posterior predictive distribution by count (IBIS cohort)",
        x = "Proportion in posterior samples",
        y = "Density"
    ) +
    theme_minimal()

# Plot for CONTROL
p_control <- stats_long_cohort %>%
    filter(cohort == "control") %>%
    ggplot(aes(x = yrep_stats)) +
    geom_density(fill = "orange", alpha = 0.6) +
    geom_vline(aes(xintercept = y_obs_stat), color = "red", linetype = "dashed") +
    facet_wrap(~ count, scales = "free") +
    labs(
        title = "Posterior predictive distribution by count (Control cohort)",
        x = "Proportion in posterior samples",
        y = "Density"
    ) +
    theme_minimal()

p_ibis
p_control

patients_events %>% summarize(.by = cohort, mean(inpatient_count > 0))
patients_events %>% summarize(.by = cohort, mean(inpatient_count == 0))
patients_events %>% summarize(.by = cohort, mean(inpatient_count))
