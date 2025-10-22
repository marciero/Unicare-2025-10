zero_inf_bayes0 ## is zero_inf_bayes1 with default priors.

## models saved for quick quarto rendering

zero_inf_bayes1 <- brm(inpatient_count ~ cohort,
                        data = patients_events, family = zero_inflated_poisson(),
                       prior = c(prior(normal(0.5, 1), class = Intercept),
                                 prior(normal(0, 0.5), class = b, coef = "cohortibis")),
                        chains = 4, iter = 5000*2, seed = 84735,
                        sample_prior = "no"
                        )



zero_inf_bayes2 <- brm(bf(inpatient_count ~ cohort,
                         zi ~ condition_count),
                       data = patients_events,
                       family = zero_inflated_poisson(),

                       prior = c(
                            ## Count priors
                              prior(normal(0.5, 1), class = Intercept),
                             prior(normal(0, 0.5), class = b, coef = "cohortibis"),
                            ## Zero-inflation priors
                             prior(normal(2, 0.5), class = Intercept, dpar = zi),
                             prior(normal(0, 0.1), class = b, coef = "condition_count", dpar = zi)
                            ),

                       chains = 4, iter = 5000*2, seed = 84735,
                       sample_prior = "no"
)


save(zero_inf_bayes0, zero_inf_bayes1, zero_inf_bayes2,
     file = "data/zero_inf_bayes_models.RData")

summary(zero_inf_bayes2)



get_prior(bf(inpatient_count ~ cohort,
             zi ~ condition_count),
          data = patients_events,
          family = zero_inflated_poisson())

prior_summary(zero_inf_bayes2)

post <- as.data.frame(zero_inf_bayes2)
dimnames(post)

mcmc_trace(zero_inf_bayes2, pars = c("b_Intercept", "b_cohortibis", "b_zi_Intercept", "b_zi_condition_count"),
           facet_args = list(ncol = 2)) +
  labs(title = "Posterior distributions for zero-inflated model parameters")

fixef(zero_inf_bayes2)

mcmc_hist(zero_inf_bayes2, pars = c("b_Intercept", "b_cohortibis", "b_zi_Intercept", "b_zi_condition_count"),
           facet_args = list(ncol = 2)) +
  labs(title = "Posterior distributions for zero-inflated model parameters")
mcmc_hist(posterior_array, pars = vars(starts_with("b_")))
mcmc_intervals(zero_inf_bayes2, pars = vars(starts_with("b_"))) +
  labs(title = "Posterior intervals for zero-inflated model parameters")
mcmc_intervals(posterior_df, pars = vars(starts_with("r_")))

preds <- posterior_predict(zero_inf_bayes2, newdata = patients_events[1,])
preds_df <- data.frame(inpatient_count = preds)

preds_df %>%
       group_by(inpatient_count) %>%
      summarize(frequency = n(), .groups = "drop") %>%
       mutate(density = frequency / sum(frequency)) %>%
       mutate(cohort = "ibis")

######
##########
#### From ibis_03 presentation
############
###########



mcmc_dens(zero_inf_bayes2)

pp_check(zero_inf_bayes2, type = "dens_overlay")


You can see the model estimates an additional shape parameter, $\phi$, which controls the standard deviation. (`inv_phi` = $1/\phi$ was created for convenience in the model, and was given a normal mean zero, variance one prior.)

### Post predictive check

pp_check(zero_inf_bayes2, type = "dens_overlay")


y_rep <- posterior_predict(zero_inf_bayes2)

y_obs <- zero_inf_bayes2$data$inpatient_count

ppc_dens_overlay(y = y_obs, yrep = y_rep[1:100, ])


prop <- function(x) mean(x == 0)
ppc_stat(y = augusta_unique_hcc$hcc_conds, yrep = y_rep, stat = "prop") + labs(title = "Proportion of hcc count = 0 in posterior samples compared to the data")
```

prop <- function(x) mean(x == 2)
ppc_stat(y = y_obs, yrep = y_rep, stat = "prop") + labs(title = "Proportion of hcc count = 2 in posterior samples compared to the data")



prop <- function(x) mean(x == 5)
ppc_stat(y = augusta_unique_hcc$hcc_conds, yrep = y_rep, stat = "prop") + labs(title = "Proportion of hcc count = 5 in posterior samples compared to the data")



y_rep <- as.matrix(zero_inf_bayes2, variable = "y_rep")
ppc_dens_overlay(y = augusta_unique_hcc$hcc_conds, yrep = y_rep[1:200, ])

So we can easily do things like find the probability that coeffs lie in $any$ given interval, or compute statistics based on the parameters. We also can generate `visits` samples, so can find probabilites for outcomes, likelihood ratios, etc. For example, we can compare the likelihood that a person with 10  `hcc_conds` has  more than 4 visits, to that of a person with 5 hcc's.

$$
\frac{P(\rm{vists} > 4 | \rm{hcc} = 10)}{P(\rm{vists} > 4 | \rm{hcc} = 5)}
$$

We simulate 2000 samples of each and compare the proportion of the samples in each case that are greater than 4.

samps_df <- as.data.frame(vis_01_sim)



set.seed(84732)
pred_new <- samps_df %>% mutate(vis_5cond = rpois(2000, exp(alpha + 5*beta))) %>%
         mutate(vis_10cond = rpois(2000, exp(alpha + 10*beta))) %>% select(c(vis_10cond, vis_5cond))

mean(pred_new$vis_10cond > 4)/mean(pred_new$vis_5cond > 4)


