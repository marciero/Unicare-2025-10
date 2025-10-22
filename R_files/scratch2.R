## Model comparison

#### Negative binomial [with **`cohort`**]{style="color:#931e18"} as predictor

[**control**]{style="color:#f0be3d"} cohort

####  #### Negative binomial [without **`cohort`**]{style="color:#931e18"} as predictor


For the [**ibis**]{style="color:#931e18"} cohort

```{r}
ibis_chi <- mod_nb_predict_admit(patients_events) %>%
    full_join(obs_freqs_cohort_inpatient, by = c("inpatient_count", "cohort")) %>%
    mutate(expected = prob * frequency) %>% filter(cohort == "ibis")


sum(ibis_chi$chisq)

chisq.test(ibis_df$frequency, p = ibis_df$prob)

```

for the [**control**]{style="color:#f0be3d"} cohort

```{r}

control_df <- mod_nb_predict_admit(patients_events) %>%
    full_join(obs_freqs_cohort_inpatient, by = c("inpatient_count", "cohort")) %>%
    mutate(expected = prob * frequency) %>% filter(cohort == "control")  %>%
    filter(expected >= 5) %>% select(frequency, expected)

chisq.test(control_df, expected ~ frequency)

```


