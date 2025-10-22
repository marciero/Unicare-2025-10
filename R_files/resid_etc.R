
library(ggfortify)
library(MASS)
mod <- logistic_reg() %>% fit(as.factor(inpatient_count > 0) ~ cohort, data = patients_events)
tidy(mod)

patients_events %>%  summarize(.by = cohort,
                             mean(inpatient_count > 0))

autoplot(mod_nb, which = 4)

mod_aug <- mod$fit %>% augment()

glance(mod$fit)

patients_events[mod_aug$.resid > 2,] %>% glimpse()

prop.test(inpatient_count ~ cohort, data = patients_events)

