library(MatchIt)

m.out0 <- matchit(as.factor(cohort)  ~  age + chf + atrial_fibrillation, data = patients_events,
                  method = "nearest", distance = "glm")

summary(m.out0)

m.out0$distance 

library(tidymodels)
wt_mod <- logistic_reg() %>% fit(as.factor(cohort)  ~  age + chf + atrial_fibrillation, data = patients_events,
    family = "binomial")

predict(wt_mod, new_data = patients_events, type = "prob")

