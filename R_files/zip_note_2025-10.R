### short version of zip_note. For quick model estimates. See zip_note for more


mod_zip <- zeroinfl(inpatient_count ~ cohort | cohort, data = patients_events, dist = "poisson")
    summary(mod_zip)

df <- patients_events[1,]

predict(mod_zip, newdata = df, type = "response")  # overall expected count
predict(mod_zip,  newdata = df, type = "count")     # Poisson mean (μ)
predict(mod_zip,  newdata = df, type = "zero")      # structural zero probability (π)
predict(mod_zip,  newdata = df, type = "prob") # probabilities that Y = k

coef(mod_zip)[1]
coef(mod_zip, model = "count")


mod_zinb <- zeroinfl(inpatient_count ~ cohort | cohort, data = patients_events, dist = "negbin")
summary(mod_zinb)


## Below only works for non zero infl negbin from MASS package
predict(mod_zinb, type = "response") ## mean
predict(mod_zinb, type = "link")    ## linear predictor
predict(mod_zinb, type = "terms")      ## linear predictor for each term
predict(mod_zinb, type = "prob", newdata = patients_events[1,]) # probability that Y = 0
## mean

pscl::zeroinfl(inpatient_count ~ 
                             cohort  + age  + atrial_fibrillation | chf + age, 
                           data = patients_events, dist = "poisson") %>% summary()
