#### Proportion of patients with conditions, by cohort


patients_events %>% group_by(cohort) %>% 
  summarize(across(-c(pid, inpatient_count), mean)) %>% 
  pivot_longer(-cohort, names_to = "condition", values_to = "mean") %>% 
  filter(condition != "age") %>% 
  filter(condition != "condition_count") %>% 
  ggplot(aes(mean, condition,  fill = cohort)) +
  geom_col(position = "dodge")



patients_events %>% group_by(cohort) %>% 
  summarize(across(-pid, mean)) %>% 
  kbl()


## The non Unicare have some zero chronic conditions counts

patients_events %>% group_by(cohort) %>% 
  summarize(mean(condition_count == 0)) %>% 
  kbl()


 
