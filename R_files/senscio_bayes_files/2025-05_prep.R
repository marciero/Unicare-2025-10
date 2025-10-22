X <- patients_events %>% select(-c(pid, inpatient_count, cohort, condition_count))

lm(age ~ ., X) %>% summary()
lm(chd ~ ., X) %>% summary()

XX <- cor(X)

eig <- eigen(XX)
eig$values

library(tidymodels)

rec <- recipe(admit ~ ., data = patients_events %>% select(-c(pid, inpatient_count, condition_count)))  %>%
    step_nzv(all_predictors())

rec_prep <- rec %>% prep()

## These are removed
rec_prep %>% tidy(number = 1)
tidy(rec_prep$steps[[1]])

## These are retained
rec_prep %>% bake(patients_events %>% select(-c(pid, inpatient_count, condition_count))) %>% glimpse()

tidy(rec_prep$steps[[1]]) %>%
    filter(!is.na(id))



patients_events %>% select(-c(pid, age, inpatient_count, condition_count)) %>%
    pivot_longer(c(2:33), names_to = "condition", values_to = "value") %>%
    group_by(condition, cohort) %>%
    summarise(mean = mean(value)) %>%
    ggplot(aes(x = mean, y = fct_reorder(condition, mean), fill = cohort)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Proportion", y =  NULL)

patients_events %>% select(-c(pid, age, admit, inpatient_count, condition_count)) %>%
    pivot_longer(c(2:33), names_to = "condition", values_to = "value") %>%
    group_by(condition) %>%
    summarise(mean = mean(value)) %>%
    ggplot(aes(x = mean, y = fct_reorder(condition, mean))) +
    #geom_col() +
    geom_point(size  = 3) +
    geom_segment(aes(x = 0, xend = mean, yend = condition), size = 1) +
    labs(x = "Proportion", y =  NULL)

df <- rec_prep %>% bake( patients_events %>% select(-c(pid, inpatient_count, condition_count)))

mod_covariate <- logistic_reg() %>% fit(as.factor(admit) ~ ., df %>% select(-c(cohort)))
tidy(mod_covariate)


XX <- cor(df %>% select(-c(admit, cohort)))
eig <- eigen(XX)
eig$values

XX
