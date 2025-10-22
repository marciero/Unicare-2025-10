

## Zero inflated poisson regression

```{r}
mod3 <- zeroinfl(inpatient_count ~ cohort | 1, data = inpatient_coverage_table, dist = "poisson")
summary(mod3)
```

## Condition on age, chronic conditions

"standard" Poisson

```{r}
mod4 <- glm(inpatient_count ~ ., data = patients_events %>% select(-c(pid)), family = "poisson")
tidy(mod4)
```

Zero inflated Poisson

```{r}
mod5 <- zeroinfl(inpatient_count ~ . | 1, data = patients_events %>%
                     select(-c(pid, urologic_cancer)), dist = "poisson")
summary(mod5)
```

p(Y = y; \mu, k) =
    \frac{\Gamma(y + k)}{\Gamma(k) \Gamma(y+1)}
\left(\frac{\mu}{\mu + k}\right)^y \left(\frac{k}{\mu + k}\right)^k


pulse %>%
    ggplot(aes(x = books)) +
    geom_bar() +
    labs(x = "Number of books read", y =  NULL,
         title = "Books Read Counts",
         subtitle = "Pulse survey data",
         caption = "Source:Cards Against Humanity's Pulse of the Nation project (https://thepulseofthenation.com/)")

p1 <- patients_events %>%
    group_by(condition_count) %>%
    summarize(frequency = n(), .groups = "drop") %>%
    mutate(density = frequency / sum(frequency)) %>%
    ggplot(aes(x = condition_count, y = density)) +
    geom_col() +
    labs(x = "Number of conditions", y =  NULL,
         title = "Proportions; observed") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))

set.seed(12445)

y_reps <- rpois(20000, lambda = 1.45)

y_reps_df <- data.frame(condition_count = y_reps) %>%
    group_by(condition_count) %>%
    summarize(frequency = n(), .groups = "drop") %>%
    mutate(density = frequency / sum(frequency)) %>%
    mutate(cohort = "simulated")

ibis_freqs <- patients_events %>%
    group_by(condition_count) %>%
    summarize(frequency = n(), .groups = "drop") %>%
    mutate(density = frequency / sum(frequency)) %>%
    mutate(cohort = "observed")

df <- bind_rows(y_reps_df, ibis_freqs)

p2 <- df %>%
    ggplot(aes(x = condition_count, y = density)) +
    geom_col(aes(fill = cohort), position = "dodge") +
    labs(x = "Number of conditions", y =  NULL,
         title = "Proportions; simulated and observed",
         fill = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))

p1 + p2 + plot_layout(ncol = 2)


mod_nb <- MASS::glm.nb(condition_count ~ cohort, data = patients_events, link = "log")
summary(mod_nb)
tidy(mod_nb)


pchisq(2 * (logLik(mod_nb) - logLik(mod_p3$fit)), df = 1, lower.tail = FALSE)



mod_zip1 <- zeroinfl(condition_count ~ 1 | cohort, data = patients_events, dist = "poisson")
summary(mod3)

library(wesanderson)
library(MetBrewer)
library(RColorBrewer)

theme_nice <- function() {
    theme_minimal(base_family = "Jost") +
        theme(panel.grid.minor = element_blank(),
              plot.title = element_text(family = "Jost", face = "bold"),
              axis.title = element_text(family = "Jost Medium"),
              strip.text = element_text(family = "Jost", face = "bold",
                                        size = rel(1), hjust = 0),
              strip.background = element_rect(fill = "grey80", color = NA))
}

clrs <- MetBrewer::met.brewer("Lakota")

patients_events %>% mutate(is_zero  = if_else(condition_count == 0, "yes", "no")) %>%
    ggplot(aes(x = condition_count)) +
    geom_bar(aes(fill = is_zero)) +
    labs(x = "Number of conditions", y =  NULL,
         fill = "Is zero?",
         title = "Chronic Conditions Counts") +
    scale_fill_manual(values = c(clrs[2], clrs[3])) +
    theme_minimal()

    #scale_fill_manual(values = wes_palette("BottleRocket2"))

patients_events %>% mutate(is_zero  = if_else(condition_count == 0, "yes", "no")) %>%
    ggplot(aes(x = condition_count)) +
    geom_bar(aes(fill = is_zero)) +
    labs(x = "Number of conditions", y =  NULL,
         fill = "Is zero?",
         title = "Chronic Conditions Counts") +
    scale_fill_brewer(palette = 8) +
    theme_minimal()

my_colors <- brewer.pal(n = 8, name = "Dark2")

patients_events %>% mutate(is_zero  = if_else(condition_count == 0, "yes", "no")) %>%
    ggplot(aes(x = condition_count)) +
    geom_bar(aes(fill = is_zero)) +
    labs(x = "Number of conditions", y =  NULL,
         fill = "Is zero?",
         title = "Chronic Conditions Counts") +
    scale_fill_manual(values = c(my_colors[5], my_colors[2])) +
    theme_minimal()


patients_events %>%
    group_by(cohort, condition_count) %>%
    summarize(frequency = n(), .groups = "drop") %>%
    mutate(.by = cohort, density = frequency / sum(frequency)) %>%
    ggplot(aes(x = condition_count, y = density)) +
    geom_col(aes(fill = cohort), position = "dodge") +
    labs(x = "Number of conditions", y =  NULL,
         title = "Proportions of Chronic Conditions",
         fill = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values = wes_palette("IsleofDogs1"))


df %>%
    mutate(is_zero_cohort  = case_when(cohort == "simulated" & condition_count == 0 ~ "simulated zero",
                                       cohort == "observed" & condition_count == 0 ~ "observed zero",
                                       cohort == "simulated" & condition_count != 0 ~ "simulated non-zero",
                                       cohort == "observed" & condition_count != 0 ~ "observed non-zero"
    )) %>%
    ggplot(aes(x = condition_count, y = density)) +
    geom_col(aes(fill = is_zero_cohort), position = "dodge") +
    labs(x = "Number of conditions", y =  NULL,
         title = "Simulated and observed chronic conditions",
         fill = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values = c(clrs5[4], clrs4[3], clrs5[1], clrs4[7])) +
    theme_minimal(base_size = 12) +
    theme(axis.text.y = element_blank(),
          #legend.position = "none",
          #strip.background = element_blank(),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey80", color = NA)
    )

