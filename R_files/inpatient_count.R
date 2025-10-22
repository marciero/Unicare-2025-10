

patients_events %>% group_by(inpatient_count) %>%
    summarize(frequency = n(), .groups = "drop") %>%
    mutate(is_zero  = if_else(inpatient_count == 0, "zero", "non-zero")) %>%
    mutate(density = frequency / n()) %>%
    ggplot(aes(x = inpatient_count, y = density)) +
    geom_col(aes(fill = is_zero)) +
    scale_x_continuous(limits = c(-0.5,10),
                       breaks = c(0, 5, 10)) +
    labs(x = "Number of admissions", y =  NULL,
         fill = "Is zero?",
         title = "Overall inpatient admissions") +
    scale_fill_manual(values = c(clrs1[2], clrs1[3])) +
    theme_minimal(base_size = 12) +
    theme(#axis.text.y = element_blank(),
          legend.position = "none",
          #strip.background = element_blank(),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey80", color = NA)
    )


patients_events %>%
    group_by(cohort, inpatient_count) %>%
    summarize(frequency = n(), .groups = "drop") %>%
    mutate(.by = cohort, density = frequency / sum(frequency)) %>%
    mutate(is_zero_cohort  = case_when(cohort == "ibis" & inpatient_count == 0 ~ "ibis zero",
                                       cohort == "control" & inpatient_count == 0 ~ "control zero",
                                       cohort == "ibis" & inpatient_count != 0 ~ "ibis non-zero",
                                       cohort == "control" & inpatient_count != 0 ~ "control non-zero"
    )) %>%
    ggplot(aes(x = inpatient_count, y = density)) +
    geom_col(aes(fill = is_zero_cohort), position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(limits = c(-0.5,10),
                       breaks = c(0, 5, 10)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values = c(clrs3[1], clrs2[1], clrs3[2], clrs2[2])) +
    labs(x = "Number of admissions", y =  NULL,
         title = "Inpatient admissions by cohort",
         fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(#axis.text.y = element_blank(),
          #strip.background = element_blank(),
          panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey80", color = NA)
    )



