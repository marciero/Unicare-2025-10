ibis_ER_table <- events_full %>%
    #filter(org_name == "Unicare - Study")  %>%
    filter(str_detect(org_name, "Unicare")) %>%
    filter(event_type == "emergency") %>%
    filter(event_start_date > ibis_coverage_start_date &
               event_start_date < ibis_coverage_end_date) %>%
    summarize(.by = pid, ER_count = n()) %>%
    mutate(cohort = "ibis")

control_ER_table <- events_full %>%
    #filter(org_name != "Unicare - Study")  %>%
    filter(!str_detect(org_name, "Unicare")) %>%
    filter(event_type == "emergency") %>%
    filter(event_start_date <= ibis_coverage_start_date |
               ibis_coverage_start_date == "1969-12-31") %>%
    summarize(.by = pid, ER_count = n()) %>%
    mutate(cohort = "control")


ibis_coverage_table <- report_full %>%
    #filter(org_name == "Unicare - Study") %>%
    filter(str_detect(org_name, "Unicare")) %>%
    select(pid) %>% mutate(cohort = "ibis")

control_coverage_table <- report_full %>%
    #filter(org_name != "Unicare - Study") %>%
    filter(!str_detect(org_name, "Unicare")) %>%
    select(pid) %>% mutate(cohort = "control")

coverage_table <- bind_rows(ibis_coverage_table, control_coverage_table)


ER_coverage_table <- control_ER_table %>%
    bind_rows(ibis_ER_table) %>% right_join(coverage_table, by = c("pid", "cohort")) %>%
    replace(is.na(.), 0)

patients_ER <- report_full %>% select(pid, age, c(condition_count:urologic_cancer))  %>%
    filter(pid %in% ER_coverage_table$pid)

patients_events_ER <- ER_coverage_table %>% inner_join(patients, by = "pid")

patients_events_ER %>% mutate(is_zero  = if_else(ER_count == 0, "zero", "non-zero")) %>%
    ggplot(aes(x = ER_count)) +
    geom_bar(aes(fill = is_zero)) +
    labs(x = "Number of ER visits", y =  NULL,
         fill = "Is zero?",
         title = "ER visits") +
    scale_fill_manual(values = c(clrs1[2], clrs1[3])) +
    theme_minimal(base_size = 12) +
    theme(axis.text.y = element_blank(),
          legend.position = "none",
          #strip.background = element_blank(),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey80", color = NA)
    )

