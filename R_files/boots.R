### Bootstrapped predictions and plots
## for the negative binomial model and zerio-inflated model,
## also plots. See zer0infl_2025-05.R  for most up to date plots.


mod_nb_predict <- function(data) {
  mod_nb <- MASS::glm.nb(condition_count ~ cohort, data = data, link = "log")
  ibis_df = data.frame(cohort = "ibis")
  control_df = data.frame(cohort = "control")

  mu_ibis_nb <- predict(mod_nb, newdata = ibis_df, type = "response")
  mu_control_nb <- predict(mod_nb, newdata = control_df, type = "response")

  theta_nb <- mod_nb$theta

  mod_nb_probs <- data.frame(condition_count = 0:10) %>%
      mutate(prob = dnbinom(condition_count, mu = mu_ibis_nb, size = theta_nb)) %>%
      mutate(cohort = "ibis") %>%
      bind_rows(data.frame(condition_count = 0:10) %>%
                    mutate(prob = dnbinom(condition_count, mu = mu_control_nb, size = theta_nb)) %>%
                    mutate(cohort = "control"))

  return(mod_nb_probs)
}

mod_nb_predict(patients_events) ## try one time

boots_probs_nb <- function(data){
    idx <- sample(1:nrow(data), nrow(data), replace = TRUE)
    probs <- mod_nb_predict(data[idx, ])
    return(probs)
    }

boots_probs_nb(patients_events)

boot_samples_nb <- replicate(2000, boots_probs_nb(patients_events), simplify = FALSE)

boot_df <- bind_rows(boot_samples_nb, .id = "replicate")

boot_stats1 <- boot_df %>% group_by(cohort, condition_count) %>%
    summarize(mean_prob = mean(prob),
              se_prob = sd(prob))

legend_df <- tibble(
    x = 11 , y = 0,
    source = c("Estimate", "Observed")
)

boot_stats1 %>% full_join(mod_nb_probs_freqs) %>%
    ggplot(aes(x = condition_count)) +
    geom_pointrange(aes(y = mean_prob, ymin = mean_prob - 1.96*se_prob,
                        ymax = mean_prob + 1.96*se_prob),
                    position = position_dodge(width = 0.25),
                    color = clrs5[1])  +
    geom_line(aes(y = mean_prob), color = clrs5[1], alpha = 0.5) +
    geom_point(aes(y = density), color = clrs4[4], shape = 15, size = 2) +
    geom_line(aes(y = density), color = clrs4[4], alpha = 0.7) +

    # Add dummy layers to trigger legend entries
    geom_line(data = legend_df, aes(x = x, y = y, color = source),
              # inherit.aes = FALSE,
              size = 1) +
    geom_point(data = legend_df, aes(x = x, y = y, color = source, shape = source),
               size = 3) +

    scale_color_manual(values = c("Estimate" = clrs5[1], "Observed" = clrs4[4])) +
    scale_shape_manual(values = c("Estimate" = 16, "Observed" = 15)) +

    scale_x_continuous(breaks = 0:10) +
    coord_cartesian(xlim = c(0, 10)) +

    facet_wrap(~ cohort) +
    theme_minimal(base_size = 12) +
    theme(#axis.text.y = element_blank(),
        #legend.position = "none",
        #strip.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey80", color = NA),
        legend.position = "bottom"
    ) +
    labs(x = "Number of conditions", y =  "probability/proportion",
         title = "Model estimates and confidence intervals vs observed proportions",
         fill = NULL)

## Now dont group  by cohort

obs_freqs <- patients_events %>%
    group_by(condition_count) %>%
    summarize(frequency = n()) %>%
    mutate(density = frequency / sum(frequency)) %>% filter(condition_count <= 10)

boot_stats2 <- boot_df %>% group_by(condition_count) %>%
    summarize(mean_prob = mean(prob),
              se_prob = sd(prob))

boot_stats2 %>% full_join(obs_freqs) %>%
    ggplot(aes(x = condition_count)) +
    geom_pointrange(aes(y = mean_prob, ymin = mean_prob - 1.96*se_prob,
                        ymax = mean_prob + 1.96*se_prob),
                    color = clrs5[1],
                    position = position_dodge(width = 0.25))  +
    geom_line(aes(y = mean_prob), color = clrs5[1], alpha = 0.5) +
    geom_line(aes(y = density), color = clrs4[4], alpha = 0.7) +
    geom_point(aes(y = density), shape = 15, color = clrs4[4],  size = 3) +
    scale_x_continuous(
        breaks = c(0:10)) +
    theme_minimal(base_size = 12) +
    theme(axis.text.y = element_blank(),
          #legend.position = "none",
          #strip.background = element_blank(),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey80", color = NA)
    )


##########.
## Now for the zero-inflated model

mod_zip_predict <- function(data) {
  mod_zip <- pscl::zeroinfl(condition_count ~ 1 | cohort, data = data, dist = "poisson")

   ibis_df = data.frame(cohort = "ibis")
  control_df = data.frame(cohort = "control")


ibis_predict <- predict(mod_zip, newdata = ibis_df, type = "prob")
control_predict <- predict(mod_zip, newdata = control_df, type = "prob")

  ibis_probs <- data.frame(condition_count = 0:10) %>%
                 mutate(prob = ibis_predict[condition_count + 1]) %>%
                 mutate(cohort = "ibis")

  control_probs <- data.frame(condition_count = 0:10) %>%
                 mutate(prob = control_predict[condition_count + 1]) %>%
                 mutate(cohort = "control")

  mod_zip_probs <- bind_rows(ibis_probs, control_probs)

  return(mod_zip_probs)
}

 mod_zip_predict(patients_events) ## try one time

boots_probs_zip <- function(data){
    idx <- sample(1:nrow(data), nrow(data), replace = TRUE)
    probs <- mod_zip_predict(data[idx, ])
    return(probs)
}

boot_samples_zip <- replicate(2000, boots_probs_zip(patients_events), simplify = FALSE)

boot_df_zip <- bind_rows(boot_samples_zip, .id = "replicate")

boot_stats3 <- boot_df_zip %>% group_by(cohort, condition_count) %>%
    summarize(mean_prob = mean(prob),
              se_prob = sd(prob))

 boot_stats3 %>% full_join(mod_nb_probs_freqs) %>%
    ggplot(aes(x = condition_count)) +
    geom_pointrange(aes(y = mean_prob, ymin = mean_prob - 1.96*se_prob,
                        ymax = mean_prob + 1.96*se_prob),
                    position = position_dodge(width = 0.25),
                    color = clrs5[1])  +
    geom_line(aes(y = mean_prob), color = clrs5[1], alpha = 0.5) +
    geom_point(aes(y = density), shape = 15, size = 2) +
    geom_line(aes(y = density), color = clrs4[4], alpha = 0.7) +
    # geom_point(aes(y = density), size = 4) +
    #scale_color_manual(values =  c("ibis" = clrs5[4], "control" = clrs5[1])) +
    #scale_fill_manual(values = c("control" = "orange", "ibis" = "purple")) +
    scale_x_continuous(
        breaks = c(0:10)) +
    facet_wrap(~ cohort) +
    theme_minimal(base_size = 12) +
    theme(axis.text.y = element_blank(),
          #legend.position = "none",
          #strip.background = element_blank(),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey80", color = NA)
    )

save(boot_samples_nb, boot_samples_zip, file = "boot_samples_conditions.RData")
load("boot_samples_conditions.RData")

boot_stats3 <- boot_df_zip %>% group_by(cohort, condition_count) %>%
    summarize(mean_prob = mean(prob),
              se_prob = sd(prob))


legend_df <- tibble(
    x = 11 , y = 0,
    source = c("Estimate", "Observed")
)


boot_stats3 %>% full_join(mod_nb_probs_freqs) %>%
    ggplot(aes(x = condition_count)) +
    geom_pointrange(aes(y = mean_prob, ymin = mean_prob - 1.96*se_prob,
                        ymax = mean_prob + 1.96*se_prob),
                    position = position_dodge(width = 0.25),
                    color = clrs5[1])  +
    geom_line(aes(y = mean_prob), color = clrs5[1], alpha = 0.5) +
    geom_point(aes(y = density), color = clrs4[4], shape = 15, size = 2) +
    geom_line(aes(y = density), color = clrs4[4], alpha = 0.7) +

    # Add dummy layers to trigger legend entries
    geom_line(data = legend_df, aes(x = x, y = y, color = source),
              # inherit.aes = FALSE,
              size = 1) +
    geom_point(data = legend_df, aes(x = x, y = y, color = source, shape = source),
               size = 3) +

    scale_x_continuous(
        breaks = c(0:10)) +
    xlim(0,10) +

    scale_color_manual(values = c("Estimate" = clrs5[1], "Observed" = clrs4[4])) +
    scale_shape_manual(values = c("Estimate" = 16, "Observed" = 15)) +
    guides(
        color = guide_legend(override.aes = list(
            shape = c(16, 15),
            linetype = c("solid", "solid")
        )),
        shape = "none"
    ) +
    #scale_fill_manual(values = c("control" = "orange", "ibis" = "purple")) +
    facet_wrap(~ cohort) +
    theme_minimal(base_size = 12) +
    theme(#axis.text.y = element_blank(),
        #legend.position = "none",
        #strip.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey80", color = NA),
        legend.position = "bottom"
    ) +
    labs(x = "Number of conditions", y =  "probability/proportion",
         title = "Probability estimates and bootstrapped confidence intervals vs observed proportions",
         fill = NULL,
         color = NULL,
         shape = NULL
    )


    scale_color_manual(values = c("Estimate" = clrs5[1], "Observed" = clrs4[4])) +
    scale_shape_manual(values = c("Estimate" = 16, "Observed" = 15)) +
    guides(
        color = guide_legend(override.aes = list(
            shape = c(16, 15),
            linetype = c("solid", "solid")
        )),
        shape = "none"  # hide duplicate shape legend if desired
    )


