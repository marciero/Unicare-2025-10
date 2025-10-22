### This file used in power_calc_zero_inf_2025-06-13
df <- gen_zi_data(500)

df$data %>% 
  ggplot(aes(x = y)) +
  geom_histogram(aes(fill = cohort), position = "dodge") +
  labs(x = NULL, y =  NULL,
       fill = NULL,
       title = "Simulated inpatient admissions") +
  scale_fill_manual(values = c(clrs1[2], clrs1[3])) +
  theme_minimal(base_size = 12) +
  theme(#axis.text.y = element_blank(),
    legend.position = "bottom",
    #strip.background = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = NA)
  )
 
######
###### Power curves. Uses power_calc_2025 functions. Full analysis takes four hours for negbin, 1 hr poisson

### Set up: Generate grid of sample sizes and effect sizes
eff_size <- data.frame(eff_size = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)) %>%
  mutate(lambda_ibis = log(1 - eff_size))


n_vals <- seq(100, 1500, by = 100)
sim_grid <- expand_grid(n = n_vals, eff_size)

### Run simulation wrapper
set.seed(123)  # for reproducibility

tic()
sim_results <- sim_grid %>%
  mutate(prop_sig = map2_dbl(n, lambda_ibis, function(n_i, lambda_ibis_i) {
    p_vals <- replicate(2000, {
      dat <- gen_zi_data(n_i, cor_mat, lambda_ibis = lambda_ibis_i)$data
      tryCatch({
        pval <- run_zi_poisson_coef_test(dat)[3]
        as.numeric(pval) < 0.05
      }, error = function(e) NA)
    })
    mean(p_vals, na.rm = TRUE)
  }))
toc()


load( "data/power_curve_sims.RData")

# plot power curves

sim_results_pois_pois %>% ggplot(aes(x = n, y = prop_sig, color = factor(eff_size))) +
  geom_line() +
  geom_point() +
  labs(title = "Power Curve by Effect Size",
       x = "Sample Size (n)", y = "Proportion p < 0.05",
       color = "Effect Size") +
  theme_minimal()



##sim_results_negbin_negbin <- sim_results
##sim_results_pois_pois <- sim_results

## save(sim_results_negbin_negbin, sim_results_negbin_pois, sim_results_pois_pois, file = "data/power_curve_sims.RData") ## 


#### Power curve for the prob of zero admit effect size
## assume centered age is zero, chr and afib both 1.
c2 <- 0.46 + 0.43
c1 <- invlogit(1.35)

### Wait- lets just find based on the old lambda values


eff_size %>% mutate(eff_size_zero_raw = (c1 + (1-c1)*exp(-exp(lambda_ibis +c2)))/ 
                                           (c1 + (1-c1)*exp(-exp(c2)))) %>% 
                    mutate(eff_size_zero = round(eff_size_zero_raw, 3) - 1)

sim_results_pois_pois %>% 
  mutate(eff_size_zero_raw = (c1 + (1-c1)*exp(-exp(lambda_ibis +c2)))/ 
           (c1 + (1-c1)*exp(-exp(c2)))) %>% 
  mutate(eff_size_zero = round(eff_size_zero_raw - 1, 3)) %>% 
   ggplot(aes(x = n, y = prop_sig, color = as.factor(eff_size_zero))) +
  geom_line() +
  geom_point() +
  labs(title = "Power Curve by Effect Size",
       x = "Sample Size (n)", y = "Proportion p < 0.05",
       color = "Effect Size") +
  theme_minimal()



sim_results_pois_pois %>%
  mutate(eff_size_zero_raw = (c1 + (1 - c1) * exp(-exp(lambda_ibis + c2))) /
           (c1 + (1 - c1) * exp(-exp(c2))),
         eff_size_zero = round(eff_size_zero_raw - 1, 3),
         eff_size_label = paste0("mean: ", eff_size, "\nzero prob: ", eff_size_zero)) %>%
  ggplot(aes(x = n, y = prop_sig, color = eff_size_label, group = eff_size_label)) +
  geom_line() +
  geom_point() +
  labs(title = "Power Curve by Effect Size",
       x = "Sample Size (n)", y = "Proportion p < 0.05",
       color = "Effect Sizes") +
  theme_minimal()

