## spotify example. Grouping vs predictor. As in BR Ch_16-17.R we have
## no pool, complete pool, and hierarchical.
## Now we compare  hierarchical popularity ~ (1 | artist) with popularity ~  artist,
## Do the same with runner data. Can do intercept vs slopes.
## Use {brms} on runner data to compare hier vs not, slopes vs not, explore priors,

spotify_complete_pooled <- stan_glm(
    popularity ~ 1,
    data = spotify, family = gaussian,
    prior_intercept = normal(50, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4, iter = 5000*2, seed = 84735)

spotify_no_pooled <- stan_glm(
    popularity ~ artist - 1,
    data = spotify, family = gaussian,
    prior = normal(50, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4, iter = 5000*2, seed = 84735)


spotify_hierarchical <- stan_glmer(
    popularity ~ (1 | artist),
    data = spotify, family = gaussian,
    prior_intercept = normal(50, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735)

## Note we want priors on both intercept and artist
spotify_predictor <- stan_glm(
    popularity ~ artist,
    data = spotify, family = gaussian,
    prior_intercept = normal(50, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4, iter = 5000*2, seed = 84735)


## Can compare eliminating prior on artist
spotify_predictor_no_prior <- stan_glm(
    popularity ~ artist,
    data = spotify, family = gaussian,
    prior_intercept = normal(50, 2.5, autoscale = TRUE),
    #prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4, iter = 5000*2, seed = 84735)

##### brms to expose stan code
#####

fit1 <- brm(formula =
                popularity ~ (1 | artist),
            data = spotify, family = gaussian,
            #prior = c(set_prior("normal(0,5)", class = "b")),
                 prior =    set_prior("cauchy(0,2)", class = "sd"),
              #        set_prior("lkj(2)", class = "cor")),
            warmup = 1000,
            iter = 2000, chains = 4, control = list(adapt_delta = 0.95))

## just use defaults to get idea and  using get_prior()
fit2 <- brm(formula =
                popularity ~  (1|artist),
            data = spotify, family = gaussian,
           )

make_stancode(formula =
                  popularity ~ (1 | artist),
              data = spotify, family = gaussian)


get_prior(formula =
               popularity ~ artist,
           data = spotify, family = gaussian)

#########33
#########   Take subset of runners- for ease of  viewing
running <- running %>% group_nest(runner) %>% slice_sample(n = 5) %>% unnest(data)

fit_non_hier <- brm(formula =
                        net ~ age,
                    data = running, family = gaussian
)

fit_non_hier_df <- as.data.frame(fit_non_hier)

fit_int <- brm(formula =
                   net ~ age + (1 |runner),
               data = running, family = gaussian)

fit_int_df <- as.data.frame(fit_int)

fit_slopes <- brm(formula =
              net ~ age + (age |runner),
          data = running, family = gaussian)

fit_slopes_df <- as.data.frame(fit_slopes)

get_prior(formula =
              net ~ age,
          data = running, family = gaussian)

fit_one_plus <- brm(formula =
                      net ~ age + (1 + age |runner),
                  data = running, family = gaussian)

fit_one_plus_df <- as.data.frame(fit_one_plus)

make_stancode(formula =
                  net ~ age + (age |runner),
              data = running, family = gaussian)

