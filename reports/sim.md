Coach Effect Simulation
================

# Introduction

In Cohort 1, village clusters were randomly assigned to treatment or
control in each strata (host and refugee communities). Eligible
households in each treatment village were then randomly assigned to
groups of 25 households, and these groups were randomly assigned to 1 of
4 unmarked bins. A public lottery was held to assign bins to study arms.

Separately, coaches were randomly assigned to be individual or group
coaches. Individual coaches were randomly assigned to a group of 25
households in study arms 1 or 3 (individual coaching), whereas group
coaches were each randomly assigned to 5 groups of 25 households in
study arm 2 (group coaching). The households assigned to arm 4 were in
the spillover control and did not receive the program.

![](cohort%201%20diagram.png)<!-- -->

This design represents two levels of partial nesting:

1.  Treatment households were nested in groups.
2.  Groups were nested in coaches (also treatment only).

The nesting is partial because control households did not have coaches
or groups. Additionally, groups were nested in village clusters, but
coaches were not. Therefore, the design has crossed random effects
(i.e., coaches could work across villages).

# Function to simulate data

The first step is to create a function to simulate data for this
structure.

``` r
# load the necessary packages
  library(tidyverse)
  library(lme4)
  library(broom.mixed)
  library(broom.helpers)
  library(brms)
  library(cmdstanr)
  library(tidybayes)
  library(sjPlot)

  options("cmdstanr_write_stan_file_dir" = "cmdstanr")
```

## Model

### Simple

John and David suggested the following Stata code for a simplified model
of villages assigned to treatment or control and random effects for
villages and coaches.

    mixed y trt || _all:R.village || coach:trt, nocons reml

> This tells Stata that the village random effect is crossed with the
> coach random effect, and that the coach random effect is only in the
> treatment arm.

Coaches are partially nested, so John and David followed the approach of
[Candlish et
al. (2018)](https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-018-0559-x)
to code the partially nested clusters as ‘singleton’ clusters and (I
think) fit a partially nested homoscedastic mixed effects model.

I believe the same model in `{lme4}` would be:

    lmer(y ~ trt + (1 | village) + (0 + trt | coach))

where `trt` is an indicator of assignment to any treatment arm,
`village` is the village code, and `coach` is the coach code.
Individuals in the control arm have unique, singleton, codes for coaches
since they did not have a coach.

### Actual

The actual design is more complicated. Households in villages assigned
to treatment were assigned to groups of 25, and these groups were
assigned to 1 of 3 treatment arms or a spillover control arm.

IPA ran the following model (coefficient names changed to match this
simulation):

![](empirical.png)<!-- -->

A few things to note about the IPA strategy:

1.  They entered village as a fixed effect
2.  They account for the multilevel nature of the design with
    bootstrapping.
3.  They do not include setting (host/refugee) in the model.

> To account for the two levels of randomization — village level
> randomization into treatment and control and household-level
> randomization into experimental arms within treatment villages — we
> construct standard errors of the parameter estimates of interest by
> using a bootstrap procedure that mirrors the two stages randomization
> process at the two different levels.

For this exercise we’ll fit a mixed model and incorporate information
about setting as a fixed effect:

    lmer(y ~ coaching_individual +  # assigned to arm 1
             coaching_group +       # assigned to arm 2
             no_asset +             # assigned to arm 3
             spillover_control +    # assigned to arm 4 (no program)
             host +                 # village is in a host community (vs refugee)
             (1 | village) + 
             (0 + trt | coach) +    # trt is arm 1, 2, or 3; coaches only in trt 
             (0 + trt | group))     # all trt had some element of group structure
                                    #   only coaching_group had group coaching
                                    #   but arms 1 and 3 had group program elements

## Function

``` r
#' Simulate data
#' @param seed
#' @param action simulate "data only" or "fit" (sim and fit)
#' @param method "lmer" or "brm"
#' @param ci interval width
#' @param n_village_t0 participants per village, control
#' @param n_group participants per group
#' @param group_village_t1 groups per village, treatment
#' @param k_villages number of villages
#' @param village_rho village ICC
#' @param coach_rho coach ICC
#' @param group_rho group ICC
#' @param b_0 mean in pure control
#' @param sd standard deviation in pure control
#' @param b_ind_asset impact of individual coaching with asset transfer
#' @param b_group impact of group coaching with asset transfer
#' @param b_ind_noasset impact of individual coaching without asset transfer
#' @param b_spill spillover from T households to C households in T villages
#' @param b_host impact of host community setting (vs refugee)

  simfit <- function(seed,
                     action = "data only",
                     method = "lmer",
                     ci = 0.89,
                     k_villages,
                     n_village_t0,
                     group_village_t1,
                     n_group,
                     village_rho, 
                     coach_rho,
                     group_rho,
                     b_0,
                     sd,
                     b_ind_asset,
                     b_group,
                     b_ind_noasset,
                     b_spill,
                     b_host,
                     ... # helps the function work with pmap() 
                     ) {
    
# simulate data -----------------------------------------------------
  set.seed(seed)
  
  n1 <- (k_villages/2)*group_village_t1*n_group
  n0 <- (k_villages/2)*n_village_t0
  ntot <- n0 + n1
  
# different id for each observation
  id <- 1:(n1+n0) 
  
# village t/c assignment indicator
  village_trt <- c(rep(1, n1), rep(0, n0)) 
  
# village indicator
  villages_t1 <- rep(1:(k_villages/2), each = group_village_t1*n_group)
  villages_t0 <- rep(((k_villages/2)+1):k_villages, each = n_village_t0)
  village <- paste("v", c(villages_t1, villages_t0), sep="_")
  
# group indicator for treatment villages, singleton groups in control villages
# need to add singleton groups for spillover control hhs
  group <- paste("g",
                 c(rep(1:((k_villages/2)*group_village_t1), each = n_group),
                   (n1+1):(n1+n0)),
                 sep="_")

# combine
  df <- tibble(id = id,
               village = village,
               village_trt = village_trt,
               group = group) 
  
# add strata
  strata <- df %>%
    select(village, village_trt) %>%
    distinct(village, .keep_all = TRUE) %>%
    group_by(village_trt) %>%
    mutate(host = c(rep(1, n()%/%2),
                    rep(0, (n()%/%2) + n()%%2)))
  
  df <- df %>%
    left_join(strata) 
   
# arm indicator
  arm <- df %>%
    filter(village_trt==1) %>%
    select(village, group, host) %>%
    distinct(village, group, .keep_all = TRUE) %>%
    group_by(host) %>%
    mutate(arm = c(rep(1, n()%/%4),
                   rep(2, n()%/%4),
                   rep(3, n()%/%4),
                   rep(4, (n()%/%4) + (n()%%4)))) %>%
    ungroup() %>%
    select(village, group, arm)
  
  df <- df %>%
    left_join(arm) %>%
    mutate(arm = case_when(
      village_trt == 0 ~ 0,
      TRUE ~ arm
    ))
    
# individual t/c indicator
  df <- df %>%
    mutate(trt = case_when(
      village_trt == 0 ~ 0,
      arm == 4 ~ 0,
      TRUE ~ 1)) %>%
    mutate(spillover_control = case_when(
      arm == 4 ~ 1,
      TRUE ~ 0
    ))
  
# indicators for coaching type
  df <- df %>%
    mutate(coaching_individual = case_when(
      arm==1 | arm==3 ~ 1,
      TRUE ~ 0
    )) %>%
    mutate(coaching_group = case_when(
      arm==2 ~ 1,
      TRUE ~ 0
    ))
  
# indicator for no asset
  df <- df %>%
    mutate(no_asset = case_when(
      arm==3 ~ 1,
      TRUE ~ 0
    ))
  
# add singleton groups to spillover control
  df <- df %>%
    mutate(group = case_when(
      arm == 4 ~ paste(group, id, sep="_"),
      TRUE ~ group
    ))
  
# coach indicator in arm 1 (trt) and introducing singleton clusters into arm 0 (ctrl)
  df <- df %>%
    mutate(coach = case_when(
      trt == 0 ~ paste("c", village, id, sep="_"),
      arm == 1 | arm == 3 ~ paste("c", group, sep="_"),
      arm == 2 ~ "c",
      TRUE ~ NA_character_)
    )
  
  # some coaches need to have extra groups
  n_coaches_arm2 <- ((length(unique(df$group[df$arm==2])))%/%3)
  n_left_over_groups_arm2 <- ((length(unique(df$group[df$arm==2])))%%3)
  coaches_id_arm2 <- unique(rep(1:((length(unique(df$group[df$arm==2])))%/%3),
                                each = n_group*3))
  left_over_start <- length(coaches_id_arm2)-n_left_over_groups_arm2+1
  left_over_end <- length(coaches_id_arm2)
    
  df_arm2 <- df %>%
    filter(arm == 2) %>%
    arrange(id) %>%
    mutate(coach2 = paste("c",
                          c(rep(1:((length(unique(df$group[df$arm==2])))%/%3),
                                each = n_group*3),
                            rep(left_over_start:left_over_end,
                                each = n_group*1)), 
                          sep="_")) %>%
    select(id, coach2)
  
  df <- df %>%
    left_join(df_arm2) %>%
    mutate(coach = case_when(
      is.na(coach2) ~ coach,
      TRUE ~ coach2
    )) %>%
    select(-coach2)
  
# clean up coach IDs
  coach_id <- df %>%
    distinct(coach) %>%
    mutate(coach_id = paste("c",
                            rep(1:n()),
                            sep="_"))
  
  df <- df %>%
    left_join(coach_id) %>%
    select(-coach) %>%
    rename(coach = coach_id)
  
# clean up group IDs
  group_id <- df %>%
    distinct(group) %>%
    mutate(group_id = paste("g",
                            rep(1:n()),
                            sep="_"))
  
  df <- df %>%
    left_join(group_id) %>%
    select(-group) %>%
    rename(group = group_id)

# add strata
  strata <- df %>%
    select(village, village_trt) %>%
    distinct(village, .keep_all = TRUE) %>%
    group_by(village_trt) %>%
    mutate(host = c(rep(1, n()%/%2),
                    rep(0, (n()%/%2) + n()%%2)))
  
  df <- df %>%
    left_join(strata) 
  
# add random effects
  df <- df %>%
    arrange(village, group, id) %>%
  # village
    group_by(village) %>%
    mutate(village_variation = rep(rnorm(1,  sd = sqrt(village_rho)),
                                   each = n())) %>%
  # coach
    group_by(coach) %>%
    mutate(coach_variation = rep(rnorm(1, sd = sqrt(coach_rho)),
                                   each = n())) %>%
  # group
    group_by(group) %>%
    mutate(group_variation = rep(rnorm(1, sd = sqrt(group_rho)),
                                 each = n())) %>%
  # residual variation
    ungroup() %>%
    mutate(sigma = rnorm(n(), mean = 0, sd = sd)) %>%
  # simulated outcome data
    mutate(y = b_0 +
               b_ind_asset*coaching_individual +
               b_group*coaching_group +
               b_ind_noasset*no_asset +
               b_spill*spillover_control +
               b_host*host +
               village_variation +   # applied to all
               coach_variation*trt + # applied to arms 1-3
               group_variation*trt + # applied to arms 1-3
               sigma                 # apply sigma to all (homoscedastic model)
             )

# fit ---------------------------------------------------------------

if (action == "fit"){
    
  set.seed(seed)
  
  # fit by method
    if (method == "lmer") {
      
      fit <- lmer(y ~ coaching_individual + 
                      coaching_group + 
                      no_asset + 
                      spillover_control + 
                      host + 
                      (1 | village) + 
                      (0 + trt | coach) + 
                      (0 + trt | group), 
                  data=df)
  
    # results 
      res <- broom.mixed::tidy(fit, conf.int = TRUE, conf.level = ci) %>%
        mutate(type = "raw")
      
    } else if (method == "brm") {
      
      # todo 
      
    }
    
  } else {
      return(df)
    }
  }
```

## Design Checks

Next we use this function to simulate some data.

``` r
  df <- simfit(seed = 8675309, 
               k_villages = 114,
               n_village_t0 = 40,
               group_village_t1 = 6,
               n_group = 25,
               village_rho = 0.02,
               coach_rho = 0.06,
               group_rho = 0.10,
               b_0 = 0,             # mean pure control
               sd = 1,              # sd pure control
               b_ind_asset = 0.63,
               b_group = 0.63,
               b_ind_noasset = 0.51,
               b_spill = 0.08,
               b_host = 0.20)

  df %>% 
    select(id, village, village_trt, host, arm, coach, y) %>%
    as_tibble() %>%
    print(n=16)
```

    ## # A tibble: 10,830 × 7
    ##       id village village_trt  host   arm coach       y
    ##    <int> <chr>         <dbl> <dbl> <dbl> <chr>   <dbl>
    ##  1     1 v_1               1     1     1 c_1   -0.774 
    ##  2     2 v_1               1     1     1 c_1    1.18  
    ##  3     3 v_1               1     1     1 c_1    1.27  
    ##  4     4 v_1               1     1     1 c_1    0.140 
    ##  5     5 v_1               1     1     1 c_1    1.83  
    ##  6     6 v_1               1     1     1 c_1   -0.569 
    ##  7     7 v_1               1     1     1 c_1   -0.0722
    ##  8     8 v_1               1     1     1 c_1    0.833 
    ##  9     9 v_1               1     1     1 c_1    3.05  
    ## 10    10 v_1               1     1     1 c_1    0.0581
    ## 11    11 v_1               1     1     1 c_1    2.31  
    ## 12    12 v_1               1     1     1 c_1    0.313 
    ## 13    13 v_1               1     1     1 c_1    1.35  
    ## 14    14 v_1               1     1     1 c_1    0.173 
    ## 15    15 v_1               1     1     1 c_1   -0.396 
    ## 16    16 v_1               1     1     1 c_1   -0.119 
    ## # … with 10,814 more rows

``` r
  df %>% 
    mutate(arm_f = factor(arm,
                          levels = 0:4,
                          labels = c("pure control",
                                     "individual coaching with asset",
                                     "group coaching with asset",
                                     "individual coaching without asset",
                                     "spillover control"))) %>%
    group_by(host, village_trt, arm_f) %>%
    count() %>%
    as_tibble()
```

    ## # A tibble: 10 × 4
    ##     host village_trt arm_f                                 n
    ##    <dbl>       <dbl> <fct>                             <int>
    ##  1     0           0 pure control                       1160
    ##  2     0           1 individual coaching with asset     1075
    ##  3     0           1 group coaching with asset          1075
    ##  4     0           1 individual coaching without asset  1075
    ##  5     0           1 spillover control                  1125
    ##  6     1           0 pure control                       1120
    ##  7     1           1 individual coaching with asset     1050
    ##  8     1           1 group coaching with asset          1050
    ##  9     1           1 individual coaching without asset  1050
    ## 10     1           1 spillover control                  1050

``` r
  df %>% 
    mutate(arm_f = factor(arm,
                          levels = 0:4,
                          labels = c("pure control",
                                     "individual coaching with asset",
                                     "group coaching with asset",
                                     "individual coaching without asset",
                                     "spillover control"))) %>%
    group_by(coach, arm_f) %>%
    count() %>%
    filter(n!=1) %>%
    group_by(arm_f) %>%
    count() %>%
    as_tibble()
```

    ## # A tibble: 3 × 2
    ##   arm_f                                 n
    ##   <fct>                             <int>
    ## 1 individual coaching with asset       85
    ## 2 group coaching with asset            28
    ## 3 individual coaching without asset    85

# Simulate and fit

``` r
  m1 <- simfit(seed = 8675309, 
               action = "fit",
               method = "lmer",
               ci = 0.95,
               k_villages = 114,
               n_village_t0 = 40,
               group_village_t1 = 6,
               n_group = 25,
               village_rho = 0.02,
               coach_rho = 0.06,
               group_rho = 0.10,
               b_0 = 0,             # mean pure control
               sd = 1,              # sd pure control
               b_ind_asset = 0.63,
               b_group = 0.63,
               b_ind_noasset = 0.51,
               b_spill = 0.08,
               b_host = 0.20)

  m1 %>% 
    as_tibble()
```

    ## # A tibble: 10 × 9
    ##    effect   group    term  estimate std.error statistic conf.low conf.high type 
    ##    <chr>    <chr>    <chr>    <dbl>     <dbl>     <dbl>    <dbl>     <dbl> <chr>
    ##  1 fixed    <NA>     (Int…  -0.0314    0.0320    -0.981  -0.0940    0.0313 raw  
    ##  2 fixed    <NA>     coac…   0.567     0.0629     9.02    0.444     0.690  raw  
    ##  3 fixed    <NA>     coac…   0.622     0.0732     8.50    0.478     0.765  raw  
    ##  4 fixed    <NA>     no_a…   0.533     0.0805     6.63    0.376     0.691  raw  
    ##  5 fixed    <NA>     spil…   0.0672    0.0463     1.45   -0.0235    0.158  raw  
    ##  6 fixed    <NA>     host    0.262     0.0369     7.11    0.190     0.334  raw  
    ##  7 ran_pars group    sd__…   0.301    NA         NA      NA        NA      raw  
    ##  8 ran_pars coach    sd__…   0.243    NA         NA      NA        NA      raw  
    ##  9 ran_pars village  sd__…   0.122    NA         NA      NA        NA      raw  
    ## 10 ran_pars Residual sd__…   0.993    NA         NA      NA        NA      raw