###############################################################
##
## BAYESIAN MAMS SIMULATION
##
## File: 06_bayesian_model.R
##
## Purpose:
##   Fit Bayesian hierarchical logistic regression
##
###############################################################

library(cmdstanr)
library(posterior)
library(dplyr)

## Compile Stan model
###############################################################
model <- cmdstan_model(
  "stan/hierarchical_model2.stan"
)

## Bayesian model function
###############################################################
fit_bayesian_model <- function(dat){
  
  ## Encode categorical variables
  #############################################################
  dat <- dat %>%
    mutate(
      trt_id =
        match(
          treatment,
          coding$treatment
        )-1L,
      country_id =
        match(
          country,
          coding$country
        ),
      group_id =
        match(
          group,
          coding$group
        ),
      site_id =
        as.integer(
          factor(site)
        ),
      age_id =
        match(
          age_group,
          coding$age
        ),
      adolescent =
        ifelse(
          age_group=="Adolescent",
          1L,
          0L
        ),
      HIV_id =
        match(
          HIV,
          coding$HIV
        ),
      smear_id =
        match(
          smear,
          coding$smear
        ),
      cavity_id =
        match(
          cavity,
          coding$cavity
        ),
      y =
        ifelse(
          failure=="Failure",1,0
        )
    )
  X <-
    model.matrix(
      ~ HIV +
        smear +
        cavity,
      data=dat
    )[,-1]
  
  ## Stan data
  #############################################################
  stan_data <- list(
    N = nrow(dat),
    y = dat$y,
    trt = dat$trt_id,
    country = dat$country_id,
    site = dat$site_id,
    group = dat$group_id,
    adolescent = dat$adolescent,
    J_country = length(unique(dat$country_id)),
    J_site = length(unique(dat$site_id)),
    J_group = length(coding$group),
    P = ncol(X),
    X = X
  )
  ## Fit model
  #############################################################
  fit <-
    model$sample(
      data = stan_data,
      chains = mcmc$chains,
      parallel_chains = mcmc$parallel_chains,
      iter_warmup = mcmc$warmup,
      iter_sampling = mcmc$iter,
      max_treedepth = mcmc$max_treedepth,
      seed = seed,
      refresh = mcmc$refresh
    )
  
  ## Posterior draws
  #############################################################
  draws <-
    fit$draws()
  draws_df <-
    as_draws_df(draws)
  
  ## Risk Difference
  #############################################################
  RD <- draws_df$RD
  RD_summary <-
    tibble(
      Treatment="Intervention",
      RD_mean=mean(RD),
      RD_lower=quantile(RD,.025),
      RD_upper=quantile(RD,.975)
    )
  
  ## Posterior probability of NI
  #############################################################
  posterior_probability <-
    tibble(
      treatment="Intervention",
      probability=
        mean(RD < NI$margin)
    ) 
  
  ## Odds Ratios
  #############################################################
  OR <- draws_df$OR
  OR_summary <-
    tibble(
      Treatment="Intervention",
      OR_mean=mean(OR),
      OR_lower=quantile(OR,.025),
      OR_upper=quantile(OR,.975)
    )
  
  ## Summaries
  #############################################################
  country_summary <-
    tibble(
      country = coding$country,
      RD_mean = sapply(
        seq_along(coding$country),
        function(j)
          mean(draws_df[[paste0("RD_country[",j,"]")]])
      ),
      RD_lower = sapply(
        seq_along(coding$country),
        function(j)
          quantile(draws_df[[paste0("RD_country[",j,"]")]],0.025)
      ),
      RD_upper = sapply(
        seq_along(coding$country),
        function(j)
          quantile(draws_df[[paste0("RD_country[",j,"]")]],0.975)
      ),
      OR_mean = sapply(
        seq_along(coding$country),
        function(j)
          mean(draws_df[[paste0("OR_country[",j,"]")]])
      ),
      OR_lower = sapply(
        seq_along(coding$country),
        function(j)
          quantile(draws_df[[paste0("OR_country[",j,"]")]],0.025)
      ),
      OR_upper = sapply(
        seq_along(coding$country),
        function(j)
          quantile(draws_df[[paste0("OR_country[",j,"]")]],0.975)
      ),
      PrNI = sapply(
        seq_along(coding$country),
        function(j)
          mean(
            draws_df[[paste0("RD_country[",j,"]")]] < NI$margin
          )
      )
    )
  group_summary <-
    tibble(
      group = coding$group,
      RD_mean = sapply(
        seq_along(coding$group),
        function(j)
          mean(draws_df[[paste0("RD_group[",j,"]")]])
      ),
      RD_lower = sapply(
        seq_along(coding$group),
        function(j)
          quantile(draws_df[[paste0("RD_group[",j,"]")]],0.025)
      ),
      RD_upper = sapply(
        seq_along(coding$group),
        function(j)
          quantile(draws_df[[paste0("RD_group[",j,"]")]],0.975)
      ),
      OR_mean = sapply(
        seq_along(coding$group),
        function(j)
          mean(draws_df[[paste0("OR_group[",j,"]")]])
      ),
      OR_lower = sapply(
        seq_along(coding$group),
        function(j)
          quantile(draws_df[[paste0("OR_group[",j,"]")]],0.025)
      ),
      OR_upper = sapply(
        seq_along(coding$group),
        function(j)
          quantile(draws_df[[paste0("OR_group[",j,"]")]],0.975)
      ),
      PrNI = sapply(
        seq_along(coding$group),
        function(j)
          mean(
            draws_df[[paste0("RD_group[",j,"]")]] < NI$margin
          )
      )
    )
  
  ## Return
  #############################################################
  return(
    list(
      fit = fit,
      draws = draws_df,
      RD = RD_summary,
      OR = OR_summary,
      country = country_summary,
      group = group_summary,
      posterior_probability =
        posterior_probability
    )
  )
}