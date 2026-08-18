###############################################################
##
## BAYESIAN MAMS SIMULATION
##
## File: 04_simulate_outcomes.R
##
###############################################################

library(tidyverse)

simulate_outcomes <- function(randomized,
                              scenario = scenario1,
                              save = FALSE){
  dat <- randomized
  
  ## Random country effects
  #############################################################
  country_random <- rnorm(
    length(scenario$country_beta),
    mean = scenario$country_beta,
    sd   = scenario$country_sd
  )
  names(country_random) <- names(scenario$country_beta)
  
  ## Random lineage-group effects
  #############################################################
  lineage_random <- rnorm(
    length(scenario$lineage_beta),
    mean = scenario$lineage_beta,
    sd   = scenario$lineage_sd
  )
  names(lineage_random) <- names(scenario$lineage_beta)
  
  ## Indicator for intervention
  #############################################################
  trt <- as.numeric(dat$treatment == "Intervention")
  
  ## Linear predictor
  #############################################################
  lp <-
    scenario$beta0 +
    trt * scenario$beta_trt +
    scenario$age_beta[dat$age_group] +
    scenario$HIV_beta[as.character(dat$HIV)] +
    scenario$smear_beta[dat$smear] +
    scenario$cavity_beta[as.character(dat$cavity)] +
    country_random[dat$country] +
    lineage_random[dat$group] +
    trt * scenario$interaction_TC[dat$country] +
    trt * scenario$interaction_TG[dat$group] +
    trt * scenario$interaction_TA[dat$age_group]
  
  ## Outcome generation
  #############################################################
  dat$failure_probability <- plogis(lp)
  dat$failure <- rbinom(
    nrow(dat),
    1,
    dat$failure_probability
  )
  dat$failure <- factor(
    dat$failure,
    levels = c(0,1),
    labels = c("Success","Failure")
  )
  
  ## Summary
  #############################################################
  
  cat("\n")
  cat("=========================================\n")
  cat("Observed failure rates\n")
  cat("=========================================\n")
  print(
    dat %>%
      group_by(treatment) %>%
      summarise(
        N = n(),
        Failures = sum(failure == "Failure"),
        Risk = mean(failure == "Failure"),
        .groups = "drop"
      )
  )
  
  ## Save
  #############################################################
  if(save){
    saveRDS(
      dat,
      "output/simulated_trial.rds"
    )
    write_csv(
      dat,
      "output/simulated_trial.csv"
    )
  }
  return(dat)
}