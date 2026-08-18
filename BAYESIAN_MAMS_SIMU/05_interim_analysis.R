###############################################################
##
## BAYESIAN MAMS SIMULATION
##
## File: 05_interim_analysis.R
##
## Purpose:
##    Perform interim or final Bayesian analysis
##
###############################################################

library(dplyr)

###############################################################
## Posterior decision rule
###############################################################

decision_rule <- function(prob,
                          stage,
                          success_threshold = 0.95,
                          futility_threshold = 0.10,
                          final_threshold = 0.975){
  if(stage == "FINAL"){
    ifelse(prob >= final_threshold,
           "Non-inferior",
           "Not non-inferior")
  } else {
    case_when(
      prob < futility_threshold ~ "Drop for futility",
      prob >= success_threshold ~ "Graduate",
      TRUE ~ "Continue"
    )
  }
}

###############################################################
## Interim analysis
###############################################################
interim_analysis <- function(trial,
                             stage = c("IA1","IA2","FINAL")){
  stage <- match.arg(stage)
  
  ## Select participants
  #############################################################
  dat <-
    switch(
      stage,
      IA1   = trial %>% slice(1:interim$analysis1),
      IA2   = trial %>% slice(1:interim$analysis2),
      FINAL = trial
    )
  
  ## Descriptive summaries
  #############################################################
  treatment_summary <-
    dat %>%
    group_by(treatment) %>%
    summarise(
      N = n(),
      Failures = sum(failure == "Failure"),
      FailureRate = mean(failure == "Failure"),
      .groups = "drop"
    )
  
  ## Bayesian model
  #############################################################
  fit <- fit_bayesian_model(dat)
  
  ## Posterior probability
  #############################################################
  posterior <-
    fit$posterior_probability %>%
    mutate(
      Stage = stage,
      Decision = decision_rule(
        probability,
        stage = stage
      )
    )
  
  ## Country-specific results
  #############################################################
  country_results <-
    fit$country %>%
    mutate(
      Stage = stage
    )
  
  ## Lineage-group results
  #############################################################
  group_results <-
    fit$group %>%
    mutate(
      Stage = stage
    )
  
  ## Return
  #############################################################
  out <- list(
    stage = stage,
    n = nrow(dat),
    data = dat,
    treatment = treatment_summary,
    posterior = posterior,
    country = country_results,
    group = group_results,
    fit = fit
  )
  class(out) <- "interim_analysis"
  return(out)
}