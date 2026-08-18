###############################################################
##
## BAYESIAN MAMS SIMULATION
##
## File: 07_assurance.R
##
## Purpose:
##   Bayesian assurance simulation
##
###############################################################

library(tidyverse)

source("R/01_parameters.R")
source("R/02_generate_population.R")
source("R/03_randomization.R")
source("R/04_simulate_outcomes.R")
source("R/05_interim_analysis.R")
source("R/06_bayesian_model.R")

library(progress)

###############################################################
## One complete simulated trial
###############################################################

one_simulation <- function(sim,
                           scenario){
  cat("\n")
  cat("========================================\n")
  cat("Simulation",sim,"\n")
  cat("========================================\n")
  
  ## Generate trial
  #############################################################
  population <-
    generate_population()
  randomized <-
    randomize_population(population)
  trial <-
    simulate_outcomes(
      randomized,
      scenario = scenario
    )
  
  ## Analyses
  #############################################################
  IA1 <-
    interim_analysis(
      trial,
      stage="IA1"
    )
  IA2 <-
    interim_analysis(
      trial,
      stage="IA2"
    )
  FINAL <-
    interim_analysis(
      trial,
      stage="FINAL"
    )
  list(
    IA1 = IA1,
    IA2 = IA2,
    FINAL = FINAL
  )
}

###############################################################
## Assurance
###############################################################
assurance <- function(
    scenario,
    nsim = 500, seed = 2026){
  set.seed(seed)
  results <- vector("list", nsim)
  
  pb <-
    progress_bar$new(
      total = nsim,
      width = 60,
      format =
        "[:bar] :percent | ETA: :eta"
    )
  
  start.time <- Sys.time()
  
  #############################################################
  ## Simulation loop
  #############################################################
  for(sim in seq_len(nsim)){
    results[[sim]] <- one_simulation(sim,scenario)
    pb$tick()
    saveRDS(
      results,
      "output/current_results.rds"
    )
  }
  
  ## IA1
  print("IA1 results") 
  #############################################################
  IA1_results <-
    bind_rows(
      lapply(
        seq_len(nsim),
        function(i){
          results[[i]]$IA1$posterior %>%
            mutate(
              Simulation=i
            )
        })
    )

    ## IA2
  print("IA2 results") 
  #############################################################
  IA2_results <-
    bind_rows(
      lapply(
        seq_len(nsim),
        function(i){
          results[[i]]$IA2$posterior %>%
            mutate(
              Simulation=i
            )
        })
    )

  ## FINAL
  print("FINAL results") 
  #############################################################
  FINAL_results <-
    bind_rows(
      lapply(
        seq_len(nsim),
        function(i){
          results[[i]]$FINAL$posterior %>%
            mutate(
              Simulation=i
            )
        })
    )

  ## Country summaries
  print("Country summaries") 
  #############################################################
  country_results <-
    bind_rows(
      lapply(
        seq_len(nsim),
        function(i){
          results[[i]]$FINAL$fit$country %>%
            mutate(
              Simulation=i
            )
        })
    )
  
  ## Lineage-group summaries
  print("Group summaries") 
  #############################################################
  group_results <-
    bind_rows(
      lapply(
        seq_len(nsim),
        function(i){
          results[[i]]$FINAL$fit$group %>%
            mutate(
              Simulation=i
            )
        })
    )
  
  ## Overall assurance
  print("Overall assurance") 
  #############################################################
  assurance_summary <-
    FINAL_results %>%
    summarise(
      Assurance =
        mean(
          Decision=="Non-inferior"
        ),
      MeanPrNI =
        mean(probability)
    )
  
  ## Country operating characteristics
  print("Country O.C") 
  #############################################################
  country_summary <-
    country_results %>%
    group_by(country) %>%
    summarise(
      RD_mean =
        mean(RD_mean),
      RD_lower =
        mean(RD_lower),
      RD_upper =
        mean(RD_upper),
      OR_mean =
        mean(OR_mean),
      OR_lower =
        mean(OR_lower),
      OR_upper =
        mean(OR_upper),
      MeanPrNI =
        mean(PrNI),
      .groups="drop"
    )
  
  ## Lineage-group operating characteristics
  print("Group O.C") 
  #############################################################
  group_summary <-
    group_results %>%
    group_by(group) %>%
    summarise(
      RD_mean =
        mean(RD_mean),
      RD_lower =
        mean(RD_lower),
      RD_upper =
        mean(RD_upper),
      OR_mean =
        mean(OR_mean),
      OR_lower =
        mean(OR_lower),
      OR_upper =
        mean(OR_upper),
      MeanPrNI =
        mean(PrNI),
      .groups="drop"
    )
  
  ## Early stopping
  #############################################################
  early_summary <-
    IA1_results %>%
    summarise(
      Graduate =
        mean(
          Decision=="Graduate"
        ),
      Futility =
        mean(
          Decision=="Drop for futility"
        )
    )
  
  ## Running time
  #############################################################
  runtime <-
    Sys.time()-start.time
  cat("\n")
  cat("========================================\n")
  cat("Simulation completed\n")
  cat("Elapsed time :",runtime,"\n")
  cat("========================================\n")
  
  ## Return
  #############################################################
  list(
    scenario =
      scenario$name,
    assurance =
      assurance_summary,
    country =
      country_summary,
    group =
      group_summary,
    early =
      early_summary,
    IA1 =
      IA1_results,
    IA2 =
      IA2_results,
    FINAL =
      FINAL_results,
    raw =
      results
  )
}