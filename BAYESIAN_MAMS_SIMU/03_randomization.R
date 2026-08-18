###############################################################
##
## BAYESIAN MAMS SIMULATION
##
## File: 03_randomization.R
##
## Purpose:
##   Randomize participants
##
##  2026/07/30 Serge M.A. SOMDA
##
###############################################################

library(tidyverse)
set.seed(seed)


randomize_population <- function(population,
                                 save = FALSE){
  population$recruitment_order <-
    seq_len(
      nrow(population)
    )
  arms <- coding$treatment
  block_sizes <- c(4,6,8)
  randomize_stratum <- function(df,
                                arms ,
                                block_sizes = block_sizes) {
    n <- nrow(df)
    allocation <- character(0)
    while(length(allocation) < n){
      b <- sample(block_sizes, 1)
      # ensure block size is a multiple of number of arms
      if(b %% length(arms) != 0)
        next
      reps <- b / length(arms)
      block <-
        rep(arms, each = reps)
      block <-
        sample(block)
      allocation <-
        c(allocation, block)
    }
    allocation <- allocation[1:n]
    df$treatment <- allocation
    df
  }
  randomized <-
    population %>%
    group_by(
      country,
      age_group
    ) %>%
    group_modify(~ randomize_stratum(
      .x,
      arms = arms,
      block_sizes = block_sizes
    )) %>%
    ungroup()
  randomized <-
    randomized %>%
    mutate(
      treatment_code =
        match(
          treatment,
          coding$treatment
        ) - 1L
    )
  
  cat("\n")
  cat("=====================================\n")
  cat("Treatment Allocation\n")
  cat("=====================================\n")
  print(table(randomized$treatment))
  cat("\n")
  country_balance <-
    table(
      randomized$country,
      randomized$treatment
    )
  print(country_balance)
  lineage_balance <-
    table(
      randomized$group,
      randomized$treatment
    )
  print(lineage_balance)
  age_balance <-
    table(
      randomized$age_group,
      randomized$treatment
    )
  print(age_balance)
  balance <- randomized %>%
    count(
      country,
      lineage,
      treatment
    )
  print(balance)
  
  randomized <-
    randomized %>%
    arrange(
      recruitment_order
    )
  
  if(save){
    write_csv(
      randomized,
      "output/randomized_population.csv"
    )
    saveRDS(
      randomized,
      "output/randomized_population.rds"
    )  
    }
  
  cat("\n")
  cat("=====================================\n")
  cat("Randomization completed\n")
  cat("=====================================\n")
  cat("Participants :", nrow(randomized), "\n")
  cat("Treatment arms :", length(arms), "\n")
  cat("Randomization ratio : 1:1\n")
  cat("Stratification : Country × Lineage Group\n")
  cat("=====================================\n")
  return(randomized)
}


