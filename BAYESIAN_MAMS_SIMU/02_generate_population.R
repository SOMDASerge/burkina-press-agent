###############################################################
##
## BAYESIAN MAMS SIMULATION
##
## File: 02_generate_population.R
##
## Purpose:
##   Generate the study population before randomization
##
##  2026/07/30 Serge M.A. SOMDA
##
###############################################################

library(tidyverse)
set.seed(seed)

generate_population <- function(seed = NULL,
                                save = FALSE){
  if(!is.null(seed))
    set.seed(seed)
  population <- tibble(
    participant_id = sprintf(
      "P%05d",
      1:sample.size$total
    )
  )
  country_vector <-
    rep(
      country$country,
      times = country$N
    )
  population$country <- sample(
    country_vector,
    size = sample.size$total,
    replace = FALSE
  )
  population$site <- NA_character_
  for(cc in unique(population$country)){
    available_sites <-
      site_lookup %>%
      filter(country == cc)
    idx <- which(population$country == cc)
    population$site[idx] <-
      sample(
        available_sites$site,
        length(idx),
        replace = TRUE
      )
  }
  population$age_group <-
    factor(
      sample(
      age$labels,
      size = nrow(population),
      replace = TRUE,
      prob = age$proportion
      ),
      levels = age$labels
    )
  population <-
      population %>%
      mutate(
        adolescent =
          if_else(
            age_group == "Adolescent",
            1L,
            0L
          )
      )
  population$sex <-
    factor(
      sample(
        sex$labels,
        size = nrow(population),
        replace = TRUE,
        prob = sex$prob
      ),
      levels = sex$labels
    )
  population$HIV <-
    factor(
      sample(
      hiv$labels,
      size = nrow(population),
      replace = TRUE,
      prob = hiv$prob
      ),
      levels = hiv$labels
    )
  population$smear <-
    factor(
      sample(
      smear$labels,
      size = nrow(population),
      replace = TRUE,
      prob = smear$prob
      ),
      levels = smear$labels
    )
  population$cavity <-
    factor(
      sample(
        cavity$labels,
        size = nrow(population),
        replace = TRUE,
        prob = cavity$prob
      ),
      levels = cavity$labels
    )
  lineage_long <-
    lineage %>%
    pivot_longer(
      cols = starts_with("L"),
      names_to = "lineage",
      values_to = "probability"
    )
  population$lineage <- NA_character_
  for(i in seq_len(nrow(population))){
    cc <- population$country[i]
    probs <-
      lineage_long %>%
      filter(country == cc)
    population$lineage[i] <-
      sample(
        probs$lineage,
        size = 1,
        prob = probs$probability
      )
  }
  population <-
    population %>%
    left_join(
      lineage_lookup,
      by = "lineage"
    )
  population <-
    population %>%
    left_join(
      country_lookup,
      by = "country"
    ) %>%
    left_join(
      site_lookup %>%
        select(site, site_id),
      by = "site"
    ) %>%
    left_join(
      lineage_group_lookup,
      by = c("group")
    )
  
  population <-
    population %>%
    select(
      participant_id,
      country,
      country_id,
      site,
      site_id,
      lineage,
      group,
      lineage_id,
      age_group,
      adolescent,
      sex,
      HIV,
      smear,
      cavity
    )

  stopifnot(
    nrow(population) ==
      sample.size$total
  )
  stopifnot(
    !any(is.na(population$country_id))
  )
  stopifnot(
    !any(is.na(population$site_id))
  )
  stopifnot(
    !any(is.na(population$lineage))
  )
  stopifnot(
    !any(is.na(population$lineage_id))
  )

  if(save){
    saveRDS(population,
            "output/population.rds")
    write_csv(population,
              "output/population.csv")
  }
  return(population)
}

