###############################################################
## BAYESIAN MAMS SIMULATION
##
## File: 01_parameters.R
## Purpose:
##   Define all simulation parameters
##
##  2026/07/30 Serge M.A. SOMDA
##
###############################################################

#rm(list = ls())

###############################################################
## PACKAGES
###############################################################
library(tidyverse)

###############################################################
## RANDOM SEED
###############################################################
seed <- 2026
set.seed(seed)

###############################################################
## TRIAL DESIGN
###############################################################

trial <- list(
  name = "OPTIMISE-TB12",
  design = "Bayesian Hierarchical Two-arm Non-inferiority Trial",
  phase = "IIb/III",
  narms = 2,
  arm.names = c(
    "SOC",
    "Experimental"
  ),
  allocation = c(1,1),
  countries = c(
    "Burkina Faso",
    "Benin",
    "The Gambia",
    "Gabon",
    "Uganda",
    "Malawi"
  )
)

###############################################################
## SAMPLE SIZE
###############################################################
sample.size <- list(
  per.arm = 1200,
  total  =  2400,
  dropout = 0.10
)

###############################################################
## AGE GROUPS
###############################################################

age <- list(
  labels = c(
    "Adolescent",
    "Adult"
  ),
  proportion = c(
    0.15,
    0.85
  )
)

###############################################################
## COUNTRY RECRUITMENT TARGETS
###############################################################

country <- tibble(
  country = c(
    "Burkina Faso",
    "Benin",
    "The Gambia",
    "Gabon",
    "Uganda",
    "Malawi"
  ),
  N = c(
    400,
    400,
    400,
    400,
    400,
    400
  )
)
stopifnot(
  sum(country$N) ==
    sample.size$total
)
country_lookup <- country |>
  dplyr::transmute(
    country,
    country_id = dplyr::row_number()
  )

###############################################################
## SITES PER COUNTRY
###############################################################

sites <- tribble(
  ~country,             ~nsite,
  "Burkina Faso",       3,
  "Benin",              2,
  "The Gambia",         1,
  "Gabon",              2,
  "Uganda",             1,
  "Malawi",             1
)
site_lookup <-
  sites %>%
  rowwise() %>%
  do({
    tibble(
      country = .$country,
      site =
        paste0(
          substr(.$country,1,3),
          "_S",
          seq_len(.$nsite)
        )
    )
  }) %>%
  ungroup() %>%
  mutate(
    site_id =
      row_number()
  )

###############################################################
## MTBC LINEAGE DISTRIBUTION
###############################################################

###############################################################
## LINEAGE DISTRIBUTION
###############################################################

lineage_names <-
  c(
    "L1",
    "L2",
    "L3",
    "L4",
    "L5",
    "L6"
  )
lineage <- read_csv(
#  "data/lineage_distribution.csv",
#  "data/lineage_distribution_Taane.csv",
  "data/lineage_distribution_Andre.csv",
  show_col_types = FALSE
)
check <-
  lineage %>%
  mutate(
    total = L1 + L2 + L3 + L4 + L5 + L6
  )
if(any(abs(check$total - 1) > 1e-8)){
  stop("Lineage probabilities must sum to 1 for every country.")
}

lineage_groups <-
  lineage %>%
  mutate(
    G1 = L4,
    G2 = L5 + L6,
    G3 = L1 + L2 + L3
  )

lineage_lookup <-
  tibble(
    lineage = c(
      "L1",
      "L2",
      "L3",
      "L4",
      "L5",
      "L6"
    ),
    group = c(
      "G3",
      "G3",
      "G3",
      "G1",
      "G2",
      "G2"
    )
  )
lineage_group_lookup <-
  tibble(
    group = c(
      "G1",
      "G2",
      "G3"
    ),
    lineage_id = 1:3
  )
###############################################################
## PROBABILITIES FOR COVARIATES
###############################################################

# SEX
sex <- list(
  labels = c("Female","Male"),
  prob = c(0.45,0.55)
)
# HIV
hiv <- list(
  labels = c("Negative","Positive"),
  prob = c(0.82,0.18)
)
# SMEAR
smear <- list(
  labels = c("Scanty","1+","2+","3+"),
  prob = c(0.10,0.25,0.35,0.30)
)
# CAVITY
cavity <- list(
  labels = c("No","Yes"),
  prob = c(0.65,0.35)
)

coding <- list(
  treatment = c(
    "SOC",
    "Experimental"
  ),
  age = c(
    "Adult",
    "Adolescent"
  ),
  HIV = c(
    "Negative",
    "Positive"
  ),
  smear = c(
    "Scanty",
    "1+",
    "2+",
    "3+"
  ),
  cavity = c(
    "No",
    "Yes"
  ),
  lineage = c(
    "L1","L2","L3","L4","L5","L6"
  ),
  group = c(
    "G1","G2","G3"
  ),
  country = country$country
)
###############################################################
## INTERIM ANALYSES
###############################################################

interim <- list(
  analysis1 =  800,
  analysis2 = 1600,
  final = sample.size$total
)

###############################################################
## NON-INFERIORITY SETTINGS
###############################################################

NI <- list(
  margin = 0.06,
  alpha = 0.025,
  power = 0.90
)

###############################################################
## MCMC settings
###############################################################

mcmc <- list(
  chains = 4,
  parallel_chains = 4,
  warmup = 1000,
  iter = 1000,
  adapt_delta = 0.95,
  max_treedepth = 12,
  refresh = 100
)
nsim <- 5


