###############################################################
##
## BAYESIAN MAMS SIMULATION
##
## File: scenarios.R
##
##  2026/07/31 Serge M.A. SOMDA
##
###############################################################

###############################################################
## Scenario 1
## All treatments truly non-inferior
###############################################################
scenario1 <- list(
  name = "All non-inferior",
  beta0 = qlogis(0.10),
  beta_trt = log(1.00),
  age_beta = c(
    Adult = 0,
    Adolescent = -0.20
  ),
  HIV_beta = c(
    Negative = 0,
    Positive = 0.60
  ),
  smear_beta = c(
    "Scanty" = 0,
    "1+" = 0.15,
    "2+" = 0.35,
    "3+" = 0.60
  ),
  cavity_beta = c(
    No = 0,
    Yes = 0.40
  ),
  country_beta = c(
    "Burkina Faso" = 0.00,
    "Benin" = 0.05,
    "The Gambia" = -0.10,
    "Gabon" = 0.00,
    "Uganda" = 0.10,
    "Malawi" = 0.05
  ),
  lineage_beta = c(
    G1 = 0,
    G2 = -0.10,
    G3 = 0.05
  ),
  interaction_TC = c(
    "Burkina Faso" = 0,
    "Benin" = 0.10,
    "The Gambia" = 0,
    "Gabon" = 0,
    "Uganda" = 0.20,
    "Malawi" = 0
  ),
  interaction_TG = c(
    G1 = 0,
    G2 = -0.20,
    G3 = 0.05
  ),
  interaction_TA = c(
    Adult = 0,
    Adolescent = -0.10
  )
) 
scenario1$country_sd <- 0.15
scenario1$lineage_sd <- 0.10

###############################################################
## Scenario 2
## Experimental inferior
###############################################################
scenario2 <- scenario1
scenario2$name <- "Experimental inferior"
scenario2$beta_trt <- log(1.35)

###############################################################
## Scenario 3
## Experimental superior
###############################################################
scenario3 <- scenario1
scenario3$name <- "Experimental superior"
scenario3$beta_trt <- log(0.80)

###############################################################
## Scenario 4
## strong Lineage-specific efficacy
###############################################################
scenario4 <- scenario1
scenario4$name <- "Lineage interaction"
scenario4$interaction_TG <- c(
  G1 = 0,
  G2 = -0.50,
  G3 = 0.10
)

###############################################################
## Scenario 5
## Large country heterogeneity
###############################################################
scenario5 <- scenario1
scenario5$name <- "Country interaction"
scenario5$interaction_TC <- c(
  "Burkina Faso" = 0,
  "Benin" = 0.30,
  "The Gambia" = 0,
  "Gabon" = 0,
  "Uganda" = -0.30,
  "Malawi" = 0
)

###############################################################
## Scenario 6
## Adolescents respond differently
###############################################################
scenario6 <- scenario1
scenario6$name <- "Age interaction"
scenario6$interaction_TA <- c(
  Adult = 0,
  Adolescent = -0.50
)

###############################################################
## Scenario 7
## Worst-case operating characteristics
###############################################################
scenario7 <- scenario1
scenario7$name <- "Worst case"
scenario7$beta0 = qlogis(0.15)
scenario7$beta_trt = log(1.35)
scenario7$interaction_TC <- c(
  "Burkina Faso" = 0,
  "Benin" = 0.40,
  "The Gambia" = -0.30,
  "Gabon" = 0.10,
  "Uganda" = -0.40,
  "Malawi" = 0.20
)
scenario7$interaction_TG <- c(
  G1 = 0,
  G2 = -0.40,
  G3 = 0.20
)
