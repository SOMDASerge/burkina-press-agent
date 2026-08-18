###############################################################
##
## BAYESIAN MAMS SIMULATION
##
## File: utils.R
##
## Purpose:
##    Utility functions
##
###############################################################

library(tidyverse)

###############################################################
## Logistic functions
###############################################################

logit <- function(p){
  log(p/(1-p))
}

invlogit <- function(x){
  exp(x)/(1+exp(x))
}

###############################################################
## Credible interval
###############################################################

credible_interval <- function(x,
                              level=0.95){
  alpha <- (1-level)/2
  tibble(
    Mean = mean(x),
    SD = sd(x),
    Median = median(x),
    Lower = quantile(x, alpha),
    Upper = quantile(x, 1-alpha)
  )
}

###############################################################
## Encode factor
###############################################################

encode_factor <- function(x){
  as.integer(
    factor(x)
  )
}

###############################################################
## Decode factor
###############################################################

decode_factor <- function(id,
                          levels){
  factor(
    levels[id],
    levels=levels
  )
}

###############################################################
## Risk difference
###############################################################

risk_difference <- function(y,
                            trt){
  tab <-
    tibble(
      y,
      trt
    ) %>%
    group_by(trt) %>%
    summarise(
      risk=mean(y),
      .groups="drop"
    )
  tab
}

###############################################################
## Odds ratio
###############################################################

odds_ratio <- function(a,b,c,d){
  (a*d)/(b*c)
}

###############################################################
## Print section
###############################################################

print_section <- function(title){
  cat("\n")
  cat(rep("=",60),sep="")
  cat("\n")
  cat(title,"\n")
  cat(rep("=",60),sep="")
  cat("\n")
}

###############################################################
## Save object
###############################################################

save_object <- function(object,
                        file){
  saveRDS(
    object,
    file=file
  )
}

###############################################################
## Load object
###############################################################

load_object <- function(file){
  readRDS(file)
}

###############################################################
## Create directory
###############################################################

create_directory <- function(path){
  
  if(!dir.exists(path))
    
    dir.create(
      
      path,
      
      recursive=TRUE
      
    )
  
}

###############################################################
## Timestamp
###############################################################

timestamp <- function(){
  
  format(
    
    Sys.time(),
    
    "%Y-%m-%d %H:%M:%S"
    
  )
  
}

###############################################################
## Simulation progress
###############################################################

progress_message <- function(i,
                             n){
  
  cat(
    
    sprintf(
      
      "[%s] Simulation %d of %d\n",
      
      timestamp(),
      
      i,
      
      n
      
    )
    
  )
  
}

###############################################################
## Check treatment balance
###############################################################

check_balance <- function(dat){
  
  dat %>%
    
    count(
      
      treatment
      
    ) %>%
    
    mutate(
      
      Percent=
        
        round(
          
          100*n/sum(n),
          
          1
          
        )
      
    )
  
}

###############################################################
## Posterior probability
###############################################################

posterior_probability <- function(draws,
                                  threshold){
  
  mean(
    
    draws<threshold
    
  )
  
}

###############################################################
## Monte Carlo Standard Error
###############################################################

mcse <- function(x){
  
  sd(x)/sqrt(length(x))
  
}

###############################################################
## Effective Sample Size
###############################################################

effective_n <- function(draws){
  
  posterior::ess_basic(draws)
  
}

###############################################################
## Gelman-Rubin diagnostic
###############################################################

rhat <- function(draws){
  
  posterior::rhat(draws)
  
}

###############################################################
## Non-inferiority decision
###############################################################

NI_decision <- function(prob,
                        stage){
  
  if(stage=="FINAL"){
    
    if(prob>=0.975){
      
      return("Non-inferior")
      
    }
    
    return("Not non-inferior")
    
  }
  
  if(prob<0.10){
    
    return("Drop")
    
  }
  
  if(prob>=0.95){
    
    return("Graduate")
    
  }
  
  "Continue"
  
}

###############################################################
## Summarize posterior draws
###############################################################

summarize_draws <- function(draws,
                            level=0.95){
  
  alpha <- (1-level)/2
  
  tibble(
    
    Mean=mean(draws),
    
    SD=sd(draws),
    
    Median=median(draws),
    
    Lower=quantile(draws,alpha),
    
    Upper=quantile(draws,1-alpha),
    
    MCSE=mcse(draws)
    
  )
  
}

###############################################################
## Standardize probabilities
###############################################################

standardize_probability <- function(eta){
  
  mean(
    
    invlogit(eta)
    
  )
  
}