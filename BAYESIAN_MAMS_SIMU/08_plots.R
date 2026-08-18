###############################################################
##
## BAYESIAN MAMS SIMULATION
##
## File: 08_plots.R
##
## Purpose:
## Graphics for Bayesian assurance study
##
###############################################################

library(tidyverse)

theme_set(theme_bw())

###############################################################
## Allocation by country and treatment
###############################################################

plot_allocation <- function(dat){
  dat %>%
    count(country, treatment) %>%
    ggplot(
      aes(
        x = country,
        y = n,
        fill = treatment
      )
    ) +
    geom_col(position = "dodge") +
    labs(
      x = "",
      y = "Participants",
      fill = "Treatment",
      title = "Treatment allocation by country"
    ) +
    theme(
      axis.text.x =
        element_text(
          angle = 45,
          hjust = 1
        )
    )
}

###############################################################
## Failure rates
###############################################################

plot_failure_rate <- function(dat){
  dat %>%
    group_by(treatment) %>%
    summarise(
      FailureRate =
        mean(failure == "Failure"),
      .groups = "drop"
    ) %>%
    ggplot(
      aes(
        treatment,
        FailureRate,
        fill = treatment
      )
    ) +
    geom_col(width = .6) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      x = "",
      y = "Failure rate",
      title = "Observed failure rate"
    ) +
    guides(fill = "none")
}

###############################################################
## Posterior probability of NI
###############################################################

plot_PrNI <- function(fit){
  fit$posterior_probability %>%
    ggplot(
      aes(
        treatment,
        probability,
        fill = treatment
      )
    ) +
    geom_col(width=.6) +
    geom_hline(
      yintercept = NI$alpha,
      linetype = 2,
      colour = "red"
    ) +
    ylim(0,1) +
    labs(
      y="Posterior probability",
      x="",
      title="Posterior probability of non-inferiority"
    ) +
    guides(fill="none")
}

###############################################################
## Risk difference
###############################################################

plot_RD <- function(fit){
  fit$RD %>%
    ggplot(
      aes(
        Treatment,
        RD_mean
      )
    ) +
    geom_point(size=3) +
    geom_errorbar(
      aes(
        ymin=RD_lower,
        ymax=RD_upper
      ),
      width=.15
    ) +
    geom_hline(
      yintercept=NI$margin,
      colour="red",
      linetype=2
    ) +
    labs(
      x="",
      y="Risk difference",
      title="Posterior risk difference"
    )
}

###############################################################
## Odds ratio
###############################################################

plot_OR <- function(fit){
  fit$OR %>%
    ggplot(
      aes(
        Treatment,
        OR_mean
      )
    ) +
    geom_point(size=3) +
    geom_errorbar(
      aes(
        ymin=OR_lower,
        ymax=OR_upper
      ),
      width=.15
    ) +
    geom_hline(
      yintercept=1,
      linetype=2,
      colour="red"
    ) +
    scale_y_log10() +
    labs(
      x="",
      y="Odds ratio",
      title="Posterior odds ratio"
    )
}

###############################################################
## Assurance
###############################################################

plot_assurance <- function(results){
  ggplot(
    results$assurance,
    aes(
      x = "Intervention",
      y = Assurance
    )
  ) +
    geom_col(fill = "steelblue", width = .5) +
    ylim(0,1) +
    labs(
      x = "",
      y = "Assurance",
      title = "Bayesian assurance"
    ) +
    theme_bw()
}

###############################################################
## Early stopping
###############################################################

plot_early_stopping <- function(results){
  dat <-
    results$IA1 %>%
    count(Decision)
  ggplot(
    dat,
    aes(
      Decision,
      n,
      fill=Decision
    )
  ) +
    geom_col(width=.6) +
    labs(
      x="",
      y="Number of simulations",
      title="Interim decisions (IA1)"
    ) +
    guides(fill="none")
}

###############################################################
## Country-specific posterior RD
###############################################################

plot_country <- function(country_summary){
  if(nrow(country_summary)==0){
    message("No country summaries available.")
    return(invisible(NULL))
  }
  country_summary %>%
    ggplot(
      aes(
        reorder(country,RD_mean),
        RD_mean
      )
    ) +
    geom_point(size=3) +
    geom_errorbar(
      aes(
        ymin=RD_lower,
        ymax=RD_upper
      ),
      width=.15
    ) +
#    coord_flip() +
    geom_hline(
      yintercept=NI$margin,
      colour="red",
      linetype=2
    ) +
    labs(
      x="Country",
      y="Risk difference",
      title="Country-specific treatment effects"
    )
}

###############################################################
## Lineage-group posterior RD
###############################################################

plot_lineage <- function(group_summary){
  if(nrow(group_summary)==0){
    message("No lineage summaries available.")
    return(invisible(NULL))
  }
  group_summary %>%
    ggplot(
      aes(
        group,
        RD_mean
      )
    ) +
    geom_point(size=3) +
    geom_errorbar(
      aes(
        ymin=RD_lower,
        ymax=RD_upper
      ),
      width=.15
    ) +
    geom_hline(
      yintercept=NI$margin,
      colour="red",
      linetype=2
    ) +
    labs(
      x="Lineage group",
      y="Risk difference",
      title="Lineage-group treatment effects"
    )
}

###############################################################
## Forest plot of subgroup effects
###############################################################

plot_forest <- function(results){
  bind_rows(
    results$country %>%
      mutate(Subgroup=country),
    results$group %>%
      mutate(Subgroup=group)
  ) %>%
    ggplot(
      aes(
        reorder(Subgroup,RD_mean),
        RD_mean
      )
    ) +
    geom_point(size=2.5) +
    geom_errorbar(
      aes(
        ymin=RD_lower,
        ymax=RD_upper
      ),
      width=.15
    ) +
#    coord_flip() +
    geom_hline(
      yintercept=NI$margin,
      colour="red",
      linetype=2
    ) +
    labs(
      x="",
      y="Risk difference",
      title="Subgroup treatment effects"
    )
  
}