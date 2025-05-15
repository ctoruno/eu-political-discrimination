## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Polarization Measures
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     January 5th, 2025
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(glue)
library(haven)
library(fixest)
library(cowplot)
library(modelsummary)
library(tidyverse)

if (interactive()){
  source("src/config.R")
  source("src/data_loading.R")
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Loading Data ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

eugpp <- load_data(path2SP, "gpp")

target_vars <- c(
  "gap", "center"
)
control_vars <- c(
  "female", "young", "fconst", "hedu", "ipol", "polid", "rural", "minority", 
  "incpp", "employed", "married", "foreigner"
)

diap_data <- eugpp %>%
  mutate(
    polid_left = if_else(left == 1, polid, NA_real_),
    polid_right = if_else(right == 1, polid, NA_real_)
  ) %>%
  filter(
    polid >=0 & polid <= 10
  ) %>%
  group_by(nuts_id) %>%
  mutate(
    avg_polid_left  = mean(polid_left, na.rm = TRUE),
    avg_polid_right = mean(polid_right, na.rm = TRUE),
    distance2left   = abs(polid-avg_polid_left),
    distance2right  = abs(polid-avg_polid_right),
    gap = if_else(
      left == 1,
      abs(distance2left-distance2right),
      abs(distance2right-distance2left)
    ),
    center = case_when(
      polid %in% c(4,5,6) ~ 1,
      polid %in% c(0,1,2,3,7,8,9,10) ~ 0,
    )
  ) %>%
  ungroup() %>%
  select(
    country = country_name_ltn, nuts_id, 
    poldis,
    all_of(target_vars),
    all_of(control_vars)
  )


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Scatter Plot - D/H Experiences and Affective Polarization (diap) ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Aggregating into NUTS Regions
diap_scatter_data <- diap_data %>%
  group_by(nuts_id) %>%
  summarise(
    poldis = mean(poldis, na.rm = TRUE),
    gap_mean = mean(gap, na.rm = TRUE),
    center_sum = sum(center, na.rm = TRUE),
    sample_size = n()
  ) %>%
  mutate(
    center_prop = center_sum/sample_size
  )

# Individual Scatter Plots
scatter_plots <- imap(
  c(
    "Avg. Affective Polarization Score" = "gap_mean", 
    "Proportion of Respondents with a Centered Ideology" = "center_prop"
  ),
  function(target, axis_title){
    
    scatter <- diap_scatter_data %>%
      select(
        target = all_of(target),
        poldis
      ) %>%
      ggplot(
        aes(
          x = target,
          y = poldis
        )
      ) +
      geom_point(
        color = "#658FA4"
      ) +
      geom_smooth(
        method = "lm", 
        color = "#153243",
        fill = "#C1DCEB",
        se = TRUE,
      ) +
      labs(
        # title = "Correlation between Political Discrimination and Affective Polarization",
        y = "Political Discrimination Rate",
        x = axis_title
      ) +
      theme_minimal()
    
    return(scatter)
  }
)

# Assembling Bipanel Cowplot
fig_diap <- plot_grid(
  scatter_plots[[1]],
  scatter_plots[[2]] + ylab(NULL) + theme(axis.text.y = element_blank())
)
fig_diap <- add_sub(
  fig_diap,
  paste(
    "*Note*: Avg. Affective Polarization Score is estimated as the regional average difference (gap)",
    "between (1) how close an individual's political ideology is to their own group average, and (2)",
    "how far away an individual's political\nideology is to the opposite group average. The Proportion",
    "of Respondents with a Centered Ideology represents the proportion of individuals within a NUTS",
    "region that have a political ideology of 4,5, or 6 in a scale that ranges\nfrom 0 to 10."
  ),
  size  = 7,
  hjust = 0,
  x = 0
)

# Saving Chart
ggsave(
  plot     = fig_diap,
  filename = glue("viz/fig_diap.png"),
  width    = 10,
  height   = 5,
  units    = "in"
) 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  FEOLS - D/H Experiences and Affective Polarization (diap) ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

feols_models <- list(
  "(I)" = "gap ~ poldis",
  "(II)" = paste0(
    "gap ~ ",
    "poldis + female + young + fconst + hedu + ipol + rural + employed + married + foreigner + minority + incpp"
  ),
  "(III)" = paste0(
    "gap ~ ",
    "poldis + female + young + fconst + hedu + ipol + rural + employed + married + foreigner + minority + incpp",
    " | country"
  )
)

coef_map = c(
  "poldis" = "D/H Experience"
)

feols_models_fit <- lapply(
  feols_models,
  function(x){
    
    feols_fitted <- feols(
      as.formula(x),
      cluster = ~country,
      data = diap_data
    )
    
    return(feols_fitted)
    
  }
)

modelsummary(
  feols_models_fit,
  estimate  = "{estimate}{stars}",
  stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  coef_map  = coef_map,
  gof_omit  = "RMSE",
  output = "tables/tab_diap_feols.png",
  notes  = c(
    "*Note*: Table displays the results of fitting an OLS regression on the Affective Polarization Score
    as a function of having experienced political discrimination or harrasment (D/H) or not. Model (I) fits
    a simple linear regression with no additional covariates beyond the variable of interest. Model (II) fits
    a multivariate linear regression with a series of sociodemographic controls. Model (III) fits a multivariate
    linear regression with country fixed effects. *, **, and *** represent statistical significance at 
    p < 0.05, p < 0.01, and p < 0.001, respectively. Standard Errors are clustered at the country level."
  ),
  escape = FALSE
)




