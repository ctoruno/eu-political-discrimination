## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Political Discrimination & Political Change
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     January 5th, 2025
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(glue)
library(haven)
library(fixest)
library(logisticPCA)
library(tidyverse)

if (interactive()){
  source("src/config.R")
  source("src/data_loading.R")
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Data Loading & TRUST Scores ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Master Data
eugpp <- load_data(path2SP, "gpp")
control_vars <- c(
  "female", "young", "rural", "fconst", "hedu", "employed", "married", "foreigner", 
  "ipol", "polid", "minority", "incpp"
)

# Data subset for estimating TRT scores using a logistic PCA
trt_data <- eugpp %>%
  select(
    starts_with("TRT_"),
    -c(TRT_people, TRT_media, TRT_inst_eu)
  ) %>%
  mutate(
    across(
      everything(),
      \(x) case_when(
        x %in% c(1,2) ~ 1,
        x %in% c(3,4,98) ~ 0,
      ) 
    )
  )

# Fitting and predicting Logistic PCA
logPCA <- logisticPCA(trt_data, k = 1)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  TRT Scores By D/H Experience (tsbd) ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tsbd_data <- eugpp %>%
  select(
    country = country_name_ltn, nuts_id,
    poldis,
    all_of(control_vars)
  ) %>%
  mutate(
    dh_exp = case_when(
      poldis == 1 ~ "D/H Experience",
      poldis == 0 ~ "No D/H Experience",
    )
  ) %>%
  bind_cols(trt_data)
tsbd_data[["trt_score"]] <- predict(logPCA, type = "PCs")[,1]*-1

fig_tsbd <- ggplot(
  data = tsbd_data %>% 
    filter(!is.na(dh_exp)) %>%
    group_by(dh_exp) %>%
    mutate(
      mean = mean(trt_score, na.rm = TRUE),
      median = median(trt_score, na.rm = TRUE),
      sd  = sd(trt_score, na.rm = TRUE),
      n  = n(),
      se = sd/sqrt(n),
      t_val  = qt(p = 0.975, df = n - 1),
      lower  = median - t_val * se,
      upper  = median + t_val * se
    ),
  aes(
    x = dh_exp,
    y = trt_score
  )
) +
  geom_boxplot() +
  geom_errorbar(
    aes(
      ymin = lower, 
      ymax = upper
    ), 
    width = 0.75,
    color = "#901C14",
    linewidth = 3
  ) +
  labs(
    y = "Trust in Political Institutions (Score)",
    x = "",
    caption = paste(
      "The figure displays the distribution of the Trust in Political Institutions Index between individuals",
      "that have experienced political discrimination\nor harrasment (D/H) and those who have not. The index scores",
      "are estimated using a Logistic PCA reduction of the individual's answers\nto their levels of trust in local",
      "authorities, national authorities, police, prosecutors, public defense attorneys, judges, magistrates,",
      "political parties,\nand members of Parliament. The red area displays the 95% confidence interval of the",
      "median score for each group."
    )
  ) + 
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "plot",
    panel.grid.major.x = element_blank()
  )

ggsave(
  plot     = fig_tsbd,
  filename = glue("viz/fig_tsbd.png"),
  width    = 8,
  height   = 6,
  units    = "in"
) 
  

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  TRT Scores By D/H Experience (tsbd) ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fe_models <- list(
  "(I)" = paste0(
    "trt_score ~ poldis +", paste(control_vars, collapse = " + "), " | country"
  ),
  "(II)" = paste0(
    "TRT_govt_national ~ poldis +", paste(control_vars, collapse = " + "), " | country"
  ),
  "(III)" = paste0(
    "TRT_govt_local ~ poldis +", paste(control_vars, collapse = " + "), " | country"
  ),
  "(IV)" = paste0(
    "TRT_parliament ~ poldis +", paste(control_vars, collapse = " + "), " | country"
  ),
  "(V)" = paste0(
    "TRT_judges ~ poldis +", paste(control_vars, collapse = " + "), " | country"
  )
)

fe_models_fit <- imap(
  fe_models,
  function(x, model){
    
    if (model %in% c("(I)")){
      fitted_model <- feols(
        as.formula(x),
        cluster = ~country,
        data = tsbd_data
      )
    } else {
      fitted_model <- feglm(
        as.formula(x),
        cluster = ~country,
        family  = binomial(link = "logit"),
        data    = tsbd_data
      )
    }
    
    return(fitted_model)
  }
)

coef_map = c(
  "poldis" = "D/H Experience"
)

modelsummary(
  fe_models_fit,
  stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  coef_map  = coef_map,
  gof_omit  = "R2|RMSE",
  output = "tables/tab_tsbd_fe.png",
  notes  = c(
    "*Note*: Table shows the results of regressing having experienced political discrimination or harrasment (D/H)
    on different measures of trust in political institutions and using a set of social and political traits as control
    variables. Model (I) uses the Trust in Political Institutions Index as a dependant variable, while models (II), 
    (III), (IV), and (V) use a binary variables equal to one if the person answered to have a lot or some trust in
    national authoroties, local authorities, members of parliament, and judges and magistrates, respectively.
    *, **, and *** represent statistical significance at p < 0.05, p < 0.01, and p < 0.001, respectively. 
    Standard Errors are clustered at the country level."
  ),
  escape = FALSE
)

