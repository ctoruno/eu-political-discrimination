## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Political Discrimination & Political Change - Trust estimations
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     January 5th, 2025
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(glue)
library(haven)
library(fixest)
library(logisticPCA)
library(modelsummary)
library(marginaleffects)
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
  "ipol", "polid", "minority"
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
    all_of(control_vars), incpp
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
      "Note: The figure shows the distribution of Trust in Political Institutions Index scores for individuals who",
      "experienced political discrimination\nor harassment (D/H) versus those who did not. Index scores are",
      "derived from Logistic PCA of trust responses across nine institutional categories: local authorities,\n",
      "national authorities, police, prosecutors, public defense attorneys, judges, magistrates, political parties,",
      "and Parliament members.\nRed areas show 95% confidence intervals of median scores."
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


fig_tsbd_2 = ggplot(
  data = tsbd_data %>%
    mutate(
      incpp = case_when(
        incpp == 1 ~ "Incumbent Party",
        incpp == 0 ~ "Other Alignment"
      )
    ) %>%
    filter(
      !is.na(poldis) & !is.na(incpp)
    ),
  aes(
    x = incpp,
    y = trt_score,
    fill = dh_exp
  )
) +
  geom_boxplot() +
  labs(
    y = "Trust in Political Institutions (Score)",
    x = ""
  ) + 
  scale_fill_manual(
    "",
    values = c(
      "D/H Experience" = "#494C6F",
      "No D/H Experience" = "#ABB4C4"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "plot",
    panel.grid.major.x = element_blank()
  )

ggsave(
  plot     = fig_tsbd_2,
  filename = glue("viz/fig_tsbd__2.png"),
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
    "trt_score ~ poldis + incpp + poldis*incpp + ", paste(control_vars, collapse = " + "), " | country"
  ),
  "(II)" = paste0(
    "TRT_govt_national ~ poldis + incpp + poldis*incpp + ", paste(control_vars, collapse = " + "), " | country"
  ),
  "(III)" = paste0(
    "TRT_govt_local ~ poldis + incpp + poldis*incpp + ", paste(control_vars, collapse = " + "), " | country"
  ),
  "(IV)" = paste0(
    "TRT_parliament ~ poldis + incpp + poldis*incpp + ", paste(control_vars, collapse = " + "), " | country"
  ),
  "(V)" = paste0(
    "TRT_judges ~ poldis + incpp + poldis*incpp + ", paste(control_vars, collapse = " + "), " | country"
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

extra_info <- tibble(
  term    = c("Variable", "Method"),
  `(I)`   = c("Trust Index", "OLS"),
  `(II)`  = c("Nat. Govt.", "Logit"),
  `(III)` = c("Loc. Govt.", "Logit"),
  `(IV)`  = c("Parliament", "Logit"),
  `(V)`   = c("Judges", "Logit")
)
# attr(extra_info, "position") <- c(8,9)
# See the add_rows section in: https://modelsummary.com/vignettes/modelsummary.html#shape

coef_map = c(
  "poldis" = "D/H Experience",
  "incpp"  = "Incumbent Party Follower",
  "poldis:incpp" = "D/H Experience * Incumbent Party Follower"
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
    national authorities, local authorities, members of parliament, and judges and magistrates, respectively.
    *, **, and *** represent statistical significance at p < 0.05, p < 0.01, and p < 0.001, respectively. 
    Standard Errors are clustered at the country level."
  ),
  add_rows = extra_info,
  escape = FALSE
)

marginal_effects <- lapply(
  c("NoInt" = "NoInt", 
    "Interaction" = "Interaction"),
  function(type){
    
    if (type == "NoInt") {
      
      mgeffects <- lapply(
        fe_models_fit,
        function(x){
          avg_comparisons(
            x,
            variables = "poldis"
          )
        }
      )
      extra_info <- tibble(
        term    = c("Variable", "Method"),
        `(I)`   = c("Trust Index", "OLS"),
        `(II)`  = c("Nat. Govt.", "Logit"),
        `(III)` = c("Loc. Govt.", "Logit"),
        `(IV)`  = c("Parliament", "Logit"),
        `(V)`   = c("Judges", "Logit")
      )
      
      modelsummary(
        mgeffects,
        stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
        gof_omit  = "RMSE",
        add_rows  = extra_info,
        output = "tables/tab_tsbd_mgeffects_1.png",
        notes = c(
          "*Note*: Table shows the predicted probability of having experienced political discrimination or harrasment (D/H)
          on different measures of trust in political institutions using a set of social and political traits as control
          variables. Model (I) uses the Trust in Political Institutions Index as a dependant variable, while models (II), 
          (III), (IV), and (V) use a binary variables equal to one if the person answered to have a lot or some trust in
          national authorities, local authorities, members of parliament, and judges and magistrates, respectively.
          *, **, and *** represent statistical significance at p < 0.05, p < 0.01, and p < 0.001, respectively. 
          Standard Errors are clustered at the country level."
        ),
        escape = FALSE
      )
      
    } else {
      
      mgeffects <- lapply(
        fe_models_fit,
        function(x){
          avg_comparisons(
            x,
            variables = "poldis",
            by = "incpp"
          )
        }
      )
      extra_info <- tibble(
        term    = c("Variable", "Method"),
        incpp   = c("", ""),
        `(I)`   = c("Trust Index", "OLS"),
        `(II)`  = c("Nat. Govt.", "Logit"),
        `(III)` = c("Loc. Govt.", "Logit"),
        `(IV)`  = c("Parliament", "Logit"),
        `(V)`   = c("Judges", "Logit")
      )
      
      modelsummary(
        mgeffects,
        stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
        gof_omit  = "R2|RMSE|AIC|BIC",
        shape     = term + incpp ~ model,
        add_rows  = extra_info,
        output = "tables/tab_tsbd_mgeffects_2.png",
        notes = c(
          "*Note*: Table shows the predicted probability of having experienced political discrimination or harrasment (D/H)
          on different measures of trust in political institutions by political alignment with the incumbent political party
          (incpp =1) and using a set of social and political traits as control variables. Model (I) uses the Trust in 
          Political Institutions Index as a dependant variable, while models (II), (III), (IV), and (V) use a binary variables 
          equal to one if the person answered to have a lot or some trust in national authorities, local authorities, members 
          of parliament, and judges and magistrates, respectively. *, **, and *** represent statistical significance at 
          p < 0.05, p < 0.01, and p < 0.001, respectively. Standard Errors are clustered at the country level."
        ),
        escape = FALSE
      )
    }
  }
)



