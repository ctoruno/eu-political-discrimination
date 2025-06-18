## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Political Discrimination & Political Change - Full Script Calculations
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     January 5th, 2025
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(glue)
library(haven)
library(scales)
library(fixest)
library(rbounds)
library(MatchIt)
library(fastDummies)
library(sensemakr)
library(kableExtra)
library(mediation)
library(logisticPCA)
library(modelsummary)
library(marginaleffects)
library(tidyverse)

if (interactive()){
  source("src/config.R")
  source("src/data_loading.R")
  source("src/tabs.R")
  source("src/viz.R")
}

# Master Data
eugpp <- load_data(path2SP, "gpp") %>%
  filter(
    country_name_ltn != "Luxembourg"
  )
dem_vars <- c(
  "female", "age", "rural", "fconst", "hedu", "employed", "married", 
  "foreigner", "minority"
)
pol_vars <- c(
  "ipol", "polid", "incpp"
)
control_vars <- c(dem_vars, pol_vars, "cp_score")

## Discrimination Incidence Table
discrimination_incidence(eugpp)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1. TRT & CP Scores ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Data subset for estimating scores using a logistic PCA
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

cp_data <- eugpp %>%
  select(
    CP_protest, CP_consultation, CP_cso
  ) %>%
  mutate(
    across(
      everything(),
      \(x) case_when(
        x %in% c(1) ~ 1,
        x %in% c(2,98) ~ 0,
      ) 
    )
  )

# Fitting and predicting Logistic PCA
logPCA_trt <- logisticPCA(trt_data, k = 1)
logPCA_cp  <- logisticPCA(cp_data, k = 1)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Assembling Data Subset ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

study_data_prep <- eugpp %>%
  select(
    country_year_id,
    country = country_name_ltn, nuts_id,
    poldis,
    all_of(dem_vars), all_of(pol_vars)
  ) %>%
  mutate(
    dh_exp = case_when(
      poldis == 1 ~ "D/H Experience",
      poldis == 0 ~ "No D/H Experience",
    )
  ) %>%
  bind_cols(trt_data) %>%
  bind_cols(cp_data)
study_data_prep[["trt_score"]] <- predict(logPCA_trt, type = "PCs")[,1]*-1
study_data_prep[["cp_score"]]  <- predict(logPCA_cp, type = "PCs")[,1]*-1

study_data <- study_data_prep %>%
  drop_na() %>%
  mutate(
    trt_score_scaled = rescale(trt_score, to = c(1, 10))
  )

# Descriptive Statistics Table
descriptive_stats(study_data)

# Trust Naive Comparison figure
trust_comparison_naive(study_data)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Check Initial Imbalance ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

m.out0 <- matchit(
  as.formula(
    paste0(
      "poldis ~ ",
      paste(control_vars, collapse = " + "),
      " + nuts_id"
    )
  ),
  data = study_data,
  method = NULL,
  distance = "glm"
)

## Initial Balance Table
balance_assessment(m.out0)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4. Matching ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# NN matching
m.out1 <- matchit(
  as.formula(
    paste0(
      "poldis ~ ",
      paste(control_vars, collapse = " + "),
      " + nuts_id"
    )
  ),
  data = study_data,
  method = "nearest",
  discard = "control",
  distance = "glm"
)

## Post-matching Balance Table
balance_assessment(m.out1, initial = FALSE)

plot(
  summary(m.out1, addlvariables = ~ I(age^2) + I(polid^2)), 
  var.order = "unmatched"
)
matched_data <- match_data(m.out1)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5. FE-OLS Estimation ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fe_model <- paste(
  "trt_score_scaled ~ poldis +", 
  paste(dem_vars, collapse = " + "), "+",
  paste(pol_vars, collapse = " + "), "+",
  "cp_score + age^2 | nuts_id"
)
fitted_model <- feols(
  as.formula(fe_model),
  data = matched_data,
  cluster = ~subclass,
  weights = matched_data$weights
)
summary(fitted_model)

mgeffects <- avg_comparisons(
  fitted_model,
  variables = "poldis",
  transform = expm1,
  newdata = subset(poldis == 1)
)
modelsummary(
  mgeffects,
  stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_omit  = "R2|RMSE|AIC|BIC"
)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 6. Sensitivity Analysis ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Rosenbaum Bounds 
treatment_group <- matched_data %>% filter(poldis == 1)
control_group   <- matched_data %>% filter(poldis == 0)

psens_res <- psens(
  treatment_group$trt_score,
  control_group$trt_score,
  Gamma = 2.0,
  GammaInc = 0.05
)

## Coefficient-Stability Bounds
poldis.sensitivity <- sensemakr(
  model = fitted_model, 
  treatment = "poldis",
  benchmark_covariates = c("incpp"),
  kd = 1:10
)
poldis.sensitivity
ovb_minimal_reporting(poldis.sensitivity, format = "html")
plot(poldis.sensitivity)
plot(poldis.sensitivity, type = "extreme")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 7. Affective Polarization Score ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Estimating an Affective Polarization Metric
polarization_data <- eugpp %>%
  mutate(
    polid_left = if_else(left == 1, polid, NA_real_),
    polid_right = if_else(right == 1, polid, NA_real_),
    polid_center = if_else(center == 1, polid, NA_real_)
  ) %>%
  filter(
    polid>=0 & polid<=10
  ) %>%
  group_by(nuts_id) %>%
  mutate(
    avg_polid_left = mean(polid_left, na.rm = TRUE),
    avg_polid_right = mean(polid_right, na.rm = TRUE),
    avg_polid_center = mean(polid_center, na.rm = TRUE),
    
    distance2left = abs(polid-avg_polid_left),
    distance2right = abs(polid-avg_polid_right),
    distance2center = abs(polid-avg_polid_center),
    
    polgap = case_when(
      left == 1 ~ mean(c(
        abs(distance2left-distance2right),
        abs(distance2left-distance2center)
      )),
      right == 1 ~ mean(c(
        abs(distance2right-distance2left),
        abs(distance2right-distance2center)
      )),
      center == 1 ~ mean(c(
        abs(distance2center-distance2left),
        abs(distance2center-distance2right)
      ))
    )
  ) %>%
  ungroup() %>%
  select(
    country_year_id, polgap
  )

## Adding polarizaton gap to study data
study_data <- study_data %>%
  left_join(
    polarization_data,
    by = "country_year_id"
  )
matched_data <- matched_data %>%
  left_join(
    polarization_data,
    by = "country_year_id"
  )


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 8. Mediation Analysis ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set.seed(281299)
mediation_data <- dummy_cols(
  matched_data, 
  select_columns = "nuts_id", 
  remove_first_dummy = TRUE, 
  remove_selected_columns = TRUE
)

## Mediator Model
m.model <- paste(
  "polgap ~ poldis +",
  paste(dem_vars, collapse = " + "), "+",
  paste(pol_vars, collapse = " + "), "+",
  paste(names(mediation_data %>% select(starts_with("nuts_"))), collapse = " + "), "+",
  "cp_score + age^2"
)
m.model_fitted <- lm(
  as.formula(m.model),
  data = mediation_data
)
summary(m.model_fitted)

## Outcome Model
y.model <- paste(
  "trt_score ~ poldis + polgap + poldis*polgap +",
  paste(dem_vars, collapse = " + "), "+",
  paste(pol_vars, collapse = " + "), "+",
  paste(names(mediation_data %>% select(starts_with("nuts_"))), collapse = " + "), "+",
  "cp_score + age^2"
)
y.model_fitted <- lm(
  as.formula(y.model),
  data = mediation_data
)
summary(y.model_fitted)

## ACME bootstrapping
mediation_results <- mediate(
  m.model_fitted,
  y.model_fitted,
  treat    = "poldis",
  mediator = "polgap",
  boot     = TRUE,
  sims     = 10000,
  boot.ci.type = "perc"
)
summary(mediation_results)
saveRDS(
  mediation_results,
  "R.objs/mediation_results.rds"
)

## ACME sensitivity
mediation_sensitivity <- medsens(
  mediation_results, 
  rho.by = 0.2,
  sims = 1000
)
summary(mediation_sensitivity)
plot(mediation_sensitivity)
plot(mediation_sensitivity, sens.par = "R2", r.type = "total", sign.prod = "positive")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 9. Heterogeneous Effects ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fe_model.het <- paste(
  "trt_score_scaled ~ poldis +", 
  paste(dem_vars, collapse = " + "), "+",
  paste(pol_vars, collapse = " + "), "+",
  "cp_score + age^2 + poldis*incpp | nuts_id"
)
fitted_model.het <- feols(
  as.formula(fe_model.het),
  data = matched_data,
  cluster = ~subclass,
  weights = matched_data$weights
)
summary(fitted_model.het)

mgeffects.het <- avg_comparisons(
  fitted_model.het,
  variables = "poldis",
  by = "incpp",
  transform = expm1,
  newdata = subset(poldis == 1)
)
mgeffects.het

modelsummary(
  mgeffects.het,
  stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_omit  = "R2|RMSE|AIC|BIC",
  shape     = term + incpp ~ model,
)
