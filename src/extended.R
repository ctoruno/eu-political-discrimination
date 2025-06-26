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

bootstraping = FALSE

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

## Post-matching love plot
love_plot(m.out1)

## Extracting matched data
matched_data <- match_data(m.out1)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5. FE-OLS Estimation ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fe_models <- list(
  "(I)" = paste0(
    "log(trt_score_scaled) ~ poldis | nuts_id"
  ),
  "(II)" = paste0(
    "log(trt_score_scaled) ~ poldis + ", 
    paste(dem_vars, collapse = " + "), " + ", 
    "age^2 | nuts_id"
  ),
  "(III)" = paste0(
    "log(trt_score_scaled) ~ poldis + ", 
    paste(dem_vars, collapse = " + "), " + ",
    paste(pol_vars, collapse = " + "), " + ", 
    "age^2 + polid^2 | nuts_id"
  ),
  "(IV)" = paste0(
    "log(trt_score_scaled) ~ poldis + ", 
    paste(dem_vars, collapse = " + "), " + ",
    paste(pol_vars, collapse = " + "), " + ", 
    "age^2 + polid^2 | nuts_id"
  )
)

fe_models_fit <- imap(
  fe_models,
  function(x, model){
    
    if (model %in% c("(IV)")){
      fitted_model <- feols(
        as.formula(x),
        data = study_data,
        cluster = ~nuts_id
      )
    } else {
      fitted_model <- feols(
        as.formula(x),
        data = matched_data,
        cluster = ~subclass,
        weights = matched_data$weights
      )
    }
    
    mgeffects <- avg_comparisons(
      fitted_model,
      variables = "poldis",
      transform = expm1,
      newdata = subset(poldis == 1)
    )
    
    return(
      list(
        "fit" = fitted_model,
        "mge" = mgeffects
      )
    )
  }
)

## FEOLS Main Results Table
main_results_feols(fe_models_fit)


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
  model = fe_models_fit[["(III)"]][["fit"]], 
  treatment = "poldis",
  benchmark_covariates = c("incpp"),
  kd = c(1,5,10,15)
)
summary(poldis.sensitivity)

## Sensitivity Analysis Table
sensitivity_analysis(poldis.sensitivity)

## Sensitivity Analysis Fxigure
png("viz/sensitivity_analysis.png", width = 6, height = 6, units = "in", res = 300)
plot(poldis.sensitivity, type = "contour")
dev.off()


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
    
    distance2left = abs((polid-avg_polid_left)^2),
    distance2right = abs((polid-avg_polid_right)^2),
    distance2center = abs((polid-avg_polid_center)^2),
    
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
## 8. Heterogeneous Effects: Political Alignment ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Fitting Model
fe_model.het <- paste0(
  "log(trt_score_scaled) ~ poldis + incpp*poldis +", 
  paste(dem_vars, collapse = " + "), " + ",
  paste(pol_vars, collapse = " + "), " + ", 
  "age^2 + polid^2 | nuts_id"
)
fitted_model.het <- feols(
  as.formula(fe_model.het),
  data = matched_data,
  cluster = ~subclass,
  weights = matched_data$weights
)
modelsummary(fitted_model.het)

## Marginal Effects
mgeffects.het <- avg_comparisons(
  fitted_model.het,
  variables = "poldis",
  by = "incpp",
  transform = expm1,
  newdata = subset(poldis == 1),
  vcov = sandwich::vcovCL(
    fitted_model.het,
    cluster = matched_data$subclass
  ),
  conf_level = 0.95
)

## Heterogeneous Effects Table
mgeffects.het
modelsummary(
  mgeffects.het,
  stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_omit  = "R2|RMSE|AIC|BIC",
  shape     = term + incpp ~ model,
)
het.effects(mgeffects.het)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 9. Heterogeneous Effects: Political Conformity/Dissonance ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Political Dissonance Data
political_dissonance_by_country <- study_data %>%
  group_by(country) %>%
  mutate(
    avg_polid = mean(polid, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    dist2avg.polid = (avg_polid - polid)^2
  )

median_dist <- median(political_dissonance_by_country$dist2avg.polid)

political_dissonance_data <- matched_data %>%
  left_join(
    political_dissonance_by_country %>% 
      select(country_year_id, dist2avg.polid),
    by = "country_year_id"
  ) %>%
  mutate(
    high_dist2avg.polid = if_else(
      dist2avg.polid >= median_dist, 1, 0
    )
  )

## Fitting Model
fe_model.dissonance <- paste0(
  "log(trt_score_scaled) ~ poldis + poldis*high_dist2avg.polid*incpp +", 
  paste(dem_vars, collapse = " + "), " + ",
  "ipol + cp_score | nuts_id"
)
fitted_model.dissonance <- feols(
  as.formula(fe_model.dissonance),
  data = political_dissonance_data,
  cluster = ~subclass,
)
modelsummary(fitted_model.dissonance)

## Marginal Effects
mgeffects.dissonance <- avg_comparisons(
  fitted_model.dissonance,
  variables = c("poldis"),
  by = c("high_dist2avg.polid", "incpp"),
  transform = expm1,
  newdata = subset(poldis == 1),
  conf_level = 0.95
)

## Heterogeneous Effects Table
mgeffects.dissonance
modelsummary(
  mgeffects.dissonance,
  stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_omit  = "R2|RMSE|AIC|BIC",
  shape     = term + incpp + high_dist2avg.polid ~ model,
)
het.effects.dissonance(mgeffects.dissonance)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 10. Mediation Analysis: Political Conformity/Dissonance ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set.seed(140892)

## Mediation Data
mediation_data <- dummy_cols(
  political_dissonance_data, 
  select_columns = "nuts_id", 
  remove_first_dummy = TRUE, 
  remove_selected_columns = TRUE
)

## Mediator Model
m.model <- paste(
  "dist2avg.polid ~ poldis +",
  paste(dem_vars, collapse = " + "), "+ age^2 +",
  paste(names(mediation_data %>% select(starts_with("nuts_id"))), collapse = " + "), "+",
  "ipol + incpp + cp_score"
)
m.model_fitted <- lm(
  as.formula(m.model),
  data = mediation_data
)
summary(m.model_fitted)

## Outcome Model
y.model <- paste(
  "trt_score_scaled ~ poldis + dist2avg.polid + poldis*dist2avg.polid +",
  paste(dem_vars, collapse = " + "), "+ age^2 +",
  paste(names(mediation_data %>% select(starts_with("nuts_"))), collapse = " + "), "+",
  "ipol + incpp + cp_score"
)
y.model_fitted <- lm(
  as.formula(y.model),
  data = mediation_data
)
summary(y.model_fitted)

## ACME bootstrapping (computationally expensive)
if(bootstraping){
  mediation_results <- mediate(
    m.model_fitted,
    y.model_fitted,
    treat    = "poldis",
    mediator = "dist2avg.polid",
    boot     = TRUE,
    sims     = 10000,
    # robustSE = TRUE,
    boot.ci.type = "bca"
  )
  saveRDS(
    mediation_results,
    "R.objs/mediation_results_3.rds"
  )
} else {
  mediation_results <- readRDS(
    "R.objs/mediation_results_3.rds"
  )
}

## Mediation Analysis Table
summary(mediation_results)
mediation_analysis(mediation_results)

## ACME sensitivity
mediation_sensitivity <- medsens(
  mediation_results, 
  rho.by = 0.2,
  sims = 1000
)
summary(mediation_sensitivity)
# plot(mediation_sensitivity)
# plot(mediation_sensitivity, sens.par = "R2", r.type = "total", sign.prod = "positive")




x = paste0(
  "trt_score_scaled ~ poldis + ", 
  paste(dem_vars, collapse = " + "), " + ",
  paste(pol_vars, collapse = " + "), " + ", 
  "age^2 + polid^2 | nuts_id"
)

fitted_model <- feols(
  as.formula(x),
  data = matched_data,
  cluster = ~subclass,
  weights = matched_data$weights
)
