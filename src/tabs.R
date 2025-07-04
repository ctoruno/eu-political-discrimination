## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Political Discrimination & Political Change - LaTex tabs
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     January 5th, 2025
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

descriptive_stats <- function(data){
  
  data4table <- data %>%
    mutate(
      female = case_when(
        female == 1 ~ "Female",
        female == 0 ~ "Male"
      ),
      employed = case_when(
        employed == 1 ~ "Employed",
        employed == 0 ~ "Unemployed/Inactive"
      ),
      rural = case_when(
        rural == 1 ~ "Rural",
        rural == 0 ~ "Urban"
      ),
      married = case_when(
        married == 1 ~ "Married",
        married == 0 ~ "Not married"
      ),
      foreigner = case_when(
        foreigner == 1 ~ "Foreigner",
        foreigner == 0 ~ "Citizen"
      ),
      minority = case_when(
        minority == 1 ~ "Ethnic Minority",
        minority == 0 ~ "Ethnic Majority"
      ),
      fconst = case_when(
        fconst == 1 ~ "Constrained",
        fconst == 0 ~ "Unconstrained"
      ),
      hedu = case_when(
        hedu == 1 ~ "Higher Education",
        hedu == 0 ~ "No Higher Education"
      ),
      ipol = case_when(
        ipol == 1 ~ "Interested",
        ipol == 0 ~ "Uninterested"
      ),
      incpp = case_when(
        incpp == 1 ~ "Incumbent Political Party",
        incpp == 0 ~ "Non-Incumbent Political Party"
      )
    ) %>%
    select(
      Country = country,
      Sex  = female, 
      Age  = age,
      Employment  = employed,
      `Area of Residence`    = rural,
      `Citizenship Status`   = foreigner,
      `Marital Status`       = married,
      `Ethnic Group`         = minority,
      `Financial Situation`  = fconst,
      `Education`            = hedu,
      `Political Ideology`   = polid,
      `Interest in Politics` = ipol,
      `Political Alignment`  = incpp,
      `Political D/H`        = dh_exp 
    )
  
  datasummary_skim(
    data4table %>% select(-Country), 
    output = "tex/descriptive_stats.tex",
    fmt = \(x) format(
      round(x,1), 
      nsmall=1, 
      big.mark=",", 
      scientific = FALSE
    ),
    fun_numeric = getOption(
      "modelsummary_fun_numeric", 
      default = list(
        Unique = NUnique,
        # `Missing Pct.` = PercentMissing, 
        Mean = Mean, 
        SD = SD, 
        Min = Min, 
        Max = Max
        # Median = Median,
        # Histogram = function(x) ""
      )
    )
  )
  
  datasummary_skim(
    data4table %>% select(Country), 
    output = "tex/descriptive_stats_country_counts.tex",
    fmt = \(x) format(
      round(x,1), 
      nsmall=1, 
      big.mark=",", 
      scientific = FALSE
    )
  )
  
  return(TRUE)
  
}


discrimination_incidence <- function(eugpp){
  
  dis_grounds <- names(
    eugpp %>% 
      select(starts_with("DIS_")) %>% 
      select(!starts_with("DIS_exp"))
  )
  
  tab_dicg <- eugpp %>%
    select(
      country = country_name_ltn, all_of(dis_grounds)
    ) %>%
    mutate(
      across(
        all_of(dis_grounds),
        \(x) case_when(
          x == 1  ~ 1,
          x == 2  ~ 0,
          x == 98 ~ 0,
        )
      )
    )
  
  datasummary(
    Heading("") * Mean * (`Country` = country) ~ 
      (`Political Opinion` = DIS_politics) +
      (`Sex` = DIS_sex) + 
      (`Gender` = DIS_gender) +
      (`Ethnicity` = DIS_ethni) +
      (`Migration Status` = DIS_migration) +
      (`Social Status` = DIS_ses) +
      (`Religion` = DIS_religion),
    data   = tab_dicg,
    fmt    = function(x) format(round(x*100,1), nsmall = 1),
    align  = "llccccccc",
    output = "tex/discrimination_experiences.tex",
    notes  = "*Note*: Table displays the percentage of respondents in each country that answered to have had experienced discrimination
  or harrasment for each of the grounds presented to them by the survey.",
    escape = TRUE
  )
  
  return(TRUE)
}


balance_assessment <- function(balance, initial=TRUE){
  
  # Balance summary
  if(initial) {
    summ <- summary(
      balance, 
      addlvariables = ~ I(age^2) + I(polid^2)
    )[["sum.all"]]
  } else {
    summ <- summary(
      balance, 
      un = FALSE, 
      addlvariables = ~ I(age^2) + I(polid^2)
    )[["sum.matched"]]
  }
  
  # Summary as Data Frame
  df <- as.data.frame(
    summ
  ) %>%
    select(-7) %>%
    mutate(
      across(
        everything(),
        \(x) round(x, 3)
      )
    )
  rownames(df) <- case_when(
    rownames(df) == "distance" ~ "Distance",
    rownames(df) == "female" ~ "Gender: Female",
    rownames(df) == "age" ~ "Age",
    rownames(df) == "rural" ~ "Area: Rural",
    rownames(df) == "fconst" ~ "Fin. Situation: Constrained",
    rownames(df) == "hedu" ~ "Education: Higher Education Diploma",
    rownames(df) == "employed" ~ "Employment: Employed",
    rownames(df) == "married" ~ "Marital Status: Married",
    rownames(df) == "foreigner" ~ "Citizenship: Foreigner",
    rownames(df) == "minority" ~ "Ethnic Group: Minority",
    rownames(df) == "ipol" ~ "Interest in Politics: High",
    rownames(df) == "polid" ~ "Political Ideology",
    rownames(df) == "incpp" ~ "Political Alignment: Incumbent",
    rownames(df) == "cp_score" ~ "Civic Participation Score",
    TRUE ~ rownames(df)
  )
  
  # Transforming to Kable and writting as TEX
  tbl <- kbl(
    df[c(1:14),], 
    format = "latex"
  ) %>% 
    kable_classic("striped")
  
  if(initial) {
    writeLines(tbl, "tex/initial_balance.tex")
  } else {
    writeLines(tbl, "tex/postmatch_balance.tex")
  }
  
  return(TRUE)
}


main_results_feols <- function(fitted_models){
  
  results <- lapply(
    fitted_models,
    function(x){
      x[["mge"]]
    }
  )
  
  coef_map = c(
    "poldis" = "D/H Experience"
  )
  
  rsq_list <- lapply(
    fitted_models,
    function(x){
      format(
        round(
          fitstat(x$fit, "ar2")[[1]],
          3
        ),
        nsmall = 3
      )
    }
  )
  
  wrsq_list <- lapply(
    fitted_models,
    function(x){
      format(
        round(
          fitstat(x$fit, "wr2")[[1]],
          3
        ),
        nsmall = 3
      )
    }
  )
  
  awrsq_list <- lapply(
    fitted_models,
    function(x){
      format(
        round(
          fitstat(x$fit, "awr2")[[1]],
          3
        ),
        nsmall = 3
      )
    }
  )
  
  extra_info <- tibble(
    term    = c("Adj. R.sq.", "Adj. Within R.sq.", "Region FE", "Dem. Cov.", "Pol. Cov.", "Sample"),
    `(I)`   = c(rsq_list[[1]], awrsq_list[[1]], "X", "", "", "Matched"),
    `(II)`  = c(rsq_list[[2]], awrsq_list[[2]], "X", "X", "", "Matched"),
    `(III)` = c(rsq_list[[3]], awrsq_list[[3]], "X", "X", "X", "Matched"),
    `(IV)`  = c(rsq_list[[4]], awrsq_list[[4]], "X", "X", "X", "Full"),
  )
  
  modelsummary(
    results,
    estimate  = "{estimate}{stars}",
    stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
    gof_omit  = "R2|RMSE|AIC|BIC|FE",
    coef_map  = coef_map,
    add_rows  = extra_info,
    output    = "tex/main_results.tex"
  )
  
  return(TRUE)
}


sensitivity_analysis <- function(output){
  tex.output = ovb_minimal_reporting(output, format = "latex")
  writeLines(tex.output, "tex/sensitivity_analysis.tex")
}


mediation_analysis <- function(mediation_results){
  
  ## Table Columns
  estimates = c(
    mediation_results$d.avg, 
    mediation_results$z.avg, 
    mediation_results$tau.coef, 
    mediation_results$n.avg
  )
  
  ci_lower = c(
    mediation_results$d.avg.ci[["2.5%"]],
    mediation_results$z.avg.ci[["2.5%"]],
    mediation_results$tau.ci[["2.5%"]],
    mediation_results$n.avg.ci[["2.5%"]]
  )
  
  ci_upper = c(
    mediation_results$d.avg.ci[["97.5%"]],
    mediation_results$z.avg.ci[["97.5%"]],
    mediation_results$tau.ci[["97.5%"]],
    mediation_results$n.avg.ci[["97.5%"]]
  )
  
  pvalue = c(
    mediation_results$d.avg.p, 
    mediation_results$z.avg.p, 
    mediation_results$tau.p, 
    mediation_results$n.avg.p
  )
  
  ## Data Frame
  df = data.frame(
    estimates, ci_lower, ci_upper, pvalue
  )
  names(df) <- c(
    "Estimate",
    "95% CI Lower",
    "95% CI Upper",
    "p-value"
  )
  row.names(df)<-c(
    "ACME",
    "ADE",
    "Total Effect",
    "Prop. Mediated"
  )
  
  ## Kable
  t = kbl(
    df,
    booktabs = TRUE,
    format = "latex",
    digits = 3
  )
  writeLines(t, "tex/mediation_analysis.tex")
  
  return(TRUE)
}


het.effects <- function(output){
  
  ## Data Frame
  df = output %>%
    select(c(3:5, 7,8)) %>%
    mutate(
      incpp = if_else(
        incpp == 1,
        "Incumbent",
        "Non-Incumbent"
      ),
      viz = ""
    )
  names(t) <- c("Political Alignment", "Estimate", "Pr(>|z|)", "95% CI Lower",  "95% CI Upper", " ")
  
  ## Kable
  t = kbl(
    df,
    booktabs = TRUE,
    format = "latex",
    digits = 3
  ) %>%
    kable_styling(full_width = FALSE) %>%
    column_spec(
      6,
      image = spec_pointrange(
        x=output$estimate, 
        xmin=output$conf.low, 
        xmax= output$conf.high, 
        vline=0
      ) 
    )
  writeLines(t, "tex/het_effects.tex")
  
  return(TRUE)
}


het.effects.dissonance <- function(output){
  
  ## Data Frame
  df = output %>%
    select(c(3:6, 8,9)) %>%
    mutate(
      incpp = if_else(
        incpp == 1,
        "Incumbent",
        "Non-Incumbent"
      ),
      high_dist2avg.polid = if_else(
        high_dist2avg.polid == 1,
        "High",
        "Low"
      ),
      viz = ""
    )
  names(df) <- c("Political Dissonance", "Political Alignment", "Estimate", "Pr(>|z|)", "95% CI Lower",  "95% CI Upper", " ")
  
  ## Kable
  t = kbl(
    df,
    booktabs = TRUE,
    format = "latex",
    digits = 3
  ) %>%
    kable_styling(full_width = FALSE) %>%
    column_spec(
      7,
      image = spec_pointrange(
        x=output$estimate, 
        xmin=output$conf.low, 
        xmax= output$conf.high
      ) 
    )
  writeLines(t, "tex/het_effects_dissonance.tex")
  
  return(TRUE)
}
