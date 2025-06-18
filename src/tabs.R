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
