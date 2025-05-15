## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Exploratory Data Analysis
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     January 5th, 2025
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(glue)
library(haven)
library(estimatr)
library(fixest)
library(marginaleffects)
library(modelsummary)
library(tidyverse)

if (interactive()){
  source("src/config.R")
  source("src/data_loading.R")
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Data Loading ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

eugpp <- load_data(path2SP, "gpp")

dis_grounds <- names(
  eugpp %>% 
    select(starts_with("DIS_")) %>% 
    select(!starts_with("DIS_exp"))
)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Discrimination Incidence By Association Ground (dicg) ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
  output = "tables/tab_dicg.png",
  notes  = "*Note*: Table displays the percentage of respondents in each country that answered to have had experienced discrimination
  or harrasment for each of the grounds presented to them.",
  escape = TRUE
)
  

#### Bubble chart ----
fig_dicg_data <- tab_dicg %>%
  group_by(country) %>%
  summarise(
    across(
      everything(),
      \(x) round(mean(x, na.rm = TRUE)*100,1)
    )
  ) %>%
  pivot_longer(
    !country,
    names_to  = "ground",
    values_to = "incidence" 
  ) %>%
  mutate(
    country = factor(
      country,
      levels = rev(unique(eugpp$country_name_ltn))
    ),
    ground = case_when(
      ground == "DIS_sex"       ~ "Sex",
      ground == "DIS_ethni"     ~ "Ethnicity",
      ground == "DIS_migration" ~ "Migration\nStatus",
      ground == "DIS_ses"       ~ "Socioeconomic\nStatus",
      ground == "DIS_religion"  ~ "Religion",
      ground == "DIS_gender"    ~ "Gender",
      ground == "DIS_politics"  ~ "Political\nOpinion"
    )
  ) %>%
  filter(
    !is.na(ground)
  )

fig_dicg <- ggplot(
  data = fig_dicg_data,
  aes(
    x = ground,
    y = rev(country),
    size = incidence,
    color = incidence
  )
) +
  geom_point(
    show.legend = c(
      "size" = FALSE,
      "color" = TRUE
    )
  ) +
  labs(
    title = "Experiences of Discrimination",
    subtitle = "Incidence rate by discrimination ground"
  ) +
  theme_minimal() +
  scale_x_discrete(
    position = "top"
  ) +
  scale_color_gradient2(
    "% of respondents",
    low  = "#F5BFAE",
    mid  = "#EA7E5D",
    high = "#901C14",
    midpoint = 12
  ) +
  scale_size_continuous(
    range = c(1, 10)
  ) +
  theme(
    axis.title.x          = element_blank(),
    axis.title.y          = element_blank(),
    axis.text.x           =  element_text(face = "bold",
                                          hjust = 0.5),
    axis.text.y           =  element_text(face = "bold",
                                          hjust = 0),
    legend.position       = "bottom",
    legend.title          = element_text(face = "italic",
                                         hjust = 0.5),
    legend.title.position = "top",
    # legend.key.width      = unit(23, "mm"),
    panel.grid.major.x    = element_blank(),
    panel.grid.major.y    = element_blank(),
    plot.margin           = margin(2,0,0,0), 
    plot.title            = element_text(face = "bold"),
    plot.title.position   = "plot",
    plot.subtitle         = element_text(face = "italic")
  )

ggsave(
  plot     = fig_dicg,
  filename = "viz/fig_dicg.png",
  width    = 7,
  height   = 10,
  units    = "in"
)  


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Demographic Composition Of People Who Has Experienced Political Discrimination or Harrasment (dpod) ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

determinant_vars <- c(
  "female", "young", "rural", "fconst", "hedu", "ipol", "left", "right", "polid", 
  "minority", "employed", "married", "foreigner", "incpp"
)

dpod_data <- eugpp %>%
  select(
    country = country_name_ltn,
    poldis,
    all_of(determinant_vars)
  )

tab_dpod <- dpod_data %>%
  mutate(
    poldis = case_when(
      poldis == 1 ~ "D/H Experience",
      poldis == 0 ~ "No D/H Experience",
    )
  ) %>%
  select(
    poldis,
    `Female` = female,
    `Young (18-35)`        = young,
    `Rural Residence`      = rural,
    `Higher Education`     = hedu,
    `Fin. Constrained`     = fconst,
    `Employed`             = employed,
    `Married`              = married,
    `Foreigner`            = foreigner,
    `Interest in Politics` = ipol,
    `Left (Ideology)`      = left,
    `Right (Ideology)`     = right,
    `Inc. Party Follower`  = incpp,
    `Minority Group`       = minority
  )

datasummary_balance(
  ~poldis,
  data = tab_dpod,
  fmt = function(x) format(round(x*100,1), nsmall = 1),
  stars = TRUE,
  output = "tables/tab_dpod.png",
  notes  = c(
    "*Note*: Table displays the difference in proportions between people who answered to 
    have experienced discrimination or harrasment due to their political opinion and people who
    answered to not have experienced such events. *, **, and *** represent statistical 
    significance at p < 0.05, p < 0.01, and p < 0.001, respectively."
  ),
  align  = "lcccccc",
  escape = FALSE
)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Conditional Likelihood Of Demographic Traits On D/H Experiences (dpod) ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

soc_controls <- "female + young + fconst + hedu + rural + minority + employed + married + foreigner"
pol_controls <- "+ polid + incpp + ipol"

dpod_models <- list(
  "(I)" = paste0("poldis ~ ", soc_controls),
  "(II)" = paste0("poldis ~ ", soc_controls, pol_controls),
  "(III)" = paste0("poldis ~ ", soc_controls, pol_controls, " | country")
)

coef_rename_map = c(
  "female"   = "Gender (female)",
  "young"    = "Age (18-35)",
  "fconst"   = "Fin. Constrained",
  "hedu"     = "Higher Education",
  "rural"    = "Rural Residence",
  "minority" = "Minority Group",
  "employed" = "Employed",
  "married"  = "Married",
  "foreigner"= "Foreigner",
  "polid"    = "Political Ideology",
  "incpp"    = "Inc. Party Follower",
  "ipol"     = "Interest in Politics"
)

dpod_fit_models <- lapply(
  dpod_models,
  function(x){
    
    model_logit <- feglm(
      as.formula(x),
      cluster = ~country,
      family  = binomial(link = "logit"),
      data    = dpod_data
    )
    
    return(model_logit)
  }
)

log.lik <- function(model) {
  data.frame(
    "Log.Lik" = round(model$loglik, 1)
  )
}

modelsummary(
  dpod_fit_models,
  estimate     = "{estimate}{stars}",
  stars        = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  coef_omit    = 1,
  gof_omit     = "R2|RMSE",
  gof_function = log.lik,
  coef_rename  = coef_rename_map,
  output = "tables/tab_dpod_logit.png",
  notes  = c(
    "*Note*: Table displays the results of fitting a logistic regression on the probability of having experienced 
    political discrimination or harrasment (D/H) as a function of multiple social, economic, and political traits.
    Model (I) fits a logistic regression with a set of demographic covariates. Model (II) fits a logistic regression
    with a set of demographic and political covariates. Model (III) fits a logistic regression with a set of demographic 
    and political covariates along with country fixed effects. *, **, and *** represent statistical significance at 
    p < 0.05, p < 0.01, and p < 0.001, respectively. Standard Errors are clustered at the country level."
  ),
  escape = FALSE
)
