## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Data Loading Script
## Author(s):         Carlos Toruno   (ctoruno@worldjusticeproject.org)
## Creation date:     May 7th, 2025
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(glue)
library(haven)
library(tidyverse)

load_data <- function(path2EU, source){
  
  if (!(source %in% c("gpp"))){
    stop("'source' argument should be one of: 'gpp', ... ")
  }
  
  if (source == "gpp"){
    path2data <- file.path(
      path2EU,
      "eu-gpp", "1. Data", "3. Merge", "EU_GPP_2024.dta"
    )
    
    # GPP Cleaning and Feature Engineering
    if (file.exists(path2data)){
      
      df <- read_stata(path2data) %>%
        mutate(
          # Control Variables
          female = case_when(
            gend == 2 ~ 1,
            gend == 1 ~ 0,
          ),
          young = case_when(
            age >= 18 & age <= 34 ~ 1,
            age >= 35 ~ 0,
            age %in% c(1,2) ~ 1,    # Luxembourg
            age %in% c(3,4,5,6) ~ 0 # Luxembourg
          ),
          fconst = case_when(
            fin %in% c(1,2) ~ 1,
            fin %in% c(3,4,98) ~ 0
          ),
          hedu = case_when(
            edu %in% c(5,6) ~ 1,
            edu %in% c(1,2,3,4,7,98) ~ 0
          ),
          ipol = case_when(
            politics %in% c(1,2) ~ 1,
            politics %in% c(3,4,98) ~ 0
          ),
          left = case_when(
            polid %in% c(0,1,2,3) ~ 1,
            polid %in% c(4,5,6,7,8,9,10,98) ~ 0,
          ),
          right = case_when(
            polid %in% c(7,8,9,10) ~ 1,
            polid %in% c(0,1,2,3,4,5,6,7,98) ~ 0,
          ),
          rural = case_when(
            urban == 1 ~ 0,
            urban == 2 ~ 1
          ),
          minority = case_when(
            ethni_groups == 1 ~ 0,
            ethni_groups == 0 ~ 1
          ),
          employed = case_when(
            emp %in% c(1,2) ~ 1,
            emp %in% c(3,4,5,6,7,8,98) ~ 0,
          ),
          married = case_when(
            marital %in% c(2,3) ~ 1,
            marital %in% c(1,4,5,98) ~ 0
          ),
          foreigner = case_when(
            nation == 1  ~ 0,
            nation == 2  ~ 1,
            nation == 98 ~ 0,
          ),
          
          # Experiences of Political D/H
          poldis = case_when(
            DIS_politics == 1  ~ 1,
            DIS_politics == 2  ~ 0,
            DIS_politics == 98 ~ 0
          )
        )
      
      
    } else {
      stop(glue("File '{path2data}' does not exist."))
    }
  }
  
  return(df)
}