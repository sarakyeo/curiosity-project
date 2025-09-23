
# Load packages -------------------

library(tidyverse)
# library(infer)
library(weights)
library(rstatix)
library(summarytools)
library(psych)
library(jtools)
library(interactions)
library(stargazer)
library(huxtable)
library(gtsummary)
library(kableExtra)
library(gridExtra)
library(gtsummary)
library(labelled)
library(srvyr)
library(tidyLPA)
library(factoextra)
library(FactoClass)


# Functions ----------------
var_recode <- function(vars, data){
        data |> 
                mutate(across({{vars}},
                              .fns = parse_number,
                              .names = "{.col}c"))
}

var_remove <- function(vars, data){
        data |> 
                mutate(across({{vars}},
                              .fns = ~replace(., 
                                              . > 10,
                                              NA_real_),
                              .names = "{.col}c"))
}


# Load data ---------------------------------------------------------------

pilot <- read_csv(here::here("data", "curiosity-2-data.csv"))
glimpse(pilot)


# Filtering out survey questionnaire tests --------------------------------

pilot |> freq(Q30_1)
pilot <- pilot |> 
        filter(is.na(Q30_1) == FALSE) # remove NAs for names from sample

pilot |> freq(Q30_2)
pilot <- pilot |> 
        filter(Q30_2 != "ff" &
                       Q30_2 != "kkkklknljlnl" &
                       Q30_2 != "Mike") # removed 3 obs

pilot |> freq(StartDate)



