
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

pilot <- read_csv(here::here("data", "curiosity-data-F25.csv"))
glimpse(pilot)


# Filtering out those who did not consent to participate --------------------------------

pilot |> freq(Q2) # 2 R (respondents) did not consent
pilot <- pilot |> 
        filter(Q2 == "Yes") # keep those who responded "Yes" to Q2 in data frame; 265 R remaining

# Filtering out those who do not live in the US -----------

pilot |> freq(Q3) # 263 R reside in
pilot <- pilot |> 
        filter(Q3 == "Yes") # removed 2 obs; 263 R remaining


# Check R age ----------------
pilot |> freq(Q5)
pilot <- pilot |> 
        rowwise() |> 
        mutate(age = 2025 - Q5)
pilot |> freq(age)
pilot |>
        group_by() |> 
        descr(age) # M = 21.0, SD = 3.62; min = 18


# Demographics --------------------
## Race ----------
pilot |> freq(Q8_5) # 230 R identify as White
pilot <- pilot |>
        mutate(
                white = case_when(
                        Q8_5 == "White" ~ "White",
                        is.na(Q8_5) == TRUE ~ "non-White"
                )) |> 
        mutate(
                white = factor(
                        white,
                        levels = c("non-White", "White")
                ))
pilot |> freq(white) # 87.5 % White


# Check student names ---------------------------------
# Need to get rid of test responses that I know are either Aidan's or Casey's responses
pilot |> freq(Q52_1)
pilot <- pilot |>
        filter(Q52_1 != "aidan craig sundine" | is.na(Q52_1)) |>
        filter(Q52_1 != "Aidan Craig Sundine" | is.na(Q52_1)) # 260 R remaining



