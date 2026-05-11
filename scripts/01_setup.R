
# Load packages -------------------

library(tidyverse)
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

cdata <- read_csv(here::here("data", "curiosity-final-data_April-2026.csv"))
glimpse(cdata) # 1,002 rows, 134 columns


# Filtering out those who did not consent to participate --------------------------------

cdata |> freq(Q2) # 1,000 consented
cdata <- cdata |> 
        filter(Q2 == "Yes") # keep those who responded "Yes" to Q2 in data frame; 1,000 R remaining

# Filtering out those who do not live in the US -----------

cdata |> freq(Q3) # 1,000 R reside in the US; no need to filter


# Check R age ----------------
cdata |> freq(Q5)
cdata <- cdata |> 
        mutate(Q5n = as.numeric(Q5))
cdata |> freq(Q5n)
cdata <- cdata |> 
        rowwise() |> 
        mutate(age = 2026 - Q5n)
cdata |> freq(age) # 1 missing data point
cdata |>
        group_by() |> 
        descr(age) # M = 47.3, SD = 17.2; min = 19; max = 86


# Demographics --------------------
## Gender ---------------
cdata |> freq(Q6) # 520 (52%) female, 480 (48%) male
cdata <- cdata |> 
  mutate(
        female = factor(
                Q6,
                levels = c("Female", "Male")
        ))
cdata |> freq(female)

## Education --------------
cdata |> freq(Q7)
cdata <- cdata |> 
  mutate(
        educ = factor(
                Q7,
                levels = c(
                        "Less than high school",
                        "High school graduate",
                        "Some college",
                        "2 year degree (e.g., associate degree)",
                        "4 year college degree (e.g., bachelor's degree)",
                        "Advanced degree (e.g., graduate school, JD, MD, PhD)"
                )
        ))
cdata |> freq(educ)
cdata <- cdata |> 
  mutate(educn = as.numeric(educ))
cdata |> freq(educn)
cdata |> 
  group_by() |> 
  descr(educn) # Median = "2 year degree (e.g., associate degree)"

## Race and Ethnicity --------------
cdata |> freq(Q9) # 355 Hispanic
cdata <- cdata |> 
  mutate(Hispanic = case_when(
        Q9 == "Yes" ~ "Hispanic",
        Q9 == "No" ~ "Non-Hispanic"
  )) |> 
  mutate(
        Hispanic = factor(
                Hispanic,
                levels = c("Non-Hispanic", "Hispanic")
        ))
cdata |> freq(Hispanic) # 355 Hispanic (35.5%)

cdata |> freq(Q10)
cdata <- cdata |> 
  mutate(
        White = case_when(
                Q10 == "White" ~ "White",
                Q10 != "White" ~ "Non-White"
        )) |> 
  mutate(White = factor(
        White,
        levels = c("Non-White", "White")
  ))
cdata |> freq(White) # 544 White (54.4%)

cdata <- cdata |> 
  mutate(
        Black = case_when(
                Q10 == "Black or African American" ~ "Black",
                Q10 != "Black or African American" ~ "Non-Black"
        )) |> 
  mutate(
        Black = factor(
                Black,
                levels = c("Non-Black", "Black")
        ))
cdata |> freq(Black) # 338 Black (33.8%)


## Household income ------------------
cdata |> freq(Q11)
cdata <- cdata |> 
  mutate(
        HHinc = case_when(
                Q11 == "Less than $10,000" ~ 1,
                Q11 == "$10,000 to $19,999" ~ 2,
                Q11 == "$20,000 to $29,999" ~ 3,
                Q11 == "$30,000 to $39,999" ~ 4,
                Q11 == "$40,000 to $49,999" ~ 5,
                Q11 == "$50,000 to $59,999" ~ 6,
                Q11 == "$60,000 to $69,999" ~ 7,
                Q11 == "$70,000 to $79,999" ~ 8,
                Q11 == "$80,000 to $89,999" ~ 9,
                Q11 == "$90,000 to $99,999" ~ 10,
                Q11 == "$100,000 to $149,999" ~ 11,
                Q11 == "$150,000 or more" ~ 12,
        ))
cdata |> freq(HHinc)
cdata |> 
  group_by() |> 
  descr(HHinc) # Median = "$50,000 to $59,999"
