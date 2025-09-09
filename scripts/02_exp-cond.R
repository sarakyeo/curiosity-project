
# Creating experimental conditions ----------------------------------------
## Control
pilot |> freq(`Q14_Page Submit`) # 18 in control condition

## No question, no curiosity prime
pilot |> freq(`Q17_Page Submit`) # 21

## Question, no curiosity prime
pilot |> freq(`Q20_Page Submit`) # 21

## Question, curiosity prime
pilot |> freq(`Q23_Page Submit`) # 20

pilot <- pilot |> 
        mutate(stim = case_when(
                `Q14_Page Submit` == `Q14_Page Submit` ~ "Control",
                `Q17_Page Submit` == `Q17_Page Submit` ~ "No question, no curiosity prime",
                `Q20_Page Submit` == `Q20_Page Submit` ~ "Question, no curiosity prime",
                `Q23_Page Submit` == `Q23_Page Submit` ~ "Question, curiosity prime"
        )) |> 
        mutate(stim = factor(stim,
                             levels = c("Control",
                                        "No question, no curiosity prime",
                                        "Question, no curiosity prime",
                                        "Question, curiosity prime")))

pilot |> freq(stim)

pilot |> freq(Q24)


# Examine manipulation check (Q24; presence of question) ------------------
tbl <- table(pilot$Q24, pilot$stim)
tbl

chisq_test(tbl)


# Combine conditions ------------------------------------------------------
## Question vs. no question
pilot <- pilot |> 
        mutate(stimQ = case_when(stim == "Control" ~ "No question",
                                 stim == "No question, no curiosity prime" ~ "No question",
                                 stim == "Question, no curiosity prime" ~ "Question",
                                 stim == "Question, curiosity prime" ~ "Question"))
pilot |> freq(stimQ) # no question = 84, question = 81


## Curiosity prime vs. no curiosity prime
pilot <- pilot |> 
        mutate(stimC = case_when(stim == "Control" ~ "No curiosity prime",
                                 stim == "No question, no curiosity prime" ~ "No curiosity prime",
                                 stim == "Question, no curiosity prime" ~ "No curiosity prime",
                                 stim == "Question, curiosity prime" ~ "Curiosity prime"))
pilot |> freq(stimC) # 39 in prime condition, 126 in no prime condition
