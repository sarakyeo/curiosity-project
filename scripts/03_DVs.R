
# Situational curiosity ---------------------------------------------------
pilot |>
        select(Q25_1:Q25_6, Q46_1:Q46_6) |> 
        freq()

pilot <- pilot |> 
        mutate(across(c(Q25_1:Q25_6, Q46_1:Q46_6),
                      ~case_when(. == "Strongly disagree" ~ 1,
                                 . == "Disagree" ~ 2,
                                 . == "Somewhat disagree" ~ 3,
                                 . == "Neither agree nor disagree" ~ 4,
                                 . == "Somewhat agree" ~ 5,
                                 . == "Agree" ~ 6,
                                 . == "Strongly agree" ~ 7),
                      .names = "{.col}c"))

pilot |> 
        select(Q25_1c:Q25_6c, Q46_1c:Q46_6c) |> 
        freq()

## Astronomy ----------------
pilot |> 
        select(Q25_1c, Q25_2c, Q25_3c, Q25_4c, Q25_5c, Q25_6c) |> 
        KMO() # Overall MSA = .88

pilot |> 
        select(Q25_1c, Q25_2c, Q25_3c, Q25_4c, Q25_5c, Q25_6c) |> 
        cortest.bartlett() # sig.

pilot |> 
        select(Q25_1c, Q25_2c, Q25_3c, Q25_4c, Q25_5c, Q25_6c) |> 
        fa.parallel() # 1 factor, 1 component

pilot |>
        select(Q25_1c, Q25_2c, Q25_3c, Q25_4c, Q25_5c, Q25_6c) |>
        fa(
                .,
                nfactors = 1,
                fm = "pa",
                max.iter = 100,
                rotate = "promax"
        ) |> 
        fa.diagram() # item 5 loads poorly, remove from index (but see note from line 86)

pilot |> 
        select(Q25_1c, Q25_2c, Q25_3c, Q25_4c) |> 
        alpha() # Cronbach's alpha = .92

pilot <- pilot |> 
        rowwise() |> 
        mutate(acurious = mean(
                c(Q25_1c, Q25_2c, Q25_3c, Q25_4c),
                na.rm = TRUE)
        )

pilot |> freq(acurious)
pilot |> 
        group_by() |>
        descr(acurious) # M = 4.20, SD = 1.48


## Rain ----------------
pilot |> 
        select(Q46_1c, Q46_2c, Q46_3c, Q46_4c, Q46_5c, Q46_6c) |> 
        KMO() # Overall MSA = .84

pilot |> 
        select(Q46_1c, Q46_2c, Q46_3c, Q46_4c, Q46_5c, Q46_6c) |> 
        cortest.bartlett() # sig.

pilot |> 
        select(Q46_1c, Q46_2c, Q46_3c, Q46_4c, Q46_5c, Q46_6c) |> 
        fa.parallel() # 2 factors, 1 component

pilot |>
        select(Q46_1c, Q46_2c, Q46_3c, Q46_4c, Q46_5c, Q46_6c) |> 
        fa(
                .,
                nfactors = 1,
                fm = "pa",
                max.iter = 100,
                rotate = "promax"
        ) |> 
        fa.diagram() # items 5 and 6 load poorly (probably due to topic, i.e., geosmin), remove from index
# this probably means I should remove item 5 from the acurious index as well; went back up and fixed it

pilot |> 
        select(Q46_1c, Q46_2c, Q46_3c, Q46_4c) |> 
        alpha() # Cronbach's alpha = .93

pilot <- pilot |> 
        rowwise() |> 
        mutate(rcurious = mean(
                c(Q46_1c, Q46_2c, Q46_3c, Q46_4c),
                na.rm = TRUE)
        )

pilot |> freq(rcurious)
pilot |> 
        group_by() |>
        descr(rcurious) # M = 4.57, SD = 1.45


# Information Seeking -------------------------
pilot |> 
        select(Q26_1:Q26_5, Q47_1:Q47_5) |> 
        freq()

pilot <- pilot |> 
        mutate(across(c(Q26_1:Q26_5, Q47_1:Q47_5),
                      ~case_when(. == "Extremely unlikely" ~ 1,
                                 . == "Moderately unlikely" ~ 2,
                                 . == "Slightly unlikely" ~ 3,
                                 . == "Neither likely nor unlikely" ~ 4,
                                 . == "Slightly likely" ~ 5,
                                 . == "Moderately likely" ~ 6,
                                 . == "Extremely likely" ~ 7),
                      .names = "{.col}c"))

pilot |> 
        select(Q26_1c:Q26_5c, Q47_1c:Q47_5c) |> 
        freq()


## Astronomy ----------------------
pilot |> 
        select(Q26_1c:Q26_5c) |> 
        KMO() # Overall MSA = .87

pilot |> 
        select(Q26_1c:Q26_5c) |> 
        cortest.bartlett() # sig.

pilot |> 
        select(Q26_1c:Q26_5c) |> 
        fa.parallel() # 1 factor, 1 component

pilot |>
        select(Q26_1c:Q26_5c) |>
        fa(
                .,
                nfactors = 1,
                fm = "pa",
                max.iter = 100,
                rotate = "promax"
        ) |> 
        fa.diagram()

pilot <- pilot |> 
        mutate(ainfoseek = mean(
                c(Q26_1c, Q26_2c, Q26_3c, Q26_4c, Q26_5c),
                na.rm = TRUE
        ))

pilot |> freq(ainfoseek)
pilot |>
        group_by() |> 
        descr(ainfoseek) # M = 3.11, SD = 1.58


## Rain ----------------------
pilot |> 
        select(Q47_1c:Q47_5c) |> 
        KMO() # Overall MSA = .83

pilot |> 
        select(Q47_1c:Q47_5c) |> 
        cortest.bartlett() # sig.

pilot |> 
        select(Q47_1c:Q47_5c) |> 
        fa.parallel() # 1 factor, 1 component

pilot |>
        select(Q47_1c:Q47_5c) |>
        fa(
                .,
                nfactors = 1,
                fm = "pa",
                max.iter = 100,
                rotate = "promax"
        ) |> 
        fa.diagram()

pilot <- pilot |> 
        mutate(rinfoseek = mean(
                c(Q47_1c, Q47_2c, Q47_3c, Q47_4c, Q47_5c),
                na.rm = TRUE
        ))

pilot |> freq(rinfoseek)
pilot |>
        group_by() |> 
        descr(rinfoseek) # M = 3.26, SD = 1.51


# Provided closure ----------------------------
pilot |> 
        select(Q27_3, Q27_4, Q48_3, Q48_4) |> 
        freq()

pilot <- pilot |> 
        mutate(across(c(Q27_3, Q27_4, Q48_3, Q48_4),
                      ~case_when(. == "Strongly disagree" ~ 1,
                                 . == "Disagree" ~ 2,
                                 . == "Somewhat disagree" ~ 3,
                                 . == "Neither agree nor disagree" ~ 4,
                                 . == "Somewhat agree" ~ 5,
                                 . == "Agree" ~ 6,
                                 . == "Strongly agree" ~ 7),
                      .names = "{.col}c"))

pilot |> 
        select(Q27_3c, Q27_4c, Q48_3c, Q48_4c) |> 
        freq()

## Astronomy --------------------------
pilot |> 
        select(Q27_3c, Q27_4c) |> 
        cor_test() # Pearson's r = .74, p < .001

pilot <- pilot |> 
        mutate(
                aclosure = mean(
                        c(Q27_3c, Q27_4c),
                        na.rm = TRUE
                )
        )
pilot |> freq(aclosure)
pilot |> 
        group_by() |> 
        descr(aclosure) # M = 2.81, SD = 1.50

## Rain --------------------
pilot |> 
        select(Q48_3c, Q48_4c) |> 
        cor_test() # Pearson's r = .78, p < .001

pilot <- pilot |> 
        mutate(
                rclosure = mean(
                        c(Q48_3c, Q48_4c),
                        na.rm = TRUE
                )
        )
pilot |> freq(rclosure)
pilot |> 
        group_by() |> 
        descr(rclosure) # M = 3.33, SD = 1.69
