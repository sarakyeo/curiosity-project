
# Creating experimental conditions ----------------------------------------
## Issue: Rain, Astro
## Astronomy experimental conditions: Q14, Q17, Q20, Q23 ----------------------
pilot |> freq(`Q14_Page Submit`)
pilot |> freq(`Q17_Page Submit`)
pilot |> freq(`Q20_Page Submit`)
pilot |> freq(`Q23_Page Submit`)

pilot <- pilot |> 
        mutate(astim = case_when(
                `Q14_Page Submit` == `Q14_Page Submit` ~ "No curious, no resolution",
                `Q17_Page Submit` == `Q17_Page Submit` ~ "No curious, resolution",
                `Q20_Page Submit` == `Q20_Page Submit` ~ "Curious, no resolution",
                `Q23_Page Submit` == `Q23_Page Submit` ~ "Curious, resolution",
        )) |> 
        mutate(astim = factor(
                astim,
                levels = c(
                        "No curious, resolution",
                        "No curious, no resolution",
                        "Curious, resolution",
                        "Curious, no resolution"
                )
        ))
pilot |> freq(astim)

#                                   Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
# ------------------------------- ------ --------- -------------- --------- --------------
#          No curious, resolution     63     26.25          26.25     24.23          24.23
#       No curious, no resolution     60     25.00          51.25     23.08          47.31
#             Curious, resolution     61     25.42          76.67     23.46          70.77
#          Curious, no resolution     56     23.33         100.00     21.54          92.31
#                            <NA>     20                               7.69         100.00
#                           Total    260    100.00         100.00    100.00         100.00


## Rain experimental conditions: Q35, Q38, Q41, Q44 ---------------
pilot |> freq(`Q35_Page Submit`)
pilot |> freq(`Q38_Page Submit`)
pilot |> freq(`Q41_Page Submit`)
pilot |> freq(`Q44_Page Submit`)

pilot <- pilot |> 
        mutate(rstim = case_when(
                `Q35_Page Submit` == `Q35_Page Submit` ~ "Curious, resolution",
                `Q38_Page Submit` == `Q38_Page Submit` ~ "No curious, resolution",
                `Q41_Page Submit` == `Q41_Page Submit` ~ "Curious, no resolution",
                `Q44_Page Submit` == `Q44_Page Submit` ~ "No curious, no resolution",
        )) |> 
        mutate(rstim = factor(
                rstim,
                levels = c(
                        "No curious, resolution",
                        "No curious, no resolution",
                        "Curious, resolution",
                        "Curious, no resolution"
                )
        ))
pilot |> freq(rstim)

                                #  Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
# ------------------------------- ------ --------- -------------- --------- --------------
#          No curious, resolution     60     24.90          24.90     23.08          23.08
#       No curious, no resolution     60     24.90          49.79     23.08          46.15
#             Curious, resolution     62     25.73          75.52     23.85          70.00
#          Curious, no resolution     59     24.48         100.00     22.69          92.69
#                            <NA>     19                               7.31         100.00
#                           Total    260    100.00         100.00    100.00         100.00


# Curious vs. no curious condition ------------------------
## Astronomy -------------
pilot <- pilot |> 
        mutate(
                acurious = case_when(
                        astim == "Curious, no resolution" ~ "Curious",
                        astim == "Curious, resolution" ~ "Curious",
                        astim == "No curious, no resolution" ~ "No curious",
                        astim == "No curious, resolution" ~ "No curious"
                )) |> 
        mutate(acurious = factor(
                acurious,
                levels = c(
                        "No curious",
                        "Curious"
                )
        ))
pilot |> freq(acurious)

#                    Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
# ---------------- ------ --------- -------------- --------- --------------
#       No curious    123     51.25          51.25     47.31          47.31
#          Curious    117     48.75         100.00     45.00          92.31
#             <NA>     20                               7.69         100.00
#            Total    260    100.00         100.00    100.00         100.00

## Rain ---------------------
pilot <- pilot |> 
        mutate(
                rcurious = case_when(
                        rstim == "Curious, no resolution" ~ "Curious",
                        rstim == "Curious, resolution" ~ "Curious",
                        rstim == "No curious, no resolution" ~ "No curious",
                        rstim == "No curious, resolution" ~ "No curious"
                )) |> 
        mutate(rcurious = factor(
                rcurious,
                levels = c(
                        "No curious",
                        "Curious"
                )
        ))
pilot |> freq(rcurious)

#                    Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
# ---------------- ------ --------- -------------- --------- --------------
#       No curious    120     49.79          49.79     46.15          46.15
#          Curious    121     50.21         100.00     46.54          92.69
#             <NA>     19                               7.31         100.00
#            Total    260    100.00         100.00    100.00         100.00


# Resolution vs. no resolution condition ------------------------
## Astronomy -------------
pilot <- pilot |> 
        mutate(
                areso = case_when(
                        astim == "Curious, no resolution" ~ "No resolution",
                        astim == "Curious, resolution" ~ "Resolution",
                        astim == "No curious, no resolution" ~ "No resolution",
                        astim == "No curious, resolution" ~ "Resolution"
                )) |> 
        mutate(areso = factor(
                areso,
                levels = c(
                        "No resolution",
                        "Resolution"
                )
        ))
pilot |> freq(areso)

#                       Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
# ------------------- ------ --------- -------------- --------- --------------
#       No resolution    116     48.33          48.33     44.62          44.62
#          Resolution    124     51.67         100.00     47.69          92.31
#                <NA>     20                               7.69         100.00
#               Total    260    100.00         100.00    100.00         100.00

## Rain ------------------
## Astronomy -------------
pilot <- pilot |> 
        mutate(
                rreso = case_when(
                        rstim == "Curious, no resolution" ~ "No resolution",
                        rstim == "Curious, resolution" ~ "Resolution",
                        rstim == "No curious, no resolution" ~ "No resolution",
                        rstim == "No curious, resolution" ~ "Resolution"
                )) |> 
        mutate(rreso = factor(
                rreso,
                levels = c(
                        "No resolution",
                        "Resolution"
                )
        ))
pilot |> freq(rreso)

#                       Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
# ------------------- ------ --------- -------------- --------- --------------
#       No resolution    119     49.38          49.38     45.77          45.77
#          Resolution    122     50.62         100.00     46.92          92.69
#                <NA>     19                               7.31         100.00
#               Total    260    100.00         100.00    100.00         100.00