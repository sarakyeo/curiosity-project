
# Creating experimental conditions ----------------------------------------
## All conditions ----------------
cdata |> 
        select(
                `Q20_Page Submit`,
                `Q22_Page Submit`,
                `Q24_Page Submit`,
                `Q26_Page Submit`
        ) |> 
        freq()

cdata <- cdata |> 
        mutate(
                stim = case_when(
                        `Q20_Page Submit` == `Q20_Page Submit` ~ "No curiosity, Resolution",
                        `Q22_Page Submit` == `Q22_Page Submit` ~ "No curiosity, No resolution",
                        `Q24_Page Submit` == `Q24_Page Submit` ~ "Curiosity, Resolution",
                        `Q26_Page Submit` == `Q26_Page Submit` ~ "Curiosity, No resolution",
                )) |> 
        mutate(
                stim = factor(
                        stim,
                        levels = c(
                                "No curiosity, Resolution",
                                "No curiosity, No resolution",
                                "Curiosity, Resolution",
                                "Curiosity, No resolution"               
                        )
                ))
cdata |> freq(stim)
#                                     Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
# --------------------------------- ------ --------- -------------- --------- --------------
#          No curiosity, Resolution    251     25.10          25.10     25.10          25.10
#       No curiosity, No resolution    262     26.20          51.30     26.20          51.30
#             Curiosity, Resolution    256     25.60          76.90     25.60          76.90
#          Curiosity, No resolution    231     23.10         100.00     23.10         100.00
#                              <NA>      0                               0.00         100.00
#                             Total   1000    100.00         100.00    100.00         100.00


## Curiosity priming conditions --------------
cdata <- cdata |> 
        mutate(
                cstim = case_when(
                        stim == "No curiosity, Resolution" ~ "No curiosity",
                        stim == "No curiosity, No resolution" ~ "No curiosity",
                        stim == "Curiosity, Resolution" ~ "Curiosity",
                        stim == "Curiosity, No resolution" ~ "Curiosity"
                )) |> 
        mutate(
                cstim = factor(
                        cstim,
                        levels = c("No curiosity", "Curiosity")
                ))
cdata |> freq(cstim)
#                      Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
# ------------------ ------ --------- -------------- --------- --------------
#       No curiosity    513     51.30          51.30     51.30          51.30
#          Curiosity    487     48.70         100.00     48.70         100.00
#               <NA>      0                               0.00         100.00
#              Total   1000    100.00         100.00    100.00         100.00


## Resolution conditions ------------------
cdata <- cdata |> 
        mutate(
                rstim = case_when(
                        stim == "No curiosity, Resolution" ~ "Resolution",
                        stim == "No curiosity, No resolution" ~ "No resolution",
                        stim == "Curiosity, Resolution" ~ "Resolution",
                        stim == "Curiosity, No resolution" ~ "No resolution"
                )) |> 
        mutate(
                rstim = factor(
                        rstim,
                        levels = c("No resolution", "Resolution")
                ))
cdata |> freq(rstim)
#                       Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
# ------------------- ------ --------- -------------- --------- --------------
#       No resolution    493     49.30          49.30     49.30          49.30
#          Resolution    507     50.70         100.00     50.70         100.00
#                <NA>      0                               0.00         100.00
#               Total   1000    100.00         100.00    100.00         100.00


# Check DV assignment ---------------------
cdata |> freq(`2nd_order`) # 497 assigned to DV set 1; 503 assigned to DV set 2
cdata |> freq(Q29_14) # 497 in DV 1 
cdata |> freq(Q30_6) # 503 in DV 2

cdata <- cdata |> 
        mutate(
                DVset = case_when(
                        Q29_14 == Q29_14 ~ "DV set 1: Info Seeking",
                        Q30_6 == Q30_6 ~ "DV set 2: Risks/Benefits, Support"
                )) |> 
        mutate(
                DVset = factor(
                        DVset,
                        levels = c("DV set 1: Info Seeking", "DV set 2: Risks/Benefits, Support")
                ))
cdata |> freq(DVset)