
# Situational curiosity ---------------------------------------------------
pilot |> 
        select(Q25_1:Q25_4) |> 
        freq()

pilot <- pilot |> 
        mutate(across(c(Q25_1:Q25_4),
                      ~case_when(. == "Not at all" ~ 1,
                                 . == "2" ~ 2,
                                 . == "3" ~ 3,
                                 . == "Somewhat" ~ 4,
                                 . == "5" ~ 5,
                                 . == "6" ~ 6,
                                 . == "Very much" ~ 7),
                      .names = "{.col}c"))

pilot |> 
        select(Q25_1c:Q25_4c) |> 
        freq()

pilot |> 
        select(Q25_1c:Q25_4c) |> 
        alpha() # Cronbach's alpha = .95

pilot <- pilot |> 
        rowwise() |> 
        mutate(curious = mean(
                c(Q25_1c, Q25_2c, Q25_3c, Q25_4c),
                na.rm = TRUE)
        )

pilot |> freq(curious)
pilot |> 
        group_by() |> 
        summarize(mean(curious, na.rm = TRUE),
                  sd(curious, na.rm = TRUE),
                  n_obs = n())
# M = 3.65, SD = 1.65

pilot |> 
        ggplot(aes(x = curious)) +
        geom_histogram(bins = 7)
