
# Mediator: State/Situational curiosity ---------------------------------------------------
cdata |> 
        select(Q27_1:Q27_2) |> 
        freq()
cdata <- var_recode(
        data = cdata,
        vars = c(Q27_1:Q27_2)
        )
cdata |> 
        select(Q27_1c:Q27_2c) |> 
        freq()
cdata |> 
        select(Q27_1c:Q27_2c) |> 
        cor_test()  # Pearson's r = 0.79, p < .001
cdata <- cdata |> 
        rowwise() |> 
        mutate(curiosity = mean(
                c(Q27_1c, Q27_2c),
                na.rm = TRUE
        ))
cdata |> freq(curiosity)
cdata |> 
        group_by() |> 
        descr(curiosity, weights = cdata$wtvar) # M = 5.49, SD = 1.50


# Dialogue (Q29_6, Q29_7) ---------------------------------
cdata |> 
        select(Q29_6:Q29_13) |> 
        freq()
cdata <- var_recode(data = cdata, vars = c(Q29_6:Q29_13))
cdata |> 
        select(Q29_6c:Q29_13c) |> 
        freq()

## Factor analysis ----------------------------
cdata |> 
        select(Q29_6c:Q29_13c) |> 
        cortest.bartlett() # sig.
cdata |> 
        select(Q29_6c:Q29_13c) |> 
        KMO() # Overall MSA = .96
cdata |> 
        select(Q29_6c:Q29_13c) |> 
        fa.parallel() # 1 factor, 1 component
fa <- cdata |>
        select(Q29_6c:Q29_13c) |>
        fa(
                .,
                nfactors = 1,
                fm = "pa",
                max.iter = 100,
                rotate = "promax"
        )
fa |> fa.diagram()
print(fa$loadings, cutoff = .3, digits = 3)

## Cronbach's alpha ----------------------
cdata |> 
        select(Q29_6c:Q29_13c) |>
        group_by() |> 
        psych::alpha() # Cronbach's alpha = 0.96

cdata <- cdata |>
        rowwise() |>
        mutate(
                dialogue = mean(
                        c(
                                Q29_6c,
                                Q29_7c,
                                Q29_8c,
                                Q29_9c,
                                Q29_10c,
                                Q29_12c,
                                Q29_13c
                        ),
                        na.rm = TRUE
                ))
cdata |> freq(dialogue)
cdata |> 
        group_by() |> 
        descr(dialogue) # M = 4.66, SD = 1.77

