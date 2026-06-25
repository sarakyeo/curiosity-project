
# Mediator: State/Situational curiosity ---------------------------------------------------
cdata |> 
        select(Q27_1:Q27_4) |> 
        freq()
cdata <- var_recode(
        data = cdata,
        vars = c(Q27_1:Q27_4)
        )
cdata |> 
        select(Q27_1c:Q27_4c) |> 
        freq()

## Factor analysis -----------------
cdata |> 
        select(Q27_1c, Q27_2c, Q27_3c, Q27_4c) |> 
        cortest.bartlett() # sig.
cdata |> 
        select(Q27_1c, Q27_2c, Q27_3c, Q27_4c) |> 
        KMO() # Overall MSA = .87
cdata |> 
        select(Q27_1c, Q27_2c, Q27_3c, Q27_4c) |> 
        fa.parallel() # 1 factor, 1 component
fa <- cdata |> 
        select(Q27_1c, Q27_2c, Q27_3c, Q27_4c) |> 
        fa(
                .,
                nfactors = 1,
                fm = "pa",
                max.iter = 100,
                rotate = "promax"
        )
fa |> fa.diagram()
print(fa$loadings, cutoff = .3, digits = 3)

cdata <- cdata |> 
        rowwise() |> 
        mutate(
                curiosity4 = mean(
                        c(Q27_1c, Q27_2c, Q27_3c, Q27_4c),
                        na.rm = TRUE
                ))
cdata |> freq(curiosity4)
cdata |> group_by() |> 
        descr(curiosity4, weights = cdata$wtvar) # M = 5.51, SD = 1.44

cdata |> 
        cor_test(Q27_1c, Q27_4c) # Pearson's r = .78, p < .001
cdata <- cdata |>
        rowwise() |>
        mutate(
                curiosity2 = mean(c(Q27_1c, Q27_4c), na.rm = TRUE)
        )
cdata |> freq(curiosity2)
cdata |> 
        group_by() |> 
        descr(curiosity2, weights = cdata$wtvar) # M = 5.52, SD = 1.48


# DV: Dialogue (Q29_6:Q29_13) ---------------------------------
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
        descr(dialogue, weights = cdata$wtvar) # M = 4.52, SD = 1.77


# DV: Seek information (Q29_1:Q29_6) ---------------------------------
cdata |> 
        select(Q29_1:Q29_6) |> 
        freq()
cdata <- var_recode(
        data = cdata,
        vars = c(Q29_1, Q29_2, Q29_3, Q29_4, Q29_5, Q29_6)
)
cdata |> 
        select(Q29_1c, Q29_2c, Q29_3c, Q29_4c, Q29_5c, Q29_6c) |> 
        freq()

## Factor analysis -----
cdata |> 
        select(Q29_1c, Q29_2c, Q29_3c, Q29_4c, Q29_5c, Q29_6c) |> 
        cortest.bartlett() # sig.
cdata |> 
        select(Q29_1c, Q29_2c, Q29_3c, Q29_4c, Q29_5c, Q29_6c) |> 
        KMO() # Overall MSA = .92
cdata |> 
        select(Q29_1c, Q29_2c, Q29_3c, Q29_4c, Q29_5c, Q29_6c) |> 
        fa.parallel() # 1 factor, 1 component
fa <- cdata |> 
        select(Q29_1c, Q29_2c, Q29_3c, Q29_4c, Q29_5c, Q29_6c) |> 
        fa(
                .,
                nfactors = 1,
                fm = "pa",
                max.iter = 100,
                rotate = "promax"
        )
fa |> fa.diagram()
print(fa$loadings, cutoff = .3, digits = 3)

## Cronbach's alpha ---------
cdata |> 
        select(Q29_1c, Q29_2c, Q29_3c, Q29_4c, Q29_5c, Q29_6c) |> 
        group_by() |> 
        psych::alpha() # Cronbach's alpha = .94

cdata <- cdata |> 
        rowwise() |> 
        mutate(
                infoseek = mean(
                        c(Q29_1c, Q29_2c, Q29_3c, Q29_4c, Q29_5c, Q29_6c),
                        na.rm = TRUE
                ))
cdata |> freq(infoseek)
cdata |> 
        group_by() |> 
        descr(infoseek, weights = cdata$wtvar) # M = 4.53, SD = 1.81

# Create interest as control variable --------------------
cdata |> freq(Q12_5)
cdata <- var_recode(data = cdata, vars = Q12_5)
cdata |> freq(Q12_5c)
cdata <- cdata |> 
        mutate(interest = Q12_5c)
cdata |> freq(interest)
cdata |>
        group_by() |>
        descr(interest, weights = cdata$wtvar) # M = 4.53, SD = 1.94


# Create trait curiosity as control variable -------------------
cdata |> 
        select(Q14_1:Q14_4) |> 
        freq()
cdata <- var_recode(data = cdata, vars = Q14_1:Q14_4)
cdata |> 
        select(Q14_1c:Q14_4c) |> 
        freq()

## Factor analysis ---------------
cdata |> 
        select(Q14_1c:Q14_4c) |> 
        cortest.bartlett() # sig.
cdata |> 
        select(Q14_1c:Q14_4c) |> 
        KMO() # Overall MSA = 0.84
cdata |> 
        select(Q14_1c:Q14_4c) |> 
        fa.parallel() # 1 factor, 1 component
fa <- cdata |> 
        select(Q14_1c:Q14_4c) |> 
        fa(
                .,
                nfactors = 1,
                fm = "pa",
                max.iter = 100,
                rotate = "promax"
        )
fa |> fa.diagram()
print(fa$loadings, cutoff = .3, digits = 3)

cdata |> 
        select(Q14_1c:Q14_4c) |> 
        group_by() |> 
        psych::alpha() # Cronbach's alpha = 0.89

cdata <- cdata |>
        mutate(
                dispcurious = mean(
                        c(Q14_1c, Q14_2c, Q14_3c, Q14_4c),
                        na.rm = TRUE
                ))
cdata |> freq(dispcurious)
cdata |> 
        group_by() |> 
        descr(dispcurious, weights = cdata$wtvar) # M = 5.32, SD = 1.35


# Create frustration as a mediator --------------
cdata |> 
        select(Q27_9:Q27_10) |> 
        freq()
cdata <- var_recode(data = cdata, vars = c(Q27_9:Q27_10))
cdata |> 
        select(Q27_9c:Q27_10c) |> 
        freq()
cdata |> 
        select(Q27_9c:Q27_10c) |> 
        cor_test() # Pearson's r. = .82, p < .001
cdata <- cdata |> 
        mutate(
                frustration = mean(
                        c(Q27_9c, Q27_10c),
                        na.rm = TRUE
                ))
cdata |> freq(frustration)
cdata |> 
        group_by() |> 
        descr(frustration, weights = cdata$wtvar) # M = 2.40, SD = 1.94
