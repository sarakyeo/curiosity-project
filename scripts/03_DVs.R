
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


# Create interest as control variable --------------------
cdata |> freq(Q12_5)
cdata <- var_recode(data = cdata, vars = Q12_5)
cdata |> freq(Q12_5c)
cdata <- cdata |> 
        mutate(interest = Q12_5c)
cdata |> freq(interest)
cdata |>
        group_by() |>
        descr(interest) # M = 4.60, SD = 1.96


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
        descr(dispcurious) # M = 5.42, SD = 1.35


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
        descr(frustration) # M = 2.53, SD = 2.03


# OLS path models --------------------------------
# PROCESS cannot handle sample weights
mcur <- cdata |> 
        filter(DVset == "DV set 1: Info Seeking") |> 
        select(ncstim, nrstim, dialogue, curiosity, frustration, interest, dispcurious, wtvar) |> 
        lm(formula = curiosity ~ ncstim + nrstim + frustration + dispcurious, weights = wtvar)

mfrus <- cdata |> 
        filter(DVset == "DV set 1: Info Seeking") |> 
        select(ncstim, nrstim, dialogue, curiosity, frustration, interest, dispcurious, wtvar) |> 
        lm(formula = frustration ~ ncstim + nrstim + curiosity + dispcurious, weights = wtvar)

mdialogue <- cdata |>
        filter(DVset == "DV set 1: Info Seeking") |>
        select(
                ncstim,
                nrstim,
                dialogue,
                curiosity,
                frustration,
                interest,
                dispcurious,
                wtvar
        ) |>
        lm(
                formula = dialogue ~ ncstim +
                        nrstim +
                        curiosity +
                        frustration +
                        dispcurious +
                        dispcurious:ncstim +
                        curiosity:ncstim +
                        frustration:ncstim,
                weights = wtvar
        )

huxreg("Curiosity" = mcur, "Frustation" = mfrus, "Dialogue" = mdialogue)

cdata |> 
        select(dispcurious, interest, curiosity, dialogue) |> 
        cor_pmat()

interact_plot(
        model = mdialogue,
        pred = cstim,
        modx = dispcurious,
        interval = TRUE,
        int.type = c("confidence"),
        int.width = 0.95,
)
interact_plot(
        mdialogue,
        pred = cstim,
        modx = dispcurious,
        interval = TRUE,
        int.width = .95,
        legend.main = "Dispositional / Trait Curiosity"
) +
        scale_y_continuous(
                name = "Dialogic intentions",
                limits = c(1, 7),
                expand = c(0, 0),
                breaks = seq(1, 7, 1)
        ) +
        scale_x_discrete(
                name = "",
                labels = c(
                        "Curiosity prime",
                        "No curiosity prime"
                )) +
        jtools::theme_apa()
