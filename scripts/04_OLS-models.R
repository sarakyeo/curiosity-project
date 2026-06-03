# OLS path models --------------------------------
# PROCESS cannot handle sample weights
mcur <- cdata |> 
        filter(DVset == "DV set 1: Info Seeking") |> 
        select(cstim, rstim, dialogue, curiosity, frustration, interest, dispcurious, wtvar) |> 
        lm(formula = curiosity ~ cstim + rstim + frustration + dispcurious, weights = wtvar)

mfrus <- cdata |> 
        filter(DVset == "DV set 1: Info Seeking") |> 
        select(cstim, rstim, dialogue, curiosity, frustration, interest, dispcurious, wtvar) |> 
        lm(formula = frustration ~ cstim + rstim + curiosity + dispcurious, weights = wtvar)

mdialogue <- cdata |>
        filter(DVset == "DV set 1: Info Seeking") |>
        select(
                cstim,
                rstim,
                dialogue,
                curiosity,
                frustration,
                interest,
                dispcurious,
                wtvar
        ) |>
        lm(
                formula = dialogue ~ cstim +
                        rstim +
                        curiosity +
                        frustration +
                        dispcurious +
                        dispcurious:cstim +
                        dispcurious:rstim,
                weights = wtvar
        )

huxreg("Curiosity" = mcur, "Frustation" = mfrus, "Dialogue" = mdialogue)

cdata |> 
        select(dispcurious, frustration, curiosity, dialogue) |> 
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
        legend.main = "Dispositional curiosity"
) +
        scale_y_continuous(
                name = "Dialogic intentions",
                limits = c(1, 7),
                expand = c(0, 0),
                breaks = seq(1, 7, 1)
        ) +
        scale_x_discrete(
                name = "Curiosity prime",
                labels = c("present", "absent")
        ) +
        jtools::theme_apa(legend.use.title = TRUE)