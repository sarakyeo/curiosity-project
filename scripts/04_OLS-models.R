# OLS path models --------------------------------
# PROCESS cannot handle sample weights
mcur <- cdata |> 
        filter(DVset == "DV set 1: Info Seeking") |> 
        select(cstim, rstim, dialogue, curiosity, frustration, interest, dispcurious, wtvar) |> 
        lm(formula = curiosity ~ cstim + rstim + frustration + dispcurious + cstim:rstim + cstim:dispcurious + rstim:dispcurious + cstim:rstim:dispcurious, weights = wtvar)

mfrus <- cdata |> 
        filter(DVset == "DV set 1: Info Seeking") |> 
        select(cstim, rstim, dialogue, curiosity, frustration, interest, dispcurious, wtvar) |> 
        lm(formula = frustration ~ cstim + rstim + curiosity + dispcurious + cstim:rstim + cstim:dispcurious + rstim:dispcurious + cstim:rstim:dispcurious, weights = wtvar)

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
                        cstim:rstim +
                        cstim:dispcurious +
                        rstim:dispcurious +
                        cstim:rstim:dispcurious,
                weights = wtvar
        )

huxreg("Curiosity" = mcur, "Frustation" = mfrus, "Dialogue" = mdialogue,
stars = c(`***` = .001, `**` = .01, `*` = .05),
error_format = "({std.error}) {p.value}",
error_pos = "same")

cdata |> 
        select(dispcurious, frustration, curiosity, dialogue) |> 
        cor_pmat()

interact_plot(
        model = mdialogue,
        pred = cstim,
        modx = dispcurious,
        mod2 = rstim,
        interval = TRUE,
        int.type = c("confidence"),
        int.width = 0.95,
) +
        jtools::theme_apa()

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
                labels = c("absent", "present")
        ) +
        jtools::theme_apa(legend.use.title = TRUE)

# Export regression table for Overleaf --------
huxreg(
        "Curiosity" = mcur,
        "Frustation" = mfrus,
        "Dialogue" = mdialogue,
        number_format = 2,
        stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
        ci_level = .95,
        align = ".",
        statistics = c(
                "N" = "nobs",
                "Adj. R-squared" = "adj.r.squared",
                "F" = "statistic",
                "df" = "df",
                "p" = "p.value"
        ),
        error_format = "({std.error})",
        error_pos = c("same"),
        coefs = c(
                "(Intercept)" = "(Intercept)",
                "Curiosity prime (present)" = "cstimCuriosity",
                "Resolution (present)" = "rstimResolution",
                "Frustration" = "frustration",
                "Trait curiosity" = "dispcurious",
                "Situational curiosity" = "curiosity",
                "Curiosity prime (present) × Trait curiosity" = "cstimCuriosity:dispcurious",
                "Resolution (present) × Trait curiosity" = "rstimResolution:dispcurious",
                "Curiosity prime (present) × Resolution prime (present)" = "cstimCuriosity:rstimResolution",
                "Curiosity prime (present) × Resolution prime (present) × Trait curiosity" = "cstimCuriosity:rstimResolution:dispcurious"
        )
) |>
        set_all_padding(0) |>
        set_label("tab:OLS-model") |>
        set_caption(
                "Unstandardized regression coefficients (standard errors in parentheses followed by p-values) for regression models in the path analysis predicting intentions to engage in dialogue about astrobiology and space science."
        ) |>
        set_font_size(11) |> 
        print_latex() |> 
        capture.output(file = here::here("outputs", "tab-OLS-model.tex"))

# Export interaction plot for Overleaf --------
ixn.plot <- interact_plot(
        model = mdialogue,
        pred = cstim,
        modx = dispcurious,
        mod2 = rstim,
        mod2.labels = c("No resolution", "Resolution"),
        interval = TRUE,
        int.type = c("confidence"),
        int.width = 0.95,
        legend.main = "Trait curiosity"
) +
        scale_y_continuous(
                name = "Dialogic intentions",
                limits = c(1, 7),
                expand = c(0, 0),
                breaks = seq(1, 7, 1)
        ) +
        scale_x_discrete(
                name = "Curiosity prime",
                labels = c("absent", "present")
        ) +
        jtools::theme_apa(legend.use.title = TRUE)

ggsave(plot = ixn.plot,
       here::here("outputs", "fig-three-way.png"),
       width = 7,
       height = 5)
