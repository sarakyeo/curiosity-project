# OLS path models --------------------------------
# PROCESS cannot handle sample weights
mcur <- cdata |>
        filter(DVset == "DV set 1: Info Seeking") |>
        select(
                cstim,
                rstim,
                curiosity2,
                dispcurious,
                dialogue,
                wtvar
        ) |>
        lm(
                formula = curiosity2 ~
                        cstim +
                        rstim +
                        dispcurious +
                        cstim:rstim +
                        cstim:dispcurious,
                weights = wtvar
        )
summ(mcur, vifs = TRUE, scale = TRUE)

mdialogue <- cdata |>
        filter(DVset == "DV set 1: Info Seeking") |>
        select(
                cstim,
                rstim,
                dialogue,
                curiosity2,
                dispcurious,
                dialogue,
                wtvar
        ) |>
        lm(
                formula = dialogue ~ cstim +
                        rstim +
                        dispcurious +
                        curiosity2 +
                        cstim:rstim +
                        cstim:dispcurious +
                        dispcurious:curiosity2,
                weights = wtvar
        )
summ(mdialogue, vifs = TRUE, scale = TRUE)

probe_interaction(mcur, pred = dispcurious, modx = cstim)


## Regression table for Overleaf -----------
# huxreg(
#         "Elicited curiosity" = mcur,
#         "Intentions to engage in dialogue" = mdialogue,
#         number_format = 2,
#         stars = NULL,
#         ci_level = .95,
#         align = ".",
#         statistics = c(
#                 "N" = "nobs",
#                 "Adj. R-squared" = "adj.r.squared",
#                 "F" = "statistic",
#                 "df" = "df",
#                 "p" = "p.value"
#         ),
#         error_format = "({std.error}), {p.value}",
#         error_pos = c("same"),
#         coefs = c(
#                 "(Intercept)" = "(Intercept)",
#                 "Curiosity manipulation (present)" = "cstimCuriosity",
#                 "Resolution manipulation (present)" = "rstimResolution",
#                 "Trait curiosity" = "dispcurious",
#                 "Elicited curiosity" = "curiosity2",
#                 "Curiosity manip. (present) × Resolution manip. (present)" = "cstimCuriosity:rstimResolution",
#                 "Curiosity manip. (present) × Trait curiosity" = "cstimCuriosity:dispcurious",
#                 "Trait curiosity × Elicited curiosity" = "dispcurious:curiosity2"
#         )) |>
#         set_font_size(11) |>
#         set_label("tab:OLS-model") |>
#         set_caption("Standardized regression coefficients and standard errors (in parentheses) followed by p-values in the OLS regression models predicting situational curiosity, and intentions to seek information about the search for life in the universe.") |>
#         print_latex() |>
#         capture.output(file = here::here("outputs", "tab-OLS-model.tex"))


export_summs(
        mcur,
        mdialogue,
        model.names = c(
                "Elicited curiosity",
                "Intentions to engage in dialogue"
        ),
        scale = TRUE,
        stars = NULL,
        ci_level = 0.95,
        align = ".",
        error_format = "({std.error}), {p.value}",
        error_pos = "same",
        statistics = c(
                "N" = "nobs",
                "Adj. R-squared" = "adj.r.squared",
                "F" = "statistic",
                "df" = "df",
                "p" = "p.value"
        ),
        coefs = c(
                "(Intercept)" = "(Intercept)",
                "Curiosity manipulation (present)" = "cstimCuriosity",
                "Resolution manipulation (present)" = "rstimResolution",
                "Trait curiosity" = "dispcurious",
                "Elicited curiosity" = "curiosity2",
                "Curiosity manip. (present) × Resolution manip. (present)" = "cstimCuriosity:rstimResolution",
                "Curiosity manip. (present) × Trait curiosity" = "cstimCuriosity:dispcurious",
                "Trait curiosity × Elicited curiosity" = "dispcurious:curiosity2"
        )) |>
        set_font_size(11) |>
        set_label("tab:OLS-model") |>
        set_caption(
                "Standardized regression coefficients and standard errors (in parentheses) followed by p-values in the OLS regression models predicting situational curiosity, and intentions to engage in dialogue about the search for life in the universe."
        ) |>
        print_latex() |>
        capture.output(file = here::here("outputs", "tab-OLS-model.tex"))
# The reason that this LaTeX table appears to be formatted incorrectly has to do with the text wrapping of the footnote (or lack thereof). If I move the stars to a different line, then this resolves the text wrapping issue and fits the table on to a page in portrait format.
# I can then find and replace the 0.00 with < .001, add the -- for the empty cells, and remove the "-" in front of any "-0.00" coefficients manually in Overleaf.


## Interaction plot code ------------
probe_interaction(model = mcur, pred = dispcurious, cstim)
# Johnson-Neyman intervals are not available for factor predictors or moderators. 
# SIMPLE SLOPES ANALYSIS
# Slope of dispcurious when cstim = No curiosity: 
#   Est.   S.E.   t val.      p
# ------ ------ -------- ------
#   0.67   0.06    11.82   0.00
# Slope of dispcurious when cstim = Curiosity: 
#   Est.   S.E.   t val.      p
# ------ ------ -------- ------
#   0.77   0.06    12.98   0.00
ixn.plot1 <- interact_plot(
        model = mcur,
        pred = dispcurious,
        modx = cstim,
        interval = TRUE,
        int.type = c("confidence"),
        int.width = 0.95,
        johnson_neyman = TRUE,
        jnplot = TRUE,
        colors = c("grey", "black"),
        legend.main = ""
) +
        scale_y_continuous(
                name = "Elicited curiosity",
                limits = c(1, 7),
                expand = c(0, 0),
                breaks = seq(1, 7, 1)
        ) +
        scale_x_continuous(
                name = "Trait curiosity",
                limits = c(1, 7),
                expand = c(0, 0),
                breaks = seq(1, 7, 1)
        ) +
        jtools::theme_apa(legend.use.title = TRUE)

probe_interaction(model = mdialogue, pred = dispcurious, cstim)
# Johnson-Neyman intervals are not available for factor predictors or moderators. 
# SIMPLE SLOPES ANALYSIS
# Slope of dispcurious when cstim = No curiosity: 
#   Est.   S.E.   t val.      p
# ------ ------ -------- ------
#   0.58   0.06     9.40   0.00
# Slope of dispcurious when cstim = Curiosity: 
#   Est.   S.E.   t val.      p
# ------ ------ -------- ------
#   0.50   0.07     7.52   0.00
ixn.plot2 <- interact_plot(
        model = mdialogue,
        pred = dispcurious,
        modx = cstim,
        interval = TRUE,
        int.type = c("confidence"),
        int.width = 0.95,
         johnson_neyman = TRUE,
        jnplot = TRUE,
        colors = c("grey", "black"),
        legend.main = ""
) +
        scale_y_continuous(
                name = "Intentions to engage in dialogue",
                limits = c(1, 7),
                expand = c(0, 0),
                breaks = seq(1, 7, 1)
        ) +
        scale_x_continuous(
                name = "Trait curiosity",
                limits = c(1, 7),
                expand = c(0, 0),
                breaks = seq(1, 7, 1)
        ) +
        jtools::theme_apa(legend.use.title = TRUE)

probe_interaction(model = mdialogue, pred = dispcurious, modx = curiosity2, jnplot = TRUE)
ixn.plot3 <- interact_plot(
        model = mdialogue,
        pred = dispcurious,
        modx = curiosity2,
        interval = TRUE,
        int.type = c("confidence"),
        int.width = 0.95,
        johnson_neyman = TRUE,
        colors = c("grey", "black"),
        legend.main = "Elicited curiosity"
) +
        scale_y_continuous(
                name = "Intentions to engage in dialogue",
                limits = c(1, 7),
                expand = c(0, 0),
                breaks = seq(1, 7, 1)
        ) +
        scale_x_continuous(
                name = "Trait curiosity",
                limits = c(1, 7),
                expand = c(0, 0),
                breaks = seq(1, 7, 1)
        ) +
        jtools::theme_apa(legend.use.title = TRUE)
jnplot.3 <- johnson_neyman(
        model = mdialogue,
        pred = curiosity2,
        modx = dispcurious,
        plot = TRUE,
        sig.color = "#363636",
        insig.color = "grey",
        title = ""
)
jnplotfinal <- jnplot.3$plot +
        scale_y_continuous(
                name = "Slope of elicited curiosity",
                expand = c(0, 0)
        ) +
        scale_x_continuous(
                name = "Trait curiosity",
                limits = c(1, 7),
                expand = c(0, 0),
                breaks = seq(1, 7, 1)
        )
ixn3final <- ggpubr::ggarrange(
        ixn.plot3,
        jnplotfinal,
        nrow = 2,
        ncol = 1,
        align = "hv"
)



# Code for saving ixn plot
ggsave(
        ixn.plot1,
        filename = here::here("outputs", "fig1.png"),
        width = 6.5,
        height = 5
)

ggsave(
        ixn.plot2,
        filename = here::here("outputs", "fig2.png"),
        width = 6.5,
        height = 5
)
ggsave(
        ixn3final,
        filename = here::here("outputs", "fig3.png"),
        width = 6.5,
        height = 10
)
