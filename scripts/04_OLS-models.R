# OLS path models --------------------------------
# PROCESS cannot handle sample weights
mcur <- cdata |>
        filter(DVset == "DV set 1: Info Seeking") |>
        select(
                cstim,
                rstim,
                curiosity2,
                dispcurious,
                infoseek,
                wtvar
        ) |>
        lm(
                formula = curiosity2 ~ cstim +
                        rstim +
                        dispcurious +
                        cstim:rstim,
                weights = wtvar
        )
summ(mcur)

minfoseek <- cdata |>
        filter(DVset == "DV set 1: Info Seeking") |>
        select(
                cstim,
                rstim,
                dialogue,
                curiosity2,
                dispcurious,
                infoseek,
                wtvar
        ) |>
        lm(
                formula = infoseek ~ cstim +
                        rstim +
                        dispcurious +
                        curiosity2 +
                        cstim:rstim,
                weights = wtvar
        )
summ(minfoseek)


## Regression table for Overleaf -----------
huxreg(
        "Curiosity" = mcur,
        "Information seeking intentions" = minfoseek,
        number_format = 2,
        stars = NULL,
        ci_level = .95,
        align = ".",
        statistics = c(
                "N" = "nobs",
                "Adj. R-squared" = "adj.r.squared",
                "F" = "statistic",
                "df" = "df",
                "p" = "p.value"
        ),
        error_format = "({std.error}) {p.value}",
        error_pos = c("same"),
        coefs = c(
                "(Intercept)" = "(Intercept)",
                "Curiosity manipulation (present)" = "cstimCuriosity",
                "Resolution manipulation (present)" = "rstimResolution",
                "Trait curiosity" = "dispcurious",
                "Situational curiosity" = "curiosity2",
                "Curiosity manip. (present) × Resolution manip. (present)" = "cstimCuriosity:rstimResolution"
        )) |>
        set_font_size(11) |>
        set_label("tab:OLS-model") |>
        set_caption("Unstandardized regression coefficients and standard errors (in parentheses) in the OLS regression models predicting situational curiosity, and intentions to seek information about the search for life in the universe.") |>
        print_latex() |>
        capture.output(file = here::here("outputs", "tab-OLS-model.tex"))

## Interaction plot code ------------
# ixn.plot1 <- interact_plot(
#         model = mcur,
#         pred = cstim,
#         modx = dispcurious,
#         interval = TRUE,
#         int.type = c("confidence"),
#         int.width = 0.95,
#         colors = c("black", "black", "black"),
#         legend.main = "Trait curiosity"
# ) +
#         scale_y_continuous(
#                 name = "Situational curiosity",
#                 limits = c(1, 7),
#                 expand = c(0, 0),
#                 breaks = seq(1, 7, 1)
#         ) +
#         scale_x_discrete(
#                 name = "Curiosity manipulation"
#         ) +
#         jtools::theme_apa(legend.use.title = TRUE)

# Code for saving ixn plot
# ggsave(
#         ixn.plot1,
#         filename = here::here("outputs", "fig-mcur.png"),
#         width = 6.5,
#         height = 5
# )