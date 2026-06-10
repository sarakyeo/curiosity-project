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
                formula = curiosity2 ~
                        cstim +
                        rstim +
                        dispcurious +
                        cstim:rstim +
                        cstim:dispcurious,
                weights = wtvar
        )
summ(mcur, scale = TRUE)

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
                        cstim:rstim + 
                        cstim:dispcurious +
                        dispcurious:curiosity2,
                weights = wtvar
        )
summ(minfoseek)


## Regression table for Overleaf -----------
huxreg(
        "Elicited curiosity" = mcur,
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
        error_format = "({std.error}), {p.value}",
        error_pos = c("same"),
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
        set_caption("Unstandardized regression coefficients and standard errors (in parentheses) followed by p-values in the OLS regression models predicting situational curiosity, and intentions to seek information about the search for life in the universe.") |>
        print_latex() |>
        capture.output(file = here::here("outputs", "tab-OLS-model-formattedwell.tex"))


export_summs(
        mcur,
        minfoseek,
        model.names = c(
                "Elicited curiosity",
                "Information seeking intentions"
        ),
        scale = TRUE,
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
                "Standardized regression coefficients and standard errors (in parentheses) followed by p-values in the OLS regression models predicting situational curiosity, and intentions to seek information about the search for life in the universe."
        ) |>
        print_latex() |>
        capture.output(file = here::here("outputs", "tab-OLS-model.tex"))

# Because huxreg() cannot scale the coefficients in the model, but the LaTex formatting is better than export_summ(), I used huxreg to generate the table format, export_summs() to generate the values. Then I put both into Claude and gave it the following commands:
# Please replace the values in tab-OLS-model-formattedwell.tex with those from tab-OLS-model.tex. Retain stars and commas. 
# Can you please change the p-values that are 0.00 to < .001 in the TeX table I uploaded?
# I did these in separate prompts, but I might be able to combine them in the future.
# I then manually added to the TeX file the "--" for the cells that did not contain estimates and changed the -0.00 coefficient to 0.00.

## Interaction plot code ------------
ixn.plot1 <- interact_plot(
        model = mcur,
        pred = cstim,
        modx = dispcurious,
        interval = TRUE,
        int.type = c("confidence"),
        int.width = 0.95,
        colors = c("grey", "black"),
        legend.main = "Trait curiosity"
) +
        scale_y_continuous(
                name = "Elicited curiosity",
                limits = c(1, 7),
                expand = c(0, 0),
                breaks = seq(1, 7, 1)
        ) +
        scale_x_discrete(
                name = "Curiosity manipulation"
        ) +
        jtools::theme_apa(legend.use.title = TRUE)

ixn.plot2 <- interact_plot(
        model = minfoseek,
        pred = cstim,
        modx = dispcurious,
        interval = TRUE,
        int.type = c("confidence"),
        int.width = 0.95,
        colors = c("grey", "black"),
        legend.main = "Trait curiosity"
) +
        scale_y_continuous(
                name = "Information seeking intentions",
                limits = c(1, 7),
                expand = c(0, 0),
                breaks = seq(1, 7, 1)
        ) +
        scale_x_discrete(
                name = "Curiosity manipulation"
        ) +
        jtools::theme_apa(legend.use.title = TRUE)

ixn.plot3 <- interact_plot(
        model = minfoseek,
        pred = curiosity2,
        modx = dispcurious,
        interval = TRUE,
        int.type = c("confidence"),
        int.width = 0.95,
        colors = c("grey", "black"),
        legend.main = "Trait curiosity"
) +
        scale_y_continuous(
                name = "Information seeking intentions",
                limits = c(1, 7),
                expand = c(0, 0),
                breaks = seq(1, 7, 1)
        ) +
        scale_x_continuous(
                name = "Elicited curiosity",
                limits = c(1, 7),
                expand = c(0, 0),
                breaks = seq(1, 7, 1)
        ) +
        jtools::theme_apa(legend.use.title = TRUE)

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
        ixn.plot3,
        filename = here::here("outputs", "fig3.png"),
        width = 6.5,
        height = 5
)