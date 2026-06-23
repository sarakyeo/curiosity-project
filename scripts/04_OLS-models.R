# OLS path models (general population) --------------------------------
# PROCESS cannot handle sample weights

## DV = elicited curiosity --------------------
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

### Probing and graphing significant interactions ---------------------
probe_interaction(model = mcur, pred = dispcurious, modx = cstim)
# Slope of dispcurious when cstim = No curiosity: 
#   Est.   S.E.   t val.      p
#   0.67   0.06    11.82   0.00

# Slope of dispcurious when cstim = Curiosity: 
#   Est.   S.E.   t val.      p
#   0.77   0.06    12.98   0.00

mcur.cstim.disp <- interact_plot(
        model = mcur,
        pred = cstim,
        modx = dispcurious,
        interval = TRUE,
        centered = "all",
        int.type = c("confidence"),
        int.width = 0.95,
        colors = c("grey70", "grey45", "black"),
        legend.main = "Trait curiosity"
) +
        scale_y_continuous(
                name = "Elicited curiosity",
                limits = c(1, 7),
                expand = c(0, 0),
                breaks = seq(1, 7, 1)
        ) +
        scale_x_discrete(
                name = ""
        ) +
        jtools::theme_apa(legend.use.title = TRUE) +
        theme(
                axis.text.x = element_text(size = 13, color = "black"),
                axis.text.y = element_text(size = 13, color = "black"),
                axis.title.y = element_text(size = 13)
        )
mcur.cstim.disp
ggsave(
        mcur.cstim.disp,
        filename = here::here("outputs", "mcur-cstim-disp.png"),
        width = 6.5,
        height = 5
)


## DV = intentions to engage in dialogue ----------------------
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

### Probing and graphing significant interactions ----------------
#### cstim x dispcurious --------------------
probe_interaction(mcur, pred = dispcurious, modx = cstim)
# Slope of dispcurious when cstim = No curiosity: 
#   Est.   S.E.   t val.      p
#   0.67   0.06    11.82   0.00

# Slope of dispcurious when cstim = Curiosity: 
#   Est.   S.E.   t val.      p
#   0.77   0.06    12.98   0.00

mdialogue.cstim.disp <- interact_plot(
        model = mdialogue,
        pred = cstim,
        modx = dispcurious,
        interval = TRUE,
        int.type = "confidence",
        int.width = 0.95,
        colors = c("grey70", "grey45", "black"),
        legend.main = "Trait curiosity"
) +
        scale_y_continuous(
                name = "Intentions to engage in dialogue",
                limits = c(1, 7),
                expand = c(0, 0),
                breaks = seq(1, 7, 1)
        ) +
        scale_x_discrete(
                name = ""
        ) +
        jtools::theme_apa(legend.use.title = TRUE) +
        theme(
                axis.text.x = element_text(size = 13, color = "black"),
                axis.text.y = element_text(size = 13, color = "black"),
                axis.title.y = element_text(size = 13)
        )
mdialogue.cstim.disp
ggsave(
        mdialogue.cstim.disp,
        filename = here::here("outputs", "mdialogue-cstim-disp.png"),
        width = 6.5,
        height = 5
)

#### curiosity2 x dispcurious --------------------
probe_interaction(model = mdialogue, pred = dispcurious, modx = curiosity2)
# Slope of dispcurious when curiosity2 = 3.987380 (- 1 SD): 
#   Est.   S.E.   t val.      p
#   0.39   0.06     6.28   0.00

# Slope of dispcurious when curiosity2 = 5.494214 (Mean): 
#   Est.   S.E.   t val.      p
#   0.58   0.06     9.40   0.00

# Slope of dispcurious when curiosity2 = 7.001047 (+ 1 SD): 
#   Est.   S.E.   t val.      p
#   0.76   0.08    10.07   0.00

mdialogue.curiosity2.disp <- interact_plot(
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
mdialogue.curiosity2.disp


## Regression table for Overleaf -----------
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
                "Elicited curiosity × Trait curiosity" = "dispcurious:curiosity2"
        )) |>
        set_font_size(11) |>
        set_label("tab:OLS-model") |>
        set_caption(
                "Standardized regression coefficients and standard errors (in parentheses) followed by p-values in the OLS regression models predicting elicited curiosity, and intentions to seek information about the search for life in the universe."
        ) |>
        print_latex() |>
        capture.output(file = here::here("outputs", "tab-OLS-model.tex"))
# The reason that this LaTeX table appears to be formatted incorrectly has to do with the text wrapping of the footnote (or lack thereof). If I move the stars to a different line, then this resolves the text wrapping issue and fits the table on to a page in portrait format.
# I can then find and replace the 0.00 with < .001, add the -- for the empty cells, and remove the "-" in front of any "-0.00" coefficients manually in Overleaf.





# Extra Stuff ----------------------------
# # OLS path models (Black) --------------------------------
# # PROCESS cannot handle sample weights
# mcurB <- cdata |>
#         filter(DVset == "DV set 1: Info Seeking") |>
#         filter(Black == "Black") |>
#         select(
#                 cstim,
#                 rstim,
#                 curiosity2,
#                 dispcurious,
#                 dialogue,
#                 wtvar
#         ) |>
#         lm(
#                 formula = curiosity2 ~
#                         cstim +
#                         rstim +
#                         dispcurious +
#                         cstim:rstim +
#                         cstim:dispcurious
#         )
# summ(mcurB, vifs = TRUE, scale = TRUE)

# mdialogueB <- cdata |>
#         filter(DVset == "DV set 1: Info Seeking") |>
#         filter(Black == "Black") |>
#         select(
#                 cstim,
#                 rstim,
#                 dialogue,
#                 curiosity2,
#                 dispcurious,
#                 dialogue,
#                 wtvar
#         ) |>
#         lm(
#                 formula = dialogue ~ cstim +
#                         rstim +
#                         dispcurious +
#                         curiosity2 +
#                         cstim:rstim +
#                         cstim:dispcurious +
#                         dispcurious:curiosity2
#         )
# summ(mdialogueB, vifs = TRUE, scale = TRUE)
# probe_interaction(mdialogueB, pred = dispcurious, modx = cstim)
# ixn.plot2B <- interact_plot(
#         model = mdialogueB,
#         pred = cstim,
#         modx = dispcurious,
#         interval = TRUE,
#         int.type = "confidence",
#         int.width = 0.95,
#         colors = c("grey70", "grey45", "black"),
#         legend.main = "Trait curiosity"
# ) +
#         scale_y_continuous(
#                 name = "Intentions to engage in dialogue",
#                 limits = c(1, 7),
#                 expand = c(0, 0),
#                 breaks = seq(1, 7, 1)
#         ) +
#         scale_x_discrete(
#                 name = ""
#         ) +
#         jtools::theme_apa(legend.use.title = TRUE) +
#         theme(
#                 axis.text.x = element_text(size = 13, color = "black"),
#                 axis.text.y = element_text(size = 13, color = "black"),
#                 axis.title.y = element_text(size = 13)
#         )
# ggsave(
#         ixn.plot2B,
#         filename = here::here("outputs", "B-cstim-trait-elicited.png"),
#         width = 6.5,
#         height = 5
# )


# ## Regression table for Overleaf -----------
# export_summs(
#         mcurB,
#         mdialogueB,
#         model.names = c(
#                 "Elicited curiosity",
#                 "Intentions to engage in dialogue"
#         ),
#         scale = TRUE,
#         stars = NULL,
#         ci_level = 0.95,
#         align = ".",
#         error_format = "({std.error}), {p.value}",
#         error_pos = "same",
#         statistics = c(
#                 "N" = "nobs",
#                 "Adj. R-squared" = "adj.r.squared",
#                 "F" = "statistic",
#                 "df" = "df",
#                 "p" = "p.value"
#         ),
#         coefs = c(
#                 "(Intercept)" = "(Intercept)",
#                 "Curiosity manipulation (present)" = "cstimCuriosity",
#                 "Resolution manipulation (present)" = "rstimResolution",
#                 "Trait curiosity" = "dispcurious",
#                 "Elicited curiosity" = "curiosity2",
#                 "Curiosity manip. (present) × Resolution manip. (present)" = "cstimCuriosity:rstimResolution",
#                 "Curiosity manip. (present) × Trait curiosity" = "cstimCuriosity:dispcurious",
#                 "Elicited curiosity × Trait curiosity" = "dispcurious:curiosity2"
#         )) |>
#         set_font_size(11) |>
#         set_label("tab:OLS-model-Black") |>
#         set_caption(
#                 "Standardized regression coefficients and standard errors (in parentheses) followed by p-values in the OLS regression models predicting elicited curiosity, and intentions to engage in dialogue about the search for life in the universe among Black respondents in the sample."
#         ) |>
#         print_latex() |>
#         capture.output(file = here::here("outputs", "tab-OLS-model-B.tex"))
# # The reason that this LaTeX table appears to be formatted incorrectly has to do with the text wrapping of the footnote (or lack thereof). If I move the stars to a different line, then this resolves the text wrapping issue and fits the table on to a page in portrait format.
# # I can then find and replace the 0.00 with < .001, add the -- for the empty cells, and remove the "-" in front of any "-0.00" coefficients manually in Overleaf.


# # OLS path models (Hispanic) --------------------------------
# # PROCESS cannot handle sample weights
# mcurH <- cdata |>
#         filter(DVset == "DV set 1: Info Seeking") |>
#         filter(Hispanic == "Hispanic") |>
#         select(
#                 cstim,
#                 rstim,
#                 curiosity2,
#                 dispcurious,
#                 dialogue,
#                 wtvar
#         ) |>
#         lm(
#                 formula = curiosity2 ~
#                         cstim +
#                         rstim +
#                         dispcurious +
#                         cstim:rstim +
#                         cstim:dispcurious
#         )
# summ(mcurH, vifs = TRUE, scale = TRUE)

# mdialogueH <- cdata |>
#         filter(DVset == "DV set 1: Info Seeking") |>
#         filter(Hispanic == "Hispanic") |>
#         select(
#                 cstim,
#                 rstim,
#                 dialogue,
#                 curiosity2,
#                 dispcurious,
#                 dialogue,
#                 wtvar
#         ) |>
#         lm(
#                 formula = dialogue ~ cstim +
#                         rstim +
#                         dispcurious +
#                         curiosity2 +
#                         cstim:rstim +
#                         cstim:dispcurious +
#                         dispcurious:curiosity2
#         )
# summ(mdialogueH, vifs = TRUE, scale = TRUE)
# probe_interaction(model = mdialogueH, pred = dispcurious, modx = curiosity2)
# ixn.plot3H <- interact_plot(
#         model = mdialogueH,
#         pred = dispcurious,
#         modx = curiosity2,
#         interval = TRUE,
#         int.type = c("confidence"),
#         int.width = 0.95,
#         johnson_neyman = TRUE,
#         colors = c("grey", "black"),
#         legend.main = "Elicited curiosity"
# ) +
#         scale_y_continuous(
#                 name = "Intentions to engage in dialogue",
#                 limits = c(1, 7),
#                 expand = c(0, 0),
#                 breaks = seq(1, 7, 1)
#         ) +
#         scale_x_continuous(
#                 name = "Trait curiosity",
#                 limits = c(1, 7),
#                 expand = c(0, 0),
#                 breaks = seq(1, 7, 1)
#         ) +
#         jtools::theme_apa(legend.use.title = TRUE)

# jnplot.3H <- johnson_neyman(
#         model = mdialogueH,
#         pred = curiosity2,
#         modx = dispcurious,
#         plot = TRUE,
#         sig.color = "#363636",
#         insig.color = "grey",
#         title = ""
# )
# jnplotfinalH <- jnplot.3H$plot +
#         scale_y_continuous(
#                 name = "Slope of elicited curiosity",
#                 expand = c(0, 0)
#         ) +
#         scale_x_continuous(
#                 name = "Trait curiosity",
#                 limits = c(1, 7),
#                 expand = c(0, 0),
#                 breaks = seq(1, 7, 1)
#         )
# ixn3Hfinal <- ggpubr::ggarrange(
#         ixn.plot3H,
#         jnplotfinalH,
#         nrow = 2,
#         ncol = 1,
#         align = "hv"
# )
# ggsave(
#         ixn3Hfinal,
#         filename = here::here("outputs", "H-trait-elicited-dialogue.png"),
#         width = 6.5,
#         height = 10
# )


# ## Regression table for Overleaf -----------
# export_summs(
#         mcurH,
#         mdialogueH,
#         model.names = c(
#                 "Elicited curiosity",
#                 "Intentions to engage in dialogue"
#         ),
#         scale = TRUE,
#         stars = NULL,
#         ci_level = 0.95,
#         align = ".",
#         error_format = "({std.error}), {p.value}",
#         error_pos = "same",
#         statistics = c(
#                 "N" = "nobs",
#                 "Adj. R-squared" = "adj.r.squared",
#                 "F" = "statistic",
#                 "df" = "df",
#                 "p" = "p.value"
#         ),
#         coefs = c(
#                 "(Intercept)" = "(Intercept)",
#                 "Curiosity manipulation (present)" = "cstimCuriosity",
#                 "Resolution manipulation (present)" = "rstimResolution",
#                 "Trait curiosity" = "dispcurious",
#                 "Elicited curiosity" = "curiosity2",
#                 "Curiosity manip. (present) × Resolution manip. (present)" = "cstimCuriosity:rstimResolution",
#                 "Curiosity manip. (present) × Trait curiosity" = "cstimCuriosity:dispcurious",
#                 "Elicited curiosity × Trait curiosity" = "dispcurious:curiosity2"
#         )) |>
#         set_font_size(11) |>
#         set_label("tab:OLS-model-Hispanic") |>
#         set_caption(
#                 "Standardized regression coefficients and standard errors (in parentheses) followed by p-values in the OLS regression models predicting elicited curiosity, and intentions to engage in dialogue about the search for life in the universe among Hispanic respondents in the sample."
#         ) |>
#         print_latex() |>
#         capture.output(file = here::here("outputs", "tab-OLS-model-H.tex"))
# # The reason that this LaTeX table appears to be formatted incorrectly has to do with the text wrapping of the footnote (or lack thereof). If I move the stars to a different line, then this resolves the text wrapping issue and fits the table on to a page in portrait format.
# # I can then find and replace the 0.00 with < .001, add the -- for the empty cells, and remove the "-" in front of any "-0.00" coefficients manually in Overleaf.


# # OLS path models (White) --------------------------------
# # PROCESS cannot handle sample weights
# mcurW <- cdata |>
#         filter(DVset == "DV set 1: Info Seeking") |>
#         filter(White == "White") |>
#         select(
#                 cstim,
#                 rstim,
#                 curiosity2,
#                 dispcurious,
#                 dialogue,
#                 wtvar
#         ) |>
#         lm(
#                 formula = curiosity2 ~
#                         cstim +
#                         rstim +
#                         dispcurious +
#                         cstim:rstim +
#                         cstim:dispcurious
#         )
# summ(mcurW, vifs = TRUE, scale = TRUE)

# mdialogueW <- cdata |>
#         filter(White == "White" & DVset == "DV set 1: Info Seeking") |>
#         select(
#                 cstim,
#                 rstim,
#                 dialogue,
#                 curiosity2,
#                 dispcurious,
#                 dialogue,
#                 wtvar
#         ) |>
#         lm(
#                 formula = dialogue ~ cstim +
#                         rstim +
#                         dispcurious +
#                         curiosity2 +
#                         cstim:rstim +
#                         cstim:dispcurious +
#                         dispcurious:curiosity2
#         )
# summ(mdialogueW, vifs = TRUE, scale = TRUE)
# probe_interaction(mdialogueH, pred = dispcurious, modx = curiosity2)
# ixn.plot3W <- interact_plot(
#         model = mdialogueW,
#         pred = dispcurious,
#         modx = curiosity2,
#         interval = TRUE,
#         int.type = c("confidence"),
#         int.width = 0.95,
#         johnson_neyman = TRUE,
#         colors = c("grey", "black"),
#         legend.main = "Elicited curiosity"
# ) +
#         scale_y_continuous(
#                 name = "Intentions to engage in dialogue",
#                 limits = c(1, 7),
#                 expand = c(0, 0),
#                 breaks = seq(1, 7, 1)
#         ) +
#         scale_x_continuous(
#                 name = "Trait curiosity",
#                 limits = c(1, 7),
#                 expand = c(0, 0),
#                 breaks = seq(1, 7, 1)
#         ) +
#         jtools::theme_apa(legend.use.title = TRUE)

# jnplot.3W <- johnson_neyman(
#         model = mdialogueW,
#         pred = curiosity2,
#         modx = dispcurious,
#         plot = TRUE,
#         sig.color = "#363636",
#         insig.color = "grey",
#         title = ""
# )
# jnplotfinalW <- jnplot.3W$plot +
#         scale_y_continuous(
#                 name = "Slope of elicited curiosity",
#                 expand = c(0, 0)
#         ) +
#         scale_x_continuous(
#                 name = "Trait curiosity",
#                 limits = c(1, 7),
#                 expand = c(0, 0),
#                 breaks = seq(1, 7, 1)
#         )
# ixn3Wfinal <- ggpubr::ggarrange(
#         ixn.plot3W,
#         jnplotfinalW,
#         nrow = 2,
#         ncol = 1,
#         align = "hv"
# )
# ggsave(
#         ixn3Wfinal,
#         filename = here::here("outputs", "W-trait-elicited-dialogue.png"),
#         width = 6.5,
#         height = 10
# )

# ## Regression table for Overleaf -----------
# export_summs(
#         mcurW,
#         mdialogueW,
#         model.names = c(
#                 "Elicited curiosity",
#                 "Intentions to engage in dialogue"
#         ),
#         scale = TRUE,
#         stars = NULL,
#         ci_level = 0.95,
#         align = ".",
#         error_format = "({std.error}), {p.value}",
#         error_pos = "same",
#         statistics = c(
#                 "N" = "nobs",
#                 "Adj. R-squared" = "adj.r.squared",
#                 "F" = "statistic",
#                 "df" = "df",
#                 "p" = "p.value"
#         ),
#         coefs = c(
#                 "(Intercept)" = "(Intercept)",
#                 "Curiosity manipulation (present)" = "cstimCuriosity",
#                 "Resolution manipulation (present)" = "rstimResolution",
#                 "Trait curiosity" = "dispcurious",
#                 "Elicited curiosity" = "curiosity2",
#                 "Curiosity manip. (present) × Resolution manip. (present)" = "cstimCuriosity:rstimResolution",
#                 "Curiosity manip. (present) × Trait curiosity" = "cstimCuriosity:dispcurious",
#                 "Elicited curiosity × Trait curiosity" = "dispcurious:curiosity2"
#         )) |>
#         set_font_size(11) |>
#         set_label("tab:OLS-model-White") |>
#         set_caption(
#                 "Standardized regression coefficients and standard errors (in parentheses) followed by p-values in the OLS regression models predicting elicited curiosity, and intentions to engage in dialogue about the search for life in the universe among White respondents in the sample."
#         ) |>
#         print_latex() |>
#         capture.output(file = here::here("outputs", "tab-OLS-model-W.tex"))
# # The reason that this LaTeX table appears to be formatted incorrectly has to do with the text wrapping of the footnote (or lack thereof). If I move the stars to a different line, then this resolves the text wrapping issue and fits the table on to a page in portrait format.
# # I can then find and replace the 0.00 with < .001, add the -- for the empty cells, and remove the "-" in front of any "-0.00" coefficients manually in Overleaf.