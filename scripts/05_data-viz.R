# Astronomy: Situational curiosity --------------
pilot |> 
        select(asitcur, astim) |> 
        na.omit() |> 
        ggplot(aes(x = astim, y = asitcur)) +
        scale_y_continuous(name = "Situational curiosity",
                           limits = c(1,7),
                           breaks = seq(1, 7, 1), 
                           expand = c(0,0)) + 
        scale_x_discrete(name = "") +
        theme_apa() +
        stat_summary(fun = mean, geom = "point") +
        stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
        theme(title = element_text(size = 12, face = "bold"),
              axis.title.x = element_text(size = 12, face = "plain"),
              axis.title.y = element_text(size = 12, face = "plain"),
              axis.text = element_text(size = 12, face = "plain"),
              axis.text.x = element_text(angle = 45, hjust = 1))

# Astronomy: Information seeking --------------
pilot |> 
        select(ainfoseek, astim) |> 
        na.omit() |> 
        ggplot(aes(x = astim, y = ainfoseek)) +
        scale_y_continuous(name = "Likelihood of seeking information",
                           limits = c(1,7),
                           breaks = seq(1, 7, 1), 
                           expand = c(0,0)) + 
        scale_x_discrete(name = "") +
        theme_apa() +
        stat_summary(fun = mean, geom = "point") +
        stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
        theme(title = element_text(size = 12, face = "bold"),
              axis.title.x = element_text(size = 12, face = "plain"),
              axis.title.y = element_text(size = 12, face = "plain"),
              axis.text = element_text(size = 12, face = "plain"),
              axis.text.x = element_text(angle = 45, hjust = 1))

# Astronomy: Provided closure --------------
pilot |> 
        select(aclosure, astim) |> 
        na.omit() |> 
        ggplot(aes(x = astim, y = aclosure)) +
        scale_y_continuous(name = "Extent to which information provided closure",
                           limits = c(1,7),
                           breaks = seq(1, 7, 1), 
                           expand = c(0,0)) + 
        scale_x_discrete(name = "") +
        theme_apa() +
        stat_summary(fun = mean, geom = "point") +
        stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
        theme(title = element_text(size = 12, face = "bold"),
              axis.title.x = element_text(size = 12, face = "plain"),
              axis.title.y = element_text(size = 12, face = "plain"),
              axis.text = element_text(size = 12, face = "plain"),
              axis.text.x = element_text(angle = 45, hjust = 1))

# Rain: Situational curiosity --------------
pilot |> 
        select(rsitcur, rstim) |> 
        na.omit() |> 
        ggplot(aes(x = rstim, y = rsitcur)) +
        scale_y_continuous(name = "Situational curiosity",
                           limits = c(1,7),
                           breaks = seq(1, 7, 1), 
                           expand = c(0,0)) + 
        scale_x_discrete(name = "") +
        theme_apa() +
        stat_summary(fun = mean, geom = "point") +
        stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
        theme(title = element_text(size = 12, face = "bold"),
              axis.title.x = element_text(size = 12, face = "plain"),
              axis.title.y = element_text(size = 12, face = "plain"),
              axis.text = element_text(size = 12, face = "plain"),
              axis.text.x = element_text(angle = 45, hjust = 1))

# Rain: Information seeking --------------
pilot |> 
        select(rinfoseek, rstim) |> 
        na.omit() |> 
        ggplot(aes(x = rstim, y = rinfoseek)) +
        scale_y_continuous(name = "Likelihood of seeking information",
                           limits = c(1,7),
                           breaks = seq(1, 7, 1), 
                           expand = c(0,0)) + 
        scale_x_discrete(name = "") +
        theme_apa() +
        stat_summary(fun = mean, geom = "point") +
        stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
        theme(title = element_text(size = 12, face = "bold"),
              axis.title.x = element_text(size = 12, face = "plain"),
              axis.title.y = element_text(size = 12, face = "plain"),
              axis.text = element_text(size = 12, face = "plain"),
              axis.text.x = element_text(angle = 45, hjust = 1))

# Rain: Provided closure --------------
pilot |> 
        select(rclosure, rstim) |> 
        na.omit() |> 
        ggplot(aes(x = rstim, y = rclosure)) +
        scale_y_continuous(name = "Extent to which information provided closure",
                           limits = c(1,7),
                           breaks = seq(1, 7, 1), 
                           expand = c(0,0)) + 
        scale_x_discrete(name = "") +
        theme_apa() +
        stat_summary(fun = mean, geom = "point") +
        stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
        theme(title = element_text(size = 12, face = "bold"),
              axis.title.x = element_text(size = 12, face = "plain"),
              axis.title.y = element_text(size = 12, face = "plain"),
              axis.text = element_text(size = 12, face = "plain"),
              axis.text.x = element_text(angle = 45, hjust = 1))
