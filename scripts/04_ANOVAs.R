
# Situational curiosity (2-item measure) -------------------
cdata |> 
        select(cstim, rstim, dispcurious, curiosity2, frustration, dialogue, wtvar) |> 
        na.omit() |> 
        lm(formula = curiosity2 ~ cstim + rstim + dispcurious, weights = wtvar) |> 
        summ() # ns

cdata |> 
        select(cstim, rstim, dispcurious, curiosity2, frustration, infoseek, dialogue) |> 
        ggplot(aes(x = cstim, y = curiosity2)) +
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
              axis.text = element_text(size = 12, face = "plain"))


# Situational curiosity (4-item measure) -------------------
cdata |> 
        select(cstim, rstim, dispcurious, curiosity4, frustration, dialogue, wtvar) |> 
        na.omit() |> 
        lm(formula = curiosity4 ~ cstim + rstim + dispcurious, weights = wtvar) |> 
        summ() # ns

cdata |> 
        select(cstim, rstim, dispcurious, curiosity4, frustration, infoseek, dialogue) |> 
        ggplot(aes(x = cstim, y = curiosity4)) +
        scale_y_continuous(name = "Situational curiosity (4-item)",
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
              axis.text = element_text(size = 12, face = "plain"))


# Frustration --------------------
cdata |> 
        select(cstim, rstim, dispcurious, curiosity, frustration, dialogue) |> 
        na.omit() |> 
        lm(formula = frustration ~ cstim + rstim + dispcurious) |> 
        summ() # ns

cdata |> 
        select(cstim, rstim, dispcurious, curiosity2, frustration, infoseek, dialogue) |> 
        ggplot(aes(x = rstim, y = frustration)) +
        scale_y_continuous(name = "Experienced frustration",
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
              axis.text = element_text(size = 12, face = "plain"))

# Intentions to engage in dialogue --------------------
cdata |> 
        select(cstim, rstim, dispcurious, curiosity2, frustration, infoseek, dialogue, wtvar) |> 
        na.omit() |> 
        lm(formula = dialogue ~ cstim + rstim + curiosity2 + dispcurious, weights = wtvar) |> 
        summ() # ns

cdata |> 
        select(cstim, rstim, dispcurious, curiosity2, frustration, infoseek, dialogue) |> 
        ggplot(aes(x = cstim, y = dialogue)) +
        scale_y_continuous(name = "Intentions to engage in dialogue",
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
              axis.text = element_text(size = 12, face = "plain"))


# Intentions to seek information ---------------------
cdata |> 
        select(cstim, rstim, dispcurious, curiosity, frustration, infoseek, dialogue) |> 
        na.omit() |> 
        lm(formula = infoseek ~ cstim + rstim + dispcurious) |> 
        summ() # ns

cdata |> 
        select(cstim, rstim, dispcurious, curiosity, frustration, infoseek, dialogue) |> 
        ggplot(aes(x = cstim, y = infoseek)) +
        scale_y_continuous(name = "Intentions to seek information",
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
              axis.text = element_text(size = 12, face = "plain"))



cdata |> freq(Q27_4)
