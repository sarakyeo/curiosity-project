
# Situational curiosity -------------------
cdata |> 
        select(cstim, rstim, dispcurious, curiosity, frustration, dialogue) |> 
        na.omit() |> 
        lm(formula = curiosity ~ cstim + rstim + dispcurious) |> 
        summ() # ns

# Frustration --------------------
cdata |> 
        select(cstim, rstim, dispcurious, curiosity, frustration, dialogue) |> 
        na.omit() |> 
        lm(formula = frustration ~ cstim + rstim + dispcurious) |> 
        summ() # ns

# Intentions to engage in dialogue --------------------
cdata |> 
        select(cstim, rstim, dispcurious, curiosity, frustration, dialogue) |> 
        na.omit() |> 
        lm(formula = dialogue ~ cstim + rstim + dispcurious) |> 
        summ() # ns

