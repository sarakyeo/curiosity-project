
# Astronomy: Situational curiosity -------------------
pilot |> 
        select(acurious, ainfoseek, aclosure, asitcur, areso, astim) |> 
        na.omit() |> 
        lm(formula = asitcur ~ acurious + areso) |> 
        summ()

# Astronomy: Information Seeking ------------
pilot |> 
        select(acurious, ainfoseek, aclosure, asitcur, areso, astim) |> 
        na.omit() |> 
        lm(formula = ainfoseek ~ acurious + areso) |> 
        summ()

# Astronomy: Provided closure ----------------
pilot |> 
        select(acurious, ainfoseek, aclosure, asitcur, areso, astim) |> 
        na.omit() |> 
        lm(formula = aclosure ~ acurious + areso) |> 
        summ()

# Rain: Situational curiosity -------------------
pilot |> 
        select(rcurious, rinfoseek, rclosure, rsitcur, rreso, rstim) |> 
        na.omit() |> 
        lm(formula = rsitcur ~ rcurious + rreso) |> 
        summ()

# Rain: Information Seeking ------------
pilot |> 
        select(rcurious, rinfoseek, rclosure, rsitcur, rreso, rstim) |> 
        na.omit() |> 
        lm(formula = rinfoseek ~ rcurious + rreso) |> 
        summ()

# Rain: Provided closure ----------------
pilot |> 
        select(rcurious, rinfoseek, rclosure, rsitcur, rreso, rstim) |> 
        na.omit() |> 
        lm(formula = rclosure ~ rcurious + rreso) |> 
        summ()
