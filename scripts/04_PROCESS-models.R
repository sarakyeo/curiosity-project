source(here::here("scripts", "process.R"))

# Need to make variables all numeric ----------------
## Astronomy -------------------
pilot |> 
  select(acurious, areso) |> 
  freq()

pilot <- pilot |> 
  mutate(acuriousn = case_when(
    acurious == "Curious" ~ 2,
    acurious == "No curious" ~ 1
  ))

pilot <- pilot |> 
  mutate(areson = case_when(
    areso == "Resolution" ~ 2,
    areso == "No resolution" ~ 1
  ))

pilot |> 
  select(acurious, acuriousn, areso, areson) |> 
  freq()

## Rain ----------------------
pilot |> 
  select(rcurious, rreso) |> 
  freq()

pilot <- pilot |> 
  mutate(rcuriousn = case_when(
    rcurious == "Curious" ~ 2,
    rcurious == "No curious" ~ 1
  ))

pilot <- pilot |> 
  mutate(rreson = case_when(
    rreso == "Resolution" ~ 2,
    rreso == "No resolution" ~ 1
  ))

pilot |> 
  select(rcurious, rcuriousn, rreso, rreson) |> 
  freq()


# Just trying out this PROCESS model, not sure the theoretical underpinning is sound ----------------
process(
  data = pilot,
  y = "rsitcur",
  x = "rcuriousn",
  m = "rclosure",
  cov = "rreson",
  model = 4
)
