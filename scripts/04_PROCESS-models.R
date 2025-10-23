source(here::here("scripts", "process.R"))

# Need to make variables all numeric ----------------

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


# Just trying out this PROCESS model, not sure the theoretical underpinning is sound ----------------
process(
  data = pilot,
  y = "asitcur",
  x = "acuriousn",
  m = "aclosure",
  cov = "areson",
  model = 4
)
