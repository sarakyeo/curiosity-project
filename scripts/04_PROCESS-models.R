source(here::here("scripts", "process.R"))

# PROCESS Model 4 ---------------------
process(
  data = cdata,
  model = 4,
  y = "dialogue",
  x = "ncstim",
  m = "curiosity",
  cov = c("nrstim")
  mcx = 1,
  boot = 1000,
  seed = 20260518,
  total = 1,
  conf = 95,
  progress = 0,
  save = 2
)