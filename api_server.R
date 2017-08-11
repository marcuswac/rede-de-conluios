library(plumber)

r <- plumb("R/conluios_api.R")
r$run(port=8000)

