library(plumber)

argv <- commandArgs(TRUE)

server_port <- ifelse(is.na(argv[1]), 3839, as.integer(argv[1]))

r <- plumb("R/conluios_api.R")
r$run(port=server_port)

