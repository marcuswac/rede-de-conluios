library(plumber)

argv <- commandArgs(TRUE)

server_host <- ifelse(is.na(argv[1]), "0.0.0.0", argv[1])
server_port <- ifelse(is.na(argv[2]), 3839, as.integer(argv[2]))

r <- plumb("R/conluios_api.R")
r$run(host = server_host, port = server_port)
