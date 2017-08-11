#! /usr/local/bin/Rscript
library("shiny")

argv <- commandArgs(TRUE)

if (!is.na(argv[1])) {
  options(shiny.port = as.integer(argv[1]))
}

runApp(".")
