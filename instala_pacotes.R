#! /usr/local/bin/Rscript

pkgs <- c("tidyverse", "DT", "htmlwidgets", "networkD3", "plumber", "shiny",
          "shinythemes", "shinydashboard", "shinyBS", "shinyjs", "V8")
install.packages(pkgs, dependencies = TRUE, repos = "http://cran.rstudio.com/")
