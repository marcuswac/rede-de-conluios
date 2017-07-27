#! /usr/local/bin/Rscript

pkgs <- c("tidyverse", "DT", "htmlwidgets", "networkD3", "shiny", "shinythemes",
          "shinydashboard", "shineBS", "shinyjs", "V8")
install.packages(pkgs, dependencies = TRUE, repos = "http://cran.rstudio.com/")
