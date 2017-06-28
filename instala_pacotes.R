#! /usr/local/bin/Rscript

pkgs <- c("tidyverse", "htmlwidgets", "networkD3", "shiny", "shinythemes",
          "DT")
install.packages(pkgs, dependencies = TRUE, repos = "http://cran.rstudio.com/")
