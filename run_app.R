#!/usr/bin/env Rscript
# Run the 5 Buckets Impact Dashboard (port 3838)

if (basename(getwd()) != "Impact Dashboard") {
  if (file.exists("Impact Dashboard/app.R")) {
    setwd("Impact Dashboard")
  } else if (!file.exists("app.R")) {
    stop("Cannot find app.R. Run from project root or Impact Dashboard directory.")
  }
}

shiny::runApp("app.R", port = 3838)
