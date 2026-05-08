# packages.R
# Este archivo instala automáticamente todos los paquetes necesarios
# Render lo ejecuta antes de lanzar la app

packages <- c(
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "shinyjs",
  "readxl",
  "dplyr",
  "janitor",
  "lubridate",
  "stringr",
  "ggplot2",
  "plotly",
  "DT",
  "leaflet",
  "sf",
  "writexl",
  "tidyr",
  "zip",
  "openxlsx",
  "htmltools"
)

installed <- rownames(installed.packages())
to_install <- packages[!(packages %in% installed)]

if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cran.rstudio.com/")
}
