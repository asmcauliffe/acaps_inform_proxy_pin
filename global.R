# global.R — loaded by app.R before ui.R and server.R

suppressPackageStartupMessages({
  library(shiny)
  library(sf)
  library(dplyr)
  library(leaflet)
  library(htmltools)
  library(viridisLite)
})

source("R/psi_helpers.R", local = TRUE)

eth_geo <- sf::read_sf("eth_geo.geojson")
irn_geo <- sf::read_sf("irn_geo.geojson")
afg_geo <- sf::read_sf("afg_geo.geojson")

indicators_path <- "indicator_components.rds"
if (!file.exists(indicators_path)) {
  stop(
    "Missing indicator_components.rds in the app directory. ",
    "Obtain a pre-built copy or regenerate from the full data pipeline.",
    call. = FALSE
  )
}
indicators <- readRDS(indicators_path)

country_geo <- list(
  Ethiopia = eth_geo,
  Iran = irn_geo,
  Afghanistan = afg_geo
)

country_periods <- lapply(
  split(indicators$period, indicators$country),
  function(x) sort(unique(x))
)

severity_pal <- leaflet::colorNumeric(
  palette = rev(viridisLite::magma(256)),
  domain = c(0, 10),
  na.color = "#bdbdbd"
)

driver_choices <- c(
  "Overall" = "Overall_severity",
  "Conflict" = "Conflict_severity",
  "Political" = "Political_severity",
  "Flood" = "Flood_severity",
  "Drought" = "Drought_severity",
  "Earthquake" = "Earthquake_severity",
  "Cyclone" = "Cyclone_severity",
  "Displacement" = "Displacement_severity"
)
