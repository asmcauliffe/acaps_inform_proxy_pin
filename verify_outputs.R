# Quick pre-launch verification (run from this app directory)
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})
source("R/psi_helpers.R")

stopifnot(file.exists("indicator_components.rds"))
ind <- readRDS("indicator_components.rds")

cat("=== indicator_components.rds ===\n")
cat("Rows:", nrow(ind), "\n")
cat("Columns:", length(names(ind)), "\n\n")
print(count(ind, country, period))
cat("\nADM2 per country:\n")
print(ind %>% group_by(country) %>% summarise(n_adm2 = n_distinct(adm2_id), .groups = "drop"))

t1_cols <- c("conflict_t1_sev", "acled_protests_adm2_sev", "high_rainfall_adm2_sev", "spei_adm2_sev")
cat("\nTier-1 NA counts:\n")
print(colSums(is.na(ind[t1_cols])))

cat("\nTier-3 NA:", sum(is.na(ind$wikipedia_severity)), "wikipedia,",
    sum(is.na(ind$gdelt_severity)), "gdelt\n")

expected <- c(Ethiopia = 92L, Iran = 427L, Afghanistan = 391L)
for (nm in names(expected)) {
  n <- ind %>% filter(country == nm) %>% pull(adm2_id) %>% n_distinct()
  if (n != expected[[nm]]) {
    warning(nm, ": expected ", expected[[nm]], " ADM2, got ", n)
  }
}

cat("\n=== GeoJSON ===\n")
for (f in c("eth_geo.geojson", "irn_geo.geojson", "afg_geo.geojson")) {
  g <- read_sf(f)
  cat(f, ":", nrow(g), "features, CRS =", st_crs(g)$epsg, "\n")
}

cat("\n=== Computation smoke test ===\n")
w <- default_weights
test <- ind %>% filter(country == "Ethiopia", period == "March 2022")
out <- test %>% compute_driver_severities(w) %>% add_tiers_and_overall(w)
cat("Ethiopia March 2022:", nrow(out), "rows\n")
print(table(out$Overall_severity_tier))
ns <- compute_national_summary(out, w)
cat("National PSI:", round(ns$national_psi, 3), "\n")
cat("\nOK — ready to run shiny::runApp()\n")
