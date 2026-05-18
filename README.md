# ACAPS INFORM Proxy Severity (PSI) Dashboard

Shiny app for exploring **Proxy Severity Index (PSI)** scores and **People in Need (PiN)** by crisis driver at ADM2 level. Built by [Argot Nova LLC](https://www.argot-nova.com/) for [ACAPS](https://www.acaps.org/en/).

## Contents

| File / folder | Purpose |
|---------------|---------|
| `app.R`, `ui.R`, `server.R`, `global.R` | Shiny application |
| `R/psi_helpers.R` | Severity, weight, and aggregation logic |
| `indicator_components.rds` | Pre-processed indicator data (all countries × periods) |
| `*_geo.geojson` | ADM2 boundaries (Ethiopia, Iran, Afghanistan) |
| `www/` | ACAPS and Argot Nova logos |

## Requirements

- R 4.2+ (developed with R 4.6)
- Packages: `shiny`, `sf`, `dplyr`, `tidyr`, `tibble`, `leaflet`, `htmltools`, `viridisLite`

```r
Rscript install_packages.R
```

## Run locally

Clone the repo, set working directory to the app root (this folder), then:

```r
shiny::runApp()
```

Or:

```r
Rscript -e "shiny::runApp(launch.browser = TRUE)"
```

Optional check before launch:

```r
Rscript verify_outputs.R
```

## Updating data

This repository ships with pre-built `indicator_components.rds` and GeoJSON files. To refresh them, run the full data pipeline in the parent ACAPS workspace (`pin_proxy_update.R` against tier CSVs and kombiner GeoJSONs), then copy the outputs into this folder.

## Copyright

Dashboard software © 2026 Argot Nova LLC. All rights reserved.

Underlying ACAPS data remain subject to ACAPS terms of use.
