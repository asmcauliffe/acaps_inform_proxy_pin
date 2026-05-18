# Install R package dependencies. Run once: Rscript install_packages.R

required <- c(
  "shiny", "sf", "dplyr", "tidyr", "tibble",
  "leaflet", "htmltools", "viridisLite"
)

missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

cat("All required packages are installed.\n")
