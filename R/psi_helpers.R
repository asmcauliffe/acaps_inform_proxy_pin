# PSI proxy severity helpers (Shiny dashboard)

assign_severity <- function(z, direction = c("positive", "negative")) {
  direction <- match.arg(direction)
  z_adj <- if (direction == "negative") -z else z
  out <- numeric(length(z_adj))
  out[z_adj >= 1.28 & z_adj < 1.64] <- 3.3
  out[z_adj >= 1.64 & z_adj < 2.05] <- 5.0
  out[z_adj >= 2.05 & z_adj < 2.58] <- 6.7
  out[z_adj >= 2.58] <- 10
  out
}

tier_to_weight <- function(x) {
  dplyr::case_when(
    x == "No risk exposure (Lvl.0)" ~ 0,
    x == "Minimal (Lvl.1)" ~ 3.3,
    x == "Moderate (Lvl.2)" ~ 5,
    x == "Severe (Lvl.3)" ~ 6.7,
    x == "Critical (Lvl.4)" ~ 10,
    TRUE ~ NA_real_
  )
}

tier_label <- function(severity) {
  dplyr::case_when(
    is.na(severity) | severity < 3.3 ~ "No risk exposure (Lvl.0)",
    severity < 5.0 ~ "Minimal (Lvl.1)",
    severity < 6.7 ~ "Moderate (Lvl.2)",
    severity < 10 ~ "Severe (Lvl.3)",
    TRUE ~ "Critical (Lvl.4)"
  )
}

overall_tier_label <- function(severity) {
  dplyr::case_when(
    is.na(severity) | severity < 3 ~ "No risk exposure (Lvl.0)",
    severity < 4.5 ~ "Minimal (Lvl.1)",
    severity < 6 ~ "Moderate (Lvl.2)",
    severity < 9 ~ "Severe (Lvl.3)",
    TRUE ~ "Critical (Lvl.4)"
  )
}

primary_driver_from_weights <- function(
    conflict_w, political_w, flood_w, drought_w,
    earthquake_w, cyclone_w, displacement_w, overall) {
  dplyr::case_when(
    is.na(overall) | overall < 3 ~ "No risk exposure",
    conflict_w == overall ~ "Conflict/Violence",
    cyclone_w == overall ~ "Cyclone",
    displacement_w == overall ~ "Displacement",
    drought_w == overall ~ "Drought",
    earthquake_w == overall ~ "Earthquake",
    flood_w == overall ~ "Flooding",
    political_w == overall ~ "Political/Economic",
    TRUE ~ NA_character_
  )
}

# Default weights from calculation_agg.R
default_weights <- list(
  acled_conflict = 0.45,
  ucdp_conflict = 0.35,
  acled_protests = 0.25,
  nlr_index = 0.50,
  high_rainfall = 0.45,
  gdacs_flood = 0.55,
  spei = 0.50,
  ndvi_crop = 0.30,
  ndvi_grass = 0.20,
  iom_idp = 0.60,
  fbook = 0.40,
  wikipedia = 0.20,
  goldstein = 0.25,
  w_conflict = 0.20,
  w_political = 0.15,
  w_flood = 0.10,
  w_drought = 0.15,
  w_earthquake = 0.10,
  w_cyclone = 0.10,
  w_displacement = 0.20,
  weighted_psi = 0.60,
  max_psi = 0.40,
  # Default shares within each driver (sum to 1)
  share_acled_conflict = 0.45 / (0.45 + 0.35),
  share_acled_protests = 0.25 / (0.25 + 0.50),
  share_high_rainfall = 0.45 / (0.45 + 0.55),
  share_spei = 0.50,
  share_ndvi_crop = 0.30,
  share_iom_idp = 0.60
)

safe_weight <- function(x, default = 0) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }
  val <- suppressWarnings(as.numeric(x[[1]]))
  if (length(val) == 0 || !is.finite(val)) {
    return(default)
  }
  val
}

normalize_pair <- function(a, b) {
  a <- safe_weight(a, 0)
  b <- safe_weight(b, 0)
  s <- a + b
  if (!is.finite(s) || s <= 0) {
    return(c(0.5, 0.5))
  }
  c(a / s, b / s)
}

normalize_three <- function(a, b, c_val) {
  a <- safe_weight(a, 0)
  b <- safe_weight(b, 0)
  c_val <- safe_weight(c_val, 0)
  s <- a + b + c_val
  if (!is.finite(s) || s <= 0) {
    return(c(1 / 3, 1 / 3, 1 / 3))
  }
  c(a / s, b / s, c_val / s)
}

normalize_seven <- function(w) {
  w <- vapply(w, safe_weight, numeric(1), default = 0)
  s <- sum(w)
  if (!is.finite(s) || s <= 0) {
    return(rep(1 / 7, 7))
  }
  w / s
}

compute_driver_severities <- function(dat, w) {
  conflict_w <- normalize_pair(w$acled_conflict, w$ucdp_conflict)
  pol_w <- normalize_pair(w$acled_protests, w$nlr_index)
  flood_w <- normalize_pair(w$high_rainfall, w$gdacs_flood)
  drought_w <- normalize_three(w$spei, w$ndvi_crop, w$ndvi_grass)
  disp_w <- normalize_pair(w$iom_idp, w$fbook)

  dat %>%
    dplyr::mutate(
      Conflict_severity = conflict_w[1] * acled_conflict_adm2_sev +
        conflict_w[2] * ucdp_conflict_adm2_sev,
      Political_severity = pol_w[1] * acled_protests_adm2_sev + pol_w[2] * nlr_gate,
      Flood_severity = flood_w[1] * high_rainfall_adm2_sev + flood_w[2] * gdacs_flood_sev,
      Drought_severity = drought_w[1] * spei_adm2_sev +
        drought_w[2] * ndvi_crop_gate + drought_w[3] * ndvi_grass_gate,
      Earthquake_severity = gdacs_eq_sev,
      Cyclone_severity = gdacs_cyc_sev,
      Displacement_severity = disp_w[1] * iom_severity + disp_w[2] * fbook_severity
    )
}

add_adm2_pin <- function(dat) {
  dat %>%
    dplyr::mutate(
      conflict_pin = dplyr::if_else(
        pmax(acled_conflict_adm2_sev, ucdp_conflict_adm2_sev, na.rm = TRUE) >= 3.3,
        tltpop, 0
      ),
      political_pin = pmax(
        dplyr::if_else(acled_protests_adm2_sev >= 3.3, tltpop, 0),
        tltpop * nlr_gate,
        na.rm = TRUE
      ),
      flood_pin = dplyr::if_else(high_rainfall_adm2_sev >= 3.3, tltpop, 0) * glofas_gate,
      drought_pin = dplyr::if_else(spei_adm2_sev >= 3.3, tltpop, 0) *
        ((ndvi_crop_gate + ndvi_grass_gate) / 2),
      earthquake_pin = tltpop * earthquake_gate,
      cyclone_pin = tltpop * cyclone_gate,
      displacement_pin = dplyr::coalesce(idp_cnt, 0)
    )
}

summarise_national_pin_by_driver <- function(dat) {
  total_pop <- sum(dat$tltpop, na.rm = TRUE)
  pin_vals <- c(
    sum(dat$conflict_pin, na.rm = TRUE),
    sum(dat$political_pin, na.rm = TRUE),
    sum(dat$flood_pin, na.rm = TRUE),
    sum(dat$drought_pin, na.rm = TRUE),
    sum(dat$earthquake_pin, na.rm = TRUE),
    sum(dat$cyclone_pin, na.rm = TRUE),
    sum(dat$displacement_pin, na.rm = TRUE)
  )
  drivers <- c(
    "Conflict", "Political", "Flood", "Drought",
    "Earthquake", "Cyclone", "Displacement"
  )
  tibble::tibble(
    crisis_driver = drivers,
    national_pin = pin_vals,
    pct_of_population = if (total_pop > 0) round(100 * pin_vals / total_pop, 1) else NA_real_
  )
}

add_tiers_and_overall <- function(dat, driver_w) {
  dw <- normalize_seven(c(
    driver_w$w_conflict, driver_w$w_political, driver_w$w_flood, driver_w$w_drought,
    driver_w$w_earthquake, driver_w$w_cyclone, driver_w$w_displacement
  ))

  dat %>%
    dplyr::mutate(
      Conflict_tier = tier_label(Conflict_severity),
      Political_tier = tier_label(Political_severity),
      Flood_tier = tier_label(Flood_severity),
      Drought_tier = tier_label(Drought_severity),
      Earthquake_tier = tier_label(Earthquake_severity),
      Cyclone_tier = tier_label(Cyclone_severity),
      Displacement_tier = tier_label(Displacement_severity),
      Conflict_tier_weight = tier_to_weight(Conflict_tier),
      Political_tier_weight = tier_to_weight(Political_tier),
      Flood_tier_weight = tier_to_weight(Flood_tier),
      Drought_tier_weight = tier_to_weight(Drought_tier),
      Earthquake_tier_weight = tier_to_weight(Earthquake_tier),
      Cyclone_tier_weight = tier_to_weight(Cyclone_tier),
      Displacement_tier_weight = tier_to_weight(Displacement_tier),
      Overall_severity = pmax(
        Conflict_tier_weight, Political_tier_weight, Flood_tier_weight,
        Drought_tier_weight, Earthquake_tier_weight, Cyclone_tier_weight,
        Displacement_tier_weight, na.rm = TRUE
      ),
      Primary_driver = primary_driver_from_weights(
        Conflict_tier_weight, Political_tier_weight, Flood_tier_weight,
        Drought_tier_weight, Earthquake_tier_weight, Cyclone_tier_weight,
        Displacement_tier_weight, Overall_severity
      ),
      Overall_severity_tier = overall_tier_label(Overall_severity),
      Weighted_severity = Conflict_severity * dw[1] +
        Political_severity * dw[2] +
        Flood_severity * dw[3] +
        Drought_severity * dw[4] +
        Earthquake_severity * dw[5] +
        Cyclone_severity * dw[6] +
        Displacement_severity * dw[7],
      Weighted_severity_tier = tier_label(Weighted_severity)
    )
}

compute_national_summary <- function(dat, w) {
  drivers <- c(
    "Conflict", "Political", "Flood", "Drought",
    "Earthquake", "Cyclone", "Displacement"
  )
  sev_cols <- paste0(drivers, "_severity")

  country_scores <- dat %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(sev_cols),
        ~ sum(.x * tltpop, na.rm = TRUE) / sum(tltpop, na.rm = TRUE),
        .names = "{.col}"
      ),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = "crisis_driver",
      values_to = "adm2_weighted_severity"
    ) %>%
    dplyr::mutate(crisis_driver = sub("_severity$", "", crisis_driver))

  wiki_sev <- dplyr::first(dat$wikipedia_severity)
  gdelt_sev <- dplyr::first(dat$gdelt_severity)

  country_scores <- country_scores %>%
    dplyr::mutate(
      country_severity = dplyr::case_when(
        crisis_driver == "Conflict" ~
          adm2_weighted_severity * (1 - w$wikipedia) + wiki_sev * w$wikipedia,
        crisis_driver == "Political" ~
          adm2_weighted_severity * (1 - w$goldstein) + gdelt_sev * w$goldstein,
        TRUE ~ adm2_weighted_severity
      )
    )

  max_score <- max(country_scores$country_severity, na.rm = TRUE)
  dw <- normalize_seven(c(
    w$w_conflict, w$w_political, w$w_flood, w$w_drought,
    w$w_earthquake, w$w_cyclone, w$w_displacement
  ))
  driver_map <- c(
    Conflict = dw[1], Political = dw[2], Flood = dw[3], Drought = dw[4],
    Earthquake = dw[5], Cyclone = dw[6], Displacement = dw[7]
  )
  country_scores <- country_scores %>%
    dplyr::mutate(
      driver_weight = driver_map[crisis_driver],
      weighted_contribution = country_severity * driver_map[crisis_driver]
    )
  weighted_sum <- sum(country_scores$weighted_contribution, na.rm = TRUE)
  psi <- w$weighted_psi * max_score + w$max_psi * weighted_sum

  national_pin <- summarise_national_pin_by_driver(dat)

  list(
    driver_scores = country_scores,
    national_psi = psi,
    max_driver_score = max_score,
    weighted_driver_sum = weighted_sum,
    national_pin = national_pin,
    tier3_national = list(
      wikipedia_severity = wiki_sev,
      gdelt_severity = gdelt_sev
    )
  )
}
