# server.R — entry point for Posit Connect Cloud (with ui.R; no app.R)

function(input, output, session) {
  observeEvent(input$country, {
    periods <- country_periods[[input$country]]
    if (is.null(periods) || !length(periods)) {
      updateSelectInput(session, "period", choices = character(0), selected = character(0))
      return()
    }
    selected <- tail(periods, 1)
    if (!is.null(input$period) && input$period %in% periods) {
      selected <- input$period
    }
    updateSelectInput(session, "period", choices = periods, selected = selected)
  }, ignoreNULL = FALSE)

  observeEvent(session$clientData, {
    if (is.null(input$period) || !nzchar(input$period)) {
      periods <- country_periods[["Ethiopia"]]
      if (length(periods)) {
        updateSelectInput(session, "period", choices = periods, selected = tail(periods, 1))
      }
    }
  }, once = TRUE)

  weight_val <- function(input_id, default) {
    safe_weight(input[[input_id]], default)
  }

  # Cap drought shares so SPEI + crop + grass <= 1
  observeEvent(
    c(input$share_spei, input$share_ndvi_crop),
    {
      spei <- weight_val("share_spei", default_weights$share_spei)
      crop <- weight_val("share_ndvi_crop", default_weights$share_ndvi_crop)
      if (crop > 1 - spei) {
        updateSliderInput(session, "share_ndvi_crop", value = max(0, 1 - spei))
      }
      if (spei > 1 - crop) {
        updateSliderInput(session, "share_spei", value = max(0, 1 - crop))
      }
    },
    ignoreInit = TRUE
  )

  output$share_ndvi_grass_display <- renderUI({
    spei <- weight_val("share_spei", default_weights$share_spei)
    crop <- weight_val("share_ndvi_crop", default_weights$share_ndvi_crop)
    grass <- max(0, 1 - spei - crop)
    tags$p(
      class = "share-remainder text-muted",
      sprintf("NDVI grassland share: %.0f%% (remainder)", grass * 100)
    )
  })

  output$driver_weight_sum_display <- renderUI({
    raw <- c(
      weight_val("w_conflict", default_weights$w_conflict),
      weight_val("w_political", default_weights$w_political),
      weight_val("w_flood", default_weights$w_flood),
      weight_val("w_drought", default_weights$w_drought),
      weight_val("w_earthquake", default_weights$w_earthquake),
      weight_val("w_cyclone", default_weights$w_cyclone),
      weight_val("w_displacement", default_weights$w_displacement)
    )
    s <- sum(raw)
    tags$p(
      class = "weight-sum-ok",
      sprintf(
        "Raw slider sum: %.2f — effective sum after normalization: 1.00",
        s
      )
    )
  })

  current_weights <- reactive({
    share_acled <- weight_val("share_acled_conflict", default_weights$share_acled_conflict)
    share_protests <- weight_val("share_acled_protests", default_weights$share_acled_protests)
    share_rain <- weight_val("share_high_rainfall", default_weights$share_high_rainfall)
    share_spei <- weight_val("share_spei", default_weights$share_spei)
    share_crop <- weight_val("share_ndvi_crop", default_weights$share_ndvi_crop)
    share_grass <- max(0, 1 - share_spei - share_crop)
    share_iom <- weight_val("share_iom_idp", default_weights$share_iom_idp)

    list(
      acled_conflict = share_acled,
      ucdp_conflict = 1 - share_acled,
      acled_protests = share_protests,
      nlr_index = 1 - share_protests,
      high_rainfall = share_rain,
      gdacs_flood = 1 - share_rain,
      spei = share_spei,
      ndvi_crop = share_crop,
      ndvi_grass = share_grass,
      iom_idp = share_iom,
      fbook = 1 - share_iom,
      wikipedia = weight_val("wikipedia", default_weights$wikipedia),
      goldstein = weight_val("goldstein", default_weights$goldstein),
      w_conflict = weight_val("w_conflict", default_weights$w_conflict),
      w_political = weight_val("w_political", default_weights$w_political),
      w_flood = weight_val("w_flood", default_weights$w_flood),
      w_drought = weight_val("w_drought", default_weights$w_drought),
      w_earthquake = weight_val("w_earthquake", default_weights$w_earthquake),
      w_cyclone = weight_val("w_cyclone", default_weights$w_cyclone),
      w_displacement = weight_val("w_displacement", default_weights$w_displacement),
      weighted_psi = default_weights$weighted_psi,
      max_psi = default_weights$max_psi
    )
  })

  observeEvent(input$reset_weights, {
    updateSliderInput(session, "share_acled_conflict", value = default_weights$share_acled_conflict)
    updateSliderInput(session, "share_acled_protests", value = default_weights$share_acled_protests)
    updateSliderInput(session, "share_high_rainfall", value = default_weights$share_high_rainfall)
    updateSliderInput(session, "share_spei", value = default_weights$share_spei)
    updateSliderInput(session, "share_ndvi_crop", value = default_weights$share_ndvi_crop)
    updateSliderInput(session, "share_iom_idp", value = default_weights$share_iom_idp)
    updateSliderInput(session, "wikipedia", value = default_weights$wikipedia)
    updateSliderInput(session, "goldstein", value = default_weights$goldstein)
    updateSliderInput(session, "w_conflict", value = default_weights$w_conflict)
    updateSliderInput(session, "w_political", value = default_weights$w_political)
    updateSliderInput(session, "w_flood", value = default_weights$w_flood)
    updateSliderInput(session, "w_drought", value = default_weights$w_drought)
    updateSliderInput(session, "w_earthquake", value = default_weights$w_earthquake)
    updateSliderInput(session, "w_cyclone", value = default_weights$w_cyclone)
    updateSliderInput(session, "w_displacement", value = default_weights$w_displacement)
    updateCheckboxInput(session, "overall_use_driver_weights", value = FALSE)
  })

  map_column <- reactive({
    if (input$map_var == "Overall_severity" && isTRUE(input$overall_use_driver_weights)) {
      "Weighted_severity"
    } else {
      input$map_var
    }
  })

  map_tier_column <- reactive({
    col <- map_column()
    if (col == "Weighted_severity") {
      "Weighted_severity_tier"
    } else if (col == "Overall_severity") {
      "Overall_severity_tier"
    } else {
      sub("_severity$", "_tier", col)
    }
  })

  computed <- reactive({
    req(input$country, input$period)
    w <- current_weights()
    dat <- indicators %>%
      dplyr::filter(country == input$country, period == input$period)

    if (nrow(dat) == 0) {
      return(NULL)
    }

    dat <- compute_driver_severities(dat, w)
    dat <- add_adm2_pin(dat)
    dat <- add_tiers_and_overall(dat, w)
    dat
  })

  national_summary <- reactive({
    dat <- computed()
    req(dat)
    compute_national_summary(dat, current_weights())
  })

  map_data <- reactive({
    dat <- computed()
    req(dat)
    geo <- country_geo[[input$country]]
    req(geo)

    map_col <- map_column()
    tier_col <- map_tier_column()
    dat_map <- geo %>%
      dplyr::left_join(
        dat %>%
          dplyr::select(
            adm2_id,
            dplyr::all_of(c(map_col, tier_col)),
            Conflict_severity, Political_severity, Flood_severity,
            Drought_severity, Earthquake_severity, Cyclone_severity,
            Displacement_severity,
            Conflict_tier, Political_tier, Flood_tier, Drought_tier,
            Earthquake_tier, Cyclone_tier, Displacement_tier,
            Primary_driver, Overall_severity, Weighted_severity,
            Overall_severity_tier, Weighted_severity_tier,
            conflict_pin, political_pin, flood_pin, drought_pin,
            earthquake_pin, cyclone_pin, displacement_pin, tltpop
          ),
        by = "adm2_id"
      )
    dat_map
  })

  output$map <- renderLeaflet({
    dat_map <- map_data()
    req(dat_map, nrow(dat_map) > 0)
    map_col <- map_column()
    tier_col <- map_tier_column()

    pal <- severity_pal
    fill_vals <- as.numeric(dat_map[[map_col]])
    fill_vals[!is.finite(fill_vals)] <- NA_real_
    tier_vals <- dat_map[[tier_col]]
    tier_vals[is.na(tier_vals)] <- "No risk exposure (Lvl.0)"

    popups <- paste0(
      "<b>", dat_map$gaul2_name, "</b> (", dat_map$gaul1_name, ")<br>",
      "<b>", map_col, ":</b> ", round(fill_vals, 2),
      " (", tier_vals, ")<br>",
      "<b>Primary driver:</b> ", dat_map$Primary_driver, "<br>",
      "<b>Overall (max):</b> ", round(dat_map$Overall_severity, 2),
      " | <b>Weighted:</b> ", round(dat_map$Weighted_severity, 2), "<br>",
      "<hr>",
      "Conflict: ", round(dat_map$Conflict_severity, 2),
      " | PiN: ", format(round(dat_map$conflict_pin), big.mark = ","), "<br>",
      "Political: ", round(dat_map$Political_severity, 2),
      " | PiN: ", format(round(dat_map$political_pin), big.mark = ","), "<br>",
      "Flood: ", round(dat_map$Flood_severity, 2),
      " | PiN: ", format(round(dat_map$flood_pin), big.mark = ","), "<br>",
      "Drought: ", round(dat_map$Drought_severity, 2),
      " | PiN: ", format(round(dat_map$drought_pin), big.mark = ","), "<br>",
      "Earthquake: ", round(dat_map$Earthquake_severity, 2),
      " | PiN: ", format(round(dat_map$earthquake_pin), big.mark = ","), "<br>",
      "Cyclone: ", round(dat_map$Cyclone_severity, 2),
      " | PiN: ", format(round(dat_map$cyclone_pin), big.mark = ","), "<br>",
      "Displacement: ", round(dat_map$Displacement_severity, 2),
      " | PiN: ", format(round(dat_map$displacement_pin), big.mark = ","), "<br>",
      "<hr>Pop: ", format(dat_map$tltpop, big.mark = ",")
    )

    leaflet(dat_map) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = pal(fill_vals),
        fillOpacity = 0.75,
        weight = 1,
        color = "#444444",
        opacity = 0.6,
        highlightOptions = highlightOptions(weight = 2, color = "#000", bringToFront = TRUE),
        label = paste0(
          dat_map$gaul2_name, ": ",
          ifelse(is.na(fill_vals), "—", round(fill_vals, 2)),
          " (", tier_vals, ")"
        ),
        popup = popups
      ) %>%
      addLegend(
        pal = pal,
        values = seq(0, 10, by = 2),
        opacity = 0.85,
        title = "Severity score (0–10)"
      )
  })

  output$tier3_national <- renderText({
    ns <- national_summary()
    req(ns)
    w <- current_weights()
    t3 <- ns$tier3_national
    paste0(
      "Tier-3 inputs (identical for every ADM2 in this country/period):\n",
      "  Wikipedia severity: ", round(t3$wikipedia_severity, 2),
      " → blended into Conflict at ", round(w$wikipedia * 100), "%\n",
      "  GDELT severity: ", round(t3$gdelt_severity, 2),
      " → blended into Political at ", round(w$goldstein * 100), "%"
    )
  })

  output$national_table <- renderTable({
    ns <- national_summary()
    req(ns)
    ns$driver_scores %>%
      dplyr::mutate(
        adm2_weighted = round(adm2_weighted_severity, 3),
        country_severity = round(country_severity, 3),
        driver_weight = round(driver_weight, 3),
        weighted_contribution = round(weighted_contribution, 3)
      ) %>%
      dplyr::select(
        crisis_driver, adm2_weighted, country_severity, driver_weight, weighted_contribution
      ) %>%
      as.data.frame()
  }, striped = TRUE, spacing = "s")

  output$pin_table <- renderTable({
    ns <- national_summary()
    req(ns)
    ns$national_pin %>%
      dplyr::mutate(
        national_pin = format(round(national_pin), big.mark = ","),
        pct_of_population = paste0(pct_of_population, "%")
      ) %>%
      as.data.frame()
  }, striped = TRUE)

  output$psi_score <- renderText({
    ns <- national_summary()
    req(ns)
    paste0(
      "National Proxy Severity Index (PSI): ",
      round(ns$national_psi, 3),
      "\n",
      "Max driver score: ", round(ns$max_driver_score, 3),
      " | Weighted driver sum: ", round(ns$weighted_driver_sum, 3),
      "\n",
      "(PSI = 60% max + 40% weighted sum; Tier-3 blends applied to Conflict and Political rows above)"
    )
  })
}
