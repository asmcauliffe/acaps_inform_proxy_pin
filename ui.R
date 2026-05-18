# ui.R

weight_details <- function(title, expanded = FALSE, ...) {
  attrs <- list(class = "weight-panel")
  if (isTRUE(expanded)) {
    attrs$open <- NA_character_
  }
  do.call(
    tags$details,
    c(
      attrs,
      list(
        tags$summary(class = "weight-panel-title", title),
        div(class = "weight-panel-content", ...)
      )
    )
  )
}

share_pair_ui <- function(share_id, primary_label, other_label, default_share) {
  tagList(
    sliderInput(
      share_id, primary_label,
      min = 0, max = 1, value = default_share, step = 0.05
    ),
    tags$p(
      class = "share-remainder text-muted",
      sprintf("%s share: %.0f%% (updates automatically)", other_label, (1 - default_share) * 100),
      id = paste0(share_id, "_remainder")
    )
  )
}

ui <- fluidPage(
  title = "PSI Proxy Severity | ACAPS",
  tags$head(
    tags$style(HTML("
      .weight-panel { margin-bottom: 0.5rem; border: 1px solid #ddd; border-radius: 4px; padding: 0 0.5rem 0.5rem; }
      .weight-panel > summary.weight-panel-title {
        cursor: pointer;
        font-weight: 600;
        padding: 0.4rem 0;
        display: list-item;
        list-style-position: outside;
      }
      .weight-panel > summary.weight-panel-title::-webkit-details-marker { display: inline-block; }
      .weight-panel .form-group { margin-bottom: 0.6rem; }
      .weight-mode-hidden { display: none !important; }
      .share-remainder { font-size: 0.85em; margin-top: -0.5rem; margin-bottom: 0.5rem; }
      .weight-sum-ok { color: #2c662d; font-size: 0.9em; }
      .app-header {
        position: relative;
        display: flex;
        align-items: center;
        min-height: 64px;
        margin-bottom: 1.25rem;
        padding-bottom: 1rem;
        border-bottom: 1px solid #ddd;
      }
      .app-header-brand,
      .app-footer-brand {
        position: relative;
        z-index: 2;
        line-height: 0;
        text-decoration: none;
      }
      .app-header-brand:hover,
      .app-footer-brand:hover { opacity: 0.85; }
      .app-header-logo { height: 52px; width: auto; display: block; }
      .app-header-title-wrap {
        position: absolute;
        left: 0;
        right: 0;
        text-align: center;
        pointer-events: none;
      }
      .app-title {
        margin: 0;
        font-size: 2.25rem;
        font-weight: 600;
        line-height: 1.2;
      }
      .app-footer {
        position: relative;
        margin-top: 2rem;
        padding-top: 1.25rem;
        padding-bottom: 0.5rem;
        border-top: 1px solid #ddd;
        min-height: 52px;
        display: flex;
        align-items: center;
        color: #555;
        font-size: 0.9rem;
      }
      .app-footer-logo { height: 36px; width: auto; display: block; }
      .app-footer-text {
        position: absolute;
        left: 0;
        right: 0;
        text-align: center;
      }
      .app-footer-text p { margin: 0; }
      .app-footer-text a { pointer-events: auto; }
    "))
  ),
  div(
    class = "app-header",
    tags$a(
      href = "https://www.acaps.org/en/",
      target = "_blank",
      rel = "noopener noreferrer",
      class = "app-header-brand",
      title = "ACAPS",
      tags$img(
        src = "acaps_logo.svg",
        class = "app-header-logo",
        alt = "ACAPS"
      )
    ),
    div(
      class = "app-header-title-wrap",
      tags$h1(class = "app-title", "PSI Proxy Severity Dashboard")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      selectInput("country", "Country", choices = names(country_geo), selected = "Ethiopia"),
      selectInput("period", "Period", choices = NULL),
      selectInput("map_var", "Map variable", choices = driver_choices, selected = "Overall_severity"),
      conditionalPanel(
        condition = "input.map_var == 'Overall_severity'",
        checkboxInput(
          "overall_use_driver_weights",
          "Overall map: use driver-weighted blend (unchecked = max driver, per methodology)",
          value = FALSE
        )
      ),
      hr(),
      radioButtons(
        "weight_mode",
        "Adjust weights",
        choices = c(
          "Inputs within each driver" = "indicator",
          "One weight per driver" = "driver"
        ),
        selected = "indicator",
        inline = TRUE
      ),
      div(
        id = "indicator_weights_panel",
        helpText(
          "One slider sets the primary input share; the complementary share is the remainder (always sums to 1 within each driver)."
        ),
        weight_details(
          "Conflict / Violence",
          expanded = TRUE,
          share_pair_ui(
            "share_acled_conflict",
            "ACLED armed conflict share (standard)",
            "UCDP conflict (proxy)",
            default_weights$share_acled_conflict
          )
        ),
        weight_details(
          "Political / Economic",
          expanded = TRUE,
          share_pair_ui(
            "share_acled_protests",
            "ACLED protests share",
            "NLR index",
            default_weights$share_acled_protests
          )
        ),
        weight_details(
          "Flood",
          expanded = TRUE,
          share_pair_ui(
            "share_high_rainfall",
            "High rainfall share",
            "GDACS flood",
            default_weights$share_high_rainfall
          )
        ),
        weight_details(
          "Drought",
          sliderInput(
            "share_spei", "SPEI share",
            min = 0, max = 1, value = default_weights$share_spei, step = 0.05
          ),
          sliderInput(
            "share_ndvi_crop", "NDVI cropland share",
            min = 0, max = 1, value = default_weights$share_ndvi_crop, step = 0.05
          ),
          uiOutput("share_ndvi_grass_display")
        ),
        weight_details(
          "Displacement",
          expanded = TRUE,
          share_pair_ui(
            "share_iom_idp",
            "IOM IDP share (standard)",
            "Facebook mobility (proxy)",
            default_weights$share_iom_idp
          )
        ),
        weight_details(
          "Tier-3 national blend",
          helpText("National PSI only — not part of within-driver shares above."),
          sliderInput(
            "wikipedia", "Wikipedia blend into Conflict (national)",
            min = 0, max = 1, value = default_weights$wikipedia, step = 0.05
          ),
          sliderInput(
            "goldstein", "GDELT blend into Political (national)",
            min = 0, max = 1, value = default_weights$goldstein, step = 0.05
          )
        )
      ),
      tags$div(
        id = "driver_weights_panel",
        class = "weight-mode-hidden",
        helpText(
          "Seven driver weights (renormalized to sum to 1 for PSI and optional weighted overall map)."
        ),
        weight_details(
          "Crisis driver weights (7)",
          expanded = TRUE,
          sliderInput("w_conflict", "Conflict / Violence", min = 0, max = 1, value = default_weights$w_conflict, step = 0.05),
          sliderInput("w_political", "Political / Economic", min = 0, max = 1, value = default_weights$w_political, step = 0.05),
          sliderInput("w_flood", "Flood", min = 0, max = 1, value = default_weights$w_flood, step = 0.05),
          sliderInput("w_drought", "Drought", min = 0, max = 1, value = default_weights$w_drought, step = 0.05),
          sliderInput("w_earthquake", "Earthquake", min = 0, max = 1, value = default_weights$w_earthquake, step = 0.05),
          sliderInput("w_cyclone", "Cyclone", min = 0, max = 1, value = default_weights$w_cyclone, step = 0.05),
          sliderInput("w_displacement", "Displacement", min = 0, max = 1, value = default_weights$w_displacement, step = 0.05),
          htmlOutput("driver_weight_sum_display")
        )
      ),
      tags$script(HTML("
        $(function() {
          function toggleWeightPanels() {
            var mode = $('input[name=\"weight_mode\"]:checked').val();
            if (mode === 'indicator') {
              $('#indicator_weights_panel').removeClass('weight-mode-hidden');
              $('#driver_weights_panel').addClass('weight-mode-hidden');
            } else {
              $('#indicator_weights_panel').addClass('weight-mode-hidden');
              $('#driver_weights_panel').removeClass('weight-mode-hidden');
            }
          }
          function updateRemainder(shareId, otherLabel) {
            var v = parseFloat($('#' + shareId).val());
            if (isNaN(v)) v = 0;
            var other = Math.max(0, 1 - v);
            $('#' + shareId + '_remainder').text(
              otherLabel + ' share: ' + Math.round(other * 100) + '% (remainder)'
            );
          }
          $(document).on('change', 'input[name=\"weight_mode\"]', toggleWeightPanels);
          $(document).on('input change', '#share_acled_conflict', function() {
            updateRemainder('share_acled_conflict', 'UCDP conflict (proxy)');
          });
          $(document).on('input change', '#share_acled_protests', function() {
            updateRemainder('share_acled_protests', 'NLR index');
          });
          $(document).on('input change', '#share_high_rainfall', function() {
            updateRemainder('share_high_rainfall', 'GDACS flood');
          });
          $(document).on('input change', '#share_iom_idp', function() {
            updateRemainder('share_iom_idp', 'Facebook mobility (proxy)');
          });
          toggleWeightPanels();
          updateRemainder('share_acled_conflict', 'UCDP conflict (proxy)');
          updateRemainder('share_acled_protests', 'NLR index');
          updateRemainder('share_high_rainfall', 'GDACS flood');
          updateRemainder('share_iom_idp', 'Facebook mobility (proxy)');
        });
      ")),
      hr(),
      actionButton("reset_weights", "Reset to defaults", class = "btn-secondary")
    ),
    mainPanel(
      width = 8,
      leafletOutput("map", height = 520),
      br(),
      h4("National severity by driver"),
      helpText(
        "Tier 1–2: population-weighted mean across ADM2. Tier 3 (Wikipedia, GDELT): one national score per country, blended into Conflict and Political only."
      ),
      tableOutput("national_table"),
      verbatimTextOutput("tier3_national"),
      br(),
      h4("People in Need by driver (national)"),
      helpText("National totals summed across ADM2, using tier 1–2 inputs in the dashboard datasets."),
      tableOutput("pin_table"),
      br(),
      verbatimTextOutput("psi_score")
    )
  ),
  tags$footer(
    class = "app-footer",
    tags$a(
      href = "https://www.argot-nova.com/",
      target = "_blank",
      rel = "noopener noreferrer",
      class = "app-footer-brand",
      title = "Argot Nova LLC",
      tags$img(
        src = "small_logo.png",
        class = "app-footer-logo",
        alt = "Argot Nova LLC"
      )
    ),
    div(
      class = "app-footer-text",
      tags$p(
        "Dashboard developed by ",
        tags$a(
          href = "https://www.argot-nova.com/",
          target = "_blank",
          rel = "noopener noreferrer",
          "Argot Nova LLC"
        ),
        " for ",
        tags$a(
          href = "https://www.acaps.org/en/",
          target = "_blank",
          rel = "noopener noreferrer",
          "ACAPS"
        ),
        ". \u00a9 2026 Argot Nova LLC. All rights reserved."
      )
    )
  )
)
