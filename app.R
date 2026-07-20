# ==============================================================================
# Interactive Air Quality Index (AQI) Explorer
# ==============================================================================
#
# A single-file R Shiny app that converts raw pollutant concentrations into
# a composite Air Quality Index under two national standards:
#   - US EPA AQI      (breakpoints per the EPA's May 2024 AQI revision)
#   - India NAQI/CPCB  (breakpoints per CPCB's 2014 National Air Quality Index)
#
# See README.md for full details, screenshots, and setup instructions.
#
# ---- Quick start -------------------------------------------------------------
# install.packages(c("shiny", "bslib", "ggplot2", "plotly", "DT"))
# shiny::runApp()
#
# Requires R >= 4.1 (uses the native pipe |>) and bslib >= 0.5
# (for page_sidebar() / sidebar() / card() / layout_columns()).
#
# ---- How this file is organized ----------------------------------------------
#   1. Helpers        - generic math: truncation, sub-index interpolation,
#                        category lookup. Standard-agnostic; used by both.
#   2. Standards       - ALL breakpoints, categories, colors, units, and advisory
#                        text live in one nested list (`standards`). This is the
#                        single source of truth: to add a pollutant, edit a
#                        national standard, or add a brand-new standard
#                        altogether, this is the only section that needs to
#                        change — the UI and server code below read from it
#                        generically and don't hardcode any pollutant names.
#   3. Presets         - illustrative example scenarios for the sidebar's
#                        "Try an example" dropdown (NOT live monitoring data).
#   4. UI              - bslib sidebar + card layout.
#   5. Server          - reactive pipeline: read inputs -> per-pollutant
#                        sub-indices (`results()`) -> composite index
#                        (`overall()`) -> render gauge/table/chart/report.
# ==============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(DT)

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

# EPA/CPCB convention: TRUNCATE (don't round) to a fixed precision before
# comparing a reading against the breakpoint table.
truncate_to <- function(x, decimals) {
  factor <- 10 ^ decimals
  trunc(x * factor) / factor
}

# Generic piecewise-linear AQI/sub-index calculator.
#   I = ((I_high - I_low)/(C_high - C_low))*(C - C_low) + I_low
# Returns a status alongside the value so the UI can tell "not entered"
# apart from "entered, but off the top of the scale".
calc_subindex <- function(conc, breakpoints) {
  if (is.na(conc) || conc < 0) {
    return(list(value = NA_real_, status = "missing"))
  }
  idx <- which(conc >= breakpoints$conc_low & conc <= breakpoints$conc_high)
  if (length(idx) == 0) {
    if (conc > max(breakpoints$conc_high)) {
      top <- breakpoints[nrow(breakpoints), ]
      return(list(value = top$AQI_high, status = "offscale"))
    }
    return(list(value = NA_real_, status = "missing"))
  }
  bp <- breakpoints[idx[1], ]
  val <- ((bp$AQI_high - bp$AQI_low) / (bp$conc_high - bp$conc_low)) *
    (conc - bp$conc_low) + bp$AQI_low
  list(value = round(val), status = "ok")
}

# Look up which category row a given index value falls into.
get_category <- function(value, categories) {
  idx <- which(value >= categories$low & value <= categories$high)
  if (length(idx) == 0) {
    if (value > max(categories$high)) return(categories[nrow(categories), ])
    return(categories[1, ])
  }
  categories[idx[1], ]
}

dark_bg_categories <- c("Unhealthy", "Very Unhealthy", "Hazardous", "Very Poor", "Severe")

# ------------------------------------------------------------------------------
# Standards: breakpoints, categories, units — one place to look, one place
# to edit. Adding a pollutant or a whole new national standard means adding
# an entry here; no other code needs to change.
# ------------------------------------------------------------------------------

epa_categories <- data.frame(
  label = c("Good", "Moderate", "Unhealthy for Sensitive Groups",
            "Unhealthy", "Very Unhealthy", "Hazardous"),
  low   = c(0, 51, 101, 151, 201, 301),
  high  = c(50, 100, 150, 200, 300, 500),
  color = c("#00E400", "#FFFF00", "#FF7E00", "#FF0000", "#8F3F97", "#7E0023"),
  advisory = c(
    "Air quality is satisfactory and poses little or no risk.",
    "Acceptable air quality; unusually sensitive individuals should consider limiting prolonged outdoor exertion.",
    "Sensitive groups (children, older adults, people with heart or lung conditions) may experience health effects.",
    "Everyone may begin to experience health effects; sensitive groups may experience more serious effects.",
    "Health alert: everyone may experience more serious health effects.",
    "Health warning of emergency conditions; the entire population is likely to be affected."
  ),
  stringsAsFactors = FALSE
)

cpcb_categories <- data.frame(
  label = c("Good", "Satisfactory", "Moderate", "Poor", "Very Poor", "Severe"),
  low   = c(0, 51, 101, 201, 301, 401),
  high  = c(50, 100, 200, 300, 400, 500),
  color = c("#00E400", "#A3E635", "#FFFF00", "#FF7E00", "#FF0000", "#7E0023"),
  advisory = c(
    "Minimal impact on health.",
    "May cause minor breathing discomfort to sensitive people.",
    "May cause breathing discomfort to people with lung disease, children, and older adults.",
    "May cause breathing discomfort to most people on prolonged exposure.",
    "May cause respiratory illness on prolonged exposure.",
    "May affect healthy people and seriously impact those with existing conditions."
  ),
  stringsAsFactors = FALSE
)

standards <- list(
  EPA = list(
    label = "US EPA (AQI)",
    categories = epa_categories,
    min_pollutants = NULL,
    pollutants = list(
      PM25 = list(label = "PM2.5", unit = "\u00b5g/m\u00b3 (24-hr)", step = 0.1, decimals = 1,
                  bp = data.frame(AQI_low = c(0, 51, 101, 151, 201, 301),
                                   AQI_high = c(50, 100, 150, 200, 300, 500),
                                   conc_low = c(0.0, 9.1, 35.5, 55.5, 125.5, 225.5),
                                   conc_high = c(9.0, 35.4, 55.4, 125.4, 225.4, 325.4))),
      PM10 = list(label = "PM10", unit = "\u00b5g/m\u00b3 (24-hr)", step = 1, decimals = 0,
                  bp = data.frame(AQI_low = c(0, 51, 101, 151, 201, 301),
                                   AQI_high = c(50, 100, 150, 200, 300, 500),
                                   conc_low = c(0, 55, 155, 255, 355, 425),
                                   conc_high = c(54, 154, 254, 354, 424, 604))),
      O3   = list(label = "O\u2083", unit = "ppm (8-hr)", step = 0.001, decimals = 3,
                  bp = data.frame(AQI_low = c(0, 51, 101, 151, 201),
                                   AQI_high = c(50, 100, 150, 200, 300),
                                   conc_low = c(0.000, 0.055, 0.071, 0.086, 0.106),
                                   conc_high = c(0.054, 0.070, 0.085, 0.105, 0.200))),
      CO   = list(label = "CO", unit = "ppm (8-hr)", step = 0.1, decimals = 1,
                  bp = data.frame(AQI_low = c(0, 51, 101, 151, 201, 301),
                                   AQI_high = c(50, 100, 150, 200, 300, 500),
                                   conc_low = c(0.0, 4.5, 9.5, 12.5, 15.5, 30.5),
                                   conc_high = c(4.4, 9.4, 12.4, 15.4, 30.4, 50.4))),
      NO2  = list(label = "NO\u2082", unit = "ppb (1-hr)", step = 1, decimals = 0,
                  bp = data.frame(AQI_low = c(0, 51, 101, 151, 201, 301),
                                   AQI_high = c(50, 100, 150, 200, 300, 500),
                                   conc_low = c(0, 54, 101, 361, 650, 1250),
                                   conc_high = c(53, 100, 360, 649, 1249, 2049))),
      SO2  = list(label = "SO\u2082", unit = "ppb (1-hr)", step = 1, decimals = 0,
                  bp = data.frame(AQI_low = c(0, 51, 101, 151, 201, 301),
                                   AQI_high = c(50, 100, 150, 200, 300, 500),
                                   conc_low = c(0, 36, 76, 186, 305, 605),
                                   conc_high = c(35, 75, 185, 304, 604, 1004)))
    )
  ),
  CPCB = list(
    label = "India NAQI (CPCB)",
    categories = cpcb_categories,
    min_pollutants = 3,
    pollutants = list(
      PM25 = list(label = "PM2.5", unit = "\u00b5g/m\u00b3 (24-hr)", step = 1, decimals = 0,
                  bp = data.frame(AQI_low = c(0, 51, 101, 201, 301, 401),
                                   AQI_high = c(50, 100, 200, 300, 400, 500),
                                   conc_low = c(0, 31, 61, 91, 121, 251),
                                   conc_high = c(30, 60, 90, 120, 250, 500))),
      PM10 = list(label = "PM10", unit = "\u00b5g/m\u00b3 (24-hr)", step = 1, decimals = 0,
                  bp = data.frame(AQI_low = c(0, 51, 101, 201, 301, 401),
                                   AQI_high = c(50, 100, 200, 300, 400, 500),
                                   conc_low = c(0, 51, 101, 251, 351, 431),
                                   conc_high = c(50, 100, 250, 350, 430, 600))),
      NO2  = list(label = "NO\u2082", unit = "\u00b5g/m\u00b3 (24-hr)", step = 1, decimals = 0,
                  bp = data.frame(AQI_low = c(0, 51, 101, 201, 301, 401),
                                   AQI_high = c(50, 100, 200, 300, 400, 500),
                                   conc_low = c(0, 41, 81, 181, 281, 401),
                                   conc_high = c(40, 80, 180, 280, 400, 600))),
      SO2  = list(label = "SO\u2082", unit = "\u00b5g/m\u00b3 (24-hr)", step = 1, decimals = 0,
                  bp = data.frame(AQI_low = c(0, 51, 101, 201, 301, 401),
                                   AQI_high = c(50, 100, 200, 300, 400, 500),
                                   conc_low = c(0, 41, 81, 381, 801, 1601),
                                   conc_high = c(40, 80, 380, 800, 1600, 2000))),
      CO   = list(label = "CO", unit = "mg/m\u00b3 (8-hr)", step = 0.1, decimals = 1,
                  bp = data.frame(AQI_low = c(0, 51, 101, 201, 301, 401),
                                   AQI_high = c(50, 100, 200, 300, 400, 500),
                                   conc_low = c(0.0, 1.1, 2.1, 10.1, 17.1, 34.1),
                                   conc_high = c(1.0, 2.0, 10.0, 17.0, 34.0, 50.0))),
      O3   = list(label = "O\u2083", unit = "\u00b5g/m\u00b3 (8-hr)*", step = 1, decimals = 0,
                  bp = data.frame(AQI_low = c(0, 51, 101, 201, 301, 401),
                                   AQI_high = c(50, 100, 200, 300, 400, 500),
                                   conc_low = c(0, 51, 101, 169, 209, 749),
                                   conc_high = c(50, 100, 168, 208, 748, 1000))),
      NH3  = list(label = "NH\u2083", unit = "\u00b5g/m\u00b3 (24-hr)", step = 1, decimals = 0,
                  bp = data.frame(AQI_low = c(0, 51, 101, 201, 301, 401),
                                   AQI_high = c(50, 100, 200, 300, 400, 500),
                                   conc_low = c(0, 201, 401, 801, 1201, 1801),
                                   conc_high = c(200, 400, 800, 1200, 1800, 2400))),
      PB   = list(label = "Pb", unit = "\u00b5g/m\u00b3 (24-hr)", step = 0.1, decimals = 1,
                  bp = data.frame(AQI_low = c(0, 51, 101, 201, 301, 401),
                                   AQI_high = c(50, 100, 200, 300, 400, 500),
                                   conc_low = c(0.0, 0.51, 1.1, 2.1, 3.1, 3.51),
                                   conc_high = c(0.5, 1.0, 2.0, 3.0, 3.5, 5.0)))
    )
  )
)

# Illustrative example scenarios (not live monitoring data) for the
# "Try an example" preset selector.
presets <- list(
  EPA = list(
    "Clean day"          = c(PM25 = 6,   PM10 = 15,  O3 = 0.030, CO = 0.4, NO2 = 10, SO2 = 2),
    "Typical urban day"  = c(PM25 = 20,  PM10 = 45,  O3 = 0.062, CO = 1.2, NO2 = 35, SO2 = 10),
    "Wildfire smoke"     = c(PM25 = 180, PM10 = 220, O3 = 0.050, CO = 2.0, NO2 = 15, SO2 = 5),
    "Hazardous episode"  = c(PM25 = 300, PM10 = 380, O3 = 0.090, CO = 8.0, NO2 = 90, SO2 = 40)
  ),
  CPCB = list(
    "Clean day"          = c(PM25 = 15,  PM10 = 40,  NO2 = 20,  SO2 = 10,  CO = 0.6, O3 = 30, NH3 = 50,  PB = 0.2),
    "Typical urban day"  = c(PM25 = 65,  PM10 = 140, NO2 = 55,  SO2 = 25,  CO = 1.4, O3 = 60, NH3 = 150, PB = 0.6),
    "Delhi winter smog"  = c(PM25 = 280, PM10 = 350, NO2 = 90,  SO2 = 30,  CO = 3.5, O3 = 40, NH3 = 300, PB = 1.2),
    "Severe episode"     = c(PM25 = 450, PM10 = 500, NO2 = 250, SO2 = 500, CO = 12,  O3 = 90, NH3 = 900, PB = 3.2)
  )
)

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

ui <- page_sidebar(
  title = "Interactive Air Quality Index Explorer",
  theme = bs_theme(
    version = 5, bootswatch = "flatly",
    primary = "#2C7FB8",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  sidebar = sidebar(
    width = 340,
    radioButtons(
      "standard", "Standard",
      choices = c("US EPA (AQI)" = "EPA", "India NAQI (CPCB)" = "CPCB"),
      selected = "EPA"
    ),
    tags$hr(),
    uiOutput("presetSelector"),
    actionButton("load_preset", "Load example", icon = icon("bolt"),
                 class = "btn-sm btn-outline-primary"),
    tags$hr(),
    h5("Pollutant concentrations"),
    uiOutput("pollutantInputs"),
    actionButton("reset_btn", "Reset all", icon = icon("rotate-left"),
                 class = "btn-sm btn-outline-secondary"),
    tags$hr(),
    checkboxInput("show_life", "Show life-expectancy context (PM2.5)", value = TRUE),
    downloadButton("downloadReport", "Download report", class = "btn-sm w-100")
  ),
  layout_columns(
    col_widths = c(5, 7),
    card(
      card_header("Overall Air Quality"),
      plotlyOutput("aqiGauge", height = "260px"),
      uiOutput("categoryBanner"),
      uiOutput("lifeExpectancyNote")
    ),
    card(
      card_header("Pollutant Breakdown"),
      DT::dataTableOutput("aqiTable"),
      plotlyOutput("aqiPlot", height = "300px")
    )
  ),
  tags$footer(
    style = "margin-top:1.5rem; font-size:0.78em; color:#6c757d;",
    "PM2.5 breakpoints reflect the EPA's May 2024 AQI revision. NAQI breakpoints follow CPCB's ",
    "2014 National Air Quality Index. Life-expectancy figures follow AQLI's methodology ",
    "(Ebenstein et al., 2017) relative to the WHO annual PM2.5 guideline of 5 \u00b5g/m\u00b3. ",
    "Example scenarios are illustrative, not live monitoring data."
  )
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

server <- function(input, output, session) {

  current_standard <- reactive({
    standards[[input$standard]]
  })

  # ---- Dynamic pollutant inputs, generated from the standards list ----
  output$pollutantInputs <- renderUI({
    std <- current_standard()
    tagList(
      lapply(names(std$pollutants), function(k) {
        p <- std$pollutants[[k]]
        field <- numericInput(paste0("conc_", k), paste0(p$label, " (", p$unit, ")"),
                               value = NA, min = 0, step = p$step)
        if (k == "O3" && input$standard == "CPCB") {
          tagList(field, helpText(
            "*Above 208 \u00b5g/m\u00b3, CPCB switches to the 1-hour average for Very Poor/Severe."
          ))
        } else {
          field
        }
      })
    )
  })

  output$presetSelector <- renderUI({
    selectInput("preset_choice", "Try an example scenario",
                choices = names(presets[[input$standard]]))
  })

  observeEvent(input$load_preset, {
    req(input$preset_choice)
    vals <- presets[[input$standard]][[input$preset_choice]]
    for (nm in names(vals)) {
      updateNumericInput(session, paste0("conc_", nm), value = as.numeric(vals[[nm]]))
    }
  })

  observeEvent(input$reset_btn, {
    std <- current_standard()
    for (k in names(std$pollutants)) {
      updateNumericInput(session, paste0("conc_", k), value = NA)
    }
  })

  # Safely read a dynamically-generated numeric input.
  get_input_val <- function(key) {
    val <- input[[paste0("conc_", key)]]
    if (is.null(val) || is.na(val)) return(NA_real_)
    as.numeric(val)
  }

  # ---- Sub-index for every pollutant in the active standard ----
  # For each pollutant defined under the currently-selected standard:
  #   1. read the raw numeric input (NA if left blank),
  #   2. truncate it to that pollutant's official precision,
  #   3. run it through calc_subindex() against that pollutant's breakpoints.
  # Returns a named list (one entry per pollutant key) so downstream code can
  # look up e.g. results()[["PM25"]] directly instead of re-scanning a table.
  # Recomputes automatically whenever any input or the standard changes —
  # there's no "Calculate" button, this is why the app feels live.
  results <- reactive({
    std <- current_standard()
    keys <- names(std$pollutants)
    out <- lapply(keys, function(k) {
      p <- std$pollutants[[k]]
      conc_raw <- get_input_val(k)
      if (is.na(conc_raw)) {
        return(list(key = k, label = p$label, unit = p$unit, conc = NA_real_,
                     value = NA_real_, status = "missing"))
      }
      conc <- truncate_to(conc_raw, p$decimals)
      r <- calc_subindex(conc, p$bp)
      list(key = k, label = p$label, unit = p$unit, conc = conc,
           value = r$value, status = r$status)
    })
    names(out) <- keys
    out
  })

  # ---- Composite index: the max of all entered sub-indices ----
  # Both EPA's AQI and CPCB's NAQI are defined as the WORST (highest)
  # pollutant sub-index, not an average — a single dangerous pollutant should
  # drive the headline number even if everything else is clean. CPCB adds one
  # extra rule on top: a composite figure is only meaningful with >=3
  # pollutants measured, at least one being PM2.5 or PM10 (`sufficient`/`note`
  # below implement that; EPA has no such minimum).
  overall <- reactive({
    sub <- results()
    std <- current_standard()
    concs <- sapply(sub, function(x) x$conc)
    n_entered <- sum(!is.na(concs))

    if (n_entered == 0) {
      return(list(value = NA_real_, dominant = NA_character_, sufficient = TRUE, note = NULL))
    }

    values <- sapply(sub, function(x) x$value)
    values <- values[!is.na(values)]
    if (length(values) == 0) {
      return(list(value = NA_real_, dominant = NA_character_, sufficient = TRUE, note = NULL))
    }

    max_value <- max(values)
    dominant_key <- names(values)[which.max(values)]
    dominant_label <- sub[[dominant_key]]$label

    sufficient <- TRUE
    note <- NULL
    if (!is.null(std$min_pollutants)) {
      pm_entered <- (!is.na(sub[["PM25"]]$conc)) || (!is.na(sub[["PM10"]]$conc))
      if (n_entered < std$min_pollutants || !pm_entered) {
        sufficient <- FALSE
        note <- paste0(
          "CPCB requires at least ", std$min_pollutants,
          " pollutants, including PM2.5 or PM10, for a valid composite AQI. ",
          "The value below is indicative only, based on what's entered so far."
        )
      }
    }

    list(value = max_value, dominant = dominant_label, sufficient = sufficient, note = note)
  })

  # ---- Overall gauge ----
  output$aqiGauge <- renderPlotly({
    ov <- overall()
    std <- current_standard()
    val <- if (is.na(ov$value)) 0 else ov$value
    steps <- lapply(seq_len(nrow(std$categories)), function(i) {
      list(range = c(std$categories$low[i], std$categories$high[i]),
           color = std$categories$color[i])
    })
    plot_ly(
      type = "indicator", mode = "gauge+number",
      value = val,
      title = list(text = if (is.na(ov$value)) "Enter data to begin" else std$label),
      gauge = list(
        axis = list(range = list(0, max(std$categories$high))),
        bar = list(color = "rgba(20,20,20,0.85)", thickness = 0.25),
        steps = steps
      )
    ) |> layout(margin = list(t = 60, b = 10, l = 30, r = 30))
  })

  # ---- Category banner + advisory + dominant pollutant ----
  output$categoryBanner <- renderUI({
    ov <- overall()
    std <- current_standard()
    if (is.na(ov$value)) {
      return(div(class = "alert alert-secondary",
                  "Enter at least one pollutant concentration to see results."))
    }
    cat_row <- get_category(ov$value, std$categories)
    text_color <- if (cat_row$label %in% dark_bg_categories) "white" else "black"
    banner <- div(
      style = paste0("background-color:", cat_row$color,
                      "; padding:14px; border-radius:8px; text-align:center; color:", text_color, ";"),
      h2(ov$value, style = "margin:0;"),
      h5(cat_row$label, style = "margin:4px 0;"),
      p(cat_row$advisory, style = "margin:0; font-size:0.9em;"),
      p(paste("Dominant pollutant:", ov$dominant),
        style = "margin-top:6px; font-size:0.85em; font-style:italic;")
    )
    if (!is.null(ov$note)) {
      banner <- tagList(banner, div(class = "alert alert-warning mt-2", ov$note))
    }
    banner
  })

  # ---- Optional AQLI life-expectancy context ----
  output$lifeExpectancyNote <- renderUI({
    if (!isTRUE(input$show_life)) return(NULL)
    sub <- results()
    pm <- sub[["PM25"]]
    if (is.null(pm) || is.na(pm$conc)) return(NULL)
    who_guideline <- 5
    years_lost <- round(0.098 * max(0, pm$conc - who_guideline), 2)
    div(
      class = "alert alert-info mt-2", style = "font-size:0.85em;",
      strong("Life-expectancy context (AQLI methodology): "),
      paste0(
        "If sustained as a long-term annual average, ", pm$conc,
        " \u00b5g/m\u00b3 of PM2.5 is associated with roughly ", years_lost,
        " fewer years of life expectancy relative to the WHO guideline of 5 \u00b5g/m\u00b3 ",
        "(Ebenstein et al., 2017: ~0.098 years per \u00b5g/m\u00b3)."
      ),
      tags$br(),
      em("Illustrative only \u2014 a single reading is not the same as a sustained annual average.")
    )
  })

  # ---- Pollutant breakdown table ----
  output$aqiTable <- DT::renderDataTable({
    sub <- results()
    std <- current_standard()
    df <- do.call(rbind, lapply(sub, function(x) {
      cat_label <- if (!is.na(x$value)) get_category(x$value, std$categories)$label else "Not provided"
      subindex_display <- if (is.na(x$value)) {
        "\u2014"
      } else if (identical(x$status, "offscale")) {
        paste0(x$value, "+ (beyond scale)")
      } else {
        as.character(x$value)
      }
      data.frame(
        Pollutant = x$label,
        Concentration = if (is.na(x$conc)) "\u2014" else paste(x$conc, x$unit),
        `Sub-index` = subindex_display,
        Category = cat_label,
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }))
    color_map <- setNames(std$categories$color, std$categories$label)
    color_map <- c(color_map, "Not provided" = "#e9ecef")
    DT::datatable(df, options = list(dom = "t", paging = FALSE, ordering = FALSE),
                  rownames = FALSE) |>
      DT::formatStyle("Category", backgroundColor = DT::styleEqual(names(color_map), unname(color_map)))
  })

  # ---- Interactive bar chart ----
  output$aqiPlot <- renderPlotly({
    sub <- results()
    std <- current_standard()
    df <- do.call(rbind, lapply(sub, function(x) {
      if (is.na(x$value)) return(NULL)
      data.frame(Pollutant = x$label, Value = x$value, stringsAsFactors = FALSE)
    }))
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df$Category <- sapply(df$Value, function(v) get_category(v, std$categories)$label)
    df$Category <- factor(df$Category, levels = std$categories$label)
    color_map <- setNames(std$categories$color, std$categories$label)

    p <- ggplot(df, aes(x = reorder(Pollutant, Value), y = Value, fill = Category,
                          text = paste0(Pollutant, ": ", Value))) +
      geom_col(color = "black", width = 0.6) +
      scale_fill_manual(values = color_map, drop = FALSE) +
      coord_flip() +
      theme_minimal(base_size = 13) +
      labs(x = NULL, y = "Sub-index", fill = "Category")

    ggplotly(p, tooltip = "text") |> layout(margin = list(l = 90))
  })

  # ---- Downloadable summary report ----
  output$downloadReport <- downloadHandler(
    filename = function() paste0("aqi-report-", Sys.Date(), ".txt"),
    content = function(file) {
      sub <- results()
      ov <- overall()
      std <- current_standard()
      lines <- c(
        paste("Air Quality Report -", std$label),
        paste("Generated:", format(Sys.time())),
        "",
        sapply(sub, function(x) {
          paste0(
            x$label, ": ",
            if (is.na(x$conc)) "not provided" else paste0(x$conc, " ", x$unit),
            " -> sub-index ", if (is.na(x$value)) "NA" else x$value
          )
        }),
        "",
        paste("Overall index:", if (is.na(ov$value)) "insufficient data" else ov$value),
        paste("Dominant pollutant:", if (is.na(ov$value)) "-" else ov$dominant)
      )
      writeLines(lines, file)
    }
  )
}

# ------------------------------------------------------------------------------
# Run the app
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
