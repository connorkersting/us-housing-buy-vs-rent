# mod_tab1_intro.R -- TAB 1, INTRO. Owner: Burhan.
# Read mod_template.R first. Your three edit points are marked EDIT 1/2/3.
# The chart below already runs; it is a starting point, not the final figure.
# Make the intro figure your own and default it to Knoxville, since the
# whole audience lives here.
# Scaffolding generated with AI assistance (Claude); see AI_LOG.md.
#
# This tab is plotly, not ggplot, so a click can report back which state
# was hit -- a static PNG has no way to do that. That means it departs
# from the "plain function, data in ggplot out" template: the click
# readout has to live in the server function, not just the chart function.

# Base R's state.abb/state.name don't include DC; Zillow's StateName does.
STATE_FULL_NAMES <- c(stats::setNames(state.name, state.abb),
                       DC = "District of Columbia")

tab1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # EDIT 1: your one-sentence framing of the project.
    h3("Where does it cost the most to buy a home in America?"),
    plotly::plotlyOutput(ns("chart"), height = "500px"),
    uiOutput(ns("click_readout")),
    # EDIT 3: your 2-3 sentences of context.
    p(class = "hero-sub",
      "Click any state to see its average typical home value (ZHVI,
       35th-65th percentile) across every metro Zillow tracks there.
       All data: Zillow Research, metro level, latest month available.")
  )
}

tab1_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    d <- app_data$state_prices

    output$chart <- plotly::renderPlotly({
      if (is.null(d)) {
        return(plotly::plotly_empty(type = "scatter", mode = "markers"))
      }
      build_tab1_chart(d)
    })

    output$click_readout <- renderUI({
      if (is.null(d)) {
        return(placeholder_plot("State price adapter needs patching. See R/01_data.R."))
      }
      click <- plotly::event_data("plotly_click", source = "tab1_map", session = session)
      if (is.null(click)) {
        return(p(class = "hero-claim", "Click a state on the map to see its number."))
      }
      # plotly's Shiny event data for geo/choropleth traces drops the
      # "location" field it sends for plain JS listeners -- only
      # curveNumber/pointNumber/z come through -- so look the row up by
      # position (0-indexed from plotly) instead of by name.
      row <- d[click$pointNumber + 1, ]
      if (nrow(row) == 0 || is.na(row$state)) {
        return(p(class = "hero-claim", "Click a state on the map to see its number."))
      }
      full_name <- STATE_FULL_NAMES[[row$state]]
      p(class = "hero-claim",
        sprintf("%s: average typical home value is %s.",
                full_name %||% row$state, lab_dollar(row$avg_value)))
    })
  })
}

# EDIT 2: your figure. d: state (2-letter abbreviation) | avg_value
# (avg. ZHVI across that state's metros, latest month). See
# adapt_state_prices() in 01_data.R. `source = "tab1_map"` is what lets
# the server's event_data() call above hear this specific plot's clicks.
build_tab1_chart <- function(d) {
  d$hover <- paste0(STATE_FULL_NAMES[d$state], "<br>", lab_dollar(d$avg_value))

  plotly::plot_geo(d, locationmode = "USA-states", source = "tab1_map") |>
    plotly::add_trace(
      z = ~avg_value, locations = ~state, text = ~hover, hoverinfo = "text",
      color = ~avg_value, colors = c("grey90", PROJ_ACCENT), showscale = FALSE,
      marker = list(line = list(color = "white", width = 0.5))
    ) |>
    plotly::layout(
      geo = list(scope = "usa", projection = list(type = "albers usa"),
                 bgcolor = "rgba(0,0,0,0)"),
      paper_bgcolor = "rgba(0,0,0,0)",
      margin = list(t = 10, b = 10, l = 0, r = 0)
    ) |>
    plotly::config(displayModeBar = FALSE)
}
