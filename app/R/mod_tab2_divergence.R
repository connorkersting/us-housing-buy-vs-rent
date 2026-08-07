# mod_tab2_divergence.R -- TAB 2, INTERACTIVE CITY-VS-CITY APPRECIATION.
#
# CHANGE FROM ORIGINAL: instead of a fixed Knoxville-vs-SF chart indexed to
# Dec 2019, the user now picks City 1 / City 2 and Start date / End date.
# The chart rebases both series to 100 at the selected start date and shows
# growth to the selected end date; the writeup below it is generated from
# the same numbers, not hardcoded.
#
# Uses app_data$home_values (RegionName, date, value) added via
# adapt_home_values() in R/01_data.R -- this is raw ZHVI dollars pulled from
# raw$zhvi, NOT app_data$divergence/claim1_index, which is index-only
# (permanently pinned to Dec 2019 = 100) and can't support an arbitrary
# user-chosen start date. City choices are pulled from unique(RegionName)
# at runtime, so this works for any metro in the dataset.

tab2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Compare home price growth between any two cities"),
    fluidRow(
      column(
        3,
        selectizeInput(ns("city1"), "City 1", choices = NULL)
      ),
      column(
        3,
        selectizeInput(ns("city2"), "City 2", choices = NULL)
      ),
      column(
        3,
        dateInput(ns("date1"), "Start date", value = as.Date("2019-12-01"))
      ),
      column(
        3,
        dateInput(ns("date2"), "End date", value = Sys.Date())
      )
    ),
    plotOutput(ns("chart"), height = "500px"),
    uiOutput(ns("writeup"))
  )
}

tab2_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- populate pickers from the real data instead of hardcoding cities/dates --
    observe({
      d <- app_data$home_values
      if (is.null(d)) return(invisible(NULL))

      cities <- sort(unique(d$RegionName))
      default1 <- if ("Knoxville, TN" %in% cities) "Knoxville, TN" else cities[1]
      default2 <- if ("San Francisco, CA" %in% cities) "San Francisco, CA" else cities[min(2, length(cities))]

      updateSelectizeInput(session, "city1", choices = cities, selected = default1, server = TRUE)
      updateSelectizeInput(session, "city2", choices = cities, selected = default2, server = TRUE)

      date_min <- min(d$date, na.rm = TRUE)
      date_max <- max(d$date, na.rm = TRUE)
      updateDateInput(session, "date1", min = date_min, max = date_max,
                       value = max(date_min, min(as.Date("2019-12-01"), date_max)))
      updateDateInput(session, "date2", min = date_min, max = date_max, value = date_max)
    })

    # -- filtered + rebased data for the current selection --
    selection <- reactive({
      d <- app_data$home_values
      validate(shiny::need(!is.null(d), "home_values adapter needs patching. See R/01_data.R."))
      req(input$city1, input$city2, input$date1, input$date2)
      validate(shiny::need(input$city1 != input$city2, "Pick two different cities to compare."))

      d1 <- min(input$date1, input$date2)
      d2 <- max(input$date1, input$date2)

      sub <- dplyr::filter(d, RegionName %in% c(input$city1, input$city2), date >= d1, date <= d2)
      validate(shiny::need(nrow(sub) > 0, "No data for that city/date combination."))

      sub <- sub %>%
        dplyr::arrange(RegionName, date) %>%
        dplyr::group_by(RegionName) %>%
        dplyr::mutate(rebased = value / dplyr::first(value) * 100) %>%
        dplyr::ungroup()

      sub
    })

    # -- summary stats shared by the chart labels and the writeup --
    city_summary <- reactive({
      selection() %>%
        dplyr::group_by(RegionName) %>%
        dplyr::summarise(
          start_date = dplyr::first(date),
          end_date = dplyr::last(date),
          start_val = dplyr::first(value),
          end_val = dplyr::last(value),
          pct_change = (dplyr::last(value) / dplyr::first(value) - 1) * 100,
          .groups = "drop"
        )
    })

    output$chart <- renderPlot({
      build_tab2_chart(selection(), city_summary(), input$city1, input$city2)
    }, res = 96)

    output$writeup <- renderUI({
      s <- city_summary()
      req(nrow(s) == 2)

      c1 <- dplyr::filter(s, RegionName == input$city1)
      c2 <- dplyr::filter(s, RegionName == input$city2)

      faster <- if (c1$pct_change >= c2$pct_change) c1 else c2
      slower <- if (c1$pct_change >= c2$pct_change) c2 else c1
      gap <- faster$pct_change - slower$pct_change

      fmt_dollar <- function(x) paste0("$", formatC(round(x), format = "d", big.mark = ","))
      fmt_pct <- function(x) paste0(ifelse(x >= 0, "+", ""), sprintf("%.1f%%", x))

      tagList(
        p(sprintf(
          "Between %s and %s, a typical home in %s went from %s to %s (%s), while %s went from %s to %s (%s).",
          format(c1$start_date, "%b %Y"), format(c1$end_date, "%b %Y"),
          c1$RegionName, fmt_dollar(c1$start_val), fmt_dollar(c1$end_val), fmt_pct(c1$pct_change),
          c2$RegionName, fmt_dollar(c2$start_val), fmt_dollar(c2$end_val), fmt_pct(c2$pct_change)
        )),
        p(sprintf(
          "%s appreciated faster over this window, outpacing %s by %.1f percentage points.",
          faster$RegionName, slower$RegionName, gap
        ))
      )
    })
  })
}

build_tab2_chart <- function(d, city_summary = NULL, city1 = NULL, city2 = NULL) {
  cities <- unique(d$RegionName)
  if (is.null(city1)) city1 <- cities[1]
  if (is.null(city2)) city2 <- cities[min(2, length(cities))]

  d <- dplyr::filter(d, RegionName %in% c(city1, city2))
  if (!"rebased" %in% names(d)) {
    d <- d %>%
      dplyr::arrange(RegionName, date) %>%
      dplyr::group_by(RegionName) %>%
      dplyr::mutate(rebased = value / dplyr::first(value) * 100) %>%
      dplyr::ungroup()
  }

  end_labels <- d %>%
    dplyr::group_by(RegionName) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup()

  colors <- stats::setNames(c(PROJ_ACCENT, PROJ_DARK), c(city1, city2))

  ggplot2::ggplot(d, ggplot2::aes(x = date, y = rebased, color = RegionName)) +
    ggplot2::geom_hline(
      yintercept = 100,
      color = PROJ_DARK,
      linewidth = 0.3,
      linetype = "dashed"
    ) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggrepel::geom_text_repel(
      data = end_labels,
      ggplot2::aes(label = RegionName),
      hjust = 0,
      direction = "y",
      nudge_x = 60,
      segment.color = NA,
      fontface = "bold"
    ) +
    ggplot2::scale_color_manual(values = colors, guide = "none") +
    ggplot2::scale_x_date(expand = ggplot2::expansion(mult = c(0.02, 0.28))) +
    ggplot2::labs(
      x = NULL,
      y = paste0("Value Index (", format(min(d$date), "%b %Y"), " = 100)")
    ) +
    theme_project() +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 20))
}