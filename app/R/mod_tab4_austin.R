# mod_tab4_austin.R -- TAB 4, AUSTIN. Owner: James.
# Read mod_template.R first. This stub runs as-is.
#
# TODO(James), two deliverables:
#   1. ZORI time series for Austin from app_data$austin: mark the Aug 2022
#      peak ($1,858) and the current value ($1,653, down 11.0%). Direct
#      labels, PROJ_ACCENT for the Austin line.
#   2. Ranking table (gt) of the buy-vs-rent gap among the top-50 metros:
#      Austin at 88.4% ranks 9th, San Jose leads at 162%. The adapter for
#      this table is NOT built yet; see TODO in R/01_data.R, coordinate with
#      Connor on which artifact carries SizeRank. build_tab4_table() below
#      is where it lands. If time runs short, a plain ggplot bar of the top
#      10 replaces the gt table; the finding survives either format.
#
# TWO CAVEATS the presenter must say out loud (they are in the text block so
# they cannot be forgotten): descriptive only, no causal claim about
# construction; and Austin is 9th, not 1st.
# Scaffolding generated with AI assistance (Claude); see AI_LOG.md.

tab4_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Austin rents actually fell. Buying there still costs 88% more per month."),
    plotOutput(ns("chart"), height = "420px"),
    gt::gt_output(ns("ranking")),
    p(class = "hero-sub",
      "Descriptive, not causal: we make no claim about why rents fell.
       And Austin's buy premium ranks 9th among the top 50 metros, not first;
       San Jose leads at 162%. Zillow ZORI, metro level, through June 2026.")
  )
}

tab4_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    output$chart <- renderPlot({
      d <- app_data$austin
      if (is.null(d)) {
        return(placeholder_plot("Austin adapter needs patching. See R/01_data.R."))
      }
      build_tab4_chart(d)
    }, res = 96)

    output$ranking <- gt::render_gt({
      build_tab4_table(app_data)
    })
  })
}

# TODO(James): replace placeholder body with the annotated ZORI series.
build_tab4_chart <- function(d) {
  ggplot(d, aes(date, value)) +
    geom_line(color = PROJ_GRAY, linewidth = 1.1) +
    scale_y_continuous(labels = lab_dollar) +
    labs(
      title    = "PLACEHOLDER: Austin typical rent (ZORI)",
      subtitle = "Peak and current value to be annotated",
      x = NULL, y = NULL,
      caption  = "Zillow ZORI, metro level, through June 2026"
    ) +
    theme_project()
}

# TODO(James + Connor): real ranking once the adapter exists.
build_tab4_table <- function(app_data) {
  tibble::tibble(note = "Ranking table pending: see TODO in R/01_data.R") |>
    gt::gt() |>
    gt::tab_options(column_labels.hidden = TRUE)
}
