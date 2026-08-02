# mod_tab2_divergence.R -- TAB 2, KNOXVILLE VS SAN FRANCISCO. Owner: Jack.
# Read mod_template.R first. This stub runs as-is; replace the chart body.
#
# TODO(Jack), the whole job is one indexed line chart:
#   - Two lines from app_data$divergence: Knoxville and San Francisco,
#     indexed Dec 2019 = 100, full range 2000-2026. Do not crop to 2019:
#     the pre-2020 era is the point, and cropping invites a cherry-picking
#     objection in Q&A.
#   - Direct-label both lines at their right ends (ggrepel), no legend.
#   - Knoxville in PROJ_ACCENT, San Francisco in PROJ_DARK gray.
#   - Optional: a thin vline at Dec 2019 with a small "both = 100" note.
# Claim for EDIT 1, verified numbers: SF ~tripled 2000-2019 while Knoxville
# didn't double; today Knoxville sits at 182 vs SF at 121.
# Scaffolding generated with AI assistance (Claude); see AI_LOG.md.

tab2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("For twenty years San Francisco was the better bet. Then it inverted."),
    plotOutput(ns("chart"), height = "500px"),
    p(class = "hero-sub",
      "EDIT 3 (Jack): state the indexing convention in one sentence so the
       axis is unambiguous: both metros equal 100 in December 2019.")
  )
}

tab2_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    output$chart <- renderPlot({
      d <- app_data$divergence
      if (is.null(d)) {
        return(placeholder_plot("Divergence adapter needs patching. See R/01_data.R."))
      }
      build_tab2_chart(d)
    }, res = 96)
  })
}

# TODO(Jack): replace this placeholder body with the real indexed chart.
build_tab2_chart <- function(d) {
  ggplot(d, aes(date, index, group = RegionName)) +
    geom_line(color = PROJ_GRAY, linewidth = 1.1) +
    labs(
      title    = "PLACEHOLDER: indexed home values, Knoxville vs San Francisco",
      subtitle = "Dec 2019 = 100",
      x = NULL, y = NULL,
      caption  = "Zillow ZHVI, metro level, through June 2026"
    ) +
    theme_project()
}
