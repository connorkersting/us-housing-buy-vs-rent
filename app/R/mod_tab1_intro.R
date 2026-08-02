# mod_tab1_intro.R -- TAB 1, INTRO. Owner: Burhan.
# Read mod_template.R first. Your three edit points are marked EDIT 1/2/3.
# The chart below already runs; it is a starting point, not the final figure.
# Make the intro figure your own and default it to Knoxville, since the
# whole audience lives here.
# Scaffolding generated with AI assistance (Claude); see AI_LOG.md.

tab1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # EDIT 1: your one-sentence framing of the project.
    h3("What does it actually cost to own a home versus rent one in America?"),
    plotOutput(ns("chart"), height = "500px"),
    # EDIT 3: your 2-3 sentences of context.
    p(class = "hero-sub",
      "All data: Zillow Research, metro level, monthly through June 2026.
       Home values are ZHVI (the typical home, 35th-65th percentile).
       Rents are ZORI over the same band.")
  )
}

tab1_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    output$chart <- renderPlot({
      d <- app_data$intro
      if (is.null(d)) {
        return(placeholder_plot("Intro adapter needs patching. See R/01_data.R."))
      }
      build_tab1_chart(d)
    }, res = 96)
  })
}

# EDIT 2: your figure. Plain function, data in, ggplot out.
build_tab1_chart <- function(d) {
  latest <- d[which.max(d$date), ]
  ggplot(d, aes(date, value)) +
    geom_line(color = PROJ_ACCENT, linewidth = 1.4) +
    ggrepel::geom_text_repel(
      data = latest,
      aes(label = lab_dollar(value)),
      nudge_x = 200, fontface = "bold", size = 5.5, seed = 1
    ) +
    scale_y_continuous(labels = lab_dollar_short) +
    labs(
      title    = "The typical Knoxville home, 2000 to today",
      subtitle = "Zillow ZHVI, typical home value",
      x = NULL, y = NULL,
      caption  = "Zillow Research, metro level, through June 2026"
    ) +
    theme_project()
}
