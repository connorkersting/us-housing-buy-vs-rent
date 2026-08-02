# mod_template.R -- THE PATTERN EVERY TAB FOLLOWS. Read this once, then edit
# your own mod_tabX file. This file itself is never mounted in the app.
#
# ============================ HOW TO EDIT YOUR TAB ==========================
# You change exactly THREE things in your own module file:
#
#   1. YOUR CLAIM   -> the h3() line in the ui, one sentence, the finding.
#   2. YOUR CHART   -> the build_*_chart() function at the bottom. It is a
#                      plain function: data frame in, ggplot out. No reactives,
#                      no input$, no output$. If it runs in the console, it
#                      runs in the app.
#   3. YOUR TEXT    -> the p() paragraph under the plot, 2-3 sentences.
#
# DO NOT touch the server function or anything with `ns(` in it. That wiring
# is identical across tabs on purpose and it already works.
#
# To see your tab while you work: open app/ as the working directory and run
#   shiny::runApp()
# Style is inherited automatically: theme_project() sizes text for the
# projector, PROJ_ACCENT is the only non-gray color allowed, lab_dollar()
# and friends format numbers. See R/00_theme.R.
# Scaffolding generated with AI assistance (Claude); see AI_LOG.md.
# ============================================================================

template_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("EDIT 1: One sentence stating your finding, plainly."),
    plotOutput(ns("chart"), height = "500px"),
    p(class = "hero-sub",
      "EDIT 3: Two or three sentences of context. State data source, date
       range, and any definition the audience must know to read the chart.")
  )
}

template_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    output$chart <- renderPlot({
      d <- app_data$intro                       # your adapted data object
      if (is.null(d)) {
        return(placeholder_plot("Data adapter needs patching. See R/01_data.R."))
      }
      build_template_chart(d)
    }, res = 96)                                # crisp on a projector
  })
}

# EDIT 2 lives here: plain function, data in, ggplot out.
build_template_chart <- function(d) {
  ggplot(d, aes(date, value)) +
    geom_line(color = PROJ_ACCENT, linewidth = 1.4) +
    scale_y_continuous(labels = lab_dollar_short) +
    labs(
      title    = "PLACEHOLDER: replace with your figure",
      subtitle = "Knoxville typical home value (ZHVI)",
      x = NULL, y = NULL,
      caption  = "Zillow ZHVI, metro level, through June 2026"
    ) +
    theme_project()
}
