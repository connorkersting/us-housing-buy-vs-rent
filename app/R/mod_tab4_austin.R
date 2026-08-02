# mod_tab4_austin.R -- TAB 4, AUSTIN. Owner: unassigned, claim it in the chat.
# Read mod_template.R first. This stub runs as-is.
#
# TODO(tab 4 owner), two deliverables:
#   1. ZORI time series for Austin from app_data$austin: mark the Aug 2022
#      peak ($1,858) and the current value ($1,653, down 11.0%). Direct
#      labels, PROJ_ACCENT for the Austin line.
#   2. Top-10 bar chart of the buy premium among the top-50 metros, from
#      app_data$ranking. Austin at 88.4% ranks 9th, San Jose leads at 162%.
#      A skeleton of this chart is already written below and runs.
#
# TWO CAVEATS the presenter must say out loud (they are in the text block so
# they cannot be forgotten): descriptive only, no causal claim about
# construction; and Austin is 9th, not 1st.
# Scaffolding generated with AI assistance (Claude); see AI_LOG.md.

# ---- UNVERIFIED, Connor confirms before Wednesday --------------------------
# Which rental definition and down payment reproduce Austin 88.4% and
# San Jose 162%. The chart prints these values in its subtitle so the
# assumption is always visible on screen, which is the whole point of tab 3.
RANK_COMPARATOR <- "sfr"   # "all" or "sfr"
RANK_DOWN       <- 20      # 5, 10, or 20
# ----------------------------------------------------------------------------

tab4_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Austin rents actually fell. Buying there still costs 88% more per month."),
    plotOutput(ns("chart"), height = "380px"),
    plotOutput(ns("ranking"), height = "380px"),
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

    output$ranking <- renderPlot({
      d <- app_data$ranking
      if (is.null(d)) {
        return(placeholder_plot("Ranking adapter needs patching. See R/01_data.R."))
      }
      build_tab4_ranking(d)
    }, res = 96)
  })
}

# TODO(tab 4 owner): replace placeholder body with the annotated ZORI series.
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

# Top 10 metros by buy premium. Austin in accent, everyone else gray.
# The subtitle states the rental definition and down payment on screen.
build_tab4_ranking <- function(d) {
  sub <- if (RANK_COMPARATOR == "sfr") "single-family rentals" else
    "all rentals, including apartments"

  top10 <- d |>
    dplyr::filter(comparator_key == RANK_COMPARATOR, down == RANK_DOWN) |>
    dplyr::slice_max(gap_pct, n = 10) |>
    dplyr::mutate(is_austin = grepl("^Austin", RegionName))

  ggplot(top10, aes(gap_pct, reorder(RegionName, gap_pct))) +
    geom_col(aes(fill = is_austin), width = 0.72) +
    scale_fill_manual(values = c("TRUE" = PROJ_ACCENT, "FALSE" = "grey75")) +
    geom_text(aes(label = lab_pct(gap_pct)), hjust = -0.15,
              size = 4.6, fontface = "bold", color = PROJ_DARK) +
    scale_x_continuous(labels = lab_pct, expand = expansion(mult = c(0, 0.14))) +
    labs(
      title    = "How much more owning costs than renting, top 50 metros",
      subtitle = sprintf("Versus %s, at %d%% down", sub, RANK_DOWN),
      x = NULL, y = NULL,
      caption  = "Zillow, June 2026"
    ) +
    theme_project() +
    theme(panel.grid.major.y = element_blank())
}
