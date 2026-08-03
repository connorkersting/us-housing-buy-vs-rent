# mod_tab4_austin.R -- TAB 3 in the app, "Austin". Owner: James.
#
# NAMING NOTE: this tab is THIRD in the app but the file and its functions are
# numbered 4. That is intentional. Do not rename anything to make the numbers
# match; app.R references these names.
#
# TODO(James), one deliverable:
#   ZORI time series for Austin from app_data$austin: mark the Aug 2022 peak
#   ($1,858) and the current value ($1,653, down 11.0%). Direct labels,
#   PROJ_ACCENT for the Austin line. Replace build_tab4_chart() below.
#   The bar chart (build_tab4_ranking) is already written and runs. Leave it.
#
# To see your charts: open preview.R at the repo root, set MY_TAB <- 4
# (the FILE number, not the tab position), and source it.
#
# TWO CAVEATS the presenter must say out loud (they are in the text block so
# they cannot be forgotten): descriptive only, no causal claim about
# construction; and Austin is 9th, not 1st.
# Scaffolding generated with AI assistance (Claude); see AI_LOG.md.

# ---- Verified 2026-08-02 against d$ranking ---------------------------------
# At 20% down vs all rentals: San Jose 162% (rank 1), Austin 88.4% (rank 9).
# Austin sits at rank 9 under THIS setting, which is what puts it in the top
# 10 bar chart at all. Under "sfr" it is rank 12 and drops off the chart.
# The chart prints the rental definition and down payment in its subtitle so
# the assumption is always visible on screen.
RANK_COMPARATOR <- "all"   # "all" or "sfr"
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
       Compared against all rentals including apartments, at 20% down,
       Austin's buy premium ranks 9th among the 50 largest metros, not first;
       San Jose leads at 162%. Zillow research data, metro level, through
       June 2026.")
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
      title    = "The 10 biggest gaps between owning and renting",
      subtitle = sprintf("Among the 50 largest metros. Versus %s, at %d%% down",
                         sub, RANK_DOWN),
      x = NULL, y = NULL,
      caption  = "Zillow, June 2026"
    ) +
    theme_project() +
    theme(panel.grid.major.y = element_blank())
}