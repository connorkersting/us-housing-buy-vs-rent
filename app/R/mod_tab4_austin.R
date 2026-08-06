# tab_austin.R
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

# ---- build_tab_chart(): annotated Austin ZORI line -------------------------

build_tab4_chart <- function(d) {
  current_row <- d |> dplyr::slice_tail(n = 1)

  ggplot(d, aes(date, value)) +
    geom_line(color = PROJ_ACCENT, linewidth = 1.25) +
    geom_point(data = current_row, aes(date, value), color = PROJ_ACCENT, size = 3) +
    geom_text(
      data = current_row,
      aes(date, value, label = paste0("Current: ", lab_dollar(value))),
      vjust = -1.2, hjust = 1.1,
      color = PROJ_DARK, fontface = "bold", size = 4.6
    ) +
    scale_y_continuous(labels = lab_dollar) +
    labs(
      title    = "Austin rents peaked in 2022 and have fallen since",
      subtitle = "Typical monthly rent, Zillow ZORI",
      x = NULL, y = NULL,
      caption  = "Zillow ZORI, metro level, through June 2026"
    ) +
    theme_project()
}


# Top 10 metros by buy premium.
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