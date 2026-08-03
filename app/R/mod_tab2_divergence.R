# mod_tab2_divergence.R -- TAB 2, KNOXVILLE VS SAN FRANCISCO. Owner: unassigned, claim it in the group chat.
# Read mod_template.R first. This stub runs as-is; replace the chart body.
#
# TODO(tab 2 owner), the whole job is one indexed line chart:
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
    p("Both metros are indexed to 100 in December 2019, so the lines show
   relative growth from that shared baseline rather than dollar values.
   Today a typical Knoxville home runs about $370,149 versus $1,142,320
   in San Francisco. Data: Zillow Home Value Index research data,
   monthly through June 2026.")
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

# TODO(tab 2 owner): replace this placeholder body with the real indexed chart.
build_tab2_chart <- function(d) {
  end_labels <- d %>%
    dplyr::group_by(RegionName) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup()

  ggplot2::ggplot(d, ggplot2::aes(x = date, y = index, color = RegionName)) +
    ggplot2::geom_vline(
      xintercept = as.Date("2019-12-01"),
      color = PROJ_DARK,
      linewidth = 0.3,
      linetype = "dashed"
    ) +
    ggplot2::annotate(
      "text",
      x = as.Date("2019-12-01"),
      y = 45,
      label = "both = 100",
      hjust = 1.1,
      vjust = 0,
      size = 3,
      color = PROJ_DARK
    ) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggrepel::geom_text_repel(
      data = end_labels,
      ggplot2::aes(label = RegionName),
      hjust = 0,
      direction = "y",
      nudge_x = 120,
      nudge_y = ifelse(end_labels$RegionName == "San Francisco, CA", 12, 0),
      segment.color = NA,
      fontface = "bold"
    ) +
    ggplot2::scale_color_manual(
  values = c("Knoxville, TN" = PROJ_ACCENT, "San Francisco, CA" = PROJ_DARK),
  guide = "none"
    ) +
    ggplot2::scale_x_date(expand = ggplot2::expansion(mult = c(0.02, 0.28))) +
    ggplot2::labs(x = NULL, y = "Value Index (Dec 2019 = 100)") +
    theme_project() + 
    ggplot2::theme(plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 20))
}
