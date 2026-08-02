# mod_tab3_buyrent.R -- TAB 3, BUY VS RENT. Owner: Connor.
#
# The interactive centerpiece. Two discrete controls (radios, not sliders:
# six defensible states, no intermediate mush), a hero count that updates
# live, and a strip chart where dots visibly cross the zero line.
#
# STAGECRAFT DEFAULT: opens at 5% down vs all rentals, where the count is 0,
# so the live reveal during the talk moves 0 -> 89. To open elsewhere, change
# the two `selected =` values below.
#
# Every number on this tab is computed live from app_data$gaps. Nothing is
# hardcoded, so the app cannot drift from the data.
# Scaffolding generated with AI assistance (Claude); see AI_LOG.md.

tab3_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      radioButtons(
        ns("down"), "Down payment",
        choices = c("5%" = "5", "10%" = "10", "20%" = "20"),
        selected = "5"
      ),
      radioButtons(
        ns("comp"), "Compare owning a house to renting...",
        choices = c("Any rental, incl. apartments" = "all",
                    "A single-family house"        = "sfr"),
        selected = "all"
      ),
      helpText(
        "Owning cost = Zillow total monthly payment: principal and interest,
         insurance, property tax, maintenance at 0.5% of value, plus 1%
         mortgage insurance when the down payment is under 20%."
      )
    ),
    navset_card_underline(
      nav_panel(
        "Monthly cost",
        p(class = "hero-sub",
          "Metros where owning costs less per month than renting"),
        div(class = "hero-count", textOutput(ns("count"), inline = TRUE)),
        p(class = "hero-claim", textOutput(ns("claim"), inline = TRUE)),
        plotOutput(ns("strip"), height = "430px")
      ),
      nav_panel(
        "Years to save",
        p(class = "hero-sub",
          "Winning the monthly math is not the same as getting in the door."),
        plotOutput(ns("years"), height = "480px")
      )
    )
  )
}

tab3_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {

    sel <- reactive({
      req(app_data$gaps)
      dplyr::filter(app_data$gaps,
                    down == as.numeric(input$down),
                    comparator_key == input$comp)
    })

    comp_label <- reactive({
      if (input$comp == "all") "any rental, including apartments"
      else "a single-family rental"
    })

    output$count <- renderText({
      d <- sel()
      sprintf("%d of %d", sum(d$gap < 0, na.rm = TRUE), nrow(d))
    })

    output$claim <- renderText({
      d   <- sel()
      med <- stats::median(d$gap, na.rm = TRUE)
      dir <- if (med >= 0) "more" else "less"
      sprintf(
        "At %s%% down, buying the typical house beats %s on monthly cost in
         %d of %d metros. Median metro: owning runs %s a month %s.",
        input$down, comp_label(),
        sum(d$gap < 0, na.rm = TRUE), nrow(d),
        lab_dollar(abs(med)), dir
      )
    })

    output$strip <- renderPlot({
      d <- sel()
      if (nrow(d) == 0) return(placeholder_plot("Gaps adapter needs patching."))
      build_tab3_strip(d)
    }, res = 96)

    output$years <- renderPlot({
      if (is.null(app_data$years)) {
        return(placeholder_plot("Years adapter needs patching. See R/01_data.R."))
      }
      build_tab3_years(app_data$years)
    }, res = 96)
  })
}

# ---- charts ----------------------------------------------------------------

# One dot per metro on a horizontal cost axis. Orange left of zero = buying
# wins. y_jitter is precomputed per metro (R/01_data.R) so dots keep their
# vertical slot across radio flips and all movement reads as horizontal.
build_tab3_strip <- function(d) {
  knox     <- dplyr::filter(d, grepl("^Knoxville", RegionName))
  extremes <- dplyr::slice_max(d, gap, n = 2)
  xmax     <- max(abs(d$gap), na.rm = TRUE)

  ggplot(d, aes(gap, y_jitter)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey35") +
    geom_point(aes(color = gap < 0), size = 2.7, alpha = 0.85) +
    scale_color_manual(values = c("TRUE" = PROJ_ACCENT, "FALSE" = PROJ_GRAY)) +
    # Knoxville: outlined, always labeled with its signed gap
    geom_point(data = knox, shape = 21, size = 5,
               stroke = 1.3, color = PROJ_DARK, fill = NA) +
    ggrepel::geom_text_repel(
      data = knox,
      aes(label = paste0("Knoxville ", ifelse(gap < 0, "-", "+"),
                         lab_dollar(abs(gap)), "/mo")),
      fontface = "bold", size = 5.5, nudge_y = 0.55, seed = 7
    ) +
    ggrepel::geom_text_repel(
      data = extremes, aes(label = RegionName),
      color = "grey45", size = 4, nudge_y = -0.45, seed = 7
    ) +
    annotate("text", x = -0.98 * xmax, y = 1.45, hjust = 0, size = 5,
             fontface = "bold", color = PROJ_ACCENT, label = "Buying cheaper") +
    annotate("text", x = 0.98 * xmax, y = 1.45, hjust = 1, size = 5,
             fontface = "bold", color = "grey40", label = "Renting cheaper") +
    scale_x_continuous(labels = lab_dollar) +
    coord_cartesian(ylim = c(-1.15, 1.6), clip = "off") +
    labs(
      title   = "Monthly cost of owning minus renting, by metro",
      x = NULL, y = NULL,
      caption = "Zillow, June 2026. Owning cost includes PMI under 20% down."
    ) +
    theme_project() +
    theme(axis.text.y = element_blank(),
          panel.grid.major.y = element_blank())
}

# The closer: even metros that flip take years of saving to enter.
# Group medians and counts are computed live, never hardcoded.
build_tab3_years <- function(d) {
  d <- d |>
    dplyr::mutate(group = ifelse(flips,
                                 "Flips to buying at 20% down",
                                 "Renting still wins at 20% down"))
  meds <- d |>
    dplyr::summarise(med = stats::median(years, na.rm = TRUE),
                     n = dplyr::n(), .by = group)
  slowest <- dplyr::slice_max(d, years, n = 6)

  set.seed(11)  # stable jitter between renders
  ggplot(d, aes(years, group)) +
    geom_jitter(aes(color = flips), height = 0.18, size = 2.6, alpha = 0.8) +
    scale_color_manual(values = c("TRUE" = PROJ_ACCENT, "FALSE" = PROJ_GRAY)) +
    geom_point(data = meds, aes(med, group), shape = 124,
               size = 12, color = PROJ_DARK) +
    geom_text(data = meds,
              aes(med, group,
                  label = sprintf("median %.1f yrs (n = %d)", med, n)),
              vjust = -2.6, fontface = "bold", size = 5) +
    ggrepel::geom_text_repel(
      data = slowest, aes(label = sprintf("%s %.1f", RegionName, years)),
      color = "grey45", size = 3.8, seed = 7, max.overlaps = Inf
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(6)) +
    labs(
      title   = "Years for the median household to save a 20% down payment",
      x = "Years", y = NULL,
      caption = "Zillow Years to Save: median household income, 10% savings rate, no interest."
    ) +
    theme_project()
}
