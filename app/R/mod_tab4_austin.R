# mod_tab4_austin.R -- TAB 3 in the app, "Austin". Owner: James (Jack made
# this one interactive; see AI_LOG.md).
#
# NAMING NOTE: this tab is THIRD in the app but the file and its functions are
# numbered 4. That is intentional. Do not rename anything to make the numbers
# match; app.R references these names.
#
# CHANGE FROM ORIGINAL: down payment / rental comparator were fixed constants
# and the ranking chart + ZORI chart + headline + caveat text were all
# hardcoded to Austin specifically. Now every one of those follows whatever
# metro is picked in the "Highlight metro" dropdown -- same treatment as
# Tab 2's picker-driven interactivity, just extended to the whole tab, not
# only the ranking bar. The default selection (20% down, all rentals, Austin
# highlighted) reproduces the original verified numbers exactly, so the tab
# still opens on the same story; the controls just let you go anywhere else.
# Every number in the headline and caveat text is computed live from the
# current selection, not hardcoded, so it can't drift from what the charts
# actually show -- and neither can the ZORI chart's peak/current annotation,
# which now recomputes for whichever city is selected instead of always
# describing Austin's specific up-then-down shape.
#
# Data: app_data$zori is the FULL ZORI series for every metro (see
# adapt_zori() in 01_data.R, renamed from adapt_austin() since it's no
# longer Austin-only), filtered reactively to input$highlight via the
# city_zori() reactive below -- same pattern Tab 2 uses for home_values.
#
# TWO CAVEATS the presenter must say out loud (computed live into the text
# block below so they cannot be forgotten): descriptive only, no causal claim
# about construction; and the highlighted metro is not always 1st.
#
# To see your charts: open preview.R at the repo root, set MY_TAB <- 4
# (the FILE number, not the tab position), and source it.
# Scaffolding generated with AI assistance (Claude); see AI_LOG.md.

tab4_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("headline")),
    fluidRow(
      column(
        3,
        radioButtons(ns("down"), "Down payment",
                     choices = c("5%" = "5", "10%" = "10", "20%" = "20"),
                     selected = "20")
      ),
      column(
        4,
        radioButtons(ns("comp"), "Compare owning a house to renting...",
                     choices = c("Any rental, incl. apartments" = "all",
                                 "A single-family house"        = "sfr"),
                     selected = "all")
      ),
      column(
        4,
        selectizeInput(ns("highlight"), "Highlight metro", choices = NULL)
      )
    ),
    plotOutput(ns("chart"), height = "380px"),
    # taller than the ZORI chart above: 10-11 metro-name rows at
    # theme_project()'s base_size = 17 need real vertical room, or the
    # labels stack and overlap.
    plotOutput(ns("ranking"), height = "520px"),
    uiOutput(ns("caveat"))
  )
}

tab4_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {

    # -- populate the highlight dropdown from the real top-50 list --
    observe({
      d <- app_data$ranking
      if (is.null(d)) return(invisible(NULL))
      metros  <- sort(unique(d$RegionName))
      default <- if ("Austin, TX" %in% metros) "Austin, TX" else metros[1]
      updateSelectizeInput(session, "highlight", choices = metros,
                           selected = default, server = TRUE)
    })

    # -- ranking table filtered to the current down/comparator selection,
    # with rank computed live so the headline/caveat text can cite it --
    ranked <- reactive({
      d <- app_data$ranking
      req(d, input$down, input$comp)
      d |>
        dplyr::filter(comparator_key == input$comp, down == as.numeric(input$down)) |>
        dplyr::arrange(dplyr::desc(gap_pct)) |>
        dplyr::mutate(rank = dplyr::row_number())
    })

    # -- ZORI series for whichever metro is currently highlighted --
    city_zori <- reactive({
      d <- app_data$zori
      req(d, input$highlight)
      dplyr::filter(d, RegionName == input$highlight)
    })

    # -- the highlighted metro's row in the current ranking, used by both
    # the headline and the caveat text --
    selected_row <- reactive({
      r <- ranked()
      req(input$highlight)
      dplyr::filter(r, RegionName == input$highlight)
    })

    output$headline <- renderUI({
      sel <- selected_row()
      req(nrow(sel) == 1)

      z <- city_zori()
      rent_clause <- if (nrow(z) < 2) {
        "rent data is limited for this metro"
      } else {
        peak <- z[which.max(z$value), ]
        now  <- z[which.max(z$date), ]
        if (now$value >= peak$value) "rents are near an all-time high" else
          "rents have pulled back from their peak"
      }

      city_short <- sub(",.*$", "", input$highlight)
      h3(sprintf(
        "In %s, %s. Buying there still costs %s more per month.",
        city_short, rent_clause, lab_pct(sel$gap_pct)
      ))
    })

    output$chart <- renderPlot({
      d <- city_zori()
      if (nrow(d) == 0) {
        return(placeholder_plot("No rent data for that metro. See R/01_data.R."))
      }
      build_tab4_chart(d)
    }, res = 96)

    output$ranking <- renderPlot({
      r <- ranked()
      if (nrow(r) == 0) {
        return(placeholder_plot("Ranking adapter needs patching. See R/01_data.R."))
      }
      req(input$highlight)
      build_tab4_ranking(r, highlight = input$highlight)
    }, res = 96)

    output$caveat <- renderUI({
      sel  <- selected_row()
      top1 <- dplyr::filter(ranked(), rank == 1)
      req(nrow(sel) == 1, nrow(top1) == 1)

      comp_label <- if (input$comp == "sfr") "single-family rentals" else
        "all rentals, including apartments"

      rank_clause <- if (sel$rank == 1) {
        sprintf("%s's buy premium ranks first among the 50 largest metros", input$highlight)
      } else {
        sprintf(
          "%s's buy premium ranks %s among the 50 largest metros, not first; %s leads at %s",
          input$highlight, scales::label_ordinal()(sel$rank), top1$RegionName, lab_pct(top1$gap_pct)
        )
      }

      p(class = "hero-sub",
        sprintf(
          "Descriptive, not causal: we make no claim about why rents moved.
           Compared against %s, at %s%% down, %s. Zillow research data,
           metro level, latest month.",
          comp_label, input$down, rank_clause
        )
      )
    })
  })
}

# d: one metro's ZORI series (RegionName | date | value), already filtered
# to a single city -- see tab4_server()'s city_zori() reactive, or
# preview.R for a standalone example. Peak and current value are computed
# live from d, not hardcoded, so this works for any metro, not just Austin.
build_tab4_chart <- function(d) {
  city      <- unique(d$RegionName)[1]
  peak_row  <- d[which.max(d$value), ]
  now_row   <- d[which.max(d$date), ]
  pct       <- (now_row$value - peak_row$value) / peak_row$value
  at_peak   <- now_row$value >= peak_row$value  # peak_row is always <= now_row's value

  labels <- if (at_peak) {
    # Same point -- one label, not two stacked on top of each other.
    list(geom_point(data = now_row, color = PROJ_ACCENT, size = 3),
         ggrepel::geom_text_repel(
           data = now_row,
           aes(label = sprintf("All-time high: %s (%s)", lab_dollar(value), format(date, "%b %Y"))),
           nudge_y = 90, fontface = "bold", size = 4.6, seed = 3, segment.color = "grey50"
         ))
  } else {
    list(geom_point(data = dplyr::bind_rows(peak_row, now_row),
                     color = PROJ_ACCENT, size = 3),
         ggrepel::geom_text_repel(
           data = peak_row,
           aes(label = sprintf("Peak: %s (%s)", lab_dollar(value), format(date, "%b %Y"))),
           nudge_y = 90, fontface = "bold", size = 4.6, seed = 3, segment.color = "grey50"
         ),
         ggrepel::geom_text_repel(
           data = now_row,
           aes(label = sprintf("Now: %s (%s)", lab_dollar(value), lab_pct(pct))),
           nudge_y = -90, nudge_x = -250, fontface = "bold", size = 4.6, seed = 3,
           segment.color = "grey50"
         ))
  }

  ggplot(d, aes(date, value)) +
    geom_line(color = PROJ_ACCENT, linewidth = 1.3) +
    labels +
    scale_y_continuous(labels = lab_dollar) +
    labs(
      title    = sprintf("%s typical rent: peak vs. now", city),
      subtitle = if (at_peak) {
        sprintf("Currently at an all-time high: %s (%s)",
               lab_dollar(now_row$value), format(now_row$date, "%b %Y"))
      } else {
        sprintf("Peak %s (%s), now %s (%s)",
               lab_dollar(peak_row$value), format(peak_row$date, "%b %Y"),
               lab_dollar(now_row$value), lab_pct(pct))
      },
      x = NULL, y = NULL,
      caption  = "Zillow ZORI, metro level, latest month"
    ) +
    theme_project()
}

# d must already be filtered to one down/comparator combination -- see
# tab4_server()'s ranked() reactive, or preview.R for a standalone example.
# highlight: RegionName to mark in accent color, even if it falls outside
# the natural top 10 by gap_pct (added back in so picking an unranked metro
# still shows something instead of silently disappearing).
build_tab4_ranking <- function(d, highlight = "Austin, TX") {
  down <- unique(d$down)[1]
  comp <- unique(d$comparator_key)[1]
  sub  <- if (identical(comp, "sfr")) "single-family rentals" else
    "all rentals, including apartments"

  top10 <- d |>
    dplyr::slice_max(gap_pct, n = 10) |>
    dplyr::mutate(is_highlight = RegionName == highlight)

  if (!is.null(highlight) && !any(top10$is_highlight) && highlight %in% d$RegionName) {
    hl_row <- dplyr::filter(d, RegionName == highlight) |>
      dplyr::mutate(is_highlight = TRUE)
    top10 <- dplyr::bind_rows(top10, hl_row)
  }

  ggplot(top10, aes(gap_pct, reorder(RegionName, gap_pct))) +
    geom_col(aes(fill = is_highlight), width = 0.72) +
    scale_fill_manual(values = c("TRUE" = PROJ_ACCENT, "FALSE" = "grey75"), guide = "none") +
    geom_text(aes(label = lab_pct(gap_pct)), hjust = -0.15,
              size = 4.6, fontface = "bold", color = PROJ_DARK) +
    scale_x_continuous(labels = lab_pct, expand = expansion(mult = c(0, 0.14))) +
    labs(
      title    = "The biggest gaps between owning and renting",
      subtitle = sprintf("Among the 50 largest metros. Versus %s, at %s%% down",
                         sub, down),
      x = NULL, y = NULL,
      caption  = "Zillow, latest month"
    ) +
    theme_project() +
    theme(panel.grid.major.y = element_blank())
}
