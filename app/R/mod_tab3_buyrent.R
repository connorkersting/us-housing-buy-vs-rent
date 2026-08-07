# mod_tab3_buyrent.R -- TAB 4 in the app, "Buy or Rent?". Owner: Connor.
#
# NAMING NOTE: this tab is FOURTH in the app but the file and its functions
# are numbered 3. Intentional. app.R references these names.
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

# The reference cities the audience should see on BOTH charts. Every one is a
# deliberate editorial choice tied to something a presenter says out loud --
# nothing here is computed, picked by outlier rule, or otherwise automatic.
# Knoxville is always labeled and always emphasized.
FOCUS_METROS <- c("Knoxville", "Nashville", "Austin", "New York",
                  "San Francisco")

# San Francisco is labeled on the years chart but NOT on the strip. Its gap
# runs $3,972-$5,958 in five of the six control states, past the shared axis,
# so on the strip it could only appear pinned to the edge carrying a dollar
# figure that is not its own. Labeling it in one state and not the other five
# is exactly the flicker the fixed axis exists to prevent, so it comes off the
# strip in all six.
#
# New York is the expensive-coastal anchor in its place: it tops out at $2,855
# against a $3,292 ceiling, so it sits honestly inside the axis in all six
# states and never needs pinning. The other three do too.
STRIP_LABEL_METROS <- setdiff(FOCUS_METROS, "San Francisco")

# Kept on the years chart alongside the focus cities: these carry the "18.8
# years and it still does not flip" point. Not computed either -- the metros
# that are literally slowest are Santa Cruz, Santa Maria and Kahului, which
# nobody in the room can place.
YEARS_SLOW_METROS <- c("Los Angeles", "San Jose", "Seattle")

# Regex over RegionName for a set of metro prefixes, and the display name
# ("Nashville, TN" -> "Nashville"). Labels use the short form everywhere.
metro_pattern <- function(metros) paste0("^(", paste(metros, collapse = "|"), ")")
short_name    <- function(x) sub(",.*$", "", x)

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
    
    # Axis limits computed ONCE from all six control states, so flipping a
    # radio moves the dots and never the ruler. Upper bound trimmed to the
    # 98th percentile because a handful of Bay Area metros would otherwise
    # set the scale and squash everything else into the left third.
    gap_xlim <- local({
      g <- app_data$gaps$gap
      c(min(g, na.rm = TRUE) * 1.05,
        unname(stats::quantile(g, 0.98, na.rm = TRUE)))
    })
    
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
      build_tab3_strip(d, gap_xlim)
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

# Blend a colour toward white or black. Used only to derive lighter and darker
# variants of PROJ_ACCENT and PROJ_GRAY -- no new hue is ever introduced.
blend_toward <- function(col, toward, amount) {
  grDevices::rgb(t((1 - amount) * grDevices::col2rgb(col) +
                        amount  * grDevices::col2rgb(toward)),
                 maxColorValue = 255)
}

# Dot colours for the strip chart. HUE IS STRICTLY BINARY: orange means the
# gap is negative (buying cheaper), gray means positive. Full stop. Intensity
# then varies with magnitude WITHIN each side, so a big saving is deeper
# orange and a big premium is deeper gray.
#
# Two separate ramps, never one diverging scale, because a diverging scale
# passes through white or a neutral at zero -- and zero is both the densest
# part of this chart and the only place the reading actually matters. Each
# ramp starts from a floor: the palest orange is still unmistakably orange and
# the palest gray is still clearly a dot on a white panel.
#
# Position on the ramp is the percentile of |gap| within its own side rather
# than the raw value, so a couple of Bay Area metros cannot flatten everyone
# else onto the pale end. It is still monotone in magnitude.
gap_dot_colors <- function(gap) {
  ramp <- function(v, from, to) {
    if (length(v) == 0) return(character(0))
    p <- if (length(v) == 1) 0.5 else
      (rank(abs(v), ties.method = "average") - 1) / (length(v) - 1)
    m <- grDevices::colorRamp(c(from, to))(p)
    grDevices::rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255)
  }
  out <- character(length(gap))
  buy <- !is.na(gap) & gap < 0
  out[buy]  <- ramp(gap[buy],
                    blend_toward(PROJ_ACCENT, "white", 0.30),
                    blend_toward(PROJ_ACCENT, "black", 0.22))
  out[!buy] <- ramp(gap[!buy],
                    blend_toward(PROJ_GRAY, "white", 0.18),
                    blend_toward(PROJ_GRAY, "black", 0.40))
  out
}

# One dot per metro on a horizontal cost axis. Orange left of zero = buying
# wins. y_jitter is precomputed per metro (R/01_data.R) so dots keep their
# vertical slot across radio flips and all movement reads as horizontal.
#
# xlim is passed in from the server so it is identical across all six control
# states. Defaults to this subset's range when called directly from preview.R.
build_tab3_strip <- function(d, xlim = NULL) {
  # Pad outward from both ends. Multiplying the range by 1.05 moved the LOWER
  # bound up whenever the subset's smallest gap was positive, clipping the very
  # metro it was meant to make room for.
  if (is.null(xlim)) {
    r <- range(d$gap, na.rm = TRUE)
    xlim <- r + c(-0.05, 0.05) * diff(r)
  }
  
  # Metros past the axis are dropped, not squished. oob_squish stacked them
  # into a column on the panel edge that read as a real cluster of metros at
  # that dollar amount, when in fact they run far past it. Filtering here
  # rather than letting the scale censor keeps ggplot from warning about
  # dropped rows on every render.
  n_over <- sum(d$gap > xlim[2] | d$gap < xlim[1], na.rm = TRUE)
  edge   <- if (n_over > 0) sprintf(
    " %d metro%s beyond the axis %s not plotted.",
    n_over, if (n_over == 1) "" else "s",
    if (n_over == 1) "is" else "are") else ""
  d      <- dplyr::filter(d, gap >= xlim[1], gap <= xlim[2])
  d$dot_color <- gap_dot_colors(d$gap)

  # All labelled metros go through ONE repel layer. Two layers do not repel
  # against each other, so Knoxville's label and Nashville's could have been
  # laid straight over one another. Knoxville stays visually first via the
  # per-row size/face/colour columns below.
  foc <- d |>
    dplyr::filter(grepl(metro_pattern(STRIP_LABEL_METROS), RegionName)) |>
    dplyr::mutate(
      is_knox   = grepl("^Knoxville", RegionName),
      lab_text  = paste0(short_name(RegionName), " ", ifelse(gap < 0, "-", "+"),
                         lab_dollar(abs(gap)), "/mo"),
      lab_size  = ifelse(is_knox, 6, 4.8),
      lab_face  = ifelse(is_knox, "bold", "plain"),
      lab_color = ifelse(is_knox, PROJ_DARK, "grey35")
    )
  knox <- dplyr::filter(foc, is_knox)

  ggplot(d, aes(gap, y_jitter)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "grey35", linewidth = 0.8) +
    geom_point(aes(color = dot_color), size = 3.2, alpha = 0.85) +
    scale_color_identity() +
    # Knoxville: outlined on top of its dot
    geom_point(data = knox, shape = 21, size = 6,
               stroke = 1.6, color = PROJ_DARK, fill = NA) +
    # Labels live in a reserved lane ABOVE the cloud, never among the dots,
    # each tied to its own dot by a leader line. The dots sit at fixed
    # y_jitter slots, so a label placed near its dot would land in traffic.
    ggrepel::geom_text_repel(
      data = foc,
      aes(label = lab_text, size = lab_size,
          fontface = lab_face, color = lab_color),
      ylim = c(1.12, 1.52), direction = "both",
      seed = 7, min.segment.length = 0, segment.color = "grey45",
      box.padding = 0.45, point.padding = 0.3, max.overlaps = Inf
    ) +
    scale_size_identity() +
    scale_discrete_identity(aesthetics = "fontface") +
    # Side labels pinned to the PANEL edges, not to the data. Using the data
    # range here was pushing the axis out to the widest metro and leaving
    # roughly 40% of the canvas empty.
    annotate("text", x = -Inf, y = 1.82, hjust = -0.05, size = 5.5,
             fontface = "bold", color = PROJ_ACCENT, label = "Buying cheaper") +
    annotate("text", x = Inf, y = 1.82, hjust = 1.05, size = 5.5,
             fontface = "bold", color = "grey40", label = "Renting cheaper") +
    scale_x_continuous(labels = lab_dollar, limits = xlim) +
    coord_cartesian(ylim = c(-1.15, 1.95), clip = "off") +
    labs(
      title   = "Monthly cost of owning minus renting, by metro",
      x = NULL, y = NULL,
      caption = paste0(
        "Zillow, June 2026. Owning cost includes PMI under 20% down.", edge)
    ) +
    theme_project() +
    theme(axis.text.y = element_blank(),
          panel.grid.major.y = element_blank())
}

# The closer: even metros that flip take years of saving to enter.
# Group medians and counts are computed live, never hardcoded.
build_tab3_years <- function(d) {
  d <- d |>
    dplyr::mutate(
      group = ifelse(flips,
                     "Flips to buying at 20% down",
                     "Renting still wins at 20% down"),
      # flips group on top: it is the subject of the spoken claim
      group = factor(group, levels = c("Renting still wins at 20% down",
                                       "Flips to buying at 20% down"))
    )

  # Vertical layout, in row units: rows sit at y = 1 and y = 2.
  SWARM_H <- 0.36   # half-height a row's dots may occupy
  LAB_OFF <- 0.46   # where a direct label sits, relative to its row center

  # Offsets are baked into the frame instead of drawn by position_jitter().
  # position_jitter re-draws per LAYER, over however many rows that layer got,
  # so the Knoxville outline and label -- both one-row subsets -- landed at a
  # different height than Knoxville's own dot in the main layer. One offset
  # column keeps every layer on the same point.
  #
  # The offsets are a beeswarm, not random jitter: metros sharing an x bin are
  # stacked evenly above and below their row. Random jitter left the 293-metro
  # row as a solid blob; stacking separates the dots and lets the width of the
  # pile carry the density.
  bw <- diff(range(d$years, na.rm = TRUE)) / 70
  d <- d |>
    dplyr::mutate(bin = round(years / bw)) |>
    dplyr::mutate(slot = dplyr::row_number() - (dplyr::n() + 1) / 2,
                  .by = c(group, bin)) |>
    # step is per group so each row fills its band; capped so that a sparse
    # row does not fling three lonely dots to the ceiling.
    dplyr::mutate(step = min(SWARM_H / max(abs(slot)), 0.08), .by = group) |>
    dplyr::mutate(ypos = as.integer(group) + slot * step)

  meds <- d |>
    dplyr::summarise(med = stats::median(years, na.rm = TRUE),
                     n = dplyr::n(), .by = group) |>
    dplyr::mutate(ypos = as.integer(group))

  # Curated, not computed. The focus cities shared with the strip chart, plus
  # the slow metros that carry the "still does not flip" point. See the
  # constants at the top of this file.
  lab <- d |>
    dplyr::filter(grepl(metro_pattern(c(FOCUS_METROS, YEARS_SLOW_METROS)),
                        RegionName)) |>
    dplyr::mutate(
      is_knox   = grepl("^Knoxville", RegionName),
      lab_text  = sprintf("%s %.1f yrs", short_name(RegionName), years),
      lab_size  = ifelse(is_knox, 5.5, 4.4),
      lab_face  = ifelse(is_knox, "bold", "plain"),
      lab_color = ifelse(is_knox, PROJ_DARK, "grey40"),
      # Knoxville drops a touch deeper than the rest, to clear the median rule
      # it sits under.
      lab_y     = as.integer(group) - LAB_OFF - ifelse(is_knox, 0.12, 0)
    )
  lab_knox <- dplyr::filter(lab, is_knox)

  ggplot(d, aes(years, ypos)) +
    # identity colours, so the label layer below can carry its own per-row
    # colour through the same scale. Binary here by design: this chart is not
    # the one carrying the count, so it keeps the plain two-colour split.
    geom_point(aes(color = ifelse(flips, PROJ_ACCENT, PROJ_GRAY)),
               size = 2.8, alpha = 0.85) +
    scale_color_identity() +
    # Median rule drawn to the full height of its swarm, so it stays visible
    # through the dots instead of being buried in them.
    geom_segment(data = meds,
                 aes(x = med, xend = med,
                     y = ypos - SWARM_H - 0.04, yend = ypos + SWARM_H + 0.04),
                 color = PROJ_DARK, linewidth = 1.3) +
    geom_text(data = meds,
              aes(med, ypos + SWARM_H + 0.09,
                  label = sprintf("median %.1f yrs (n = %d)", med, n)),
              vjust = 0, fontface = "bold", size = 5.5) +
    # Knoxville outlined and bold, the rest gray. Labels hang BELOW their own
    # row rather than above it: that keeps them off the neighbouring row's dots
    # and puts them in canvas that was otherwise empty. With today's data every
    # curated metro lands in the lower row, but this holds either way.
    geom_point(data = lab_knox, aes(years, ypos), shape = 21, size = 6,
               stroke = 1.6, color = PROJ_DARK, fill = NA) +
    # One repel layer for every label, so Knoxville's cannot be laid over
    # Nashville's -- separate layers do not repel against each other. The hard
    # ylim keeps every label in a reserved lane clear of the swarm: Knoxville
    # (9.4) and Nashville (9.7) are only a third of a year apart, and without
    # the lane Nashville settled on the dots with a leader too short to trace.
    ggrepel::geom_text_repel(
      data = lab,
      aes(label = lab_text, size = lab_size,
          fontface = lab_face, color = lab_color),
      nudge_y = lab$lab_y - lab$ypos,
      ylim = c(NA, min(as.integer(lab$group)) - SWARM_H - 0.14),
      # direction "y" only: a label that slid sideways would sit above the
      # wrong number of years.
      direction = "y", seed = 7, min.segment.length = 0,
      max.overlaps = Inf, segment.color = "grey70", box.padding = 0.4
    ) +
    scale_size_identity() +
    scale_discrete_identity(aesthetics = "fontface") +
    scale_x_continuous(breaks = scales::pretty_breaks(6)) +
    scale_y_continuous(breaks = seq_along(levels(d$group)),
                       labels = levels(d$group),
                       # bottom carries the label lane, which now stacks up to
                       # three tiers deep once the focus cities are added
                       expand = expansion(add = c(0.95, 0.58))) +
    labs(
      title   = "Years for the median household to save a 20% down payment",
      x = "Years", y = NULL,
      caption = "Zillow Years to Save: median household income, 10% savings rate, no interest."
    ) +
    theme_project()
}