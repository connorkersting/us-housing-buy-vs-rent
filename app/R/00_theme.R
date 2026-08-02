# 00_theme.R -- shared look for all four tabs.
#
# THE ONE COLOR RULE: orange marks the focal thing (the highlighted metro,
# the "buy wins" points). Everything else is gray. Do not add new colors
# in your tab; if two things are important, one of them is not.
#
# Legends are OFF by default in theme_project(). Label lines and points
# directly on the chart (geom_text / ggrepel). This is a scored design point.
# Scaffolding generated with AI assistance (Claude); see AI_LOG.md.

PROJ_ACCENT <- "#FF8200"   # UT orange
PROJ_GRAY   <- "grey60"
PROJ_DARK   <- "grey15"

# bslib theme for the whole app (navbar, buttons, cards).
app_bs_theme <- function() {
  bslib::bs_theme(
    version = 5,
    primary = PROJ_ACCENT,
    base_font = bslib::font_google("Source Sans 3")
    # If fonts ever fail to load during offline dev, delete the base_font line.
  )
}

# CSS for the big live count on tab 3.
hero_css <- function() "
.hero-count { font-size: 4.5rem; font-weight: 800; line-height: 1; margin: 0; }
.hero-claim { font-size: 1.35rem; margin-top: .35rem; max-width: 46rem; }
.hero-sub   { color: #666; font-size: 1.05rem; margin-bottom: .1rem; }
"

# ggplot theme. base_size 17 so axis text survives a projector at the
# back of the room. Bump base_size in your call if a chart still reads small.
theme_project <- function(base_size = 17) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position     = "none",
      panel.grid.minor    = ggplot2::element_blank(),
      plot.title          = ggplot2::element_text(face = "bold"),
      plot.subtitle       = ggplot2::element_text(color = "grey30"),
      plot.caption        = ggplot2::element_text(color = "grey45",
                                                  size = base_size * 0.55),
      axis.title          = ggplot2::element_text(color = "grey30"),
      plot.title.position = "plot"
    )
}

# Number formatters. Use these everywhere so every tab prints numbers the
# same way. lab_dollar(2252) -> "$2,252"; lab_dollar_short(1142320) -> "$1.1M".
lab_dollar       <- scales::label_dollar(accuracy = 1)
lab_dollar_short <- scales::label_dollar(accuracy = 0.1,
                                         scale_cut = scales::cut_short_scale())
lab_pct          <- scales::label_percent(accuracy = 0.1)

# Shown when a tab's data adapter has not been patched yet. See R/01_data.R.
placeholder_plot <- function(msg) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg,
                      size = 5.5, color = "grey40") +
    ggplot2::theme_void()
}
