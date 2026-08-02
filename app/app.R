# app.R -- shell only. Modules live in R/, which Shiny auto-sources at startup.
# RULES that keep the one-file submission build safe:
#   1. ALL library() calls live here and only here. shinyapps.io package
#      detection scans this file, and the concatenation build script puts
#      this block first.
#   2. Module files contain definitions only: no library(), no source(),
#      no top-level side effects.
# Scaffolding generated with AI assistance (Claude); see AI_LOG.md.

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(gt)      # tab 4 ranking table; loaded now so the package set is frozen

# One read at startup. Modules receive `app_data`, never touch disk themselves.
app_data <- load_data("data")

ui <- page_navbar(
  title = "Buy vs Rent in US Housing",   # working title, change freely
  theme = app_bs_theme(),
  header = tags$head(tags$style(HTML(hero_css()))),
  nav_panel("Overview",     tab1_ui("tab1")),
  nav_panel("Two Cities",   tab2_ui("tab2")),
  nav_panel("Buy or Rent?", tab3_ui("tab3")),
  nav_panel("Austin",       tab4_ui("tab4"))
)

server <- function(input, output, session) {
  tab1_server("tab1", app_data)
  tab2_server("tab2", app_data)
  tab3_server("tab3", app_data)
  tab4_server("tab4", app_data)
}

shinyApp(ui, server)
