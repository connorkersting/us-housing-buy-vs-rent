# preview.R -- see YOUR chart without running the whole app.
#
# WHY: if you edit your chart and click Run App, a broken ggplot shows up as a
# Shiny error full of reactive internals, and you cannot tell whether your
# chart is wrong or the app is wrong. This file renders your chart on its own,
# so any error you get is about YOUR code and nothing else.
#
# HOW: set MY_TAB below, then source this whole file (Ctrl+Shift+S).
# It also installs any missing packages the first time you run it.
# Scaffolding generated with AI assistance (Claude); see AI_LOG.md.

MY_TAB <- 1   # <- CHANGE THIS to your tab number: 1, 2, 3, or 4

# ---- working directory check ----------------------------------------------
if (!dir.exists("app/R")) {
  stop("Run this from the repo root. Open bzan583-housing.Rproj first, ",
       "then source this file. Currently in: ", getwd())
}

# ---- first run installs anything missing ----------------------------------
needed <- c("shiny", "bslib", "dplyr", "tidyr", "ggplot2", "scales",
            "ggrepel", "tibble")
missing <- needed[!needed %in% rownames(installed.packages())]
if (length(missing)) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

library(dplyr); library(tidyr); library(ggplot2); library(scales); library(ggrepel)

source("app/R/00_theme.R")
source("app/R/01_data.R")
d <- load_data("app/data")

# ---- your data and your chart ---------------------------------------------
if (MY_TAB == 1) {
  source("app/R/mod_tab1_intro.R")
  cat("\n--- your data ---\n"); glimpse(d$intro)
  print(build_tab1_chart(d$intro))

} else if (MY_TAB == 2) {
  source("app/R/mod_tab2_divergence.R")
  cat("\n--- your data ---\n"); glimpse(d$divergence)
  cat("metros:", paste(unique(d$divergence$RegionName), collapse = ", "), "\n")
  print(build_tab2_chart(d$divergence))

} else if (MY_TAB == 3) {
  source("app/R/mod_tab3_buyrent.R")
  cat("\n--- your data ---\n"); glimpse(d$gaps)
  print(build_tab3_strip(filter(d$gaps, down == 20, comparator_key == "sfr")))
  print(build_tab3_years(d$years))

} else if (MY_TAB == 4) {
  source("app/R/mod_tab4_austin.R")
  cat("\n--- your data ---\n"); glimpse(d$austin)
  print(build_tab4_chart(d$austin))
  print(build_tab4_ranking(d$ranking))

} else {
  stop("MY_TAB must be 1, 2, 3, or 4.")
}

cat("\nChart is in the Plots pane. Widen it before judging the text size.\n")
