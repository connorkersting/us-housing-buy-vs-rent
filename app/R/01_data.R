# 01_data.R -- loads artifacts and adapts them to what the modules expect.
#
# I (Claude) have NOT seen the .rds schemas. Every assumption lives in this
# file and nowhere else. If check_artifacts.R output shows different column
# names, patch the adapter here; no module file should need to change.
# A failed adapter returns NULL with a warning, and its tab shows a
# placeholder instead of crashing the app.
#
# EXPECTED SCHEMAS (assumed, verify against check_artifacts.R output):
#   gap_all / gap_sfr : one row per metro x down level.
#                       RegionName chr | down num (5/10/20 or 0.05/0.10/0.20)
#                       | own monthly $ | rent monthly $ | maybe gap = own - rent
#   years_to_save     : RegionName chr | years num  (20% down, 10% save rate)
#   zhvi              : long: RegionName | date | value
#   claim1_index      : RegionName | date | index (Dec 2019 = 100), KNX + SF
#   zori_all          : long: RegionName | date | value
# Scaffolding generated with AI assistance (Claude); see AI_LOG.md.

load_data <- function(dir = "data") {
  paths <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  raw <- stats::setNames(lapply(paths, readRDS),
                         tools::file_path_sans_ext(basename(paths)))
  list(
    raw        = raw,                    # escape hatch while adapters settle
    intro      = adapt_safely("intro",      adapt_intro(raw)),
    divergence = adapt_safely("divergence", adapt_divergence(raw)),
    gaps       = adapt_safely("gaps",       adapt_gaps(raw)),
    years      = adapt_safely("years",      adapt_years(raw)),
    austin     = adapt_safely("austin",     adapt_austin(raw))
  )
}

# ---- helpers ---------------------------------------------------------------

adapt_safely <- function(name, expr) {
  tryCatch(expr, error = function(e) {
    warning(sprintf("Adapter '%s' failed: %s", name, conditionMessage(e)),
            call. = FALSE)
    NULL
  })
}

# First matching column name, or NA. Lets adapters tolerate naming drift.
pick_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) > 0) hit[1] else NA_character_
}

need <- function(df, col, what) {
  if (is.na(col)) stop(sprintf("could not find a %s column", what))
  df[[col]]
}

is_knox <- function(x) grepl("^Knoxville", x)

REGION_CANDS <- c("RegionName", "region", "metro", "Metro", "region_name")
DATE_CANDS   <- c("date", "Date", "month", "period")
VALUE_CANDS  <- c("value", "Value", "zhvi", "ZHVI", "zori", "ZORI")

# ---- adapters --------------------------------------------------------------

# One long frame both tab 3 views filter. Columns out:
#   RegionName | comparator_key ("all"/"sfr") | down (5/10/20) | own | rent |
#   gap (own - rent) | y_jitter (stable per metro, so dots hold their vertical
#   position when the radio flips and movement reads as horizontal only)
adapt_gaps <- function(raw) {
  stopifnot(!is.null(raw$gap_all), !is.null(raw$gap_sfr))

  one <- function(df, key) {
    region <- need(df, pick_col(df, REGION_CANDS), "metro name")
    dcol   <- pick_col(df, c("down", "down_pct", "downpayment", "down_payment"))
    down   <- need(df, dcol, "down payment")
    if (max(down, na.rm = TRUE) <= 1) down <- down * 100   # 0.05 -> 5

    own  <- df[[pick_col(df, c("own", "own_cost", "total_payment",
                               "payment", "monthly_own", "buy"))]]
    rent <- df[[pick_col(df, c("rent", "rent_cost", "monthly_rent"))]]
    gcol <- pick_col(df, c("gap", "diff", "monthly_gap"))
    gap  <- if (!is.na(gcol)) df[[gcol]] else own - rent
    if (is.null(gap)) stop("no gap column and could not compute own - rent")

    tibble::tibble(RegionName = region, comparator_key = key,
                   down = down, own = own, rent = rent, gap = gap)
  }

  out <- dplyr::bind_rows(one(raw$gap_all, "all"), one(raw$gap_sfr, "sfr"))

  set.seed(42)  # stable jitter: same metro, same vertical slot, every render
  jit <- tibble::tibble(RegionName = unique(out$RegionName),
                        y_jitter = stats::runif(length(unique(out$RegionName)),
                                                -1, 1))
  dplyr::left_join(out, jit, by = "RegionName")
}

# Columns out: RegionName | years | flips (gap < 0 at 20% down vs SFR)
adapt_years <- function(raw) {
  stopifnot(!is.null(raw$years_to_save))
  df <- raw$years_to_save
  region <- need(df, pick_col(df, REGION_CANDS), "metro name")
  years  <- need(df, pick_col(df, c("years", "years_to_save",
                                    "YearsToSave", "value")), "years")
  base <- tibble::tibble(RegionName = region, years = years)

  flips <- adapt_gaps(raw) |>
    dplyr::filter(comparator_key == "sfr", down == 20) |>
    dplyr::transmute(RegionName, flips = gap < 0)

  dplyr::inner_join(base, flips, by = "RegionName")
}

# Knoxville ZHVI series for Burhan's default intro figure.
adapt_intro <- function(raw) {
  stopifnot(!is.null(raw$zhvi))
  df <- raw$zhvi
  tibble::tibble(
    RegionName = need(df, pick_col(df, REGION_CANDS), "metro name"),
    date       = need(df, pick_col(df, DATE_CANDS), "date"),
    value      = need(df, pick_col(df, VALUE_CANDS), "value")
  ) |> dplyr::filter(is_knox(RegionName))
}

# Indexed KNX vs SF series, Dec 2019 = 100. Pass-through with checks.
adapt_divergence <- function(raw) {
  stopifnot(!is.null(raw$claim1_index))
  df <- raw$claim1_index
  tibble::tibble(
    RegionName = need(df, pick_col(df, REGION_CANDS), "metro name"),
    date       = need(df, pick_col(df, DATE_CANDS), "date"),
    index      = need(df, pick_col(df, c("index", "idx", "value")), "index")
  )
}

# Austin ZORI series for tab 4. The top-50 ranking table source is NOT
# defined here yet: it needs the size-rank filter, and I don't know which
# artifact carries SizeRank. TODO(Connor): confirm source, then extend.
adapt_austin <- function(raw) {
  stopifnot(!is.null(raw$zori_all))
  df <- raw$zori_all
  tibble::tibble(
    RegionName = need(df, pick_col(df, REGION_CANDS), "metro name"),
    date       = need(df, pick_col(df, DATE_CANDS), "date"),
    value      = need(df, pick_col(df, VALUE_CANDS), "value")
  ) |> dplyr::filter(grepl("^Austin", RegionName))
}
