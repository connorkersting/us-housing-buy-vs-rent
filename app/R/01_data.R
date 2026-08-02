# 01_data.R -- loads artifacts and adapts them to what the modules expect.
#
# PATCHED 2026-08-02 against real schemas from data-prep/check_artifacts.R.
# Schema guesses replaced with verified column names.
#
# VERIFIED SCHEMAS:
#   gap_all (1167 = 389 metros x 3 down) / gap_sfr (1146 = 382 x 3):
#     down (0.05/0.10/0.20) | RegionID | RegionName | SizeRank | date
#     | payment | zori | gap_dollars (= payment - zori) | gap_pct
#     Single date, 2026-06-30. Negative gap = buying cheaper.
#   gap_summary : 6 rows, precomputed counts. Used as a correctness check.
#   years_to_save : FULL MONTHLY SERIES, 67686 rows. Must filter to latest date.
#   claim1_index : 636 rows, 2 metros x 318 months. RegionName | date | index
#   zhvi / zori_all / zori_sfr : long series, value cols named zhvi / zori
# Scaffolding generated with AI assistance (Claude); see AI_LOG.md.

load_data <- function(dir = "data") {
  paths <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  raw <- stats::setNames(lapply(paths, readRDS),
                         tools::file_path_sans_ext(basename(paths)))

  out <- list(
    raw        = raw,
    intro      = adapt_safely("intro",      adapt_intro(raw)),
    divergence = adapt_safely("divergence", adapt_divergence(raw)),
    gaps       = adapt_safely("gaps",       adapt_gaps(raw)),
    years      = adapt_safely("years",      adapt_years(raw)),
    austin     = adapt_safely("austin",     adapt_austin(raw)),
    ranking    = adapt_safely("ranking",    adapt_ranking(raw))
  )

  # Cross-check live counts against the precomputed gap_summary. Warns only,
  # never stops the app. A mismatch means an adapter drifted from the data.
  adapt_safely("validate", validate_gaps(out$gaps, raw$gap_summary))
  out
}

# ---- helpers ---------------------------------------------------------------

adapt_safely <- function(name, expr) {
  tryCatch(expr, error = function(e) {
    warning(sprintf("Adapter '%s' failed: %s", name, conditionMessage(e)),
            call. = FALSE)
    NULL
  })
}

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

# Long frame both tab 3 views filter. Columns out:
#   RegionName | comparator_key ("all"/"sfr") | down (5/10/20) | own | rent |
#   gap (own - rent, negative = buying cheaper) | gap_pct | SizeRank | y_jitter
# y_jitter is fixed per metro so dots hold their vertical slot when the radio
# flips, making all movement read as horizontal.
adapt_gaps <- function(raw) {
  stopifnot(!is.null(raw$gap_all), !is.null(raw$gap_sfr))

  one <- function(df, key) {
    region <- need(df, pick_col(df, REGION_CANDS), "metro name")
    down   <- need(df, pick_col(df, c("down", "down_pct")), "down payment")
    if (max(down, na.rm = TRUE) <= 1) down <- down * 100   # 0.05 -> 5

    own  <- need(df, pick_col(df, c("payment", "own", "total_payment")), "owning cost")
    rent <- need(df, pick_col(df, c("zori", "rent", "monthly_rent")), "rent")

    gcol <- pick_col(df, c("gap_dollars", "gap", "diff"))
    gap  <- if (!is.na(gcol)) df[[gcol]] else own - rent

    pcol <- pick_col(df, c("gap_pct", "pct_gap"))
    pct  <- if (!is.na(pcol)) df[[pcol]] else gap / rent

    scol <- pick_col(df, c("SizeRank", "size_rank"))

    tibble::tibble(
      RegionName = region, comparator_key = key, down = down,
      own = own, rent = rent, gap = gap, gap_pct = pct,
      SizeRank = if (!is.na(scol)) df[[scol]] else NA_real_
    )
  }

  out <- dplyr::bind_rows(one(raw$gap_all, "all"), one(raw$gap_sfr, "sfr"))

  set.seed(42)  # stable jitter across renders
  metros <- unique(out$RegionName)
  jit <- tibble::tibble(RegionName = metros,
                        y_jitter = stats::runif(length(metros), -1, 1))
  dplyr::left_join(out, jit, by = "RegionName")
}

# years_to_save is a full monthly series. Filter to the latest month so it
# aligns with the gap tables (2026-06-30). Columns out:
#   RegionName | years | flips (gap < 0 at 20% down vs single-family rent)
adapt_years <- function(raw) {
  stopifnot(!is.null(raw$years_to_save))
  df <- raw$years_to_save

  base <- tibble::tibble(
    RegionName = need(df, pick_col(df, REGION_CANDS), "metro name"),
    date       = need(df, pick_col(df, DATE_CANDS), "date"),
    years      = need(df, pick_col(df, c("years_to_save", "years")), "years")
  )
  base <- dplyr::filter(base, date == max(date, na.rm = TRUE))
  base <- dplyr::select(base, RegionName, years)

  flips <- adapt_gaps(raw) |>
    dplyr::filter(comparator_key == "sfr", down == 20) |>
    dplyr::transmute(RegionName, flips = gap < 0)

  dplyr::inner_join(base, flips, by = "RegionName")
}

adapt_intro <- function(raw) {
  stopifnot(!is.null(raw$zhvi))
  df <- raw$zhvi
  tibble::tibble(
    RegionName = need(df, pick_col(df, REGION_CANDS), "metro name"),
    date       = need(df, pick_col(df, DATE_CANDS), "date"),
    value      = need(df, pick_col(df, VALUE_CANDS), "value")
  ) |> dplyr::filter(is_knox(RegionName))
}

adapt_divergence <- function(raw) {
  stopifnot(!is.null(raw$claim1_index))
  df <- raw$claim1_index
  tibble::tibble(
    RegionName = need(df, pick_col(df, REGION_CANDS), "metro name"),
    date       = need(df, pick_col(df, DATE_CANDS), "date"),
    index      = need(df, pick_col(df, c("index", "idx")), "index")
  )
}

adapt_austin <- function(raw) {
  stopifnot(!is.null(raw$zori_all))
  df <- raw$zori_all
  tibble::tibble(
    RegionName = need(df, pick_col(df, REGION_CANDS), "metro name"),
    date       = need(df, pick_col(df, DATE_CANDS), "date"),
    value      = need(df, pick_col(df, VALUE_CANDS), "value")
  ) |> dplyr::filter(grepl("^Austin", RegionName))
}

# Top-50 metros by SizeRank, both comparators and all three down levels.
# The tab 4 owner filters to the one combination that reproduces the verified numbers.
# TODO(Connor): confirm which (down, comparator_key) gives Austin 88.4% and
# San Jose 162%, then tell the tab 4 owner to hardcode that filter.
adapt_ranking <- function(raw) {
  adapt_gaps(raw) |>
    dplyr::filter(!is.na(SizeRank), SizeRank <= 50)
}

# ---- validation ------------------------------------------------------------

# Compares counts computed live from gap_all / gap_sfr against the
# precomputed gap_summary. Warns on any mismatch; never stops the app.
validate_gaps <- function(gaps, gap_summary) {
  if (is.null(gaps) || is.null(gap_summary)) return(invisible(NULL))

  expected <- gap_summary |>
    dplyr::mutate(
      comparator_key = ifelse(grepl("single", rental_type, ignore.case = TRUE),
                              "sfr", "all"),
      down = ifelse(down <= 1, down * 100, down)
    ) |>
    dplyr::select(comparator_key, down, buy_cheaper_n, metros)

  actual <- gaps |>
    dplyr::summarise(n_buy = sum(gap < 0, na.rm = TRUE),
                     n_metros = dplyr::n(),
                     .by = c(comparator_key, down))

  chk <- dplyr::inner_join(expected, actual, by = c("comparator_key", "down"))
  bad <- dplyr::filter(chk, buy_cheaper_n != n_buy | metros != n_metros)

  if (nrow(bad) > 0) {
    warning("gap_summary MISMATCH. Live counts disagree with precomputed:\n",
            paste(utils::capture.output(print(as.data.frame(bad))),
                  collapse = "\n"), call. = FALSE)
  } else {
    message(sprintf("Validation OK: %d of %d summary rows match live counts.",
                    nrow(chk), nrow(expected)))
  }
  invisible(chk)
}
