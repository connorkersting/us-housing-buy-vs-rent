# build_artifacts.R
# Reads raw Zillow CSVs from data-raw/, writes app-ready .rds to app/data/.
# Run from the project root. Safe to rerun.
#
# Locked methodology decisions. All four tabs must use these.
#   Base month for indexing : 2019-12-31 (pre-pandemic anchor)
#   Geography               : metro only (RegionType == "msa")
#   Ownership cost          : Zillow "Total Monthly Payment" = P&I + insurance
#                             + property tax + 0.5% maintenance, and +1% PMI
#                             when down payment < 20%
#   Series                  : historical only, no ZHVF/ZORF forecasts
#   Caveat for methods slide: Zillow rebuilt the full historical ZHVI in
#                             Jan 2023 using the neural Zestimate, so the
#                             long series is retrospectively modeled

library(tidyverse)

BASE <- as.Date("2019-12-31")

dir.create("app/data", recursive = TRUE, showWarnings = FALSE)

# Zillow ships wide (one column per month). Everything downstream needs long.
read_zillow <- function(path, value_name) {
  read_csv(path, show_col_types = FALSE) |>
    filter(RegionType == "msa") |>              # drops the United States row
    pivot_longer(matches("^\\d{4}-\\d{2}-\\d{2}$"),
                 names_to = "date", values_to = value_name) |>
    mutate(date = as.Date(date)) |>
    select(RegionID, RegionName, SizeRank, date, all_of(value_name))
}

raw <- function(f) file.path("data-raw", f)

# ---- load ----------------------------------------------------------------
zhvi     <- read_zillow(raw("Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"), "zhvi")
zori_all <- read_zillow(raw("Metro_zori_uc_sfrcondomfr_sm_month.csv"), "zori")
zori_sfr <- read_zillow(raw("Metro_zori_uc_sfr_sm_month.csv"), "zori")
yts      <- read_zillow(raw("Metro_years_to_save_downpayment_0.20_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"), "years_to_save")
newcon   <- read_zillow(raw("Metro_new_con_sales_count_raw_uc_sfrcondo_month.csv"), "new_con_sales")

# Three down payment scenarios stacked, so the Shiny control is a filter
# rather than three separate datasets.
pay <- c("0.05", "0.10", "0.20") |>
  set_names() |>
  map_dfr(\(d) read_zillow(
    raw(sprintf("Metro_total_monthly_payment_downpayment_%s_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv", d)),
    "payment"
  ), .id = "down") |>
  mutate(down = as.numeric(down))

# ---- tab 2: Knoxville vs San Francisco -----------------------------------
# Index to a common base month, otherwise this compares price levels
# (an expensive city vs a cheap one) instead of growth.
claim1 <- zhvi |>
  filter(RegionName %in% c("Knoxville, TN", "San Francisco, CA")) |>
  group_by(RegionName) |>
  mutate(index = zhvi / zhvi[date == BASE] * 100) |>
  ungroup()

# ---- tab 3: own vs rent, two rental definitions --------------------------
build_gap <- function(rent_df) {
  d <- min(max(rent_df$date), max(pay$date))     # align to a common month
  pay |>
    filter(date == d) |>
    inner_join(rent_df |> filter(date == d),
               by = c("RegionID", "RegionName", "SizeRank", "date")) |>
    mutate(gap_dollars = payment - zori,
           gap_pct     = gap_dollars / zori)
}

gap_all <- build_gap(zori_all)   # houses vs all rentals, apartments included
gap_sfr <- build_gap(zori_sfr)   # houses vs houses

# The headline table. Same question, six answers.
gap_summary <- bind_rows(
  gap_all |> mutate(rental_type = "All rentals (incl. apartments)"),
  gap_sfr |> mutate(rental_type = "Single-family only")
) |>
  group_by(rental_type, down) |>
  summarise(metros          = n(),
            buy_cheaper_n   = sum(gap_dollars < 0),
            own_costs_more  = mean(gap_dollars > 0),
            median_gap      = median(gap_dollars),
            .groups = "drop") |>
  arrange(rental_type, down)

# ---- write ---------------------------------------------------------------
saveRDS(zhvi,          "app/data/zhvi.rds")
saveRDS(zori_all,      "app/data/zori_all.rds")
saveRDS(zori_sfr,      "app/data/zori_sfr.rds")
saveRDS(pay,           "app/data/payment.rds")
saveRDS(claim1,        "app/data/claim1_index.rds")
saveRDS(gap_all,       "app/data/gap_all.rds")
saveRDS(gap_sfr,       "app/data/gap_sfr.rds")
saveRDS(gap_summary,   "app/data/gap_summary.rds")
saveRDS(yts,           "app/data/years_to_save.rds")
saveRDS(newcon,        "app/data/new_construction.rds")

# ---- console checks ------------------------------------------------------
cat("\n--- tab 2: indexed to Dec 2019 ---\n")
claim1 |> filter(date == max(date)) |> select(RegionName, zhvi, index) |> print()

cat("\n--- tab 3: buy vs rent across down payment and rental type ---\n")
print(gap_summary)

cat("\n--- tab 3 closer: years to save 20% down, top 50 metros ---\n")
yts |> filter(date == max(date), SizeRank <= 50) |>
  arrange(desc(years_to_save)) |> select(RegionName, years_to_save) |> print(n = 10)

cat("\n--- tab 4: Austin rent, peak vs now ---\n")
zori_all |> filter(RegionName == "Austin, TX", date >= as.Date("2019-01-31")) |>
  summarise(peak = max(zori), peak_date = date[which.max(zori)],
            now = zori[which.max(date)]) |>
  mutate(off_peak_pct = (now - peak) / peak) |> print()
