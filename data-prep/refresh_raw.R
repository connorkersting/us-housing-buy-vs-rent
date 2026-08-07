# refresh_raw.R
# Downloads the 8 current Zillow Research CSVs into data-raw/, overwriting
# whatever is there. Run from the project root:
#   Rscript data-prep/refresh_raw.R
# Then rerun data-prep/build_artifacts.R to regenerate app/data/*.rds.
# Base R only (no packages) so it can run early in CI before renv/tidyverse
# is installed.

base_url <- "https://files.zillowstatic.com/research/public_csvs"

files <- list(
  "zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv",
  "zori/Metro_zori_uc_sfrcondomfr_sm_month.csv",
  "zori/Metro_zori_uc_sfr_sm_month.csv",
  "total_monthly_payment/Metro_total_monthly_payment_downpayment_0.05_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv",
  "total_monthly_payment/Metro_total_monthly_payment_downpayment_0.10_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv",
  "total_monthly_payment/Metro_total_monthly_payment_downpayment_0.20_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv",
  "years_to_save/Metro_years_to_save_downpayment_0.20_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv",
  "new_con_sales_count_raw/Metro_new_con_sales_count_raw_uc_sfrcondo_month.csv"
)

dir.create("data-raw", showWarnings = FALSE)

for (rel_path in files) {
  dest <- file.path("data-raw", basename(rel_path))
  url  <- file.path(base_url, rel_path)
  cat("Downloading", basename(rel_path), "...\n")
  download.file(url, dest, mode = "wb", quiet = TRUE)
}

cat("\nDone. Raw CSVs updated in data-raw/.\n")
cat("Next: Rscript data-prep/build_artifacts.R\n")
