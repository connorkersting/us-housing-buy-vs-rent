# Is it cheaper to buy or rent? It depends on two assumptions nobody states.

Published analyses of buying versus renting contradict each other. Realtor.com
reported renting cheaper in all 50 largest metros. Empower reported buying
cheaper in 23 of 50. Neither is wrong. They used different definitions of
"rent."

Change two assumptions, the down payment and what you compare against, and the
number of US metros where buying costs less per month than renting moves from
**0 to 89**.

| Metros where owning costs less per month | 5% down | 10% down | 20% down |
|---|---|---|---|
| vs all rentals (n = 389) | 0 | 1 | 16 |
| vs single-family rentals (n = 382) | 6 | 12 | 89 |

Median monthly gap, owning minus renting, vs all rentals: $1,143 / $1,016 /
$579. Vs single-family only: $785 / $668 / $234.

And winning the monthly math is not the same as getting in the door. The 89
metros that flip at 20% down take a median 6.4 years for the median household
to save the down payment. The 293 that do not take 8.4. Los Angeles takes 18.8
years and still does not flip.

**Live app:** https://connorkersting.shinyapps.io/us-housing-buy-vs-rent/

## What is here

```
data-raw/                8 Zillow research CSVs
data-prep/
  build_artifacts.R      rebuilds every artifact from data-raw/
  check_artifacts.R      prints the schema of each artifact
app/
  app.R                  navbar shell, all library() calls
  R/00_theme.R           shared palette, ggplot theme, number formatters
  R/01_data.R            loads artifacts and adapts them for the modules
  R/mod_tab1_intro.R     Overview
  R/mod_tab2_divergence.R Knoxville vs San Francisco
  R/mod_tab4_austin.R    Austin (third tab; file numbering is intentional)
  R/mod_tab3_buyrent.R   Buy or Rent (fourth tab, the interactive one)
  data/                  .rds artifacts, committed so the app runs on clone
preview.R                renders one tab's chart outside Shiny
```

## Reproducing every number

```r
# 1. Rebuild the artifacts from the raw CSVs
source("data-prep/build_artifacts.R")

# 2. Run the app
shiny::runApp("app")
```

`load_data()` cross-checks its live counts against a precomputed summary table
at startup and prints `Validation OK: 6 of 6 summary rows match live counts`.
Any mismatch warns with the offending rows.

To verify the headline numbers directly:

```r
source("app/R/01_data.R")
d <- load_data("app/data")

# 0/1/16 and 6/12/89
dplyr::summarise(d$gaps, n = sum(gap < 0), .by = c(comparator_key, down))

# 6.4 (n=89) and 8.4 (n=293)
dplyr::summarise(d$years, med = median(years), n = dplyr::n(), .by = flips)
```

Packages: shiny, bslib, dplyr, tidyr, ggplot2, scales, ggrepel.

## Method

- **Geography:** metro level only, `RegionType == "msa"`.
- **Ownership cost:** Zillow's total monthly payment. Principal and interest,
  homeowner's insurance, property tax, maintenance at 0.5% of home value, plus
  1% mortgage insurance when the down payment is under 20%. That mortgage
  insurance cutoff is a large part of why 20% down changes the answer so much.
- **Like for like:** ZHVI covers the 35th to 65th percentile of homes. ZORI
  covers listed rents in the same band. Same band on both sides.
- **Alignment:** both gap tables are fixed at 2026-06-30. No cross-series lag.
- **Indexing:** December 2019 = 100.
- **No forecasts.** Every claim is about what already happened, and Zillow's
  forecast horizon is 12 months.
- **Caveat:** Zillow rebuilt the full historical ZHVI in January 2023 using the
  neural Zestimate, so the long series is retrospectively modeled rather than
  continuously measured.

## Scope

The Austin tab is descriptive. It shows that rents fell 11% from their August
2022 peak while buying still costs 88% more per month, and makes no causal
claim about why rents fell. There is no permits data here.

## Data

Zillow Research public data, https://www.zillow.com/research/data/
ZHVI, ZORI (all rentals and single-family), total monthly payment, years to
save. Monthly through June 2026.

## Attribution

Built for BZAN 583.01 at the University of Tennessee, Knoxville. AI assistance
was used for the app scaffolding and chart code; see `AI_LOG.md` for a
per-session record of what was AI-written versus author-written. All data
acquisition, preparation, methodology decisions, and findings are the authors'.
