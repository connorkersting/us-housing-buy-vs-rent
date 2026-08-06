# AI contribution log

Running ledger for the course-required AI disclosure. One entry per work
session. The submission build script will generate the disclosure header
from this file.

## 2026-07-31 (Fri) - scaffolding drop

**Claude (AI) wrote:**
- App skeleton: `app/app.R`, `app/R/00_theme.R`, `app/R/01_data.R`
- Module pattern and template: `app/R/mod_template.R`
- Tab modules: `mod_tab1_intro.R` (template instance), `mod_tab2_divergence.R`
  (stub), `mod_tab3_buyrent.R` (full), `mod_tab4_austin.R` (stub)
- Schema inspector: `data-prep/check_artifacts.R`
- Data adapters written against ASSUMED schemas, pending verification

**Connor wrote (prior and ongoing):**
- All data acquisition and preparation: `data-prep/build_artifacts.R`, all
  eight raw-CSV pipelines, every artifact in `app/data/`
- All verified findings and numbers in every tab
- Methodology decisions: indexing base, geography filter, cost definitions,
  percentile-band alignment, no-forecast rule

**Team writes:**
- Tab content (claims, charts, text) is authored by each tab owner:
  Burhan tab 1, Jack tab 2, Connor tab 3, James tab 4

**Pending attribution updates:**
- Adapter patches after schema check
- Build script, README, narrative doc (upcoming sessions)

**Jack Wrote (still ongoing)**
2026-08-03 Jack tab 2: AI edited build_tab2_chart() in mod_tab2_divergence.R (fixed missing + breaking the plot, corrected RegionName color mapping, repositioned annotation/labels, adjusted margins).
2026-08-05 Connor tab 4: AI fixed build_tab3_years in mod_tab3_buyrent.R (removed the position/nudge_y combination ggrepel rejects, which was the crash).
2026-08-05 Connor tab 4: AI fixed build_tab3_years in mod_tab3_buyrent.R (replaced per-layer position_jitter with one precomputed beeswarm column, so the Knoxville outline and label land on Knoxville's own dot).
2026-08-05 Connor tab 4: AI fixed build_tab3_years in mod_tab3_buyrent.R (labels hang below their row and vertical padding tightened, clearing overlaps and dead canvas).
2026-08-05 Connor tab 4: AI fixed build_tab3_strip in mod_tab3_buyrent.R (off-axis metros are dropped and counted in the caption instead of oob_squish stacking them into a fake cluster on the panel edge).
2026-08-05 Connor tab 4: AI fixed build_tab3_strip in mod_tab3_buyrent.R (default xlim pads outward instead of clipping the smallest metro when every gap is positive).
2026-08-05 Connor tab 4: AI edited build_tab3_strip in mod_tab3_buyrent.R (shared FOCUS_METROS labels for Knoxville, Nashville and Austin, placed in a reserved lane above the cloud with leader lines).
2026-08-05 Connor tab 4: AI edited build_tab3_strip in mod_tab3_buyrent.R (dot intensity now grades with gap magnitude on two separate ramps, hue still strictly binary by sign).
2026-08-05 Connor tab 4: AI edited build_tab3_years in mod_tab3_buyrent.R (same focus cities as the strip plus the slow metros, all in one repel layer inside a reserved lane below the row).
2026-08-05 Connor tab 4: AI edited FOCUS_METROS in mod_tab3_buyrent.R (added New York as the expensive-coastal anchor on both charts, replacing San Francisco's role on the strip).
2026-08-05 Connor tab 3: AI edited build_tab4_chart in mod_tab4_austin.R (shared theme, PROJ_ACCENT/PROJ_DARK, lab_dollar, unclipped current-value label, sentence-case title).
2026-08-05 Connor tab 2: AI edited build_tab2_chart in mod_tab2_divergence.R (restored label leader lines, trimmed right axis expansion from 0.28 to 0.14).
2026-08-05 Connor: AI deleted app/R/mod_template.R (unmounted scaffold containing "EDIT 1" and "PLACEHOLDER" strings that shipped with the deploy).
