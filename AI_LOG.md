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