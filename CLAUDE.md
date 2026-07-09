# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

R project that produces Legal Needs Survey country reports for Italy, Malta, and Poland (OECD EU Legal Needs Survey). The pipeline reads raw EU-GPP survey data (Stata format), processes it, runs statistical analysis, and generates charts and tables for the reports.

## Setup

Requires R 4.6.0 and `renv` for dependency management. On macOS, if compilation errors occur during restore, install `gettext` via Homebrew first (see README).

```bash
Rscript --vanilla -e "renv::restore(prompt = FALSE)"
```

Before running anything, add your machine's paths to `src/utils/config.R` by adding a new `else if` block matching your system username (`Sys.info()["user"]`). Two paths are required:
- `path2EU` — SharePoint root for EU Subnational data (contains raw Stata file at `eu-gpp/1. Data/3. Merge/EU_GPP_2024.dta`)
- `path2DA` — SharePoint root for Data Analytics (contains fonts at `6. Country Reports/0. Fonts/`)

## Running the Pipeline

All execution goes through `main.R` from the project root:

```bash
Rscript main.R --data                    # load & wrangle raw data → data/master.csv
Rscript main.R --analysis                # run all statistical modules
Rscript main.R --viz                     # generate all charts (runs analysis internally)
Rscript main.R --data --analysis --viz   # full pipeline
Rscript main.R --main_indicators         # country-level summary table
Rscript main.R --verbose --data          # add --verbose to any command for progress logs
Rscript main.R --update-packages         # renv::update() + renv::snapshot()
```

**Execution dependency**: `--data` must run before `--analysis`. `--viz` sources `worker-analysis.R` internally (handles the analysis dependency), but still requires `data/master.csv` to exist.

For interactive development in RStudio, run worker files directly: `src/data/worker-data.R`, `src/analysis/worker-analysis.R`, or `src/viz/worker-viz.R`.

## Architecture

### Execution Flow

`main.R` parses CLI flags via `optparse` and dispatches to worker files in fixed order: data → analysis → viz.

Each worker file bootstraps its own dependencies by sourcing `src/utils/config.R` (when run interactively) and `src/utils/utils.R`.

### Key Files

- **`src/utils/config.R`** — Machine-specific paths (`path2EU`, `path2DA`) and Chrome detection for `chromote`. Add new user entries here.
- **`src/utils/utils.R`** — Core reusable functions: `get_nuts_avg()`, `get_wgt_national_avg()`, `get_results_table()`, `export_results_kable()`, `estimate_logit()`. Also defines `study_countries` and `independent_vars` (logit regressors per country).
- **`src/data/data-loading.R`** — Single `load_data(path2EU, source)` function. `source` accepts: `"gpp"` (raw Stata file), `"subset"` (`data/master.csv`), `"regions"` (`data/regions.csv`), `"jg"` (`data/jg_data_keepdk.csv`).
- **`src/data/special-vars.R`** — All derived variable construction: recoded demographics (`add_special_demographics()`), A2J outcome variables (`add_a2j_vars()`), quality check logic (`apply_a2j_quality_check()`), justice gap variables, and Sankey flow variables. Also defines `QUALITY_CHECK_NONTRIVIAL_THRESHOLD = 25`.
- **`src/data/worker-data.R`** — Loads raw Stata file, applies quality check, joins all derived variables, saves `data/master.csv` and justice gap CSVs.
- **`src/analysis/worker-analysis.R`** — Loads `data/master.csv` + `data/regions.csv`, runs all modules (1–5), stores results as named R objects in the global environment for viz functions to consume.
- **`src/analysis/module_*.R`** — Individual analysis functions per report section. Each file defines named functions that take `(master, regions)` as primary arguments.
- **`src/viz/worker-viz.R`** — Sources `worker-analysis.R` first, then loads fonts from `path2DA`, reads EU-average comparison data, and calls chart generator functions for every figure in the report.
- **`src/viz/*.R`** — Chart type implementations: `grouped_bars.R`, `horizontal_bars.R`, `dumbbells.R`, `sankey_advice&rep.R`, `sankey_drm.R`, `jg_upset.R`.

### Data Flow

```
EU_GPP_2024.dta (Stata, via path2EU)
    └─ worker-data.R
          ├─ data/master.csv          (git-ignored, ~study_vars subset)
          ├─ data/jg_data_nodk.csv    (git-ignored)
          └─ data/jg_data_keepdk.csv  (git-ignored)

data/master.csv + data/regions.csv (committed)
    └─ worker-analysis.R → R objects in memory
          └─ worker-viz.R → output/viz/ (git-ignored)
```

### Key Design Decisions

**NUTS-weighted national averages**: All country-level estimates are NUTS region averages weighted by regional population share (`regionpoppct` in `data/regions.csv`). Cells with n < 30 are suppressed (set to NA). This happens via `get_nuts_avg()` → `get_wgt_national_avg()`.

**Quality check**: Respondents reporting more than 25 non-trivial legal problems (severity ≥ 4) are excluded from A2J experience analyses (set to NA). This is applied once in `apply_a2j_quality_check()` during the data step.

**Justice gap methods**: Two variants — `"noDK"` excludes DK responses; `"keepDK"` treats DK as unmet need. A respondent is inside the justice gap when `a2j_score < 0.67`.

**Malta sample size**: Malta has a smaller sample than Italy and Poland. Several demographic breakdown categories (e.g., "Category", "Age Group") are omitted for Malta in viz calls via the `omit.cats` argument to chart functions.

**Chrome dependency**: PNG/PDF table exports via `kableExtra` require Chrome or Chromium. The `safe_save_kable()` wrapper in `utils.R` skips exports gracefully if Chrome is unavailable, so the pipeline does not crash in headless environments.

**Namespace convention**: All calls to packages outside `base` and `tidyverse` must use the `::` operator (e.g., `haven::read_stata()`, `marginaleffects::avg_slopes()`).
