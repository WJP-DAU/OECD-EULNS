## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Worker for the Hungary additional analyses and figure corrections
## Author(s):         Santiago Pardo (spardo@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     September 2026
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Run from the project root:   Rscript src/hungary/worker-hungary.R
## Requires data/master.csv, data/jg_data_keepdk.csv (Rscript main.R --data) and access to path2EU.
## Briefs: input/Analisis Hungria/analisis/. Outputs: output/viz/Hungary/analisis adicionales/.
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("src/hungary/setup.R")
source("src/hungary/viz_helpers.R")
source("src/hungary/analysis_helpers.R")

briefs <- c("R01", "R02", "R03", "R04", "F01", "F02", "F03", "F04", "F05", "F06", "F07", "F08", "F09", "F10",
            "A01", "A03", "A04", "A05")
for (b in briefs) source(file.path("src", "hungary", paste0(b, ".R")))
source("src/hungary/summary.R")

# 1 · Reconciliations
for (b in c("R03", "R04", "R01", "R02")) {
  verbose_message(paste("---", b))
  get(paste0("run_", b))()
}

# 2 · Figures and corrections
for (b in c("F05", "F09", "F08", "F01", "F04", "F03", "F02", "F06", "F07", "F10")) {
  verbose_message(paste("---", b))
  get(paste0("run_", b))()
}

# 3 · New analyses
af <- build_analysis_frame()
for (b in c("A01", "A03", "A04", "A05")) {
  verbose_message(paste("---", b))
  get(paste0("run_", b))(af)
}

# 4 · Index
write_audit()
write_summary()
verbose_message("Hungary additional analyses finished ✅")
