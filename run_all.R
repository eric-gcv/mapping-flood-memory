# Runs the full pipeline (needs the raw data in data/ and data/flood_data.db).
# Usage: Rscript run_all.R
steps <- c("scripts/01_panel.R", "scripts/02_covariates.R", "scripts/03_changepoints.R",
           "scripts/04_pca_clusters.R", "scripts/05_export_results.R")
for (s in steps) {
  message("==== ", s)
  t0 <- Sys.time()
  source(here::here(s), echo = FALSE)
  message("     done in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
}
