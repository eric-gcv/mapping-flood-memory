# Usage:
#   Rscript tests/run_tests.R                 # fast tier: checks the committed results/ (no database needed)
#   RUN_FULL=1 Rscript tests/run_tests.R      # full tier: reruns the whole pipeline (needs data/ and data/flood_data.db),
#                                             # then checks that the new results/ equal the committed ones
library(testthat)
root <- here::here()
if (identical(Sys.getenv("RUN_FULL"), "1")) {
  stopifnot(file.exists(file.path(root, "data", "flood_data.db")))
  snap <- file.path(tempdir(), "release_results")
  dir.create(snap)
  file.copy(list.files(file.path(root, "results"), full.names = TRUE), snap)   # committed results = reference
  Sys.setenv(RELEASE_SNAPSHOT = snap)
  source(file.path(root, "run_all.R"))
}
res <- test_dir(file.path(root, "tests", "testthat"), reporter = "summary", stop_on_failure = FALSE)
df <- as.data.frame(res)
if (any(df$failed > 0 | df$error)) quit(status = 1)
