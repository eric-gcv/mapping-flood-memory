# Consistency of county_results.csv: random sample of counties re-derived from the file's own covariates,
# flags vs. NA patterns, and the cases that were inconsistent in an earlier version.
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
res  <- read.csv(file.path(root, "results", "county_results.csv"), colClasses = c(fips = "character", state_fips = "character"), na.strings = "")
ld   <- read.csv(file.path(root, "results", "pca_loadings.csv"))
cs   <- read.csv(file.path(root, "results", "cluster_summary.csv"))
xvars <- c(iniTakeup = "initial_takeup_pct", sfhaRatio = "sfhaRatio", pcntWater = "pcntWater", evtsYear = "evtsYear",
           dmgYear = "dmgYear", RISK_SCORE = "RISK_SCORE", SOVI_SCORE = "SOVI_SCORE", RESL_SCORE = "RESL_SCORE",
           dis_pop_pct = "dis_pop_pct", pcntParticipating = "pcntParticipating", pcntClassRating = "pcntClassRating")
rescore <- function(d) {                    # PCs recomputed from the csv's own input columns
  X <- as.matrix(d[, unname(xvars[ld$variable])])
  Z <- sweep(sweep(X, 2, ld$center), 2, ld$scale, "/")
  Z %*% as.matrix(ld[, c("score_loading_PC1", "score_loading_PC2", "score_loading_PC3")])
}
clustered <- res[!is.na(res$cluster), ]

test_that("PC scores exist exactly for clustered counties and are internally consistent", {
  expect_equal(is.na(res$PC1), is.na(res$cluster))
  expect_equal(is.na(res$PC2), is.na(res$cluster))
  expect_equal(is.na(res$PC3), is.na(res$cluster))
  expect_equal(res$shows_memory_effect, !is.na(res$cluster))
  expect_equal(res$shows_salience_effect, !is.na(res$salience_pct))
  expect_equal(res$shows_memory_effect, !is.na(res$memory_months))
  expect_true(all(res$shows_salience_effect[res$shows_memory_effect]))
})

test_that("random sample of clustered counties: PCs recomputed from covariates match", {
  set.seed(as.integer(Sys.time()))
  smp <- clustered[sample(nrow(clustered), 60), ]
  expect_equal(unname(rescore(smp)), unname(as.matrix(smp[, c("PC1", "PC2", "PC3")])), tolerance = 1e-6)
})

test_that("previously inconsistent counties are consistent", {
  chk <- res[res$fips %in% c("29183", "29223", "29169"), ]
  expect_equal(nrow(chk), 3)
  expect_equal(unname(rescore(chk)), unname(as.matrix(chk[, c("PC1", "PC2", "PC3")])), tolerance = 1e-6)
  no_mem <- res[res$fips %in% c("29201", "29203", "29207", "29213", "29215"), ]
  expect_equal(nrow(no_mem), 5)
  expect_true(all(is.na(no_mem$PC1) & is.na(no_mem$cluster) & !no_mem$shows_memory_effect))
})

test_that("random sample of counties: reported numbers are coherent", {
  set.seed(as.integer(Sys.time()))
  smp <- res[sample(nrow(res), 200), ]
  expect_true(all(is.na(smp$memory_months) | !is.na(smp$salience_pct_clustered)))
  expect_true(all(is.na(smp$salience_pct) == !smp$shows_salience_effect))
  expect_true(all(smp$salience_pct[smp$shows_salience_effect] > 0))
  expect_true(all(smp$memory_months[smp$shows_memory_effect] > 0))
})

test_that("cluster means of the county table equal cluster_summary (Table 2)", {
  for (k in cs$cluster) {
    d <- clustered[clustered$cluster == k, ]
    r <- cs[cs$cluster == k, ]
    expect_equal(nrow(d), r$n)
    expect_equal(mean(d$salience_pct_clustered), r$salience_mean, tolerance = 1e-8)
    expect_equal(mean(d$memory_months), r$memory_mean, tolerance = 1e-8)
    expect_equal(mean(d$PC1), r$PC1_table2, tolerance = 1e-8)
    expect_equal(mean(d$PC2), r$PC2_table2, tolerance = 1e-8)
    expect_equal(mean(d$PC3), r$PC3_table2, tolerance = 1e-8)
  }
})

test_that("PCA input columns are complete where the sources have data", {
  for (v in c("sfhaRatio", "pcntParticipating", "pcntClassRating", "initial_takeup_pct")) expect_false(anyNA(res[[v]]))
  expect_gte(sum(!is.na(res$dis_pop_pct)), 3130)
  expect_false(anyNA(clustered[, unname(xvars)]))
})
