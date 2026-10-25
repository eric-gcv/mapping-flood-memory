# Checks the published results tables (results/) against the numbers printed in the paper (tests/paper_values.csv).
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
res  <- read.csv(file.path(root, "results", "county_results.csv"), colClasses = c(fips = "character", state_fips = "character"), na.strings = "")
t1   <- read.csv(file.path(root, "results", "table1_summary.csv"))
cs   <- read.csv(file.path(root, "results", "cluster_summary.csv"))
pv   <- read.csv(file.path(root, "tests", "paper_values.csv"), na.strings = "")

test_that("paper Table 1 and recall are reproduced", {
  for (i in which(is.na(pv$cluster))) {
    got <- t1$value[t1$statistic == pv$quantity[i]]
    expect_length(got, 1)
    expect_lte(abs(got - pv$paper_value[i]), pv$tol[i] + 1e-9)
  }
})

test_that("paper Table 2 (clusters) is reproduced", {
  for (i in which(!is.na(pv$cluster))) {
    got <- cs[cs$cluster == pv$cluster[i], pv$quantity[i]]
    expect_length(got, 1)
    expect_lte(abs(got - pv$paper_value[i]), pv$tol[i] + 1e-9)
  }
})

test_that("results table is complete and consistent", {
  expect_equal(nrow(res), 3142)
  expect_false(any(duplicated(res$fips)))
  expect_equal(length(unique(res$comment)), 8)
  expect_true(all(!is.na(res$cluster[res$shows_memory_effect])))
  expect_true(all(is.na(res$cluster[!res$shows_memory_effect])))
})

test_that("lookup_county works from results alone", {
  source(file.path(root, "scripts", "lookup_county.R"), local = TRUE)
  x <- lookup_county("29510", results_dir = file.path(root, "results"), quiet = TRUE)
  expect_equal(x$county$cluster, "l3")
  y <- lookup_county("29189", results_dir = file.path(root, "results"), quiet = TRUE)
  expect_equal(y$county$comment, "No changepoint within 15 months of a flood event")
  z <- lookup_county("St. Louis County, MO", results_dir = file.path(root, "results"), quiet = TRUE)
  expect_equal(z$county$fips, "29189")
})
