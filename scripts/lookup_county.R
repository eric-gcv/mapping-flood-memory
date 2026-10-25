# --- --- --- --- --- --- --- --- --- --- --- --- ---
# Look up the flood-memory results for one county. Needs only results/*.csv (no database).
#
#   Rscript scripts/lookup_county.R 29189
#   Rscript scripts/lookup_county.R "St. Louis County, MO"
#   Rscript scripts/lookup_county.R 29189 out.csv       # also writes the county record to a csv
#
# In R:  source("scripts/lookup_county.R"); lookup_county("29189")
# --- --- --- --- --- --- --- --- --- --- --- --- ---
lookup_county <- function(query, results_dir = file.path("results"), quiet = FALSE) {
  res <- read.csv(file.path(results_dir, "county_results.csv"),
                  colClasses = c(fips = "character", state_fips = "character"))
  cs  <- read.csv(file.path(results_dir, "cluster_summary.csv"))
  ev  <- read.csv(file.path(results_dir, "events.csv"), colClasses = c(fips = "character"))

  q <- trimws(query)
  if (grepl("^[0-9]{4,5}$", q)) {
    row <- res[res$fips == sprintf("%05s", q) | res$fips == formatC(as.integer(q), width = 5, flag = "0"), ]
  } else {
    parts <- trimws(strsplit(q, ",")[[1]])
    nm <- tolower(gsub("[^a-z0-9 ]", "", tolower(parts[1])))
    cn <- tolower(gsub("[^a-z0-9 ]", "", tolower(res$county_name)))
    hit <- cn == nm | cn == paste(nm, "county")
    if (length(parts) > 1) hit <- hit & toupper(res$state_abbr) == toupper(parts[2])
    row <- res[hit, ]
  }
  if (nrow(row) != 1) stop(sprintf("'%s' matched %d counties; use the 5-digit FIPS code.", query, nrow(row)))
  events <- ev[ev$fips == row$fips, ]
  cluster <- if (!is.na(row$cluster) && nzchar(row$cluster)) cs[cs$cluster == row$cluster, ] else NULL
  group <- cs[if (row$coastal_class == "shoreline") grepl("^c", cs$cluster) else grepl("^l", cs$cluster), ]
  out <- list(county = row, events = events, cluster = cluster, same_type_clusters = group)

  if (!quiet) {
    f <- function(x, d = 2) ifelse(is.na(x), "n/a", format(round(x, d), nsmall = d))
    cat(sprintf("\n%s, %s (FIPS %s) - %s\n", row$county_name, row$state_abbr, row$fips, row$coastal_class))
    cat(sprintf("Comment: %s\n", row$comment))
    cat(sprintf("Initial take-up: %s%%   damaging NOAA episodes: %d   changepoints: %s   matched to events: %s\n",
                f(row$initial_takeup_pct), row$n_damaging_episodes, f(row$n_changepoints, 0), f(row$n_events_matched, 0)))
    if (!is.na(row$salience_pct))
      cat(sprintf("Salience: %s%% (sd %s, %d event%s)\n", f(row$salience_pct), f(row$salience_sd),
                  row$n_salience_events, ifelse(row$n_salience_events == 1, "", "s")))
    if (!is.na(row$memory_months))
      cat(sprintf("Memory:   %s months (sd %s, %d event%s)\n", f(row$memory_months, 1), f(row$memory_sd, 1),
                  row$n_memory_events_used, ifelse(row$n_memory_events_used == 1, "", "s")))
    if (!is.null(cluster)) {
      cat(sprintf("Cluster: %s (n = %d). Cluster salience mean %s%% (sd %s), memory mean %s months (sd %s)\n", cluster$cluster,
                  cluster$n, f(cluster$salience_mean), f(cluster$salience_sd), f(cluster$memory_mean, 1), f(cluster$memory_sd, 1)))
    } else {
      cat("No county-level estimate/cluster. Reference clusters of the same type:\n")
      print(group[, c("cluster", "n", "salience_mean", "salience_sd", "memory_mean", "memory_sd")], row.names = FALSE, digits = 3)
    }
    if (nrow(events)) { cat("Matched events:\n"); print(events[, c("event_date", "damage_usd", "salience_pct", "memory_months")], row.names = FALSE) }
    cat("\n")
  }
  invisible(out)
}

if (sys.nframe() == 0 && !interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) stop("usage: Rscript scripts/lookup_county.R <FIPS | 'Name, ST'> [out.csv]")
  x <- lookup_county(args[1])
  if (length(args) > 1) write.csv(x$county, args[2], row.names = FALSE)
}
