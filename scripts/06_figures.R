# --- --- --- --- --- --- --- --- --- --- --- --- ---
# 06 - Plots for single counties and maps of the results
#
#   source("scripts/06_figures.R")
#   plot_county("29189")                      # take-up series + flood episodes + detected changepoints
#   plot_county("12121", window = TRUE)       # + 15-month matching window after each changepoint
#   plot_county("12121", detrended = TRUE)    # + first-difference detrended series (what PELT sees)
#   plot_county_events("12121")               # zoom on each matched event: baseline, peak, return to baseline
#   plot_county_grid(c("12121", "29189", "29510"))
#   map_clusters(); map_salience(); map_memory()
#
# From the shell:  Rscript scripts/06_figures.R 29189     (writes results/figures/county_29189.png)
#                  Rscript scripts/06_figures.R maps      (writes the three maps)
# County plots need data/derived/*.rds (written by run_all.R); maps need only results/county_results.csv.
# Shaded band = the 15 months before each changepoint: a flood episode inside it is matched to that changepoint.
# --- --- --- --- --- --- --- --- --- --- --- --- ---
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(here)})
source(here::here("R", "functions.R"))      # detrend_signal()
col_event <- "#d95f02"; col_cpt <- "#1b9e77"; col_win <- "#7570b3"

.load_derived <- function() {
  if (is.null(.fig_cache$p)) {
    .fig_cache$p  <- readRDS(here::here("data", "derived", "panel.rds"))
    .fig_cache$cp <- readRDS(here::here("data", "derived", "changepoints.rds"))
  }
  .fig_cache
}
.fig_cache <- new.env()

.nice <- function(x) gsub("([a-z.])(County|Parish|City|Borough)", "\\1 \\2", x)
.county_data <- function(fips) {
  d <- .load_derived()
  ts <- d$p$l.counties[[fips]]
  if (is.null(ts)) stop("County ", fips, " is not in the analysis sample (no monthly series).")
  ev <- d$p$l.tau[[fips]]
  cpts <- d$cp$l.results[[fips]]$changepoints[[1]]       # one configuration is stored per county
  list(ts = ts, ev = ev, cpts = cpts, tau = d$cp$tau.change %>% filter(countyCode == fips))
}

plot_county <- function(fips, window = FALSE, detrended = FALSE, margin_months = 15) {
  x <- .county_data(fips); ts <- x$ts
  lines_df <- bind_rows(
    data.frame(t = x$ev$t, lines = "flood episode"),
    data.frame(t = ts$t[x$cpts], lines = "changepoint"))
  g <- ggplot(ts, aes(t, takeup))
  if (window && length(x$cpts))
    g <- g + geom_rect(data = data.frame(xmin = ts$t[pmax(1, x$cpts - margin_months)], xmax = ts$t[x$cpts]),
                       aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = col_win, alpha = 0.12, inherit.aes = FALSE)
  g <- g + geom_line() +
    geom_vline(data = lines_df, aes(xintercept = t, colour = lines, linetype = lines)) +
    scale_colour_manual(values = c("flood episode" = col_event, changepoint = col_cpt), name = NULL) +
    scale_linetype_manual(values = c("flood episode" = "solid", changepoint = "dashed"), name = NULL) +
    labs(title = paste0(.nice(ts$CountyName[1]), ", ", ts$StateName[1], " (", fips, ")"), x = NULL, y = "Take-up rate (%)") +
    theme_minimal() + theme(legend.position = "bottom")
  if (detrended) {
    dt <- data.frame(t = ts$t, v = detrend_signal(ts$takeup, method = "diff"))
    g <- g + geom_line(data = dt, aes(t, v), colour = "grey50", linewidth = 0.3, inherit.aes = FALSE)
  }
  g
}

# one panel per matched flood event, marking baseline (before), peak (salience) and return to baseline (memory)
plot_county_events <- function(fips) {
  x <- .county_data(fips); ts <- x$ts
  tau <- x$tau %>% filter(!is.na(m.tau_i))
  if (nrow(tau) == 0) stop("County ", fips, " has no matched flood events.")
  pts <- bind_rows(lapply(seq_len(nrow(tau)), function(i) {
    r <- tau[i, ]
    ev <- paste0("Event ", format(r$beginDate), ": salience ", round(r$m.pcnt_change, 1), "%",
                 ifelse(is.na(r$m.dist_tau_to_end), "; no return to baseline", paste0(", memory ", r$m.dist_tau_to_end, " months")))
    win <- max(1, r$r.lim_before):min(nrow(ts), ifelse(is.na(r$m.end), r$r.lim_after, r$m.end + 6))
    bind_rows(
      data.frame(event = ev, t = ts$t[win], v = ts$takeup[win], kind = "series"),
      data.frame(event = ev, t = ts$t[r$m.tau_i], v = r$m.base, kind = "baseline"),
      data.frame(event = ev, t = ts$t[r$m.cpt_peak], v = r$m.peak, kind = "peak"),
      if (!is.na(r$m.end)) data.frame(event = ev, t = ts$t[r$m.end], v = r$m.end_value, kind = "back to baseline"))
  }))
  ggplot() +
    geom_line(data = filter(pts, kind == "series"), aes(t, v)) +
    geom_point(data = filter(pts, kind != "series"), aes(t, v, colour = kind), size = 2.5) +
    facet_wrap(~event, scales = "free", ncol = 1) +
    scale_colour_manual(values = c(baseline = "grey30", peak = col_event, "back to baseline" = col_cpt), name = NULL) +
    labs(title = paste("County", fips), x = NULL, y = "Take-up rate (%)") + theme_minimal() + theme(legend.position = "bottom")
}

plot_county_grid <- function(fips_vector, ncol = 2) {
  xs <- lapply(fips_vector, .county_data)
  lab <- function(x) paste0(.nice(x$ts$CountyName[1]), ", ", x$ts$StateName[1])
  ts <- bind_rows(lapply(xs, function(x) mutate(x$ts, panel = lab(x))))
  vl <- bind_rows(lapply(xs, function(x) bind_rows(
    data.frame(t = x$ev$t, panel = lab(x), lines = "flood episode"),
    data.frame(t = x$ts$t[x$cpts], panel = lab(x), lines = "changepoint"))))
  ggplot(ts, aes(t, takeup)) + geom_line() +
    geom_vline(data = vl, aes(xintercept = t, colour = lines, linetype = lines)) +
    scale_colour_manual(values = c("flood episode" = col_event, changepoint = col_cpt), name = NULL) +
    scale_linetype_manual(values = c("flood episode" = "solid", changepoint = "dashed"), name = NULL) +
    facet_wrap(~panel, scales = "free_y", ncol = ncol) + labs(x = NULL, y = "Take-up rate (%)") +
    theme_minimal() + theme(legend.position = "bottom")
}

# --- maps (results/county_results.csv only; county shapes from USAboundaries) ---
.map_data <- function() {
  r <- read.csv(here::here("results", "county_results.csv"), colClasses = c(fips = "character"), na.strings = "")
  uc <- USAboundaries::us_counties()
  sh <- data.frame(fips = uc[["geoid"]]); sh$geometry <- sf::st_geometry(uc)
  sh <- sf::st_as_sf(sh); sh <- sh[!duplicated(sh$fips), ]
  sh <- sh[!substr(sh$fips, 1, 2) %in% c("02", "15", "60", "66", "69", "72", "78"), ]
  dplyr::left_join(sh, r, by = "fips")
}
.map <- function(var, title, fill_scale) {
  ggplot(.map_data()) + geom_sf(aes(fill = .data[[var]]), colour = NA) + fill_scale +
    labs(title = title, fill = NULL) + theme_void() + theme(legend.position = "bottom")
}
map_clusters <- function() .map("cluster", "County clusters (k-means on PCs, salience and memory)",
  scale_fill_manual(values = c(l1 = "#b2182b", l2 = "#ef8a62", l3 = "#fddbc7", c1 = "#2166ac", c2 = "#92c5de"), na.value = "grey90"))
map_salience <- function() .map("salience_pct", "Salience: % rise in take-up after a flood (counties with an effect)",
  scale_fill_viridis_c(trans = "sqrt", na.value = "grey90"))
map_memory <- function() .map("memory_months", "Memory: months until take-up returns to baseline",
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90"))

if (sys.nframe() == 0 && !interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  out <- here::here("results", "figures"); dir.create(out, showWarnings = FALSE)
  if (identical(args, "maps")) {
    for (m in c("clusters", "salience", "memory")) ggsave(file.path(out, paste0("map_", m, ".png")), get(paste0("map_", m))(), width = 9, height = 6, dpi = 200)
  } else for (f in args) {
    ggsave(file.path(out, paste0("county_", f, ".png")), plot_county(f, window = TRUE), width = 8, height = 3.5, dpi = 200)
  }
}
