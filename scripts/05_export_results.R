# --- --- --- --- --- --- --- --- --- --- --- --- ---
# 05 - Export the results database (results/*.csv)
# One row per US county (county_results.csv) plus event-level, cluster-level and PCA tables.
# Input:  data/derived/*.rds ; Output: results/*.csv
# --- --- --- --- --- --- --- --- --- --- --- --- ---
source(here::here("R", "config.R"))
p <- readRDS(file.path(path_derived, "panel.rds"))
cp <- readRDS(file.path(path_derived, "changepoints.rds"))
pc <- readRDS(file.path(path_derived, "pca_clusters.rds"))
sfd <- function(x) sf::st_drop_geometry(x)

cov <- readRDS(file.path(path_derived, "covariates.rds"))
cc  <- sfd(pc$counties.change)
imp <- sfd(pc$counties.change.imp)
mem <- sfd(pc$counties.change.mem)
base <- p$counties.base
metrics <- cp$results_setting %>% select(countyCode, n_eps, n_cpts, true_pos)

# county names (the census file strips blanks)
uc <- sfd(USAboundaries::us_counties())      # has duplicated column names: use base indexing
nm <- data.frame(fips = uc[["geoid"]], state_abbr = uc[["state_abbr"]], county_name = uc[["namelsad"]], pcntWater = uc[["awater"]] / (uc[["aland"]] + uc[["awater"]])) %>% distinct(fips, .keep_all = TRUE)
shore <- read.csv2(path_shoreline) %>% transmute(fips = str_pad(FIPS, 5, pad = "0"), shoreline_flag = 1L)

res <- base %>%
  transmute(fips = CountyCodeTxt, state_fips = str_pad(StateId, 2, pad = "0"),
            initial_takeup_pct = iniTakeup, n_damaging_episodes = noaaEpisodes,
            damage_total_usd = dmgTotal, in_sample = inSample) %>%
  left_join(nm, by = "fips") %>%
  left_join(shore, by = "fips") %>%
  mutate(coastal_class = if_else(!is.na(shoreline_flag), "shoreline", "inland"), shoreline_flag = NULL) %>%
  left_join(metrics, by = c("fips" = "countyCode")) %>%
  rename(n_events_noaa = n_eps, n_changepoints = n_cpts, n_events_matched = true_pos) %>%
  # events per year / damage per year over all matched events (used for counties outside the clustered set)
  left_join(cc %>% select(fips = countyCode, evtsYear_all = evtsYear, dmgYear_all = dmgYear), by = "fips") %>%
  left_join(imp %>% transmute(fips = countyCode, n_salience_events = nEvents,
                              salience_pct = impact, salience_sd = impact_sd), by = "fips") %>%
  left_join(mem %>% transmute(fips = countyCode, n_memory_events_used = nEvents,
                              salience_pct_clustered = impact, memory_months = memory, memory_sd = memory_sd,
                              cluster, evtsYear_mem = evtsYear, dmgYear_mem = dmgYear,
                              PC1 = PC1_SocVul, PC2 = -PC2_RiskPr, PC3 = -PC3_FlDmg), by = "fips") %>%
  left_join(cov %>% select(fips = CountyCodeTxt, sfhaRatio, RISK_SCORE, SOVI_SCORE, RESL_SCORE,
                           dis_pop_pct, pcntParticipating, pcntClassRating), by = "fips") %>%
  mutate(
    # PCA inputs: for the clustered counties exactly the values entering the PCA (events with a memory value)
    evtsYear = coalesce(evtsYear_mem, evtsYear_all), dmgYear = coalesce(dmgYear_mem, dmgYear_all),
    in_conus = !(state_fips %in% c("02", "15")),
    tp_county = fips %in% cp$tau.change$countyCode,
    shows_salience_effect = !is.na(salience_pct),
    shows_memory_effect = !is.na(memory_months),
    status = case_when(
      !in_conus ~ "outside_conus",
      initial_takeup_pct < turThreshold ~ "below_takeup_threshold",
      n_damaging_episodes < 1 ~ "no_damaging_event",
      !tp_county ~ "no_changepoint_match",
      !fips %in% cc$countyCode ~ "excluded_outlier_or_missing_covariates",
      !fips %in% imp$countyCode ~ "no_positive_salience",
      !fips %in% mem$countyCode ~ "salience_no_memory",
      TRUE ~ "full"),
    comment = recode(status,
      full = "Salience and memory estimated; clustered",
      salience_no_memory = "Salience estimated; take-up did not return to baseline",
      no_positive_salience = "No post-flood increase in take-up",
      no_changepoint_match = "No changepoint within 15 months of a flood event",
      no_damaging_event = "No flood episode with damage >= USD 6,000",
      below_takeup_threshold = "Initial take-up below 0.2%",
      outside_conus = "Outside contiguous US",
      excluded_outlier_or_missing_covariates = "Excluded: outlier or missing covariates")) %>%
  select(fips, state_abbr, county_name, state_fips, in_conus, in_sample,
         shows_salience_effect, shows_memory_effect, comment, coastal_class, cluster,
         salience_pct, salience_pct_clustered, memory_months,
         salience_sd, memory_sd, n_salience_events, n_memory_events_used,
         n_events_noaa, n_changepoints, n_events_matched, n_damaging_episodes, damage_total_usd, initial_takeup_pct,
         PC1, PC2, PC3,
         sfhaRatio, pcntWater, evtsYear, dmgYear, RISK_SCORE, SOVI_SCORE, RESL_SCORE,
         dis_pop_pct, pcntParticipating, pcntClassRating) %>%
  arrange(fips)
write.csv(res, file.path(path_results, "county_results.csv"), row.names = FALSE, na = "")

# event level
ev <- cp$tau.change %>%
  transmute(fips = countyCode, event_date = beginDate, damage_usd = dmg, n_episodes = nEps,
            salience_pct = m.pcnt_change, salience_bps = m.basepoints, salience_positive = m.is_positive,
            memory_months = m.dist_tau_to_end, is_outlier = isOutlier)
write.csv(ev, file.path(path_results, "events.csv"), row.names = FALSE, na = "")

# detected changepoints (for plotting; one row per changepoint, with the matched flood event if any)
cpts <- do.call(rbind, lapply(names(cp$l.results), function(k) {
  idx <- cp$l.results[[k]]$changepoints[[1]]
  if (length(idx) == 0) return(NULL)
  data.frame(fips = k, changepoint_idx = idx, changepoint_date = dates[idx])
}))
mt <- cp$tau.change %>% filter(TP) %>% transmute(fips = countyCode, changepoint_idx = nearestCpt, matched_event_date = beginDate)
cpts <- cpts %>% left_join(mt, by = c("fips", "changepoint_idx"))
write.csv(cpts, file.path(path_results, "changepoints.csv"), row.names = FALSE, na = "")

# cluster summary (Table 2)
cs <- mem %>% group_by(cluster) %>%
  summarise(n = n(), coastal = sum(shoreline == "shoreline"), takeup_mean_pct = mean(iniTakeup),
            PC1_table2 = mean(PC1_SocVul), PC2_table2 = mean(-PC2_RiskPr), PC3_table2 = mean(-PC3_FlDmg),
            salience_mean = mean(impact), salience_median = median(impact), salience_sd = sd(impact),
            memory_mean = mean(memory), memory_median = median(memory), memory_sd = sd(memory), .groups = "drop")
write.csv(cs, file.path(path_results, "cluster_summary.csv"), row.names = FALSE)

# PCA loadings (Fig. 3A) and variance
ld <- as.data.frame(pc$res.pca$rotation[, 1:3]) %>% tibble::rownames_to_column("variable")
# loadings that reproduce the PC1..PC3 columns of county_results.csv (Table 2 scores) from standardized inputs:
# score_k = sum_j (x_j - center_j) / scale_j * score_loading_PCk_j
rot <- pc$res.pca$rotation          # prcomp rotation with the sign flips used for the paper (used by predict())
ld$score_loading_PC1 <- -rot[, "PC2"]
ld$score_loading_PC2 <- -rot[, "PC1"]
ld$score_loading_PC3 <-  rot[, "PC3"]
ld$center <- pc$res.pca$center[ld$variable]
ld$scale  <- pc$res.pca$scale[ld$variable]
write.csv(ld, file.path(path_results, "pca_loadings.csv"), row.names = FALSE)
write.csv(as.data.frame(pc$pca_out.importance) %>% tibble::rownames_to_column("statistic"),
          file.path(path_results, "pca_variance.csv"), row.names = FALSE)

# Table 1
sal <- imp$impact
t1 <- tibble::tribble(~statistic, ~value,
  "counties_in_sample", sum(base$inSample),
  "counties_positive_recall", nrow(cc),
  "counties_with_salience", nrow(imp),
  "counties_with_memory", nrow(mem),
  "salience_mean_pct", mean(sal), "salience_median_pct", median(sal), "salience_sd_pct", sd(sal),
  "memory_mean_months", mean(mem$memory), "memory_median_months", median(mem$memory), "memory_sd_months", sd(mem$memory),
  "recall_pelt", sum(cp$results_setting$true_pos, na.rm = TRUE) / sum(cp$results_setting$n_eps, na.rm = TRUE))
write.csv(t1, file.path(path_results, "table1_summary.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(path_results, "sessionInfo.txt"))
message("05_export_results: wrote ", nrow(res), " counties")
