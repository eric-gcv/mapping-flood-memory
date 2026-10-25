# Results data dictionary

Produced by `scripts/05_export_results.R`. All values refer to the paper's setup: NFIP voluntary policies
(no mandatory-purchase or disaster-assistance policies), monthly county take-up rate Jan 2010 - Dec 2021,
first-difference detrending, non-parametric PELT (MBIC penalty, 100 quantiles, minimum segment length 3).

**Salience** = percentage increase of the take-up rate from the pre-event level to the peak changepoint after a
flood event. **Memory** (time-to-forget) = months from the event until the take-up rate is back at the pre-event level.
An event counts only if a changepoint falls within 15 months after it ("matched event"). A county's value is the mean
over its events.

## county_results.csv (one row per US county, 3,142 rows)
Columns are ordered so that the numbers reported in the paper come first.

| column | meaning |
|---|---|
| fips, state_abbr, county_name, state_fips | identifiers |
| in_conus | contiguous US (Alaska and Hawaii are excluded) |
| in_sample | one of the 2,055 analysed counties: initial take-up >= 0.2% and >= 1 NOAA flood episode with damage >= USD 6,000 |
| shows_salience_effect | TRUE if the county has a positive post-flood increase in take-up (992 counties; paper Table 1) |
| shows_memory_effect | TRUE if, in addition, take-up returned to its pre-flood level (773 counties; these are the clustered counties) |
| comment | why the county has (or lacks) an estimate, see below |
| coastal_class | `shoreline` (NOAA shoreline county) or `inland`; the two groups are clustered separately |
| cluster | k-means cluster: inland `l1`, `l2`, `l3`; coastal `c1`, `c2` (only counties with `shows_memory_effect`). Labels are arbitrary ids |
| salience_pct | **salience reported in Table 1 and Fig. 2**: mean % increase over the county's events with a positive effect (992 counties) |
| salience_pct_clustered | **salience reported in Table 2 / used for clustering**: mean over the events that also have a memory value (773 counties). Equal to `salience_pct` when all positive events return to baseline |
| memory_months | **memory reported in Table 1 / Table 2**: mean months to return to baseline (773 counties) |
| salience_sd, memory_sd | standard deviation across the county's events (NA with a single event) |
| n_salience_events, n_memory_events_used | events behind `salience_pct` and behind `salience_pct_clustered` / `memory_months` |
| n_events_noaa, n_changepoints, n_events_matched | flood events analysed (episodes merged within 3 months), changepoints detected, events matched by a changepoint |
| n_damaging_episodes, damage_total_usd | NOAA flood episodes with damage >= USD 6,000 and their total damage (available for all counties) |
| initial_takeup_pct | policies in force on 2009-12-31 / housing units 2010 x 100 |
| PC1, PC2, PC3 | principal-component scores averaged in the paper's Table 2 / Fig. 4A (clustered counties only). Signs and order were chosen for interpretation; see `pca_loadings.csv` |
| sfhaRatio, pcntWater, evtsYear, dmgYear, RISK_SCORE, SOVI_SCORE, RESL_SCORE, dis_pop_pct, pcntParticipating, pcntClassRating | inputs of the PCA (SFHA policy share %, water area share, matched events per year, damage per year of matched events, FEMA NRI risk / social vulnerability / resilience scores, CEJST disadvantaged population share, CRS participation, CRS class-rating share). Filled for every county with source data (a few NRI/CEJST gaps). `evtsYear` and `dmgYear` exist only for counties with matched events (1,374); for clustered counties they are computed over the events with a memory value, i.e. exactly what entered the PCA |

### comment values
| comment | counties |
|---|---|
| Salience and memory estimated; clustered | 773 |
| Salience estimated; take-up did not return to baseline | 219 |
| No post-flood increase in take-up | 382 |
| No changepoint within 15 months of a flood event | 670 |
| Excluded: outlier or missing covariates | 11 |
| No flood episode with damage >= USD 6,000 | 451 |
| Initial take-up below 0.2% | 602 |
| Outside contiguous US | 34 |

## Other files
- `events.csv` - one row per matched event (county, date, NOAA damage, salience %, salience in basis points, memory months, outlier flag).
- `changepoints.csv` - detected changepoints per county, with the matched flood event if any (used by the plots).
- `cluster_summary.csv` - paper Table 2: cluster size, mean take-up, mean PC scores, salience and memory mean / median / sd.
- `pca_loadings.csv` - `PC1`..`PC3` are the loadings of Fig. 3A. `score_loading_PC1..3` reproduce the `PC1..PC3` columns of `county_results.csv`
  from standardized inputs: `PCk = sum_j (x_j - center_j) / scale_j * score_loading_PCk_j`.
- `pca_variance.csv` - explained variance. `table1_summary.csv` - paper Table 1.
- `sessionInfo.txt` - R and package versions of the run that produced these files.
