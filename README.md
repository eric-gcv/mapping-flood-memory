# Mapping Flood Memory - code and results

Replication code and county-level results for

> *Mapping Flood Memory: How Risk Perception and Social Vulnerability Drive Flood Insurance Patterns in the U.S.*
> International Journal of Disaster Risk Reduction.
> DOI: [10.1016/j.ijdrr.2026.106162](https://doi.org/10.1016/j.ijdrr.2026.106162)

## Abstract
Floods cause severe damage to private property in the United States, but the vast majority of flood-exposed residents are not
insured. To gain detailed knowledge on how to improve insurance coverage for people in need, we investigate the temporal dynamics
of flood insurance adoption patterns across U.S. counties by analyzing National Flood Insurance Program data through the lens of
social memory and risk perception. Using change point detection methodologies, we identify significant shifts in insurance
purchasing behavior following flood events and quantify both the magnitude (*salience*) and memory (*time-to-forget*) of these
post-flood responses. We show that social memory of flood events, measured through insurance participation rates, may be
considerably shorter and more heterogeneous than previously suggested in the literature. By including demographic, environmental,
and institutional variables, we disentangle the multi-modal distribution of salience and memory. We identify three components
driving insurance adoption: social vulnerability, risk perception, and flood damage patterns. Subsequent cluster analysis of
these components reveals five distinct county profiles, with notable findings regarding areas where high social vulnerability
coincides with low risk perception. The results demonstrate spatiotemporal variations in community responses to flood risks and
suggest that only community-specific policy interventions can successfully maintain equitable and sustainable insurance coverage.

## What this repository provides
For every US county (3,142), `results/county_results.csv` gives the paper's two key quantities:

- **Salience**: percentage increase of the NFIP flood-insurance take-up rate after a flood event.
- **Memory** (time-to-forget): months until the take-up rate returns to its pre-flood level.

Take-up = voluntary NFIP policies / housing units, monthly, 2010-2021, first-difference detrended. Changepoints are found with
non-parametric PELT (`changepoint.np`); a flood episode (NOAA, damage >= USD 6,000) counts if a changepoint follows within 15 months.
Counties are then described by 11 covariates, reduced with PCA (social vulnerability, risk perception, flood damage) and grouped with
k-means into 3 inland (`l1`-`l3`) and 2 coastal (`c1`-`c2`) clusters (773 counties).

### Look up a county (no data download needed)
```
Rscript scripts/lookup_county.R 29189
Rscript scripts/lookup_county.R "St. Louis County, MO"
```
or open `results/county_results.csv`. The first columns are the ones to read:
`shows_salience_effect`, `shows_memory_effect`, `comment` (why a county has no estimate), `cluster`, then `salience_pct`
(Table 1 / Fig. 2), `salience_pct_clustered` (Table 2) and `memory_months`. Full definitions: `results/data_dictionary.md`.
Counties without an estimate can use their cluster's reference values in `results/cluster_summary.csv`.

### Plot a county or the maps
```r
source("scripts/06_figures.R")
plot_county("12121", window = TRUE)     # take-up series, flood episodes, changepoints, 15-month matching window
plot_county_events("12121")             # per event: baseline, peak (salience), return to baseline (memory)
plot_county_grid(c("12121", "29189"))
map_clusters(); map_salience(); map_memory()
```
County plots need `data/derived/` (created by `run_all.R`); maps need only `results/`.

## Repository layout
| path | content |
|---|---|
| `results/` | published results: `county_results.csv`, `events.csv`, `changepoints.csv`, `cluster_summary.csv`, PCA tables, Table 1 |
| `sql/` | DuckDB scripts: NFIP policies -> voluntary policies -> daily policy stock |
| `scripts/01..05_*.R` | pipeline: panel, covariates, changepoints, PCA and clusters, export |
| `scripts/06_figures.R`, `scripts/lookup_county.R` | plots, maps and county lookup |
| `R/` | shared parameters (`config.R`) and helper functions |
| `tests/` | checks that `results/` reproduce the numbers printed in the paper and are internally consistent; optional full rerun |
| `data/` | raw data (not versioned); sources and access dates in `data/README.md` |

## Reproduce
1. Download the raw data listed in `data/README.md` and build the database with `sql/*.sql` (about 19 GB).
2. `Rscript run_all.R` (about 3 minutes). Requires R >= 4.3 with `dplyr, tidyr, stringr, forcats, lubridate, here, DBI, duckdb, changepoint, changepoint.np, sf, USAboundaries, readr, tibble, ggplot2, testthat`. Package versions of the reference run: `results/sessionInfo.txt`.
3. `Rscript tests/run_tests.R` checks the committed results against the paper's Tables 1-2. `RUN_FULL=1 Rscript tests/run_tests.R` reruns the pipeline and checks that the regenerated results equal the committed ones (this overwrites `results/`).

### Randomness and sign conventions
- PELT and PCA are deterministic. **k-means uses `set.seed(1)` with a single random start**, so the clusters depend on R's random number
  generator (R >= 3.6, reference run R 4.3.2) and on the row order of the data; cluster ids (`l1`, `c2`, ...) are arbitrary labels.
  The partition (59/47/247/192/228 counties) is reproduced exactly with the reference setup, but other R versions may relabel or shift borderline counties.
- The sign and order of the principal components are arbitrary. The PC columns in `county_results.csv` (and the published Table 2 / Fig. 4A)
  were flipped and ordered to ease interpretation. Clusters, salience and memory do not depend on this choice.

## Citation
Please cite the paper above. Raw data: OpenFEMA NFIP Redacted Policies v2, NOAA Storm Events, US Census, FEMA NRI / CRS, CEJST (see `data/README.md`).
