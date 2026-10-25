# data/

Raw data is not versioned. The pipeline expects:

| Path | Source | Accessed |
|---|---|---|
| `flood_data.db` | DuckDB built with `sql/01_create_tables.sql` and `sql/02_stock_tables.sql` from the OpenFEMA *FIMA NFIP Redacted Policies v2* CSV (<https://www.fema.gov/openfema-data-page/fima-nfip-redacted-policies-v2>) | 2025-10-18 |
| `flood_NOAA/StormEvents_details-*.csv` | NOAA NCEI Storm Events Database, bulk CSV (details files 2000-2022) | 2023-03 |
| `census_data/census_data.csv` | US Census Bureau housing units 2010 and 2020 by county, compiled by the authors | 2023-06 |
| `crs/NfipCommunityStatusBook.csv` | FEMA NFIP Community Status Book | 2024-03 |
| `fema_nri/NRI_Table_Counties/NRI_Table_Counties.csv` | FEMA National Risk Index, counties table | 2024-03-13 |
| `justice/cejst-communities.csv` | CEQ Climate and Economic Justice Screening Tool | 2024-03-13 |
| `coastal_counties/noaa_shoreline_counties.csv` | NOAA coastal / shoreline county list | 2023-07 |

`derived/` holds intermediate `.rds` files and `monthly_stock.csv` written by the pipeline.
