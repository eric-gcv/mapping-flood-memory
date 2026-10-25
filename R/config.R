# --- --- --- --- --- --- --- --- --- --- --- --- ---
# Shared configuration: paths, parameters, packages
# All values are those used for the paper (Mapping Flood Memory, IJDRR, doi:10.1016/j.ijdrr.2026.106162).
# --- --- --- --- --- --- --- --- --- --- --- --- ---

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(lubridate)
  library(here)
  library(DBI)
  library(duckdb)
  library(changepoint)
  library(changepoint.np)
  library(sf)
  library(USAboundaries)
})

source(here::here("R", "functions.R"))

##### PARAMETERS ####
timestep = 'month'
margin = 15                 # months after an event in which a changepoint counts as a hit

turThreshold = .2           # min initial take-up rate (%)
dmgThreshold = 6000         # min damage per NOAA episode (USD)

t_begin = as.Date("2010-01-01")
t_end = as.Date("2022-01-01")
t_end_noaa = as.Date("2021-12-31")     # window used to load NOAA events
t_years = lubridate::time_length(difftime(t_end, t_begin), "years")

# DATES: index of dates in study period
dates = as.Date(seq(t_begin, t_end, by=timestep))
len = length(dates)

##### PATHS ####
path_noaa = here::here("data", "flood_NOAA")
path_census = here::here("data", "census_data", "census_data.csv")
path_db = here::here("data", "flood_data.db")
path_shoreline = here::here("data", "coastal_counties", "noaa_shoreline_counties.csv")
path_crs = here::here("data", "crs", "NfipCommunityStatusBook.csv")
path_risk = here::here("data", "fema_nri", "NRI_Table_Counties", "NRI_Table_Counties.csv")
path_justice = here::here("data", "justice", "cejst-communities.csv")

path_derived = here::here("data", "derived")
path_results = here::here("results")
dir.create(path_derived, showWarnings = FALSE, recursive = TRUE)
dir.create(path_results, showWarnings = FALSE, recursive = TRUE)

# The database is only ever opened read-only by the pipeline
db_connect <- function() DBI::dbConnect(duckdb::duckdb(), dbdir = path_db, read_only = TRUE)
