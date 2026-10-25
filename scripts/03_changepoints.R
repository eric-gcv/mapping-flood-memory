# --- --- --- --- --- --- --- --- --- --- --- --- ---
# 03 - Change point detection (non-parametric PELT on the detrended take-up rate)
# and event-level salience / memory (tau.change).
# Input:  data/derived/panel.rds
# Output: data/derived/changepoints.rds
# --- --- --- --- --- --- --- --- --- --- --- --- ---
source(here::here("R", "config.R"))
p <- readRDS(file.path(path_derived, "panel.rds"))
counties.base <- p$counties.base
l.counties <- p$l.counties
l.tau <- p$l.tau
county_codes_sample <- p$county_codes_sample

#### CHANGEPOINT DETECTION ####
l.results <- list()
results <- tibble()
set.seed(42)
for (i in 1:length(county_codes_sample)) {
  countyCode <- county_codes_sample[i]
  
  if (i %% 100 == 0) {
    # # Get the worker number
    # worker <- as.integer(Sys.getenv("PBS_NP", unset = "0")) + 1
    # message("Worker ", worker, " processed ", i, " counties.")
    
    # Print progress message with worker number
    message("Processed ", i, " counties.")
  }
  
  # Retrieve the signal and tau for the current countyCode
  signal <- l.counties[[countyCode]]$takeup
  signal.d <- cbind(as.Date(dates), signal)
  tau <- l.tau[[countyCode]]$idx
  
  # Decompose signal:
  signal.detrend <- detrend_signal(signal, method = 'diff')
  signal.m.detrend <- matrix(signal.detrend)            # old code: cbind(as.Date(dates), signal.detrend)
  leading_nas <- which.max(!is.na(signal.detrend)) - 1  # Count of NAs removed
  
  # Define the changepoint detection methods and their corresponding parameters
  # default penalty is MBIC
  parameters.np <-   list(list(method = "PELT", minseglen = 3, nquantiles = 100, setting = 'NonParam-PELT-MBIC-Q_100'))
  
  
  # Mapping different parameter in a human-readable way
  params_tbl <- bind_rows(
    # cpt & cpt.np
    bind_rows(c(parameters.np)) %>% 
      rename_with(~ gsub("\\.", "_", .), everything()) %>% 
      mutate(family = 'nonParam-PELT',
             test_stat = "NonParam",
             penalty = "MBIC",
             Q = nquantiles) %>% 
      select(-nquantiles)
  ) %>% 
    mutate(setting = paste(str_pad(row_number(), width = 2, pad = "0"), setting, sep = '-')) %>% 
    select("setting", "family", "test_stat", "method", "penalty", "minseglen", "Q")
  
  
  # run changepoint detection methods - first remove the settings label from the parameters
  parameters.np <- lapply(parameters.np, function(x) { x$setting <- NULL; x })
  
  results.np <- lapply(parameters.np, function(params) {
    do.call(cpt.np, c(list(na.omit(signal.detrend)), params))
  })
  county.results <- results.np
  
  # get a list of the detected change points
  changepoints <- lapply(results.np, cpts) %>% 
    lapply(function(x) {
      if (length(x) > 0) {
        x <- x + leading_nas                       # add leading_nas to the changepoints
        x[!(x == 1 | x >= length(dates))]          # exclude results with changepoints at the first or last element
      } else {
        x
      }
    })
  
  
  #glimpse(changepoints)
  # x = changepoints[[9]]
  
  # run metrics and turn into tibble
  metrics <- lapply(changepoints, function(cp) cpt_metrics(cp, tau, margin, len))
  metrics_tbl <- bind_rows(metrics, .id = "setting_id")
  
  # save metrics on the results tibble
  results = rbind(results, cbind(countyCode, metrics_tbl, params_tbl))
  
  # save county data on the list of results 
  l.results[[countyCode]] = list(county.results, changepoints, metrics)
  names(l.results[[countyCode]]) = c("results", "changepoints", "metrics")
}

#### RESULTS PER COUNTY (selected setting) ####
results_setting <- results %>% 
  #filter(setting_id == 10) %>% 
  filter(grepl('NonParam-PELT-MBIC-Q_100$', setting)) %>% 
  left_join(counties.base, by = c("countyCode"="CountyCodeTxt"))

#### COUNTIES SF + SHORELINE ####
counties.sf <- USAboundaries::us_counties() %>% sf::st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
counties.sf$code = paste(counties.sf$statefp, counties.sf$countyfp, sep='')

counties.sf <- counties.base %>% 
  left_join(counties.sf, by = c("CountyCodeTxt" = "code")) %>% 
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  filter(inSample) %>% 
  dplyr::select(-c("statefp", "countyfp", "countyns", "affgeoid", "geoid", "name", 
                   "state_name", "lsad", "state_name.1", "state_abbr", "jurisdiction_type"))

# * Shoreline: Coastal counties ---

shoreline <- read.csv2(path_shoreline) %>% 
  mutate(isShoreline = 1, isCoastal = Coastal) %>%
  dplyr::select("FIPS", "isShoreline", "isCoastal")

counties.sf <- counties.sf  %>% 
  left_join0(shoreline, by = c("CountyCode" = "FIPS"))

# add info to counties.sf
counties.sf <- counties.sf %>% 
  mutate(coastal = factor(case_when(isCoastal == 1 ~ 'coastal',
                                    isShoreline == 1 ~  'shoreline',
                                    .default = 'inland'),
                          levels = c('coastal', 'shoreline', 'inland')),
         shoreline = factor(case_when(isShoreline == 1 ~  'shoreline',
                                      .default = 'inland'),
                            levels = c('shoreline', 'inland')))
results_setting$shoreline = factor(ifelse(as.numeric(results_setting$countyCode) %in% shoreline$FIPS,
                                          "shoreline", "inland"), levels = c('shoreline', 'inland'))

counties.sf <- counties.sf %>% 
  left_join(results_setting %>% dplyr::select("countyCode", "setting_id", "n_eps", "n_cpts", "ann_error", "h_distance", "h_dist_short", "true_pos", "false_pos", "precision", "recall", "F1", "rand_index"), 
            by = c("CountyCodeTxt" = "countyCode")) 

#### EVENT LEVEL CHANGE (tau.change) ####
tp_counties <- counties.sf$CountyCodeTxt[!is.na(counties.sf$true_pos) & counties.sf$true_pos > 0]
#length(tp_counties) # REV 1385

l.tau10 <- l.tau[tp_counties]
l.change = list()

for (i in 1:length(tp_counties)) {
  countyCode <- tp_counties[i]
  
  if (i %% 100 == 0)   message("Processed ", i, " counties.")
  
  # Retrieve the signal and tau for the current countyCode
  signal <- l.counties[[countyCode]]$takeup
  tau <- l.tau10[[countyCode]]$idx
  x <- l.results[[countyCode]]$changepoints[[1]]
  
  # add cpt results to the tau list
  l.tau10[[countyCode]] <- l.tau10[[countyCode]] %>% 
    mutate(TP = neighbour_tau(x, tau, margin), 
           nCpt = neighbour_tau_n(x, tau, margin),
           nearestCpt = next_mark(i=tau, v1=x, include_start = TRUE, iterations = 1, limit = NULL, max_idx = NULL),
           distance = nearestCpt-idx)
  
  TP <- l.tau10[[countyCode]]$TP
  #tau[TP]
  #nearestCpt = l.tau10[[countyCode]]$nearestCpt
  change = change_tp(tau[TP], signal, x, tau, limit = 15)
  
  l.change[[countyCode]] <- cbind(l.tau10[[countyCode]][TP, ], as_tibble(as.list(change)))
  
}

tau.change <- bind_rows(l.change)

# tau.change has NA values when there are two consecutive events marked as TP with a single CP. 
# We remove those lines as they are not true positives for our change calculation
tau.change_na <- tau.change %>% filter(is.na(m.peak))        # filter_all(any_vars(is.na(.)))
tau.change <- tau.change %>% filter(!is.na(m.peak))

# fix the is_positive tag in m info, to account for zero
tau.change <- tau.change %>% 
  mutate(impact_sign = case_when(m.difference == 0 ~ "0",
                                 m.difference <0 ~ "-",
                                 m.difference >0 ~ "+"))
tau.change %>% group_by(m.is_positive, impact_sign)%>% summarise(n=n()) %>% ungroup()


# * Outliers ----
#tau.change %>% filter(m.pcnt_change > 100) %>% arrange(desc(m.pcnt_change))
tau.change$isOutlier = ifelse(tau.change$m.pcnt_change > 200, TRUE, FALSE)

tau.change <- tau.change %>% 
  left_join(counties.sf %>% sf::st_drop_geometry() %>% dplyr::select("CountyCodeTxt", "AvgHU", "AvgPOP", "iniTakeup", "takeup_cat", 
                                                                     "noaaEpisodes", "noaa_cat", "awater", "shoreline"),   
                                                                  # , "climateRegion", "sfha"
            by = c("countyCode" = "CountyCodeTxt")) 

saveRDS(list(results = results, results_setting = results_setting, counties.sf = counties.sf,
             tau.change = tau.change, l.results = l.results, tau.change_na = tau.change_na),
        file.path(path_derived, "changepoints.rds"))
message("03_changepoints: ", nrow(tau.change), " true-positive events in ", n_distinct(tau.change$countyCode), " counties")
