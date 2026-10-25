# --- --- --- --- --- --- --- --- --- --- --- --- ---
# SUMMARY OF FUNCTIONS
#
# EC, 19.04.2023 
# --- --- --- --- --- --- --- --- --- --- --- --- ---
# references:
# 
# 

library(tidyr) 
library(purrr)

#set ggplot base font size (11 pts by default)
# theme_set(theme_void(base_size = 18))
# theme_set(theme_gray(base_size = 18))
# theme_set(theme_minimal(base_size = 18))

# Colors
colors4 <- c("#C77CFF", "#00BFC4", "#7CAE00", "#F8766D")
colors3 <- c("#f8766d", "#00ba38", "#619cff")
#scales::show_col(scales::hue_pal()(3))


### Citations
# citation()  # R
# c("rmarkdown", "bookdown", "knitr", "dplyr", "purrr", "tidyr", "RMySQL", "ggplot2", "ggmosaic", "GGally",
#   "sf", "USAboundaries",  "rnaturalearth",  "changepoint", "changepoint.np", "ecp", "BreakoutDetection") %>%
#   purrr::map(citation) %>%
#   print(style = "text")
# c("rmarkdown", "bookdown", "sf", "USAboundaries",
#   "changepoint", "changepoint.np", "ecp", "BreakoutDetection") %>%
#   purrr::map(citation) %>%
#   print(style = "text")
# 
# knitr::write_bib(c("rmarkdown", "bookdown", "sf", "USAboundaries",
#                    "changepoint", "changepoint.np", "ecp", "BreakoutDetection"), "software.bib")


##### 1. LOAD DATA ----

#### 1.1. NOAA - DISASTER DATA ----

load_noaa <- function(path, begin, end, ...){           #, county_codes=FALSE){
  # read files
  list_of_files <- list.files(path = path,
                              recursive = FALSE,
                              pattern = ".*details.*\\.csv$",
                              full.names = TRUE)
  noaa_details <- readr::read_csv(list_of_files, id = "file_name")
  #glimpse(noaa_details)
  
  noaa_columns <- colnames(noaa_details)[-1]
  
  noaa <- noaa_details %>% filter(EVENT_TYPE %in% c("Flash Flood", "Flood", "Lakeshore Flood", "Coastal Flood",
                                                    "Heavy Rain", "Tropical Storm", "Hurricane (Typhoon)", "Hurricane")) %>%    # "Tornado"
    mutate(beginDate = lubridate::ymd(paste(BEGIN_YEARMONTH,BEGIN_DAY)),
           #endDate = lubridate::as_date(END_DATE_TIME),
           countyCode = paste(
                  as.character(formatC(STATE_FIPS, width=2, flag="0"))
                  ,as.character(formatC(CZ_FIPS, width=3, flag="0"))
                  ,sep=''),
           beginDateTime = as.POSIXct(BEGIN_DATE_TIME, format = "%d-%b-%y %T"),
           damage = ifelse(is.na(DAMAGE_PROPERTY), 0, as.double(substr(DAMAGE_PROPERTY, 1, nchar(DAMAGE_PROPERTY)-1))) * 1000,
           didDamage = ifelse(damage > 0, 1, 0)
    ) %>% 
    filter(between(beginDate, begin, end)) %>% 
    dplyr::select(c("countyCode", "didDamage", "damage", "beginDate", "beginDateTime",  all_of(noaa_columns)))
  #glimpse(noaa)
  
  if(hasArg(county_codes))
    noaa <- noaa %>% filter(as.numeric(countyCode) %in% county_codes)
  
  print("NOAA flood data loaded and filtered between dates 2010-01-01 and 2021-12-31")
  noaa
}

# merge noaa episodes on a margin range (3 for 3 months)
merge_episodes <- function(df, margin) {
  df %>%
    ungroup() %>% 
    arrange(beginDate) %>%
    mutate(groupID = cumsum(c(0, diff(beginDate) > months(margin)))) %>%
    group_by(countyCode, groupID) %>%        
    summarize(t = min(t),
              beginDate = min(beginDate),
              dmg = sum(dmg),
              nEps = sum(nEps),
              idx = first(idx), .groups = "drop_last") %>%        
    ungroup() %>%
    dplyr::select(-groupID)
}

##### 2. METRICS ----

#### 2.1. HAUSDORFF DISTANCE ----
# x <- c(100, 250, 400, 440, 650, 760, 780, 810)
# tau <- c(180, 250, 300)
#   
# x <- c(1.0, 2.7, 4.2, 5.8)
# tau <- c(1.2, 2.1, 3.5, 4.8, 5.6, 6.2)

# tau <- c(100, 200, 300, 400, 500)
# x <- c(100, 200, 540)

# tau <- c(70, 200)
# x <- c(50)
# h_distance(x,tau)

h_distance <- function(x, tau, verbose=FALSE, short=FALSE, ...) {
  tau <- sort(tau)  # Sort the tau vector in ascending order
  x <- sort(x)
  
  # Check if all elements in x are smaller than the smallest element in tau
  if (all(x < min(tau))) {
    if (verbose) cat("All elements in x are smaller than the smallest element in tau.")
    return(NA)
  }
  
  # Find the indices of the nearest higher or equal elements in tau for each element in x
  indices_x_to_tau <- findInterval(x, tau, rightmost.closed = TRUE)
  indices_tau_to_x <- findInterval(tau, x, left.open = TRUE)+1
  
  # Adjust the indices to ensure they are within the valid range
  indices_x_to_tau[indices_x_to_tau < 1 | indices_x_to_tau > length(tau)] <- NA
  indices_tau_to_x[indices_tau_to_x < 1 | indices_tau_to_x > length(x)] <- NA
  
  # Get the nearest higher or equal elements from tau for x
  nearest_elements_x_to_tau <- ifelse(!is.na(indices_x_to_tau), tau[indices_x_to_tau], NA)
  nearest_elements_tau_to_x <- ifelse(!is.na(indices_tau_to_x), x[indices_tau_to_x], NA)
  
  # Calculate the distance to the nearest elements
  distance_x_to_tau <- ifelse(!is.na(indices_x_to_tau), x - nearest_elements_x_to_tau, NA)
  distance_tau_to_x <- ifelse(!is.na(indices_tau_to_x), nearest_elements_tau_to_x-tau, NA)
  
  if(short) return(max(distance_tau_to_x, na.rm = TRUE))
  
  # Create data frame with x to tau and tau to x distances
  results <- rbind(
    data.frame(k = x, nearest_elements = nearest_elements_x_to_tau, distance = distance_x_to_tau),
    data.frame(k = tau, nearest_elements = nearest_elements_tau_to_x, distance = distance_tau_to_x)
  )
  if(verbose) print(results)
  
  # Return max distance
  return(max(results$distance, na.rm = TRUE))
  
}

#### 2.2. RECALL AND PRECISION ----
# https://centre-borelli.github.io/ruptures-docs/user-guide/metrics/precisionrecall/

# -- Neighbour test - UNIDIRECTIONAL: finds if each tau_i has a neighbour AFTER (within margin)
# based on https://stackoverflow.com/questions/9508518/why-are-these-numbers-not-equal
neighbour_tau <- function(x, tau, margin){
  # for each tau, are there any x within tau+margin?
  sapply(tau, function(.tau) {any(between(x - .tau,0,margin))})
}
# !!! need to improve this function to not consider neighbours if there are two tau and one x. Or not???
# count how many x neighbours for each tau (how many changepoints are TP)
neighbour_tau_n <- function(x, tau, margin){
  # for each tau, are there any x within tau+margin?
  sapply(tau, function(.tau) {sum(between(x - .tau,0,margin))})
}
neighbour_x <- function(x, tau, margin){
  # for each x, are there any tau within x-margin?
  sapply(x, function(.x) {any(between(.x - tau,0,margin))})
}
neighbour_self <- function(x, margin){
  # for each .x, are there any x within x+margin?  OR
  sapply(x, function(.x) {any(between(x - .x,1,margin))}) |
    # OR for each .x, are there any x within x-margin?
    sapply(x, function(.x) {any(between(.x - x,1,margin))})
}

#test
# tau <- c(25)
# x <- c(26, 38, 48, 71, 110, 135)
# margin = 15
# neighbour_tau(x, tau, margin=15)
# neighbour_x(x, tau, margin=15)
# precision_recall(x, tau, margin)
# h_distance(x, tau)

# -- Precision / Recall
precision_recall <- function(x, tau, margin, verbose=FALSE){
  # detected cpts - neighbours
  TP = min(sum(neighbour_x(x, tau, margin)), sum(neighbour_tau(x, tau, margin)))
  FP = length(x) - TP
  
  precision <- round(TP / length(x), 4)
  recall <- round(TP / length(tau), 4)
  F1 <- round(2 * (precision * recall) / (precision + recall), 4)
  
  if(verbose) print(TP)
  
  result <- setNames(c(TP, FP, precision, recall, F1), 
                     c("true_positives", "false_positives", "precision", "recall", "F1"))
  return(result)
}

#### 2.3 RAND INDEX ####
# https://centre-borelli.github.io/ruptures-docs/user-guide/metrics/randindex/

# tau <- c(100, 200, 300, 400)
# x <- c(80, 180, 440)
# len <- 500

rand_index <- function(x, tau, len){
  if(len < max(x) | len < max(tau)) 
    stop("len must be bigger than the largest elements in x or tau")
  
  # segments tau
  idx <- seq_along(tau)
  tau.seg <- array()
  for (i in idx){
    if(i==1)  tau.seg <- rep(i, tau[i]-1)
    else  tau.seg <- c(tau.seg, rep(i, tau[i]-tau[i-1]))
  }
  tau.seg <- c(tau.seg, rep(max(idx)+1, len-max(tau)+1))
  # tau.seg
  # tau.seg[99]
  # tau.seg[100]
  # tau.seg[399]
  # tau.seg[400]
  # length(tau.seg)
  
  # segments x
  idx <- seq_along(x)
  x.seg <- array()
  for (i in idx){
    if(i==1) x.seg <- rep(i, x[i]-1)
    else  x.seg <- c(x.seg, rep(i, x[i]-x[i-1]))
  }
  x.seg <- c(x.seg, rep(max(idx)+1, len-max(x)+1))
  # x.seg
  # length(x.seg)
  
  fossil::rand.index(x.seg, tau.seg)   # NEED SEGMENTATIONS, NOT VALUES
}


#### 2.4 ANOTATION ERROR #### 
# http://www.laurentoudre.fr/publis/TOV-EUSIPCO-17.pdf

# tau <- c(100, 200, 300, 400, 450)
# x <- c(80, 180, 440)

annotation_error <- function(x, tau, norm=FALSE){   # - normalized on tau???
  ar <- abs(length(x) - length(tau))
  if(!norm)
    return(ar)
  else
    return(round(ar/length(tau), 4))              
}

#### 2.5 ALL METRICS #### 

# tau <- c(100, 200, 300, 400, 450)
# x <- c(80, 180, 440)
# len = 500
# margin = 50

cpt_metrics <- function(x, tau, margin, len, norm=FALSE){  
  setNames(
    if (length(x) == 0 | length(tau) == 0) {
      c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    }
    else(
      c(
        length(tau),
        length(x),
        annotation_error(x, tau, norm),
        h_distance(x, tau),
        h_distance(x, tau, short=TRUE),
        precision_recall(x, tau, margin),
        rand_index(x, tau, len)
      )
    ), 
    c("n_eps", "n_cpts", "ann_error", "h_distance", "h_dist_short", "true_pos", "false_pos", 
      "precision", "recall", "F1", "rand_index")
  )
}

# metrics.vctr <- metrics[[16]]
# str(metrics.vctr)
# Score: takes a vector of weights (defaults to 1/n) and calculates weighted sum as an overall score (MDCM)
cpt_metrics_score <- function(metrics.vctr, weights = c()){
  scores <- data.frame(as.list(metrics.vctr)) %>% 
    mutate(                 # normalize every metric in 0-1 and 1 the best
      inv_h_dist = 1-h_distance/145,               # max series lenght
      inv_h_dist_tp = 1-h_dist_short/145,          # max series lenght
      inv_ann_error = 1-ann_error/25               # max ann_error
    ) %>% 
    dplyr::select("precision", "recall", "F1", "inv_h_dist", "inv_h_dist_tp", "rand_index", "inv_ann_error") %>% 
    pivot_longer(everything(), names_to = "metric", values_to = "value")
  
  scores$metric <- factor(scores$metric, 
    levels = c("precision", "recall", "F1", "inv_h_dist", "inv_h_dist_tp", "rand_index", "inv_ann_error"))
  
  if(is.null(weights))  {
    weights =     
      setNames(c(
        .15,	.25,	0, .1,	.2,	.1,	.2
      ), 
        c("precision", "recall", "F1", "inv_h_dist", "inv_h_dist_tp", "rand_index", "inv_ann_error")
      )
  }
  
  score.sum = sum(scores$value*weights)
  returns(score.sum)
}


##### 4. SIGNALS ----

detrend_signal <- function(series, method){
  series.detrend = series
  
  if(method == 'diff'){
    series.detrend <- c(NA, diff(series))
  }
  
  return(series.detrend)
}

# takes an int or vector of ints and returns the next higher element in v1 or v2
# IMPORTANT: the function will always return a number. If it goes out of bounds, it will return min(i+limit, max_idx)
next_mark <- function(idx, v1, v2 = NULL, include_start = FALSE, iterations = 1, limit, max_idx = 145) {
  # Combine v1 and v2 into a single vector
  combined <- sort(c(v1, v2))
  
  # Find the next biggest element for each index (iterative version)
  next_elements <- sapply(idx, function(i) {
    next_idx <- ifelse(include_start, 
                       which(combined >= i)[iterations], 
                       which(combined > i)[iterations])
    next_element <- combined[next_idx]
    #return(next_element)
    if(is.na(next_element) & is.null(limit) & is.null(max_idx))  return(NA)
    
    return(min(next_element, i + limit, max_idx, na.rm = TRUE))
  })
  
  return(next_elements)
}

# same but for previous mark
previous_mark <- function(idx, v1, v2 = NULL, include_start = FALSE, iterations = 1, limit, min_idx=1) {
  # Combine v1 and v2 into a single vector
  combined <- sort(c(v1, v2))
  
  # Find the previous elements for each index based on the number of iterations
  previous_elements <- sapply(idx, function(i) {
    if(include_start){ lower_idxs <- which(combined <= i) 
    }else{ lower_idxs <- which(combined < i) }
    
    previous_idx <- lower_idxs[length(lower_idxs) - iterations + 1]
    previous_element <- combined[previous_idx]
    
    if((length(lower_idxs) == 0) & is.null(limit) & is.null(min_idx))  return(NA)
    
    return(max(previous_element, i - limit, min_idx, na.rm = TRUE))
  })
  return(previous_elements)
}

# !!! define function change_tp(type = c("event","cpt"), idx, x, tau)
# return setNames(c(mean_before, mean_after,  mean_after - mean_before, (mean_after - mean_before)/mean_before*100, (mean_after - mean_before) * 10000),
#         if(mean_after - mean_before < 0, "-", "+")
#         c("mean_before", "mean_after", "diference", "pcnt_change", "basepoints", "direction"))
change_tp_range <- function(i, signal, cp, evt = NULL, limit, min_idx, max_idx){
  # since last changepoint, last event, limit or beginning of signal
  before <- max(previous_mark(i, v1=evt, iterations = 1, include_start = FALSE, limit=limit, min_idx=min_idx),
                previous_mark(i, v1=cp, iterations = 1, include_start = FALSE, limit=limit, min_idx=min_idx))
  signal_before <- signal[before:i]
  mean_before <- mean(signal_before, na.rm = TRUE)
  
  # until next event or two changepoints, or limit or end of signal
  after <- min(next_mark(i, v1=evt, iterations = 1, include_start = FALSE, limit=limit, max_idx=max_idx),
               next_mark(i, v1=cp, iterations = 2, include_start = TRUE, limit=limit, max_idx=max_idx))
  signal_after <- signal[i:after]
  
  mean_after <- mean(signal_after, na.rm = TRUE)
  
  results <- setNames(c(before, i, after, 
                        mean_before, mean_after,  
                        mean_after - mean_before, 
                        (mean_after - mean_before) * 100,
                        (mean_after - mean_before)/mean_before*100,
                        ifelse(mean_after - mean_before < 0, 0, 1)),
                      #                        ),         # , "direction"
                      c("lim_before", "tau_i", "lim_after", "mean_before", "mean_after", "difference", "basepoints", "pcnt_change", "is_positive"))
  return(results)
}

# FIND CHANGEPOINT WITH MAX VALUE (PEAK) WITHIN MARGIN RANGE (AMONG TPs)
max_cpt <- function(tau_i, cp, signal, limit) {
  next_idxs = which((cp >= tau_i) &                 # in the future
                      (cp <= tau_i+limit) &         # under limit
                      (cp <= next_mark(tau_i, v1=tau, include_start = FALSE, 
                                      iterations = 1, limit = limit))    # before next flood
  )
  next_cpts = cp[next_idxs]
  
  if(length(next_cpts) == 0)  return(NA)
  if(length(next_cpts) == 1)  
    return(setNames(c(next_cpts, signal[next_cpts]), c("peak_cpt", "peak")))
  
  peak = max(signal[next_cpts])
  peak_cpt <- next_cpts[which(signal[next_cpts] == peak)]
  
  # if there are several peak_cpts with same peak, take the first
  if(length(peak_cpt) > 1)  peak_cpt <- peak_cpt[1]
  
  #return(peak_idx)
  return(setNames(c(peak_cpt, peak), c("peak_cpt", "peak")))
}
# FIND CHANGEPOINT WITH MAX VALUE (PEAK) WITHIN MARGIN RANGE (AMONG TPs)
max_idx <- function(tau_i, signal, limit) {
  idx = seq(1, length(signal))
  next_idxs = which((idx >= tau_i) &                 # in the future
                      (idx <= tau_i+limit) &         # under limit
                      (idx <= next_mark(tau_i, v1=tau, include_start = FALSE, 
                                       iterations = 1, limit = limit))    # before next flood
  )
  
  if(length(next_idxs) == 0)  return(NA)
  if(length(next_idxs) == 1)  
    return(setNames(c(next_idxs, signal[next_idxs]), c("peak_idx", "peak")))
  
  peak = max(signal[next_idxs])
  peak_idx <- next_idxs[which(signal[next_idxs] == peak)]
  
  # if there are several peak_cpts with same peak, take the first
  if(length(peak_idx) > 1)  peak_idx <- peak_idx[1]
  
  #return(peak_idx)
  return(setNames(c(peak_idx, peak), c("peak_idx", "peak")))
}
# find the point (if any) where the signal goes back to normal
back_to_base <- function(tau_i, peak_i, signal, tolerance = .01){
  base = signal[tau_i]
  max_signal = max(signal)
  tolerance = (max_signal)*tolerance
  
  next_idxs = which(between(signal, base-tolerance, base+tolerance))
  next_idxs = next_idxs[next_idxs > peak_i]
  value = signal[next_idxs[1]]
  return(setNames(c(next_idxs[1], value, 
                    next_idxs[1]-tau_i, next_idxs[1]-peak_i)
                  , c("end", "end_value", "dist_tau_to_end", "dist_peak_to_end")))
}

# find most common values:
mode_statistic <- function(x) {
  freq_table <- table(x)
  max_freq <- max(freq_table)
  
  modes <- as.numeric(names(freq_table[freq_table == max_freq]))
  mode_indices <- lapply(modes, function(mode) which(x == mode))
  
  result <- list(
    mode_vals = modes,
    mode_indices = mode_indices
  )
  return(result)
}

# CALCULATE CHANGE vs MAX_CPT (PEAK) for one tp
change_tp_peak <- function(tau_i, cp, signal, limit){
  # we compare vs take-up rate at time of event
  base <- signal[tau_i]
  
  # compare vs the cpt with maximum value within limit range
  peak_info <- max_cpt(tau_i, cp, signal, limit)
  peak = peak_info['peak']
  peak_i = peak_info['peak_cpt']
  # add info on the back to base effect
  end_info <- back_to_base(tau_i, peak_i, signal)
  
    results <- setNames(c(tau_i, peak_i, peak_i - tau_i,
                        base, peak, peak - base,
                        (peak - base) * 100,
                        (peak - base)/base*100,
                        ifelse(peak - base > 0, 1, 0),
                        end_info),
                      c("tau_i", "cpt_peak", "dist_to_peak",
                        "base", "peak", "difference", 
                        "basepoints", "pcnt_change", "is_positive",
                        names(end_info)))
  return(results)
}
# CALCULATE CHANGE vs MAX_CPT (PEAK) for one tp
change_tp_peak_idx <- function(tau_i, cp, signal, limit){
  # we compare vs take-up rate at time of event
  base <- signal[tau_i]
  
  # compare vs the cpt with maximum value within limit range
  peak_info <- max_idx(tau_i, signal, limit)
  peak = peak_info['peak']
  peak_i = peak_info['peak_idx']
  # add info on the back to base effect
  end_info <- back_to_base(tau_i, peak_i, signal)
  
  results <- setNames(c(tau_i, peak_i, peak_i - tau_i,
                        base, peak, peak - base,
                        (peak - base) * 100,
                        (peak - base)/base*100,
                        ifelse(peak - base > 0, 1, 0),
                        end_info),
                      c("tau_i", "idx_peak", "dist_to_peak",
                        "base", "peak", "difference", 
                        "basepoints", "pcnt_change", "is_positive",
                        names(end_info)))
  return(results)
}


# calculate change for each county, for each TP
change_tp <- function(idx, signal, cp, evt = NULL, limit = 12*3, min_idx = 1, max_idx = 145){
  r <- lapply(idx, change_tp_range, signal, cp, evt, limit, min_idx, max_idx)
  r <- bind_rows(r) %>% rename_with(~paste0("r.", .), everything())
  
  m <- lapply(idx, change_tp_peak, cp = cp, signal = signal, limit = limit)
  m <- bind_rows(m) %>% rename_with(~paste0("m.", .), everything())
  
  p <- lapply(idx, change_tp_peak_idx, cp = cp, signal = signal, limit = limit)
  p <- bind_rows(p) %>% rename_with(~paste0("p.", .), everything())
  
  cbind(r, m, p)
}

# change_tp(idx, signal, cp, evt, limit = 12)
# change_tp(idx, signal, cp, evt, limit = 15)
# change_tp(idx, signal, cp, evt, limit = 18)
# change_tp(idx, signal, cp, evt, limit = 24)
# change_tp(idx, signal, cp, evt, limit = 36)
# change_tp(idx, signal, cp, evt, limit = 48)

#change_tp(idx, signal, cp, evt, limit = 15)






##### 5. OTHER ----
# left join which replaces NA with 0
# https://stackoverflow.com/questions/35186694/r-left-outer-join-with-0-fill-instead-of-na-while-preserving-valid-nas-in-left
left_join0 <- function(x, y, fill = 0L, ...){
  z <- left_join(x, y, ...)
  new_cols <- setdiff(names(z), names(x))
  z <- tidyr::replace_na(z, setNames(as.list(rep(fill, length(new_cols))), new_cols))
  z
}


# data <- counties.ori %>% dplyr::select(CountyCodeTxt, CensusHU2010, CensusHU2020)
# glimpse(data)
# dim(data)
#data %>% filter(is.na(CensusHU2020))

# interpolate monthly values between census data - Exponential method
# requires a dataset with countryCode, Census1 and Census2
interpolate_census <- function(data, start_date = as.Date("2010-01-01"), end_date = as.Date("2022-01-01")) {
  # Generate dates from start_date to end_date
  dates <- seq(start_date, end_date, by = "month")
  
  # Perform exponential interpolation for each row in the dataset
  interpolated_data <- lapply(1:nrow(data), function(i) {
    housing_units <- c(data[i,2], data[i,3])
    
    # Calculate the growth rate and Perform compounding growth interpolation
    growth_rate <- (housing_units[2] / housing_units[1])^(1/10) - 1   # 10 years btwn census
    interp_values <- housing_units[1] * (1 + growth_rate)^(as.numeric(dates - start_date) / 365.25)
    
    # Create a data frame with county code and interpolated values
    data.frame(countyCode = data[i,1], dateHU = dates, housingUnits = interp_values)
  })
  #interpolated_data[[1]]
  
  # Combine the interpolated data frames into a single data frame
  result <- do.call(rbind, interpolated_data)
  
  return(result)
}

# length(data$CountyCodeTxt)
# length(interpoated_data)




