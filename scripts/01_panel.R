# --- --- --- --- --- --- --- --- --- --- --- --- ---
# 01 - Panel construction
# Builds the county list, NOAA flood episodes, monthly NFIP policy stock and
# take-up rate panel.
# Output: data/derived/panel.rds
# --- --- --- --- --- --- --- --- --- --- --- --- ---
source(here::here("R", "config.R"))

#### CENSUS + NOAA ####
census <- read.csv2(path_census)
county_codes <- as.numeric(census$CountyCode)
noaa <- load_noaa(path_noaa, t_begin, t_end_noaa) %>% 
  filter(as.numeric(countyCode) %in% county_codes)

#### COUNTIES + INITIAL STOCK ####
counties.ori <- census %>% 
  filter(CountyId != 0) %>% 
  mutate(across(where(is.character), ~str_remove_all(., pattern = fixed(" ")))) %>% 
  mutate(CountyCodeTxt = str_pad(CountyCode, 5, side = "left", pad = "0"),
         AvgHU = as.numeric(AvgHU),
         CensusHU2010 = as.numeric(CensusHU2010),
         CensusHU2020 = as.numeric(CensusHU2020)) %>% 
  filter(!is.na(AvgHU)) %>% 
  select(c("CountyCodeTxt", colnames(census))) %>% 
  arrange(CountyCodeTxt)

conDB <- db_connect()
counties.ini.stock <- dbGetQuery(conDB, "Select
	countyCode,
	dateStock,
  sum(stock) as iniStock,
  sum(case when sfhaindicator = 1 then stock else 0 end) as iniSfhaStock,
  sum(case when sfhaindicator = 1 then stock else 0 end)/sum(stock) as iniSfhaRatio
from stock_day
where dateStock = '2009-12-31'
group by countyCode, dateStock
")
dbDisconnect(conDB, shutdown = TRUE)

counties <- counties.ori %>% 
  left_join(counties.ini.stock, by = c("CountyCodeTxt" = "countyCode")) %>% 
  mutate(iniStock = case_when(is.na(iniStock) ~ 0,
                                .default = iniStock),
         iniTakeup = round(100*iniStock/CensusHU2010, 8)) %>% 
  select(c("CountyCodeTxt", "StateId", "CountyId", "CountyCode", "StateName", "CountyName"    
           , "CensusHU2010", "CensusHU2020", "AvgHU", "AvgPOP", "dateStock", "iniStock", "iniTakeup"
           , "iniSfhaStock", "iniSfhaRatio" ))

# take-up categories
counties <- counties %>% 
  mutate(.after = iniTakeup, takeup_cat = case_when(
                                iniTakeup>=30 ~ "30%+",   # 25%
                                iniTakeup>=15 ~ "15%",    # 50%
                                iniTakeup>=5.0 ~ "5.0%",  # 70%
                                iniTakeup>=2.5 ~ "2.5%",  # 80%
                                iniTakeup>=1.5 ~ "1.5%",  # 85%
                                iniTakeup>=1.0 ~ "1.0%",  # 90%
                                iniTakeup>=0.5 ~ ".5%",   # 95%
                                iniTakeup>=0.2 ~ ".2%",   # 99%
                                  .default = "<.2%")) %>% 
  select(c(colnames(counties), "takeup_cat"))
counties$takeup_cat <- factor(counties$takeup_cat, levels = c("<.2%", ".2%", ".5%", "1.0%","1.5%","2.5%", "5.0%", "15%", "30%+"))

#### COUNTIES DATA ####
counties.base <- counties
county_codes <- counties.base$CountyCode
county_codes_txt <- counties.base$CountyCodeTxt

#### FLOOD DATA: noaa.episodes, noaa.count, l.tau ####
noaa.episodes <- noaa %>% 
  group_by(countyCode, EPISODE_ID) %>% 
  summarise(beginDate = min(beginDate),
            # endDate = max(lubridate::as_date(END_DATE_TIME)),
            dmgEps = sum(damage),
            didDamage = ifelse(sum(damage) > 0, 1, 0),
  ) %>% 
  #  arrange(desc(dmg_avg)) %>% 
  ungroup() %>% 
  mutate(countyEpisode = paste(countyCode, EPISODE_ID, sep = '-')) %>%
  filter(didDamage == 1) %>% 
  arrange(desc(dmgEps)) %>%
  mutate(cumulativeDamage = cumsum(dmgEps),
         percentile = percent_rank(dmgEps),
         dmgCovered = cumulativeDamage/max(cumulativeDamage))

noaa.episodes <- noaa.episodes %>% 
  mutate(dmg_cat = case_when(dmgEps>=3000000 ~ "3M+",   # .05
                             dmgEps>=1000000 ~ "1M",    # .15
                             dmgEps>= 750000 ~ "750K",  # .25
                             dmgEps>= 500000 ~ "500K",  # .50
                             dmgEps>= 175000 ~ "175K",  # .75
                             dmgEps>= 112000 ~ "112K",  # .80
                             dmgEps>= 100000 ~ "100K",  # .85
                             dmgEps>=  50000 ~ "50K",   # .90
                             dmgEps>=  25000 ~ "25K",   # .95
                             dmgEps>=   6000 ~ "6K",    # .99
                             .default = "5K-"))
noaa.episodes$dmg_cat <- factor(noaa.episodes$dmg_cat, levels = c("5K-", "6K", "25K", "50K", "100K", "112K", "175K", "500K", "750K", "1M", "3M+"))

# Check the manually defined threshold and apply flag
dmgThreshold
noaa.episodes <- noaa.episodes %>% 
  mutate(dmgFlag = dmgEps>=dmgThreshold)

noaa.count <- noaa.episodes %>%
  filter(dmgEps >= dmgThreshold) %>%        # -- ** filter episodes over dmg limit ---
  group_by(countyCode) %>%
  summarise(#noaaDidDmg = sum(didDamage == 1),      # remove didDmg
    #noaaEpisodes = sum(dmgFlag),           # change noaaEpisodes for total count
    noaaEpisodes = sum(didDamage == 1),
    dmgTotal = sum(dmgEps),               # sum all damage in the county, over or under dmg threshold
    dmgAvg = ifelse(noaaEpisodes == 0, 0, dmgTotal/noaaEpisodes)) %>%
  arrange(desc(noaaEpisodes)) %>%
  ungroup()

l.tau <- noaa.episodes %>% 
  filter(dmgEps >= dmgThreshold) %>%        # -- ** filter episodes over dmg limit ---
  mutate(t = lubridate::floor_date(beginDate, timestep)) %>% 
  group_by(countyCode, t) %>% 
  summarise(beginDate = min(beginDate),
            dmg = sum(dmgEps),
            nEps = n(),
  ) %>% 
  mutate(idx = match(t, dates)) %>% 
  arrange(countyCode, t) %>% 
  split(f = as.factor(.$countyCode))

# * merge episodes within a 3 months range.  // 25 sec ---
l.tau <- lapply(l.tau, merge_episodes, margin = 3)

#### POLICIES DATA ####
conDB <- db_connect()
counties.stock <- dbGetQuery(conDB, paste("Select
	countyCode,
	dateStock,
  sum(stock) as stock,
  sum(case when sfhaindicator = 1 then stock else 0 end) as sfhaStock,
  sum(case when sfhaindicator = 1 then stock else 0 end)/sum(stock) as sfhaRatio
from stock_day
where day(dateStock) = 1
group by countyCode, dateStock
order by countyCode, dateStock
;
")) %>%
  mutate(dateStock = lubridate::ymd(dateStock)) %>%
  filter(countyCode %in% county_codes_txt & dateStock %in% dates)
dbDisconnect(conDB, shutdown = TRUE)

# Interpolate data to get housing units per month
counties.hu <- counties.base %>%
  select("CountyCodeTxt", "CensusHU2010", "CensusHU2020") %>%
  interpolate_census()

# join with stock and calculate monthly takeup
counties.stock <- counties.stock %>%
  left_join(counties.hu, by = c("countyCode" = "countyCode", "dateStock" = "dateHU")) %>%
  mutate(takeup = round(100*stock/housingUnits, 8))
rm(counties.hu)

# counties.base - drop columns from the contingency step and add new episodes count (according to threshold)
counties.base <- counties.base %>% select(-dateStock)
counties.base <- counties.base %>% left_join0(noaa.count, by = c("CountyCodeTxt" = "countyCode"))

counties.base <- counties.base %>% 
  mutate(noaa_cat = case_when(noaaEpisodes>=20 ~ "20+",
                              noaaEpisodes>=10 ~ "10+",
                              noaaEpisodes>=5 ~ "5+",
                              noaaEpisodes>=1 ~ "<5",
                              .default = "0"))
counties.base$noaa_cat <- factor(counties.base$noaa_cat, levels = c("0", "<5", "5+", "10+","20+"))

#glimpse(counties.base)

# * inSample: final number of counties under analysis: ---
counties.base <- counties.base %>% 
  mutate(inSample = !(StateId %in% c(2, 15)) &       # Only Continental US (CONUS), exclude Alaska and Hawaii
        iniTakeup >= turThreshold & 
        noaaEpisodes >= 1)            # counties with at least 1 episode with dmg over dmgThreshold

length(counties.base %>% filter(inSample) %>% pull(CountyCode))

# l.counties - list of take-up rates per county
l.counties <- counties.stock %>% 
  left_join(counties.base, by = c("countyCode" = "CountyCodeTxt")) %>% 
  filter(inSample) %>%       # ** filter county in sample ---
  mutate(t = dateStock,
         idx = match(t, dates)) %>% 
  select(c("countyCode", "StateName", "CountyName", "AvgPOP",
           "t", "idx", "stock", "housingUnits", "takeup", "takeup_cat", "noaa_cat"))

county_codes_sample <- unique(l.counties$countyCode)

l.counties <- l.counties %>% split(f = as.factor(.$countyCode))

saveRDS(list(counties = counties, counties.base = counties.base,
             noaa.episodes = noaa.episodes, noaa.count = noaa.count,
             l.tau = l.tau, l.counties = l.counties,
             county_codes_sample = county_codes_sample,
             counties.stock = counties.stock),
        file.path(path_derived, "panel.rds"))
# small monthly stock table: lets later steps be inspected without the 19 GB database
write.csv(counties.stock, file.path(path_derived, "monthly_stock.csv"), row.names = FALSE)
message("01_panel: counties in sample = ", length(county_codes_sample))
