# --- --- --- --- --- --- --- --- --- --- --- --- ---
# 02 - County covariates
# NFIP Community Rating System (CRS), FEMA National Risk Index (NRI), CEJST
# disadvantaged communities and SFHA policy ratio, joined to the county list.
# Input:  data/derived/panel.rds, raw CRS / NRI / CEJST files, database (read-only)
# Output: data/derived/covariates.rds
# --- --- --- --- --- --- --- --- --- --- --- --- ---
source(here::here("R", "config.R"))
library(readr)
counties <- readRDS(file.path(path_derived, "panel.rds"))$counties

#### CRS DATA ####
crs <- read.csv(path_crs) 
crs <- crs %>%
  mutate(
    originalEntryDate = ymd_hms(originalEntryDate),
    classRatingEffectiveDate = ymd_hms(classRatingEffectiveDate),
    initialFloodInsuranceRateMap = ymd_hms(initialFloodInsuranceRateMap),
    regularEmergencyProgramDate = mdy(substr(regularEmergencyProgramDate, 1, 8))
  )
# remove double county entries. ELMORE COUNTY/AUTAUGA COUNTY -> ELMORE COUNTY
crs <- crs %>% mutate(.after = county,
  county_spl = ifelse(grepl("/", county),
                      substr(county, 1, str_locate(county, "/")[,1] - 1),
                      county))
USAboundaries::state_codes
crs <- crs %>% left_join(USAboundaries::state_codes, by = c("state" = "state_abbr"))
# add key for join
crs <- crs %>% mutate(.after = county_spl,
          county_key = paste(state_code, tolower(gsub(" ", "", gsub("[[:punct:]]", "", county_spl))), sep = ""))
# Reduce base to county level
crs_group <- crs %>% group_by(county_key) %>% 
  summarise(
    communities = n(),
    participatingInNFIP = sum(participatingInNFIP),
    pcntParticipating = participatingInNFIP/communities,
    withClassRating = sum(!is.na(classRating)),      # *** CHECK ----
    pcntClassRating = withClassRating/communities,
    avgClassRating = mean(classRating, na.rm = TRUE),
    programDate = min(regularEmergencyProgramDate, na.rm = TRUE),
    classRatingDate = min(classRatingEffectiveDate, na.rm = TRUE),
  ) %>% 
  ungroup()


#### NATIONAL RISK INDEX DATA ####
risk <- read.csv(path_risk) %>%
  mutate(
    NRI_ID = str_sub(NRI_ID,-5,-1)
  ) %>% 
  select(
    c("NRI_ID", "STATE","COUNTY","STCOFIPS","NRI_ID","RISK_VALUE","RISK_SCORE","RISK_RATNG","ALR_NPCTL","ALR_VRA_NPCTL","SOVI_SCORE","SOVI_RATNG","RESL_SCORE","RESL_RATNG", "CRF_VALUE"
      , "CFLD_RISKV","CFLD_RISKS","CFLD_RISKR","HRCN_RISKV","HRCN_RISKS","HRCN_RISKR","RFLD_RISKV","RFLD_RISKS","RFLD_RISKR"
      , 'AVLN_RISKV', 'AVLN_RISKS', 'AVLN_RISKR', 'CWAV_RISKV', 'CWAV_RISKS', 'CWAV_RISKR', 'DRGT_RISKV', 'DRGT_RISKS', 'DRGT_RISKR', 'ERQK_RISKV', 'ERQK_RISKS', 'ERQK_RISKR', 'HAIL_RISKV', 'HAIL_RISKS', 'HAIL_RISKR', 'HWAV_RISKV', 'HWAV_RISKS', 'HWAV_RISKR', 'ISTM_RISKV', 'ISTM_RISKS', 'ISTM_RISKR'
      , 'LNDS_RISKV', 'LNDS_RISKS', 'LNDS_RISKR', 'LTNG_RISKV', 'LTNG_RISKS', 'LTNG_RISKR', 'SWND_RISKV', 'SWND_RISKS', 'SWND_RISKR', 'TRND_RISKV', 'TRND_RISKS', 'TRND_RISKR', 'TSUN_RISKV', 'TSUN_RISKS', 'TSUN_RISKR', 'VLCN_RISKV', 'VLCN_RISKS', 'VLCN_RISKR', 'WFIR_RISKV', 'WFIR_RISKS', 'WFIR_RISKR', 'WNTW_RISKV', 'WNTW_RISKS', 'WNTW_RISKR'
      )
  )


risk <- risk %>% 
  mutate(FLD_RISK_VALUE = CFLD_RISKV+RFLD_RISKV+HRCN_RISKV,
         FLD_RISK_SHARE = FLD_RISK_VALUE/RISK_VALUE) %>% 
  select(NRI_ID, RISK_VALUE, RISK_SCORE, RISK_RATNG, FLD_RISK_VALUE, FLD_RISK_SHARE,
         SOVI_SCORE, SOVI_RATNG, RESL_SCORE, RESL_RATNG)


#### JUSTICE DATA ####
justice <- read.csv(path_justice) %>% 
  mutate(
    county_id = str_sub(str_pad(Census.tract.2010.ID, 11, side = "left", pad = "0"), 1, 5),
    dis_flag = as.logical(Identified.as.disadvantaged),
    dis_share = Share.of.neighbors.that.are.identified.as.disadvantaged,
    pop = Total.population,
    dis_pop = pop*dis_share/100
  ) %>% 
  select(county_id, dis_flag, dis_share, pop, dis_pop)


justice <- justice %>% group_by(county_id) %>% 
  summarise(
    n = n(),
    dis_n = sum(dis_flag),
    dis_n_pct = dis_n/n,
    pop = sum(pop),
    #dis_pop = sum(pop*dis_share/100),
    dis_pop_pct = sum(dis_pop)/pop
  ) %>% ungroup()


#### COUNTIES DATA ####
counties.crs <- counties

# JOIN RISK
counties.crs <- counties.crs %>% left_join(risk, by = c("CountyCodeTxt" = "NRI_ID"))
# JOIN JUSTICE
counties.crs <- counties.crs %>% left_join(justice, by = c("CountyCodeTxt" = "county_id"))
# JOIN CRS
counties.crs <- counties.crs %>% mutate(.after = "CountyName",
  CountyKey = paste(str_pad(StateId, 2, side = "left", pad = "0"), 
                    tolower(gsub(" ", "", gsub("[[:punct:]]", "", CountyName))), sep = "")
)
counties.crs <- counties.crs %>% 
  left_join0(crs_group %>% select(-c("programDate", "classRatingDate")), 
             by = c("CountyKey" = "county_key"))


# JOIN SFHA: sfha stock and ratio at total level (all voluntary policies in db)
conDB <- db_connect()
sfha <- dbGetQuery(conDB, "Select
	countyCode,
	COUNT(*) as totalPolicies,
  SUM(CASE WHEN SFHAIndicator = TRUE THEN 1 ELSE 0 END) as sfhaPolicies,
    
  -- Percentages
  ROUND(SUM(CASE WHEN SFHAIndicator = TRUE THEN 1 ELSE 0 END) * 100.0 / COUNT(*), 2) as sfhaRatio
  from voluntary_policies
  
  group by countyCode
  order by countyCode;")

# Close connection
dbDisconnect(conDB, shutdown = TRUE)

counties.crs <- counties.crs %>% 
  left_join0(sfha, 
             by = c("CountyCodeTxt" = "countyCode"))


saveRDS(counties.crs, file.path(path_derived, "covariates.rds"))
message("02_covariates: ", nrow(counties.crs), " counties")
