# --- --- --- --- --- --- --- --- --- --- --- --- ---
# 04 - County aggregation, PCA and k-means clusters
# Input:  data/derived/panel.rds, covariates.rds, changepoints.rds
# Output: data/derived/pca_clusters.rds
# NOTE on PC naming: signs and order of the PC1_SocVul / PC2_RiskPr / PC3_FlDmg columns follow the
# convention used for the paper's Table 2. See README.md ("Randomness and sign conventions").
# --- --- --- --- --- --- --- --- --- --- --- --- ---
source(here::here("R", "config.R"))
library(tibble)
p <- readRDS(file.path(path_derived, "panel.rds"))
counties.crs <- readRDS(file.path(path_derived, "covariates.rds"))
cp <- readRDS(file.path(path_derived, "changepoints.rds"))
tau.change <- cp$tau.change
counties.sf <- cp$counties.sf

#### COUNTY LEVEL AGGREGATION ####
counties.change <- tau.change %>% filter(!isOutlier) %>%
  filter(!countyCode %in% c("04019", "12087", "36053","36065")) %>% 
  group_by(countyCode) %>% 
  summarise(nEvents = n()
            , evtsYear = nEvents/time_length(difftime(t_end, t_begin), "years")
            , dmgYear = sum(dmg)/time_length(difftime(t_end, t_begin), "years")
            , impact = mean(m.pcnt_change)
            , impact_sd = sd(m.pcnt_change)
            , impact_bps = mean(m.basepoints)
            , memory = mean(m.dist_tau_to_end, na.rm = TRUE)
            , memory_sd = sd(m.dist_tau_to_end, na.rm = TRUE)
  ) %>% ungroup() %>% 
  left_join(counties.sf %>% mutate(pcntWater = awater/(aland+awater)) %>% 
              select("CountyCodeTxt", "shoreline", "iniTakeup", "dmgTotal", "awater", "pcntWater")
            , by = c("countyCode" = "CountyCodeTxt")) %>% 
  left_join(counties.crs %>% 
              select("CountyCodeTxt", "RISK_SCORE", "SOVI_SCORE", "RESL_SCORE"
                     , "dis_pop_pct", "pcntParticipating", "pcntClassRating"
                     , "sfhaRatio"
              )
            , by = c("countyCode" = "CountyCodeTxt")) %>% 
  relocate(shoreline, .after = "countyCode") %>% 
  relocate(sfhaRatio, .after = "iniTakeup") %>% 
  relocate(geometry, .after = last_col())

counties.change <- counties.change %>% filter(!is.na(dis_pop_pct))

# only events with impact > 0
counties.change.imp <- tau.change %>% filter(!isOutlier & m.is_positive == 1) %>% 
  group_by(countyCode) %>% 
  summarise(nEvents = n()
            , evtsYear = nEvents/lubridate::time_length(difftime(t_end, t_begin), "years")
            , dmgYear = sum(dmg)/lubridate::time_length(difftime(t_end, t_begin), "years")
            , impact = mean(m.pcnt_change)
            , impact_sd = sd(m.pcnt_change)
            , impact_bps = mean(m.basepoints)
            , memory = mean(m.dist_tau_to_end, na.rm = TRUE)
            , memory_sd = sd(m.dist_tau_to_end, na.rm = TRUE)
  ) %>% ungroup() %>% 
  left_join(counties.sf %>% mutate(pcntWater = awater/(aland+awater)) %>% 
              select("CountyCodeTxt", "shoreline", "iniTakeup", "dmgTotal", "awater", "pcntWater")
            , by = c("countyCode" = "CountyCodeTxt")) %>% 
  left_join(counties.crs %>% 
              select("CountyCodeTxt", "RISK_SCORE", "SOVI_SCORE", "RESL_SCORE"
                     , "dis_pop_pct", "pcntParticipating", "pcntClassRating", "sfhaRatio"
              )
            , by = c("countyCode" = "CountyCodeTxt")) %>% 
  filter(!is.na(dis_pop_pct)) %>% 
  relocate(shoreline, .after = "countyCode") %>% relocate(geometry, .after = last_col())

# only events with impact > 0 and a memory value
counties.change.mem <- tau.change %>% filter(!isOutlier & m.is_positive == 1 & !is.na(m.dist_tau_to_end)) %>% 
  group_by(countyCode) %>% 
  summarise(nEvents = n()
            , evtsYear = nEvents/lubridate::time_length(difftime(t_end, t_begin), "years")
            , dmgYear = sum(dmg)/lubridate::time_length(difftime(t_end, t_begin), "years")
            , impact = mean(m.pcnt_change)
            , impact_sd = sd(m.pcnt_change)
            , impact_bps = mean(m.basepoints)
            , memory = mean(m.dist_tau_to_end, na.rm = TRUE)
            , memory_sd = sd(m.dist_tau_to_end, na.rm = TRUE)
  ) %>% ungroup() %>% 
  left_join(counties.sf %>% mutate(pcntWater = awater/(aland+awater)) %>% 
              select("CountyCodeTxt", "shoreline", "iniTakeup", "dmgTotal", "awater", "pcntWater")
            , by = c("countyCode" = "CountyCodeTxt")) %>% 
  left_join(counties.crs %>% 
              select("CountyCodeTxt", "RISK_SCORE", "SOVI_SCORE", "RESL_SCORE"
                     , "dis_pop_pct", "pcntParticipating", "pcntClassRating", "sfhaRatio"
              )
            , by = c("countyCode" = "CountyCodeTxt")) %>% 
  filter(!is.na(dis_pop_pct)) %>% 
  relocate(shoreline, .after = "countyCode") %>% relocate(geometry, .after = last_col())

#### PCA ####
set.seed(1)
res.pca_sfha <- prcomp(counties.change %>% na.omit() %>% 
                         select("iniTakeup", "sfhaRatio", "pcntWater", "evtsYear", "dmgYear"
                                , "RISK_SCORE", "SOVI_SCORE", "RESL_SCORE", "dis_pop_pct"
                                , "pcntParticipating", "pcntClassRating")
                       , scale. = TRUE)
res.pca_sfha$rotation[,"PC1"] = -res.pca_sfha$rotation[,"PC1"]
#res.pca_sfha$rotation[,"PC2"] = -res.pca_sfha$rotation[,"PC2"]
res.pca_sfha$rotation[,"PC3"] = -res.pca_sfha$rotation[,"PC3"]

# importance and rotation tables
pca_out.importance <- summary(res.pca_sfha)$importance
pca_out.rotation <- res.pca_sfha$rotation[,1:3]           # rotate for plot
pca_out.rotation[,"PC1"] = -pca_out.rotation[,"PC1"]
#pca_out.rotation[,"PC2"] = -pca_out.rotation[,"PC2"]
pca_out.rotation[,"PC3"] = -pca_out.rotation[,"PC3"]

# use the PCA dimensions
prd.pca <- as.data.frame(predict(res.pca_sfha, newdata = counties.change))
counties.change$PC1_SocVul <- -prd.pca$PC1     # swap PC1_RiskPr for PC1_SocVul
counties.change$PC2_RiskPr <- prd.pca$PC2     # swap PC2_RiskPr for PC2_RiskPr
counties.change$PC3_FlDmg <- -prd.pca$PC3   
counties.change$PC4_Part <- prd.pca$PC4
counties.change <- counties.change %>% relocate(geometry, .after = last_col())

prd.pca.imp <- as.data.frame(predict(res.pca_sfha, newdata = counties.change.imp))
counties.change.imp$PC1_SocVul <- -prd.pca.imp$PC2
counties.change.imp$PC2_RiskPr <- prd.pca.imp$PC1
counties.change.imp$PC3_FlDmg <- -prd.pca.imp$PC3   
counties.change.imp$PC4_Part <- prd.pca.imp$PC4
counties.change.imp <- counties.change.imp %>% relocate(geometry, .after = last_col())

prd.pca.mem <- as.data.frame(predict(res.pca_sfha, newdata = counties.change.mem))
counties.change.mem$PC1_SocVul <- -prd.pca.mem$PC2
counties.change.mem$PC2_RiskPr <- prd.pca.mem$PC1
counties.change.mem$PC3_FlDmg <- -prd.pca.mem$PC3   
counties.change.mem$PC4_Part <- prd.pca.mem$PC4
counties.change.mem <- counties.change.mem %>% relocate(geometry, .after = last_col())

#### CLUSTERS ####
pc_var <- c("PC1_SocVul", "PC2_RiskPr", "PC3_FlDmg")
counties.clu <- counties.change.mem %>% st_drop_geometry() %>% 
  mutate(impact_scl = scales::rescale(log(impact)),      # we use log because of long tail
         memory_scl = scales::rescale(log(memory)),      # we use log because of long tail
         PC1_SocVul_scl = scales::rescale(PC1_SocVul),
         PC2_RiskPr_scl = scales::rescale(PC2_RiskPr),
         PC3_FlDmg_scl = scales::rescale(PC3_FlDmg),
  ) %>% 
  select("countyCode", "shoreline", "impact_scl", "memory_scl", "PC1_SocVul_scl", "PC2_RiskPr_scl", "PC3_FlDmg_scl")

# Split the data
shoreline_data <- counties.clu %>% 
  filter(shoreline == "shoreline") %>% 
  select(-shoreline)

inland_data <- counties.clu %>% 
  filter(shoreline == "inland") %>% 
  select(-shoreline)

# K-Means Cluster Analysis (3 inland + 2 coastal)
set.seed(1)
cluster_vars <- c("impact_scl", "memory_scl", 
                  "PC1_SocVul_scl", "PC2_RiskPr_scl", "PC3_FlDmg_scl")

fit_inland <- kmeans(inland_data %>% select(all_of(cluster_vars)), 3) 
fit_coastal <- kmeans(shoreline_data %>% select(all_of(cluster_vars)), 2) 

inland_clusters <- inland_data %>%
  select(countyCode) %>%
  mutate(fit.cluster = as.factor(fit_inland$cluster))
shoreline_clusters <- shoreline_data %>%
  select(countyCode) %>%
  mutate(fit.cluster = as.factor(fit_coastal$cluster + 3) )    # clusters 4 and 5

all_clusters <- bind_rows(inland_clusters, shoreline_clusters)

# Merge back to original dataset
counties.change.mem <- counties.change.mem %>%
                                 left_join(all_clusters, by = "countyCode")
counties.change.mem <-  counties.change.mem  %>% relocate(geometry, .after = last_col())
counties.change.mem <- st_as_sf(counties.change.mem)

# rename clusters
counties.change.mem <- counties.change.mem %>% 
  mutate(cluster = case_when(fit.cluster == 1  ~ "l1",
                             fit.cluster == 2  ~ "l2",
                             fit.cluster == 3  ~ "l3",
                             fit.cluster == 4  ~ "c1",
                             fit.cluster == 5  ~ "c2",
                             )) %>% 
  relocate(geometry, .after = last_col())

saveRDS(list(counties.change = counties.change, counties.change.imp = counties.change.imp,
             counties.change.mem = counties.change.mem, res.pca = res.pca_sfha,
             pca_out.importance = pca_out.importance, pca_out.rotation = pca_out.rotation,
             fit_inland = fit_inland, fit_coastal = fit_coastal),
        file.path(path_derived, "pca_clusters.rds"))
message("04_pca_clusters: ", nrow(counties.change), " counties; ", nrow(counties.change.imp),
        " with salience; ", nrow(counties.change.mem), " with memory")
print(table(counties.change.mem$cluster))
