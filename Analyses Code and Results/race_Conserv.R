# black vs conservation
bgr <- census_results3 %>%
  distinct(tractce10, black_total, conservation, total_population) %>%
  mutate(black_perc = black_total / total_population)
bgr_lm <- lm(conservation ~ black_perc, data=bgr)
summary(bgr_lm) 

# hisp vs conservation
hgr <- census_results3 %>%
  distinct(tractce10, hisp_total, conservation, total_population) %>%
  mutate(hisp_perc = hisp_total / total_population)
hgr_lm <- lm(conservation ~ hisp_perc, data=hgr)
summary(hgr_lm) 

# asian vs conservation
agr <- census_results3 %>%
  distinct(tractce10, asian_total, conservation, total_population) %>%
  mutate(asian_perc = asian_total / total_population)
agr_lm <- lm(conservation ~ asian_perc, data=agr)
summary(agr_lm) 

# indian vs conservation
igr <- census_results3 %>%
  distinct(tractce10, ind_ak_total, conservation, total_population) %>%
  mutate(ind_ak_perc = ind_ak_total / total_population)
igr_lm <- lm(conservation ~ ind_ak_perc, data=igr)
summary(igr_lm) 

# white vs conservation area
wgr <- census_results3 %>%
  distinct(tractce10, white_total, conservation, total_population) %>%
  mutate(white_perc = white_total / total_population)
wgr_lm <- lm(conservation ~ white_perc, data=wgr)
summary(wgr_lm) 

# total minority vs conservation area
mgr <- census_results3 %>%
  distinct(tractce10, hisp_total, black_total, asian_total, ind_ak_total, conservation, total_population) %>%
  mutate(min_total = (hisp_total + black_total + asian_total + ind_ak_total) / total_population)
mgr_lm <- lm(conservation ~ min_total, data=mgr)
summary(mgr_lm) 