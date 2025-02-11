# black green roofs
bgr <- census_results3 %>%
  distinct(tractce10, black_total, roofs_2012, total_population) %>%
  mutate(black_perc = black_total / total_population)
View(bgr)
bgr_lm <- lm(roofs_2012 ~ black_perc, data=bgr)
summary(bgr_lm) # Estimate: -0.15791, t-value: -1.44

# hisp green roofs
hgr <- census_results3 %>%
  distinct(tractce10, hisp_total, roofs_2012, total_population) %>%
  mutate(hisp_perc = hisp_total / total_population)
View(hgr)
hgr_lm <- lm(roofs_2012 ~ hisp_perc, data=hgr)
summary(hgr_lm) # Estimate: -0.56687, t-value: -3.804

# asian green roof
agr <- census_results3 %>%
  distinct(tractce10, asian_total, roofs_2012, total_population) %>%
  mutate(asian_perc = asian_total / total_population)
View(agr)
agr_lm <- lm(roofs_2012 ~ asian_perc, data=agr)
summary(agr_lm) # Estimate: 1.87762, t-value: 4.444

# indian green roof
igr <- census_results3 %>%
  distinct(tractce10, ind_ak_total, roofs_2012, total_population) %>%
  mutate(ind_ak_perc = ind_ak_total / total_population)
View(agr)
igr_lm <- lm(roofs_2012 ~ ind_ak_perc, data=igr)
summary(igr_lm) # Estimate: -6.03959, t-value: -2.073

# white green roof
wgr <- census_results3 %>%
  distinct(tractce10, white_total, roofs_2012, total_population) %>%
  mutate(white_perc = white_total / total_population)
View(wgr)
wgr_lm <- lm(roofs_2012 ~ white_perc, data=wgr)
summary(wgr_lm) # Estimate: 0.24788, t-value: 3.392

# total minority vs green roofs
mgr <- census_results3 %>%
  distinct(tractce10, hisp_total, black_total, asian_total, ind_ak_total, roofs_2012, total_population) %>%
  mutate(min_total = (hisp_total + black_total + asian_total + ind_ak_total) / total_population)
View(mgr)
mgr %>% 
  filter(min_total > 1) %>%
  count() # 45
mgr_lm <- lm(roofs_2012 ~ min_total, data=mgr)
summary(mgr_lm) # Estimate: -0.5504, t-value: -4.009