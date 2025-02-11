census_results6 <- census_results5 %>% 
  distinct(tractce10, median_home_value, roofs_2012, conservation)

# # of roofs and conservation effect on home value
hv_gr_c_lm <- lm(median_home_value ~ roofs_2012 + conservation, data=census_results6)
summary(hv_gr_c_lm)

# median home value effect on no. of green roofs
hv_gr_c_lm2 <- lm(roofs_2012 ~ median_home_value, data=census_results6)
summary(hv_gr_c_lm2)

# median home value effect on conservation
hv_gr_c_lm3 <- lm(conservation ~ median_home_value, data=census_results6)
summary(hv_gr_c_lm3)