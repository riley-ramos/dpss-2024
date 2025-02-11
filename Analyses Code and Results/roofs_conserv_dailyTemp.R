census_results5 <- census_results4 %>%
  filter(agg_date >= "2018-02-01",
         agg_date <= "2019-02-01") 

valid_dates <- unique(census_results5$agg_date)

# Initialize an empty list to store results
results_list <- list()

# Loop through each month
for (date in valid_dates) {
  # Subset data for the month
  month_data <- subset(census_results5, agg_date == date, select = c("tractce10", "roofs_2012", "agg_date", "conservation", "avg_daily_temp"))
  
  # Fit the model
  roofs_conserv_dt_lm <- lm(avg_daily_temp ~ roofs_2012 + conservation, data = month_data)
  
  # Get the summary
  roofs_conserv_dt_sum <- summary(roofs_conserv_dt_lm)
  
  # Extract coefficients
  month_coefficients <- roofs_conserv_dt_sum$coefficients
  month_coefficients_df <- as.data.frame(month_coefficients)
  month_coefficients_df$variable <- rownames(month_coefficients_df)
  rownames(month_coefficients_df) <- NULL
  month_coefficients_df <- month_coefficients_df[, c("variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
  colnames(month_coefficients_df) <- c("variable", "estimate", "std_error", "t_value", "p_value")
  month_coefficients_df <- month_coefficients_df %>%
    mutate(variable = ifelse(variable == "(Intercept)", "intercept", variable),
           agg_date = as.Date(date),
           type="roofs_conserv_dailyTemp")
  
  # Append to the results list
  results_list[[length(results_list) + 1]] <- month_coefficients_df
}

# Combine all months into a single data frame
results_df <- do.call(rbind, results_list)

# Export to xlsx
write.xlsx(results_df, file = "Excel Results/roofs_conserv_dailyTemp.xlsx", sheetName = "Sheet1", row.names = FALSE)

# Plot the results
results_df2 <- results_df %>%
  filter(variable != "intercept")

ggplot(results_df2, aes(x = agg_date, y = estimate, color = variable, group = variable)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std_error, ymax = estimate + 1.96 * std_error), color="grey", width = 0.2) +
  labs(title = "Monthly Variation in Regression Coefficients",
       x = "Month",
       y = "Regression Coefficient (Estimate)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot the results (t-val)
ggplot(results_df2, aes(x = agg_date, y = t_value, color = variable, group = variable)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std_error, ymax = estimate + 1.96 * std_error), color="grey", width = 0.2) +
  labs(title = "Monthly Variation in T-Value",
       x = "Month",
       y = "T-Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Predict if roofs_2012 is correlated with each var
# avg_daily_temp
# gr_daily_temp <- census_results3 %>%
#   distinct(tractce10, roofs_2012, agg_date, avg_daily_temp)
# View(gr_daily_temp)
# 
# # mar18
# gr_daily_temp_mar18 <- gr_daily_temp %>%
#   filter(agg_date == "2018-03-01")
# gr_daily_temp_lm <- lm(avg_daily_temp ~ roofs_2012, data=gr_daily_temp_mar18)
# summary(gr_daily_temp_lm) # Estimate: -0.24526, t-statistic: -4.475
# 
# # apr18
# gr_daily_temp_apr18 <- gr_daily_temp %>%
#   filter(agg_date == "2018-04-01") 
# gr_daily_temp_apr18_lm <- lm(avg_daily_temp ~ roofs_2012, data=gr_daily_temp_apr18)
# summary(gr_daily_temp_2018_s2_lm) # Estimate: -0.012249, t-statistic: -3.601
# 
# # may18
# gr_daily_temp_may18 <- gr_daily_temp %>%
#   filter(agg_date == "2018-05-01") 
# gr_daily_temp_may18_lm <- lm(avg_daily_temp ~ roofs_2012, data=gr_daily_temp_may18)
# summary(gr_daily_temp_may18_lm) # Estimate: -0.012249, t-statistic: -3.601
# 
# # jun18
# gr_daily_temp_jun18 <- gr_daily_temp %>%
#   filter(agg_date == "2018-06-01") 
# gr_daily_temp_jun18_lm <- lm(avg_daily_temp ~ roofs_2012, data=gr_daily_temp_jun18)
# summary(gr_daily_temp_jun18_lm) 
# summary(gr_daily_temp_jun18_lm) # Estimate: 0.0006943, t-statistic: 1.536
# 
# # jul18
# gr_daily_temp_jul18 <- gr_daily_temp %>%
#   filter(agg_date == "2018-07-01") 
# gr_daily_temp_jul18_lm <- lm(avg_daily_temp ~ roofs_2012, data=gr_daily_temp_jul18)
# summary(gr_daily_temp_jul18_lm) # Estimate: -0.011347, t-statistic: -2.657
# 
# # aug18
# gr_daily_temp_aug18 <- gr_daily_temp %>%
#   filter(agg_date == "2018-08-01") 
# gr_daily_temp_aug18_lm <- lm(avg_daily_temp ~ roofs_2012, data=gr_daily_temp_aug18)
# summary(gr_daily_temp_aug18_lm) # Estimate: 0.006397, t-statistic: 1.414
# 
# # sep18
# gr_daily_temp_sep18 <- gr_daily_temp %>%
#   filter(agg_date == "2018-09-01") 
# gr_daily_temp_sep18_lm <- lm(avg_daily_temp ~ roofs_2012, data=gr_daily_temp_sep18)
# summary(gr_daily_temp_sep18_lm) # Estimate: 0.008831, t-statistic: 3.583
# 
# # oct18
# gr_daily_temp_oct18 <- gr_daily_temp %>%
#   filter(agg_date == "2018-10-01") 
# gr_daily_temp_oct18_lm <- lm(avg_daily_temp ~ roofs_2012, data=gr_daily_temp_oct18)
# summary(gr_daily_temp_oct18_lm) # Estimate: 0.014237, t-statistic: 3.944
# 
# # nov18
# gr_daily_temp_nov18 <- gr_daily_temp %>%
#   filter(agg_date == "2018-11-01") 
# gr_daily_temp_nov18_lm <- lm(avg_daily_temp ~ roofs_2012, data=gr_daily_temp_nov18)
# summary(gr_daily_temp_nov18_lm) # Estimate: 0.0030089, t-statistic: 3.655
# 
# # dec18
# gr_daily_temp_dec18 <- gr_daily_temp %>%
#   filter(agg_date == "2018-12-01") 
# gr_daily_temp_dec18_lm <- lm(avg_daily_temp ~ roofs_2012, data=gr_daily_temp_dec18)
# summary(gr_daily_temp_dec18_lm) # Estimate: 0.014365, t-statistic: 4.333
# 
# # jan19
# gr_daily_temp_jan19 <- gr_daily_temp %>%
#   filter(agg_date == "2019-01-01") 
# gr_daily_temp_jan19_lm <- lm(avg_daily_temp ~ roofs_2012, data=gr_daily_temp_jan19)
# summary(gr_daily_temp_jan19_lm) # Estimate: 0.02183, t-statistic: 0.379
# 
# # feb19
# gr_daily_temp_feb19 <- gr_daily_temp %>%
#   filter(agg_date == "2019-02-01") 
# gr_daily_temp_feb19_lm <- lm(avg_daily_temp ~ roofs_2012, data=gr_daily_temp_feb19)
# summary(gr_daily_temp_feb19_lm) # Estimate: 0.006854, t-statistic: 3.979