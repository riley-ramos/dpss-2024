census_results5 <- census_results4 %>%
  filter(agg_date >= "2018-02-01",
         agg_date <= "2019-02-01") 

valid_dates <- unique(census_results5$agg_date)

# Initialize an empty list to store results
results_list <- list()

# Loop through each month
for (date in valid_dates) {
  # Subset data for the month
  month_data <- subset(census_results5, agg_date == date, select = c("agg_date", "tractce10", "avg_daily_no2", "conservation", "roofs_2012"))
  
  # Fit the model
  consv_gr_no2_lm <- lm(avg_daily_no2 ~ conservation + roofs_2012, data = month_data)
  
  # Get the summary
  consv_gr_no2_sum <- summary(consv_gr_no2_lm)
  
  # Extract coefficients
  month_coefficients <- consv_gr_no2_sum$coefficients
  month_coefficients_df <- as.data.frame(month_coefficients)
  month_coefficients_df$variable <- rownames(month_coefficients_df)
  rownames(month_coefficients_df) <- NULL
  month_coefficients_df <- month_coefficients_df[, c("variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
  colnames(month_coefficients_df) <- c("variable", "estimate", "std_error", "t_value", "p_value")
  month_coefficients_df <- month_coefficients_df %>%
    mutate(variable = ifelse(variable == "(Intercept)", "intercept", variable),
           agg_date = as.Date(date),
           type="consv_greenRoof_no2")
  
  # Append to the results list
  results_list[[length(results_list) + 1]] <- month_coefficients_df
}

# Combine all months into a single data frame
results_df <- do.call(rbind, results_list)

# Plot the results
ggplot(results_df, aes(x = agg_date, y = estimate, color = variable, group = variable)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std_error, ymax = estimate + 1.96 * std_error), color="grey", width = 0.2) +
  labs(title = "Monthly Variation in\nRegression Coefficients",
       x = "Month",
       y = "Regression Coefficient (Estimate)") +
  theme_minimal() + 
  theme(
    legend.position = "right", 
    legend.title.align=0.5,
    legend.title=element_text(size=8, family="GothamBold"),
    legend.text=element_text(size=8, family = "GothamBook"),
    legend.key.width = unit(0.5, "inches"),
    plot.title = element_text(size = 16, family = "GothamBold", hjust=0.5),
    axis.title.x = element_text(size = 12, family = "GothamBold", margin=margin(t=5, r=15, b=0, l=0)),
    axis.title.y = element_text(size = 12, family = "GothamBold"),
    axis.text.x = element_text(size = 8, angle = 90, hjust = 1, family = "GothamBook"),
    axis.text.y = element_text(size = 8, family = "GothamBook")
  ) 

# Plot the results (t-va)
ggplot(results_df, aes(x = agg_date, y = t_value, color = variable, group = variable)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std_error, ymax = estimate + 1.96 * std_error), color="grey", width = 0.2) +
  labs(title = "Monthly Variation in T-Value",
       x = "Month",
       y = "T-Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Export to xlsx
write.xlsx(results_df, file = "Excel Results/consv_greenRoof_no2.xlsx", sheetName = "Sheet1", row.names = FALSE)