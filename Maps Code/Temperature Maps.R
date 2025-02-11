# New df for air pollutant mapping
census_ap_df <- census_results4 

# Monthly avg daily temp
census_ap_1 <- census_ap_df %>%
  distinct(tractce10, tract_id, agg_date, avg_daily_temp) %>%
  group_by(tractce10, tract_id, agg_date) %>%
  summarize(
    avg_monthly_temp = mean(avg_daily_temp, na.rm=T)
  )
view(census_ap_1)

# Remove geometry to turn into df
census_ap_1$geometry = NULL

# Join with tracts to get polygon geometries
census_ap_1 <- inner_join(census_ap_1, tracts, by="tract_id")

# Convert to sf object for mapping
census_ap_1 <- st_as_sf(census_ap_1)

# filter for valid dates
census_ap_1 <- census_ap_1 %>%
  filter(agg_date >= "2018-02-01", 
         agg_date <= "2019-02-01")

# store valid dates
valid_dates <- unique(census_ap_1$agg_date)
# valid_dates <- c("2018-02-01", "2018-03-01")
valid_dates

date_strings <- c("February 2018", "March 2018", "April 2018",
                  "May 2018", "June 2018", "July 2018",
                  "August 2018", "September 2018", 
                  "October 2018", "November 2018", 
                  "December 2018", "January 2019", 
                  "February 2019")

cold_months <- c("February 2018", "March 2018", "April 2018",
                   "October 2018", "November 2018", 
                   "December 2018", "January 2019", 
                   "February 2019")
# Initialize an empty list to store results
results_list <- list()
i = 1

# Loop through each month
for (date in valid_dates) {
  
  month_map = census_ap_1 %>%
    subset(agg_date == date)
  
month_plot <- ggplot() + 
    geom_sf(
      data = month_map,
      aes(color = avg_monthly_temp, fill = avg_monthly_temp), 
    ) + 
    scale_fill_gradient(low=ifelse(date_strings[i] %in% cold_months, "#002a3a", "#ECA154"), high=ifelse(date_strings[i] %in% cold_months, "#3eb1c8", "#a9431e")) +
    scale_color_gradient(low=ifelse(date_strings[i] %in% cold_months, "#002a3a", "#ECA154"), high=ifelse(date_strings[i] %in% cold_months, "#3eb1c8", "#a9431e"), guide="none") +
    labs(title="Average Daily Temperature",
         subtitle=date_strings[i],
         fill="Temperature (Fº)",
         color="Temperature (Fº)") +
    theme(
      legend.position = "right", 
      legend.title.align=0.5,
      legend.title=element_text(size=8, family="GothamBold"),
      legend.text=element_text(size=8, family = "GothamBook"),
      legend.key.width = unit(0.5, "inches"),
      plot.title = element_text(size = 14, family = "GothamBold", hjust=0.5),
      axis.title.x = element_text(size = 14, family = "GothamBold", margin=margin(t=5, r=15, b=0, l=0)),
      axis.title.y = element_text(size = 14, family = "GothamBold"),
      axis.text.x = element_text(size = 10, family = "GothamBook"),
      axis.text.y = element_text(size = 10, family = "GothamBook"),
      plot.subtitle = element_text(size = 8, family = "GothamBook", hjust=0.5)
    ) 
  
  results_list[[length(results_list) + 1]] <- month_plot
  
  i = i + 1
}

results_list_len = length(results_list)

for (x in 1:results_list_len) {
  print(results_list[[x]])
}