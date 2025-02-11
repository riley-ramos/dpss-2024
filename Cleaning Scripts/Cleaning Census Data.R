# Import libraries 
library(tidyverse)
library(dplyr)
library(sf) # for analyzing spatial data
library(tidycensus) # for analyzing census data
library(ggplot2)
library(xlsx)

setwd('/Users/rileyramos/Desktop/CapstoneDPSS')

# Read in census file
census <- read_csv('AOT MATERIALS/Data/Chicago_Census_2022_acs.csv')

# Get rid of first column
census <- census[-c(1)]

############ Cleaning Census Data ############
# View rows with na values
census_with_na <- census %>%
  filter(if_any(everything(), is.na))
# View rows with all 0 or NA values
census_all_zeroes <- census %>%
  select(-(2:6)) %>%
  filter(if_all(-all_of("tractce10"), ~ . == 0 | is.na(.))) # 3 tracts
# Remove those tracts
census2 <- census %>% 
  filter(!(tractce10 %in% c(980100, 381700, 980000)))
# View rows with na values
census_with_na <- census2 %>%
  filter(if_any(everything(), is.na))
# All na values are in median_home_value, which is not the focus of this analysis, so these rows can be kept 

# Check for negative values 
census_negatives <- census2 %>%
  select(-(2:6)) %>%
  filter(if_all(-all_of("tractce10"), ~ . < 0)) # 0

# Calculate total population
census2 %>%
  select(total_population) %>%
  sum() # 2685731

############ Merge census with green_spaces ############ 
# Read in green_spaces file
green_spaces <- read_csv('AOT MATERIALS/Data/Green_Spaces_CDP.csv')
# Get rid of first column
green_spaces <- green_spaces[-c(1)]

### CLEANING VERIFICATION ###
# Make sure data has no NAs 
green_spaces %>%
  filter(is.na(conservation) | is.na(roofs_2012)) %>%
  count() # 0
green_spaces %>%
  filter(is.na(lon) | is.na(lat)) %>%
  count() # 0
# Make sure data has no negative values
green_spaces %>%
  filter(conservation < 0 | roofs_2012 < 0) %>%
  count() # 0
# Make sure conservation is just 0 or 1
green_spaces %>%
  filter(conservation > 1) %>%
  count() # 0

### Merge with census ###
census_green_merged <- census2 %>% 
  left_join(green_spaces, by="tractce10")
# Check if any did not match 
census_green_merged %>%
  filter(is.na(conservation) & is.na(roofs_2012)) %>%
  count() # 0 (none)
# Drop 2nd lat and lon cols
census_green_merged <- census_green_merged %>%
  select(-(c("lon.y", "lat.y")))
# Remove .x suffix from column names
colnames(census_green_merged) <- str_replace(colnames(census_green_merged), "\\.x$", "")


# Count total ethnic minority
census_results <- census_green_merged %>%
  mutate(hisp_total = hisp_men + hisp_women, 
         asian_total = asian_men + asian_women,
         black_total = black_men + black_women,
         ind_ak_total = ind_ak_men + ind_ak_women,
         other_total = other_men + other_women,
         white_total = white_men + white_women + not_hisp_white_men+ not_hisp_white_women)

# plot ethnic minority by median_home_value
census_results %>% 
  ggplot(data=., aes(x=black_total, y=roofs_2012) )+
  geom_point()

ethnic_minority <- lm(median_home_value ~ white_total + hisp_total + black_total + ind_ak_total + asian_total, data=census_results)
summary(ethnic_minority)

# join census with krigged
census_results2 <- census_results %>%
  left_join(census_allEnv_merged_df, by="tractce10")

# Drop all columns ending with .y
census_results2 <- census_results2 %>%
  select(-matches("\\.y$"))

# Remove .x suffix from column names
colnames(census_results2) <- str_replace(colnames(census_results2), "\\.x$", "")

# Converg agg_date to date type
census_results2$agg_date <- my(census_results2$agg_date)

# Convert temp columns to Farenheit
census_results2 <- census_results2 %>%
  mutate(avg_daily_temp = avg_daily_temp * (9/5) + 32,
         avg_max_temp = avg_max_temp * (9/5) + 32, 
         avg_min_temp = avg_min_temp * (9/5) + 32)

# join census with allGas
census_results3 <- census_results2 %>%
  left_join(census_allGas_merged_df, by=c("tractce10", "agg_date"))

# Drop all columns ending with .y
census_results3 <- census_results3 %>%
  select(-matches("\\.y$"))

# Remove .x suffix from column names
colnames(census_results3) <- str_replace(colnames(census_results3), "\\.x$", "")

# merge census with all gas
# Converg agg_date to date type
census_allGas_merged_df <- census_allGas_merged
census_allGas_merged_df$geometry = NULL
census_allGas_merged_df$agg_date <- my(census_allGas_merged_df$agg_date)

census_results4 <- census_results3 %>%
  left_join(census_allGas_merged_df, by=c("tractce10", "agg_date")) %>%
  arrange(tractce10)
# Drop all columns ending with .y
census_results4 <- census_results4 %>%
  select(-matches("\\.y$"))
# Remove .x suffix from column names
colnames(census_results4) <- str_replace(colnames(census_results4), "\\.x$", "")
View(census_results4)

