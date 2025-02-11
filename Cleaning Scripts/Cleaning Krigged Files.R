library(tidyverse)
library(dplyr)
library(sf) # for analyzing spatial data
library(tidycensus) # for analyzing census data
library(ggplot2)

setwd('/Users/rileyramos/Desktop/CapstoneDPSS')

# Read in krigged files 
# Environmental
pressure <- read_csv('AOT MATERIALS/Data/AoT Krigged By Tract/krigged_pressure.csv')
temperature <- read_csv('AOT MATERIALS/Data/AoT Krigged By Tract/krigged_temperature.csv')
humidity <- read_csv('AOT MATERIALS/Data/AoT Krigged By Tract/krigged_humidity.csv')
# Air Pollution
carbonMonoxide <- read_csv('Additional AoT Data/AoT Krigged by Tract/krigged_carbon_monoxide.csv')
hydrogenSulfide <- read_csv('Additional AoT Data/AoT Krigged by Tract/krigged_hydrogen_sulfide.csv')
nitroDioxide <- read_csv('Additional AoT Data/AoT Krigged by Tract/krigged_nitrogen_dioxide.csv')

# Get rid of first column
pressure <- pressure[-c(1)]
temperature <- temperature[-c(1)]
humidity <- humidity[-c(1)]
carbonMonoxide <- carbonMonoxide[-c(1)]
hydrogenSulfide <- hydrogenSulfide[-c(1)]
nitroDioxide <- nitroDioxide[-c(1)]

# Rename cols for easy merging
names(pressure)[names(pressure) == "centroid_id"] = "tractce10"
names(temperature)[names(temperature) == "centroid_id"] = "tractce10"
names(humidity)[names(humidity) == "centroid_id"] = "tractce10"
names(carbonMonoxide)[names(carbonMonoxide) == "centroid_id"] = "tractce10"
names(hydrogenSulfide)[names(hydrogenSulfide) == "centroid_id"] = "tractce10"
names(nitroDioxide)[names(nitroDioxide) == "centroid_id"] = "tractce10"


################### Carbon Monoxide (CO) ################### 
# Get null values
co_nulls <- carbonMonoxide %>%
  filter(is.na(avg_co) & is.na(min_co) & is.na(max_co))

# Calculate dates and tracts of NA values 
co_null_dates_tracts <- co_nulls %>%
  distinct(tractce10, date)

# Remove NA values 
co_nullsRem <- carbonMonoxide %>%
  filter(!(is.na(avg_co) & is.na(min_co) & is.na(max_co)))

# Check for negative values
co_nullsRem %>%
  filter(avg_co < 0) %>%
  count()
co_nullsRem %>% 
  filter(min_co < 0) %>%
  count()
co_nullsRem %>% 
  filter(max_co < 0) %>%
  count()
co_nullsRem %>% 
  filter(min_co < 0 | max_co < 0 | avg_co < 0) %>%
  count()

### USE IF ONLY REMOVING ROWS WITH ALL NEG VALUES ###
# Replace negative values with NA
co_repNegs <- co_nullsRem %>%
  mutate(min_co = ifelse(min_co < 0, NA, min_co), 
         max_co = ifelse(max_co < 0, NA, max_co), 
         avg_co = ifelse(avg_co < 0, NA, avg_co))

# Remove entries that had all negative values 
co_nullsRem2 <- co_repNegs %>%
  filter(!(is.na(min_co) & is.na(max_co) & is.na(avg_co)))

### USE IF REMOVING ROWS WITH ANY NEG VALUES ###
# Remove entries with negative values 
# Any entry with a neg value is an error, compromises whole entry
# co_posOnly <- co_nullsRem %>%
#   filter(min_co > 0 & max_co > 0 & avg_co > 0) 

co_err <- co_nullsRem2 %>%
  filter(min_co > max_co) %>%
  mutate(errFlag = 1)
co_results <- co_nullsRem2 %>%
  left_join(co_err, by=c("tractce10", "date", "min_co", "avg_co", "max_co")) %>%
  filter(is.na(errFlag))
co_results$errFlag = NULL

# Remove entries where avg_co > max_co
co_err <- co_results %>%
  filter(avg_co > max_co) %>%
  mutate(errFlag = 1)
co_results <- co_results %>%
  left_join(co_err, by=c("tractce10", "date", "min_co", "avg_co", "max_co")) %>%
  filter(is.na(errFlag))
co_results$errFlag = NULL

# Remove entries where avg_co < min_co
co_err <- co_results %>%
  filter(avg_co < min_co) %>%
  mutate(errFlag = 1)
co_results <- co_results %>%
  left_join(co_err, by=c("tractce10", "date", "min_co", "avg_co", "max_co")) %>%
  filter(is.na(errFlag))
co_results$errFlag = NULL

# Count how many entries per month and year
co_monYr_dist <- co_results %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(month, year) %>%
  count() %>%
  arrange(year, month)

# Count how many per tract
co_perTract <- co_results %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(tractce10, month, year) %>%
  count() %>%
  arrange(year, month, tractce10)

# Aggregate by tract, month, and year 
options(scipen=999)
co_agg <- co_results %>% 
  mutate(month = month(date), year = year(date)) %>%
  group_by(tractce10, month, year) %>% 
  summarize(
    avg_min_co = mean(min_co, na.rm=T),
    avg_max_co = mean(max_co, na.rm=T),
    avg_daily_co = mean(avg_co, na.rm=T)
  ) %>% 
  arrange(year, month, tractce10)

# Merge month and year into 1 column 
co_agg <- co_agg %>%
  mutate(month = case_when(
    month == 1 ~ "January",
    month == 2 ~ "February",
    month == 3 ~ "March", 
    month == 4 ~ "April", 
    month == 5 ~ "May", 
    month == 6 ~ "June", 
    month == 7 ~ "July", 
    month == 8 ~ "August", 
    month == 9 ~ "September", 
    month == 10 ~ "October", 
    month == 11 ~ "November", 
    month == 12 ~ "December"
  )) %>%
  mutate(agg_date_co = paste(month, year, sep=" "))

# Remove month and year columns
co_agg <- co_agg[,!(names(co_agg) %in% c("month", "year"))]

# Merge with tracts_census_merged
census_co_merged <- tracts_census_merged %>%
  left_join(co_agg, by="tractce10")





################### Hydrogen Sulfide (H2S) ################### 
# Get null values
h2s_nulls <- hydrogenSulfide %>%
  filter(is.na(avg_h2s) & is.na(min_h2s) & is.na(max_h2s))

# Calculate dates and tracts of NA values 
h2s_null_dates_tracts <- h2s_nulls %>%
  distinct(tractce10, date)

# Remove NA values 
h2s_nullsRem <- hydrogenSulfide %>%
  filter(!(is.na(avg_h2s) & is.na(min_h2s) & is.na(max_h2s)))

# Check for negative values
h2s_nullsRem %>%
  filter(avg_h2s < 0) %>%
  count()
h2s_nullsRem %>% 
  filter(min_h2s < 0) %>%
  count()
h2s_nullsRem %>% 
  filter(max_h2s < 0) %>%
  count()
h2s_nullsRem %>% 
  filter(min_h2s < 0 | max_h2s < 0 | avg_h2s < 0) %>%
  count()

### USE IF ONLY REMOVING ROWS WITH ALL NEG VALUES ###
# Replace negative values with NA
h2s_repNegs <- h2s_nullsRem %>%
  mutate(min_h2s = ifelse(min_h2s < 0, NA, min_h2s), 
         max_h2s = ifelse(max_h2s < 0, NA, max_h2s), 
         avg_h2s = ifelse(avg_h2s < 0, NA, avg_h2s))

# Remove entries that had all negative values 
h2s_nullsRem2 <- h2s_repNegs %>%
  filter(!(is.na(min_h2s) & is.na(max_h2s) & is.na(avg_h2s)))

### USE IF REMOVING ROWS WITH ANY NEG VALUES ###
# Remove entries with negative values 
# Any entry with a neg value is an error, compromises whole entry
# h2s_posOnly <- h2s_nullsRem %>%
#   filter(min_h2s > 0 & max_h2s> 0 & avg_h2s > 0) 

# Remove entries where min_h2s > max_h2s
h2s_err <- h2s_nullsRem2 %>%
  filter(min_h2s > max_h2s) %>%
  mutate(errFlag = 1)
h2s_results <- h2s_nullsRem2 %>%
  left_join(h2s_err, by=c("tractce10", "date", "min_h2s", "avg_h2s", "max_h2s")) %>%
  filter(is.na(errFlag))
h2s_results$errFlag = NULL

# Remove entries where avg_h2s > max_h2s
h2s_err <- h2s_results %>%
  filter(avg_h2s > max_h2s) %>%
  mutate(errFlag = 1)
h2s_results <- h2s_results %>%
  left_join(h2s_err, by=c("tractce10", "date", "min_h2s", "avg_h2s", "max_h2s")) %>%
  filter(is.na(errFlag))
h2s_results$errFlag = NULL

# Remove entries where avg_h2s < min_h2s
h2s_err <- h2s_results %>%
  filter(avg_h2s < min_h2s) %>%
  mutate(errFlag = 1)
h2s_results <- h2s_results %>%
  left_join(h2s_err, by=c("tractce10", "date", "min_h2s", "avg_h2s", "max_h2s")) %>%
  filter(is.na(errFlag))
h2s_results$errFlag = NULL

# Count how many entries per month and year
h2s_monYr_dist <- h2s_results %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(month, year) %>%
  count() %>%
  arrange(year, month)

# Count how many per tract
h2s_perTract <- h2s_results %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(tractce10, month, year) %>%
  count() %>%
  arrange(year, month, tractce10)

# Aggregate by tract, month, and year 
options(scipen=999)
h2s_agg <- h2s_results %>% 
  mutate(month = month(date), year = year(date)) %>%
  group_by(tractce10, month, year) %>% 
  summarize(
    avg_min_h2s = mean(min_h2s, na.rm=T),
    avg_max_h2s = mean(max_h2s, na.rm=T),
    avg_daily_h2s = mean(avg_h2s, na.rm=T)
  ) %>% 
  arrange(year, month, tractce10)

# Merge month and year into 1 column 
h2s_agg <- h2s_agg %>%
  mutate(month = case_when(
    month == 1 ~ "January",
    month == 2 ~ "February",
    month == 3 ~ "March", 
    month == 4 ~ "April", 
    month == 5 ~ "May", 
    month == 6 ~ "June", 
    month == 7 ~ "July", 
    month == 8 ~ "August", 
    month == 9 ~ "September", 
    month == 10 ~ "October", 
    month == 11 ~ "November", 
    month == 12 ~ "December"
  )) %>%
  mutate(agg_date_h2s = paste(month, year, sep=" "))

# Remove month and year columns
h2s_agg <- h2s_agg[,!(names(h2s_agg) %in% c("month", "year"))]

# Merge with tracts_census_merged
census_h2s_merged <- tracts_census_merged %>%
  left_join(h2s_agg, by="tractce10")




################### Nitrogen Dioxide (NO2) ################### 
# Convert date to Date
nitroDioxide <- nitroDioxide %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

# Get null values
no2_nulls <- nitroDioxide %>%
  filter(is.na(avg_no2) & is.na(min_no2) & is.na(max_no2)) # 281151

# Calculate dates and tracts of NA values 
no2_null_dates_tracts <- no2_nulls %>%
  distinct(tractce10, date)

# Remove NA values 
no2_nullsRem <- nitroDioxide %>%
  filter(!(is.na(avg_no2) & is.na(min_no2) & is.na(max_no2)))

# Check for negative values
no2_nullsRem %>%
  filter(avg_no2 < 0) %>%
  count()
no2_nullsRem %>% 
  filter(min_no2 < 0) %>%
  count()
no2_nullsRem %>% 
  filter(max_no2 < 0) %>%
  count()
no2_nullsRem %>% 
  filter(min_no2 < 0 | max_no2 < 0 | avg_no2 < 0) %>%
  count()

### USE IF ONLY REMOVING ROWS WITH ALL NEG VALUES ###
# Replace negative values with NA
no2_repNegs <- no2_nullsRem %>%
  mutate(min_no2 = ifelse(min_no2 < 0, NA, min_no2), 
         max_no2 = ifelse(max_no2 < 0, NA, max_no2), 
         avg_no2 = ifelse(avg_no2 < 0, NA, avg_no2))

# Remove entries that had all negative values 
no2_nullsRem2 <- no2_repNegs %>%
  filter(!(is.na(min_no2) & is.na(max_no2) & is.na(avg_no2)))
 
### USE IF REMOVING ROWS WITH ANY NEG VALUES ###
# Remove entries with negative values 
# Any entry with a neg value is an error, compromises whole entry
# no2_posOnly <- no2_nullsRem %>%
#  filter(min_no2 > 0 & max_no2 > 0 & avg_no2 > 0) 

# Remove entries where min_no2 > max_no2
no2_err <- no2_nullsRem2 %>%
  filter(min_no2 > max_no2) %>%
  mutate(errFlag = 1)
no2_results <- no2_nullsRem2 %>%
  left_join(no2_err, by=c("tractce10", "date", "min_no2", "avg_no2", "max_no2")) %>%
  filter(is.na(errFlag))
no2_results$errFlag = NULL

# Remove entries where avg_no2 > max_no2
no2_err <- no2_results %>%
  filter(avg_no2 > max_no2) %>%
  mutate(errFlag = 1)
no2_results <- no2_results %>%
  left_join(no2_err, by=c("tractce10", "date", "min_no2", "avg_no2", "max_no2")) %>%
  filter(is.na(errFlag))
no2_results$errFlag = NULL

# Remove entries where avg_no2 < min_no2
no2_err <- no2_results %>%
  filter(avg_no2 < min_no2) %>%
  mutate(errFlag = 1)
no2_results <- no2_results %>%
  left_join(no2_err, by=c("tractce10", "date", "min_no2", "avg_no2", "max_no2")) %>%
  filter(is.na(errFlag))
no2_results$errFlag = NULL

# Count how many entries per month and year
no2_monYr_dist <- no2_results %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(month, year) %>%
  count() %>%
  arrange(year, month)

# Count how many per tract
no2_perTract <- no2_results %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(tractce10, month, year) %>%
  count() %>%
  arrange(year, month, tractce10)

# Aggregate by tract, month, and year 
options(scipen=999)
no2_agg <- no2_results %>% 
  mutate(month = month(date), year = year(date)) %>%
  group_by(tractce10, month, year) %>% 
  summarize(
    avg_min_no2 = mean(min_no2, na.rm=T),
    avg_max_no2 = mean(max_no2, na.rm=T),
    avg_daily_no2 = mean(avg_no2, na.rm=T)
  ) %>% 
  arrange(year, month, tractce10)

# Merge month and year into 1 column 
no2_agg <- no2_agg %>%
  mutate(month = case_when(
    month == 1 ~ "January",
    month == 2 ~ "February",
    month == 3 ~ "March", 
    month == 4 ~ "April", 
    month == 5 ~ "May", 
    month == 6 ~ "June", 
    month == 7 ~ "July", 
    month == 8 ~ "August", 
    month == 9 ~ "September", 
    month == 10 ~ "October", 
    month == 11 ~ "November", 
    month == 12 ~ "December"
  )) %>%
  mutate(agg_date_no2 = paste(month, year, sep=" "))

# Remove month and year columns
no2_agg <- no2_agg[,!(names(no2_agg) %in% c("month", "year"))]

# Merge with tracts_census_merged
census_no2_merged <- tracts_census_merged %>%
  left_join(no2_agg, by="tractce10")



################### Merge all census gas aggregates ################### 
census_allGas_merged <- census_co_merged %>%
  left_join(h2s_agg, by=c("tractce10", "agg_date_co"="agg_date_h2s")) %>%
  left_join(no2_agg, by=c("tractce10", "agg_date_co"="agg_date_no2"))

# Rename agg_date col
names(census_allGas_merged)[names(census_allGas_merged) == "agg_date_co"] = "agg_date"

# Sample plot - Avg daily CO in Mar 2018
test <- census_allGas_merged %>%
  filter(agg_date == "March 2018") %>% 
  distinct(tractce10, geometry, avg_daily_co)
test <- st_as_sf(test)
test %>%
  ggplot() +
  geom_sf(
    data = test,
    aes(color = avg_daily_co, fill = avg_daily_co)
  )


################### Humidity ################### 
# Get null values
humid_nulls <- humidity %>%
  filter(is.na(avg_humid) & is.na(min_humid) & is.na(max_humid)) 

# Calculate dates and tracts of NA values 
humid_null_dates_tracts <- humid_nulls %>%
  distinct(tractce10, date)

# Remove NA values 
humid_nullsRem <- humidity %>%
  filter(!(is.na(avg_humid) & is.na(min_humid) & is.na(max_humid)))

# Check for negative values
humid_nullsRem %>%
  filter(avg_humid < 0) %>%
  count()
humid_nullsRem %>% 
  filter(min_humid < 0) %>%
  count()
humid_nullsRem %>% 
  filter(max_humid < 0) %>%
  count()
humid_nullsRem %>% 
  filter(min_humid < 0 | max_humid < 0 | avg_humid < 0) %>%
  count()

### USE IF ONLY REMOVING ROWS WITH ALL NEG VALUES ###
# Replace negative values with NA
humid_repNegs <- humid_nullsRem %>%
  mutate(min_humid = ifelse(min_humid < 0, NA, min_humid), 
         max_humid = ifelse(max_humid < 0, NA, max_humid), 
         avg_humid = ifelse(avg_humid < 0, NA, avg_humid))

# Remove entries that had all negative values 
humid_nullsRem2 <- humid_repNegs %>%
  filter(!(is.na(min_humid) & is.na(max_humid) & is.na(avg_humid)))

# Check if any entries have vals > 100
humid_nullsRem2 %>%
  filter(avg_humid > 100) %>%
  count()
humid_nullsRem2 %>% 
  filter(min_humid > 100) %>%
  count()
humid_nullsRem2 %>% 
  filter(max_humid > 100) %>%
  count()
humid_nullsRem2 %>% 
  filter(min_humid > 100 | max_humid > 100 | avg_humid > 100) %>%
  count()

# Remove entries where min_humid > max_humid
humid_results <- humid_nullsRem2 %>%
  filter(!(min_humid > max_humid))

# Remove entries where avg_humid > max_humid
humid_results <- humid_results %>%
  filter(!(avg_humid > max_humid))

# Remove entries where avg_humid < min_humid
humid_results <- humid_results %>%
  filter(!(avg_humid < min_humid))

# Upon looking at the data, all the max_humid values on 2/9/2020 were significantly greater than 100, even though the max possible RH value is slightly > 100
max(humid_results$max_humid) # returns 16404323
test <- humid_results %>%
  filter(date == "2020-02-09")
min(test$max_humid) # returns 5849.963

# Remove entries from 2/9/2020
humid_results <- humid_results %>%
  filter(!(date == "2020-02-09"))

# Replace RH values > 101 with NA, since > 100 is impossible in natural conditions
humid_results <- humid_results %>% 
  mutate(max_humid = ifelse(max_humid > 101, NA, max_humid)) %>%
  arrange(-max_humid)
max(humid_results$max_humid, na.rm=T) # now 100.9922, which is reasonable

# Count how many entries per month and year
humid_monYr_dist <- humid_results %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(month, year) %>%
  count() %>%
  arrange(year, month)

# Count how many per tract
humid_perTract <- humid_results %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(tractce10, month, year) %>%
  count() %>%
  arrange(year, month, tractce10)

# Aggregate by tract, month, and year 
options(scipen=999)
humid_agg <- humid_results %>% 
  mutate(month = month(date), year = year(date)) %>%
  group_by(tractce10, month, year) %>% 
  summarize(
    avg_min_humid = mean(min_humid, na.rm=T),
    avg_max_humid = mean(max_humid, na.rm=T),
    avg_daily_humid = mean(avg_humid, na.rm=T)
  ) %>% 
  arrange(year, month, tractce10)

# Merge month and year into 1 column 
humid_agg <- humid_agg %>%
  mutate(month = case_when(
    month == 1 ~ "January",
    month == 2 ~ "February",
    month == 3 ~ "March", 
    month == 4 ~ "April", 
    month == 5 ~ "May", 
    month == 6 ~ "June", 
    month == 7 ~ "July", 
    month == 8 ~ "August", 
    month == 9 ~ "September", 
    month == 10 ~ "October", 
    month == 11 ~ "November", 
    month == 12 ~ "December"
  )) %>%
  mutate(agg_date_humid = paste(month, year, sep=" "))

# Remove month and year columns
humid_agg <- humid_agg[,!(names(humid_agg) %in% c("month", "year"))]

# Merge with tracts_census_merged
census_humid_merged <- tracts_census_merged %>%
  left_join(humid_agg, by="tractce10")



################### Temperature ################### 
# Get null values
temp_nulls <- temperature %>%
  filter(is.na(avg_temp) & is.na(min_temp) & is.na(max_temp)) 
# No Nulls!

# Remove entries where min_temp > max_temp
temp_err <- temperature %>%
  filter(min_temp > max_temp)
temp_results <- temperature %>%
  filter(!(min_temp > max_temp))

# Remove entries where avg_temp > max_temp
temp_err <- temp_results %>%
  filter(avg_temp > max_temp)
temp_results <- temp_results %>%
  filter(!(avg_temp > max_temp))

# Remove entries where avg_temp < min_temp
temp_err <- temp_results %>%
  filter(avg_temp < min_temp)
temp_results <- temp_results %>%
  filter(!(avg_temp < min_temp))

min(temp_results$min_temp) # returns -34.21239, which is within range
max(temp_results$max_temp) # returns 1066152633339379, somethings wrong!

# See max max_temp values
test <- temp_results %>%
  arrange(-max_temp)

# Upon looking at the data, all the max_temp values on 3/8/2018 were significantly greater than 125 (even neg), even though the max possible temp value is 125
test <- temperature %>%
  filter(date == "2018-03-08")
min(test$max_temp) # returns -1000941748264673

# Remove entries from 3/8/2018
temp_results <- temp_results %>%
  filter(!(date == "2018-03-08"))

max(temp_results$max_temp, na.rm=T) # returns 42.91683, which is within range

# Count how many entries per month and year
temp_monYr_dist <- temp_results %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(month, year) %>%
  count() %>%
  arrange(year, month)

# Count how many per tract
temp_perTract <- temp_results %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(tractce10, month, year) %>%
  count() %>%
  arrange(year, month, tractce10)

# Aggregate by tract, month, and year 
options(scipen=999)
temp_agg <- temp_results %>% 
  mutate(month = month(date), year = year(date)) %>%
  group_by(tractce10, month, year) %>% 
  summarize(
    avg_min_temp = mean(min_temp, na.rm=T),
    avg_max_temp = mean(max_temp, na.rm=T),
    avg_daily_temp = mean(avg_temp, na.rm=T)
  ) %>% 
  arrange(year, month, tractce10)

# Merge month and year into 1 column 
temp_agg <- temp_agg %>%
  mutate(month = case_when(
    month == 1 ~ "January",
    month == 2 ~ "February",
    month == 3 ~ "March", 
    month == 4 ~ "April", 
    month == 5 ~ "May", 
    month == 6 ~ "June", 
    month == 7 ~ "July", 
    month == 8 ~ "August", 
    month == 9 ~ "September", 
    month == 10 ~ "October", 
    month == 11 ~ "November", 
    month == 12 ~ "December"
  )) %>%
  mutate(agg_date_temp = paste(month, year, sep=" "))

# Remove month and year columns
temp_agg <- temp_agg[,!(names(temp_agg) %in% c("month", "year"))]

# Merge with tracts_census_merged
census_temp_merged <- tracts_census_merged %>%
  left_join(temp_agg, by="tractce10")



################### Pressure ################### 
# Get null values
press_nulls <- pressure %>%
  filter(is.na(avg_press) | is.na(min_press) | is.na(max_press)) 
# No Nulls!

# Remove entries where min_press > max_press
press_err <- pressure %>%
  filter(min_press > max_press)
press_results <- pressure %>%
  filter(!(min_press > max_press))

# Remove entries where avg_press > max_press
press_err <- press_results %>%
  filter(avg_press > max_press)
press_results <- press_results %>%
  filter(!(avg_press > max_press))

# Remove entries where avg_press < min_press
press_err <- press_results %>%
  filter(avg_press < min_press)
press_results <- press_results %>%
  filter(!(avg_press < min_press))

min(press_results$min_press) # returns 944.8923, which is within range
max(press_results$max_press) # returns 1058.212,which is within range

# Count how many entries per month and year
press_monYr_dist <- press_results %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(month, year) %>%
  count() %>%
  arrange(year, month)

# Count how many per tract
press_perTract <- press_results %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(tractce10, month, year) %>%
  count() %>%
  arrange(year, month, tractce10)

# Aggregate by tract, month, and year 
options(scipen=999)
press_agg <- press_results %>% 
  mutate(month = month(date), year = year(date)) %>%
  group_by(tractce10, month, year) %>% 
  summarize(
    avg_min_press = mean(min_press, na.rm=T),
    avg_max_press = mean(max_press, na.rm=T),
    avg_daily_press = mean(avg_press, na.rm=T)
  ) %>% 
  arrange(year, month, tractce10)

# Merge month and year into 1 column 
press_agg <- press_agg %>%
  mutate(month = case_when(
    month == 1 ~ "January",
    month == 2 ~ "February",
    month == 3 ~ "March", 
    month == 4 ~ "April", 
    month == 5 ~ "May", 
    month == 6 ~ "June", 
    month == 7 ~ "July", 
    month == 8 ~ "August", 
    month == 9 ~ "September", 
    month == 10 ~ "October", 
    month == 11 ~ "November", 
    month == 12 ~ "December"
  )) %>%
  mutate(agg_date_press = paste(month, year, sep=" "))

# Remove month and year columns
press_agg <- press_agg[,!(names(press_agg) %in% c("month", "year"))]

# Merge with tracts_census_merged
census_press_merged <- tracts_census_merged %>%
  left_join(press_agg, by="tractce10")



################### Merge all environment aggregates ################### 
census_allEnv_merged <- census_temp_merged %>%
  left_join(humid_agg, by=c("tractce10", "agg_date_temp"="agg_date_humid")) %>%
  left_join(press_agg, by=c("tractce10", "agg_date_temp"="agg_date_press"))

# Rename agg_date col
names(census_allEnv_merged)[names(census_allEnv_merged) == "agg_date_temp"] = "agg_date"

# Sample plot - Avg daily temp in July 2018
# Convert to Farenheit: (0°C × 9/5) + 32
test <- census_allEnv_merged %>%
  filter(agg_date == "July 2018") %>% 
  distinct(tractce10, geometry, avg_daily_temp) %>% 
  mutate(avg_daily_temp_f = avg_daily_temp * (9/5) + 32)
test <- st_as_sf(test)
test %>%
  ggplot() +
  geom_sf(
    data = test,
    aes(fill = avg_daily_temp_f), 
    color = "black"
  )

# Drop geometry column so that we can merge
census_allEnv_merged_df <- census_allEnv_merged
census_allGas_merged_df <- census_allGas_merged
census_allEnv_merged_df$geometry = NULL
census_allGas_merged_df$geometry = NULL

# Merge census_allGas and allEnv together 
allCensus_noGeom <- census_allEnv_merged_df %>%
  left_join(census_allGas_merged_df, by=c("tractce10", "agg_date"))

# Drop all columns ending with .y
allCensus_noGeom2 <- allCensus_noGeom %>%
  select(-matches("\\.y$"))

# Remove .x suffix from column names
colnames(allCensus_noGeom2) <- str_replace(colnames(allCensus_noGeom2), "\\.x$", "")

# Add geometries back
allCensus_withGeom <- inner_join(allCensus_noGeom2, tracts, by="tract_id")








