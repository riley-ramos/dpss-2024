library(tidyverse)
library(dplyr)
library(sf) # for analyzing spatial data
library(tidycensus) # for analyzing census data
library(ggplot2)

setwd('/Users/rileyramos/Desktop/CapstoneDPSS')

# Unzipping the Census Tracts shapefiles
unzip("AOT MATERIALS/Spatial Data/Boundaries - Census Tracts - 2010.zip")

# Reading in the shapefile. Note the geometry column, this is what stores the coordinates that make up a shape
tracts <- st_read("AOT MATERIALS/Spatial Data/geo_export_08ee1dde-f77f-43d7-8624-09569366eec5.shp")
glimpse(tracts)

# Plot tracts 
plot(tracts)

# Convert census data to lon lat format 
census <- st_as_sf(
  census,
  coords = c("lon", "lat"),
  crs = 4326)

# Sample plot from census data
census_sample <- census %>%
  sample_frac(0.1)
plot(census_sample["health_insured_all"])

# Reload shape file to get rid of added info
tracts <- st_read("AOT MATERIALS/Spatial Data/geo_export_08ee1dde-f77f-43d7-8624-09569366eec5.shp")

# Set tracts shape file to same CRS 
tracts <- st_transform(tracts, 4326)

# Plot to make sure census tracts are showing up correctly 
ggplot() +
  geom_sf(data=tracts)

# Join with census demographics data 
tracts_demographics_merged <- st_join(
  census,  # points
  tracts,      # polygons
  join = st_within  # which polygon is the point WITHIN?
)

# Plot to make sure census tracts are showing up correctly 
ggplot() +
  geom_sf(data=tracts_demographics_merged)

tracts_demographics_merged <- na.exclude(tracts_demographics_merged)

tracts_demographics_agg <- tracts_demographics_merged %>%
  group_by(GEOID) %>%
  summarize(
    mean = mean(health_insured_all),
    count = n()
  )

tracts_demographics_agg <- 
  left_join(tracts_demographics_agg, tracts, )

plot(tracts_demographics_agg["mean"])

ggplot() +
  geom_sf(
    data = tracts_demographics_agg,
    aes(color = mean, fill = mean)
  ) 


# Calculate perc_white per tract
census2 <- tracts_census_merged %>%
  select(tractce10, total_population, white_men, white_women) %>%
  mutate(white = white_men+white_women,
         perc_white = white/total_population) 
census2$geometry = NULL

# See if there is incorect total_population values
census2 %>%
  filter(white > total_population) %>%
  count()

# Count how many census tracts have < 50% pop white (majority non-white)
census2 %>%
  filter(perc_white < .5) %>%
  count() # 464 census tracts (~59% of all tracts)


# Calculate total population
census %>%
  select(total_population) %>%
  sum() # 2685731




################ DATA ANALYSIS ################
# subset for valid range AH
AH_results <- AH_noGeom_envJoin %>%
  filter(agg_date %in% c("February 2018", "March 2018", 
                         "April 2018", "May 2018", 
                         "June 2018", "July 2018",
                         "August 2018", "September 2018",
                         "October 2018", "November 2018",
                         "December 2018", "January 2019", 
                         "February 2019"))

# subset for valid range non AH
nonAH_results <- non_AH_noGeom_envJoin %>%
  filter(agg_date %in% c("February 2018", "March 2018", 
                         "April 2018", "May 2018", 
                         "June 2018", "July 2018",
                         "August 2018", "September 2018",
                         "October 2018", "November 2018",
                         "December 2018", "January 2019", 
                         "February 2019"))
# Drop all columns ending with .y
nonAH_results <- nonAH_results %>%
  select(-matches("\\.y$"))
# Remove .x suffix from column names
colnames(nonAH_results) <- str_replace(colnames(nonAH_results), "\\.x$", "")

# Plot avg temp AH
test_AH <- AH_results %>%
  group_by(agg_date) %>%
  summarize(
    avg_monthly_daily_temp = mean(avg_daily_temp, na.rm=T)
  ) %>%
  select(agg_date, avg_monthly_daily_temp)
test_AH %>%
  ggplot(data=., aes(x=agg_date, y=avg_monthly_daily_temp, group=1)) +
  geom_line() + 
  geom_point() 

# Plot avg temp nonAH
test_nonAH <- nonAH_results %>%
  group_by(agg_date) %>%
  summarize(
    avg_monthly_daily_temp = mean(avg_daily_temp, na.rm=T)
  ) %>%
  select(agg_date, avg_monthly_daily_temp)
test_nonAH %>%
  ggplot(data=., aes(x=agg_date, y=avg_monthly_daily_temp, group=1)) +
  geom_line() + 
  geom_point() 


# Plot max temp AH
test_AH <- AH_results %>%
  group_by(agg_date) %>%
  summarize(
    avg_monthly_max_temp = mean(avg_max_temp, na.rm=T)
  ) %>%
  select(agg_date, avg_monthly_max_temp)
test_AH %>%
  ggplot(data=., aes(x=agg_date, y=avg_monthly_max_temp, group=1)) +
  geom_line() + 
  geom_point() 

# Plot max temp nonAH
test_nonAH <- nonAH_results %>%
  group_by(agg_date) %>%
  summarize(
    avg_monthly_max_temp = mean(avg_max_temp, na.rm=T)
  ) %>%
  select(agg_date, avg_monthly_max_humid)
test_nonAH %>%
  ggplot(data=., aes(x=agg_date, y=avg_monthly_max_humid, group=1)) +
  geom_line() + 
  geom_point() 



# Plot avg daily humid AH
test_AH <- AH_results %>%
  group_by(agg_date) %>%
  summarize(
    avg_monthly_daily_humid = mean(avg_daily_humid, na.rm=T)
  ) %>%
  select(agg_date, avg_monthly_daily_humid)
test_AH %>%
  ggplot(data=., aes(x=agg_date, y=avg_monthly_daily_humid, group=1)) +
  geom_line() + 
  geom_point() 

# Plot avg daily humid nonAH
test_nonAH <- nonAH_results %>%
  group_by(agg_date) %>%
  summarize(
    avg_monthly_daily_humid = mean(avg_daily_humid, na.rm=T)
  ) %>%
  select(agg_date, avg_monthly_daily_humid)
test_nonAH %>%
  ggplot(data=., aes(x=agg_date, y=avg_monthly_daily_humid, group=1)) +
  geom_line() + 
  geom_point()


# Plot avg daily no2 AH
test_AH <- AH_results %>%
  group_by(agg_date) %>%
  summarize(
    avg_monthly_daily_no2 = mean(avg_daily_no2, na.rm=T)
  ) %>%
  select(agg_date, avg_monthly_daily_no2)
test_AH %>%
  ggplot(data=., aes(x=agg_date, y=avg_monthly_daily_no2, group=1)) +
  geom_line() + 
  geom_point() 

# Plot avg daily no2 nonAH
test_nonAH <- nonAH_results %>%
  group_by(agg_date) %>%
  summarize(
    avg_monthly_daily_no2 = mean(avg_daily_no2, na.rm=T)
  ) %>%
  select(agg_date, avg_monthly_daily_no2)
test_nonAH %>%
  ggplot(data=., aes(x=agg_date, y=avg_monthly_daily_no2, group=1)) +
  geom_line() + 
  geom_point()


# Plot max daily no2 AH
test_AH <- AH_results %>%
  group_by(agg_date) %>%
  summarize(
    avg_monthly_max_no2 = mean(avg_max_no2, na.rm=T)
  ) %>%
  select(agg_date, avg_monthly_max_no2)
test_AH %>%
  ggplot(data=., aes(x=agg_date, y=avg_monthly_max_no2, group=1)) +
  geom_line() + 
  geom_point() 

# Plot max daily no2 nonAH
test_nonAH <- nonAH_results %>%
  group_by(agg_date) %>%
  summarize(
    avg_monthly_max_no2 = mean(avg_max_no2, na.rm=T)
  ) %>%
  select(agg_date, avg_monthly_max_no2)
test_nonAH %>%
  ggplot(data=., aes(x=agg_date, y=avg_monthly_max_no2, group=1)) +
  geom_line() + 
  geom_point()

test_join <- test_AH %>%
  inner_join(test_nonAH, by="agg_date")

test_reg <- t.test(avg_monthly_max_no2.x ~ avg_monthly_max_no2.y, data = test_join)

test_reg <- t.test(test_join$avg_monthly_max_no2.x, test_join$avg_monthly_max_no2.y)
test_reg

test_reg <- lm()
summary(test_join)

############  Repeat for Affordable Housing ############ 

# Read in affordable housing dataset 
affordable_housing <- read_csv('External Data/Affordable_Rental_Housing_Developments_20240718.csv')

####### Cleaning #######
# Check if there are any NA values in relevant columns 
sum(is.na(affordable_housing$Longitude)) # 0
sum(is.na(affordable_housing$`Latitude`)) # 0
sum(is.na(affordable_housing$`Property Type`)) # 0 
sum(is.na(affordable_housing$`Zip Code`)) # 0
sum(is.na(affordable_housing$Units)) # 1
# remove that 1
affordable_housing <- affordable_housing %>%
  filter(!(is.na(Units)))

# Assign correct property type spelling
correct_property_type <- c("Multfamily" = "Multifamily", "Mutifamily" = "Multifamily")

# Correct the misspelled values 
affordable_housing <- affordable_housing %>%
  mutate(`Property Type` = recode(`Property Type`, !!!correct_property_type))


####### Spatial Join #######
# Convert AH lat and lon
affordable_housing <- st_as_sf(
  affordable_housing,
  coords = c("Longitude", "Latitude"),
  crs = 4326)

# Perform spatial join
# Check which Census tract each AH area is in
# Join that tract's data to the record for each point
AH_merged <- st_join(
  affordable_housing,
  tracts,
  join = st_within,
  left = TRUE
)

# Remove point data
AH_merged$geometry = NULL

# Rejoin with tracts to get associated polygon geometries
AH_merged <- inner_join(AH_merged, tracts, by="tract_id")

# Convert back to sf object for plotting
AH_merged <- st_as_sf(AH_merged)

# Example Plot - # of units
ggplot() +
  geom_sf(
    data = AH_merged,
    aes(color = Units, fill = Units)
  )

# Example Plot - property type
ggplot() +
  geom_sf(
    data = AH_merged,
    aes(color = `Property Type`, fill = `Property Type`)
  )


############  AFFORDABLE HOUSING DATASET WITH ALL CENSUS  ############  
AH_noGeom <- AH_merged
AH_noGeom$geometry = NULL

AH_noGeom_envJoin <- AH_noGeom %>%
  left_join(allCensus_noGeom2, by="tract_id", relationship="many-to-many")

# Create df that identifies month & years where at least 1 measurement is present across all environment cols (co/h2s/no2 + temp/pressure/humidity)
AH_valid_time <- AH_noGeom_envJoin %>%
  filter(!(is.na(avg_daily_co) | is.na(avg_min_co) | is.na(avg_max_co)) &
           !(is.na(avg_daily_h2s) | is.na(avg_min_h2s) | is.na(avg_max_h2s)) &
           !(is.na(avg_daily_no2) | is.na(avg_min_no2) | is.na(avg_max_no2)) &
           !(is.na(avg_daily_temp) | is.na(avg_min_temp) | is.na(avg_max_temp)) &
           !(is.na(avg_daily_humid) | is.na(avg_min_humid) | is.na(avg_max_humid)) &
           !(is.na(avg_daily_press) | is.na(avg_min_press) | is.na(avg_max_press)) 
  ) %>%
  distinct(agg_date, tract_id)

AH_valid_time_perNbrhd <- AH_valid_time %>%
  group_by(agg_date) %>%
  count()

# avg is present in all
AH_avg_valid_time <- AH_noGeom_envJoin %>%
  filter(!(is.na(avg_daily_co)) & 
           !(is.na(avg_daily_h2s)) &  
           !(is.na(avg_daily_no2)) &  
           !(is.na(avg_daily_temp)) &  
           !(is.na(avg_daily_humid)) &  
           !(is.na(avg_daily_press))
  ) %>%
  distinct(agg_date, tract_id)

AH_avg_valid_time_perNbrhd <- AH_avg_valid_time %>%
  group_by(agg_date) %>%
  count()

# max is present in all
AH_max_valid_time <- AH_noGeom_envJoin %>%
  filter(!(is.na(avg_max_co)) & 
           !(is.na(avg_max_h2s)) &  
           !(is.na(avg_max_no2)) &  
           !(is.na(avg_max_temp)) &  
           !(is.na(avg_max_humid)) &  
           !(is.na(avg_max_press))
  ) %>%
  distinct(agg_date, tract_id)

AH_max_valid_time_perNbrhd <- AH_max_valid_time %>%
  group_by(agg_date) %>%
  count()

# min is present in all
AH_min_valid_time <- AH_noGeom_envJoin %>%
  filter(!(is.na(avg_min_co)) & 
           !(is.na(avg_min_h2s)) &  
           !(is.na(avg_min_no2)) &  
           !(is.na(avg_min_temp)) &  
           !(is.na(avg_min_humid)) &  
           !(is.na(avg_min_press))
  ) %>%
  distinct(agg_date, tract_id)

AH_min_valid_time_perNbrhd <- AH_min_valid_time %>%
  group_by(agg_date) %>%
  count()


############  NON-AFFORDABLE HOUSING DATASET WITH ALL CENSUS  ############
# Get AH tracts
AH_tracts <- AH_merged %>%
  distinct(tract_id) %>%
  mutate(isAH_tract = 1) # binary var that indicates it's an AH tract

# Merge with all tracts 
non_AH_tracts <- tracts %>%
  left_join(AH_tracts, by="tract_id") %>%
  mutate(isAH_tract = ifelse(is.na(isAH_tract), 0, isAH_tract)) %>%
  select(tract_id, isAH_tract) 
non_AH_tracts$geometry=NULL

# Filter out AH tracts
non_AH_tracts2 <- tracts_census_merged %>%
  left_join(non_AH_tracts, by="tract_id") %>%
  filter(isAH_tract == 0)
non_AH_tracts2$isAH_tract = NULL

# Merge with census_gas and census_env
non_AH_noGeom <- non_AH_tracts2
non_AH_noGeom$geometry = NULL
non_AH_noGeom_envJoin <- non_AH_noGeom %>%
  left_join(allCensus_noGeom2, by="tract_id", relationship="many-to-many")

# Create df that identifies month & years where at least 1 measurement is present across all environment cols (co/h2s/no2 + temp/pressure/humidity)
non_AH_valid_time <- non_AH_noGeom_envJoin %>%
  filter(!(is.na(avg_daily_co) | is.na(avg_min_co) | is.na(avg_max_co)) &
           !(is.na(avg_daily_h2s) | is.na(avg_min_h2s) | is.na(avg_max_h2s)) &
           !(is.na(avg_daily_no2) | is.na(avg_min_no2) | is.na(avg_max_no2)) &
           !(is.na(avg_daily_temp) | is.na(avg_min_temp) | is.na(avg_max_temp)) &
           !(is.na(avg_daily_humid) | is.na(avg_min_humid) | is.na(avg_max_humid)) &
           !(is.na(avg_daily_press) | is.na(avg_min_press) | is.na(avg_max_press)) 
  ) %>%
  distinct(agg_date, tract_id)

non_AH_valid_time_perNbrhd <- non_AH_valid_time %>%
  group_by(agg_date) %>%
  count()

# avg is present in all
non_AH_avg_valid_time <- non_AH_noGeom_envJoin %>%
  filter(!(is.na(avg_daily_co)) & 
           !(is.na(avg_daily_h2s)) &  
           !(is.na(avg_daily_no2)) &  
           !(is.na(avg_daily_temp)) &  
           !(is.na(avg_daily_humid)) &  
           !(is.na(avg_daily_press))
  ) %>%
  distinct(agg_date, tract_id)

non_AH_avg_valid_time_perNbrhd <- non_AH_avg_valid_time %>%
  group_by(agg_date) %>%
  count()

# max is present in all
non_AH_max_valid_time <- non_AH_noGeom_envJoin %>%
  filter(!(is.na(avg_max_co)) & 
           !(is.na(avg_max_h2s)) &  
           !(is.na(avg_max_no2)) &  
           !(is.na(avg_max_temp)) &  
           !(is.na(avg_max_humid)) &  
           !(is.na(avg_max_press))
  ) %>%
  distinct(agg_date, tract_id)

non_AH_max_valid_time_perNbrhd <- non_AH_max_valid_time %>%
  group_by(agg_date) %>%
  count()

# min is present in all
non_AH_min_valid_time <- non_AH_noGeom_envJoin %>%
  filter(!(is.na(avg_min_co)) & 
           !(is.na(avg_min_h2s)) &  
           !(is.na(avg_min_no2)) &  
           !(is.na(avg_min_temp)) &  
           !(is.na(avg_min_humid)) &  
           !(is.na(avg_min_press))
  ) %>%
  distinct(agg_date, tract_id)

non_AH_min_valid_time_perNbrhd <- non_AH_min_valid_time %>%
  group_by(agg_date) %>%
  count()

#############  FIGURING OUT OVERLAP TIME #############
# Looking at the data, it seems like avg and max are present more often in both AH and non_AH

# Calculate overlap for avgs
avg_overlap_time <- AH_avg_valid_time_perNbrhd %>%
  inner_join(non_AH_avg_valid_time_perNbrhd, by=c("agg_date"))
# Rename n.x and n.y cols
names(avg_overlap_time)[names(avg_overlap_time) == "n.x"] = "AH_avg_count"
names(avg_overlap_time)[names(avg_overlap_time) == "n.y"] = "non_AH_avg_count"
### VALID RANGE: Feb 2018-Feb 2019, excluding Aug 2018

# looking to see what IS present in August 2018 entries
test <- AH_noGeom_envJoin %>%
  filter(agg_date == "August 2018")

test %>%
  filter(is.na(avg_daily_temp)) %>%
  count() ## 0

test %>%
  filter(is.na(avg_daily_humid)) %>%
  count() ## 0

test %>%
  filter(is.na(avg_daily_press)) %>%
  count() ## 0

test %>%
  filter(is.na(avg_daily_co)) %>%
  count() ## 588 (all)

test %>%
  filter(is.na(avg_daily_h2s)) %>%
  count() ## 588 (all)

test %>%
  filter(is.na(avg_daily_no2)) %>%
  count() ## 0
### PRESENT: avg_daily: temp, humid, press, no2
### NOT PRESENT: co, h2s


# Calculate overlap for maxes
max_overlap_time <- AH_max_valid_time_perNbrhd %>%
  inner_join(non_AH_max_valid_time_perNbrhd, by=c("agg_date"))
# Rename n.x and n.y cols
names(max_overlap_time)[names(max_overlap_time) == "n.x"] = "AH_max_count"
names(max_overlap_time)[names(max_overlap_time) == "n.y"] = "non_AH_max_count"
### VALID RANGE: Feb 2018-Feb 2019 (ALL)


### CONCLUSION:
### Will be able to compare from Feb 2018-Feb 2019 (no gaps) for: 
### [daily]: temp, humid, press, no2
### [max]: temp, humid, press, no2, co, h2s 
### Will be able to compare from Feb 2018-July 2018 and Sept 2018-Feb 2019 for: 
### [daily]: co, h2s 

# Create df that identifies month & years where at least 1 measurement is present across all environment cols (co/h2s/no2 + temp/pressure/humidity)
poc_valid_time <- allCensus_noGeom3 %>%
  filter(racial_majority == "Non-White" & 
           !(is.na(avg_daily_co) | is.na(avg_min_co) | is.na(avg_max_co)) &
           !(is.na(avg_daily_h2s) | is.na(avg_min_h2s) | is.na(avg_max_h2s)) &
           !(is.na(avg_daily_no2) | is.na(avg_min_no2) | is.na(avg_max_no2)) &
           !(is.na(avg_daily_temp) | is.na(avg_min_temp) | is.na(avg_max_temp)) &
           !(is.na(avg_daily_humid) | is.na(avg_min_humid) | is.na(avg_max_humid)) &
           !(is.na(avg_daily_press) | is.na(avg_min_press) | is.na(avg_max_press)) 
  ) %>%
  distinct(agg_date, tract_id)

poc_valid_time_perNbrhd <- allCensus_noGeom3 %>%
  group_by(agg_date) %>%
  count()

# avg is present in all
poc_avg_valid_time <- allCensus_noGeom3 %>%
  filter(racial_majority == "Non-White") %>%
  filter(!(is.na(avg_daily_co)) & 
           !(is.na(avg_daily_h2s)) &  
           !(is.na(avg_daily_no2)) &  
           !(is.na(avg_daily_temp)) &  
           !(is.na(avg_daily_humid)) &  
           !(is.na(avg_daily_press))
  ) %>%
  distinct(agg_date, tract_id)

poc_avg_valid_time_perNbrhd <- poc_avg_valid_time %>%
  group_by(agg_date) %>%
  count()

# max is present in all
poc_max_valid_time <- allCensus_noGeom3 %>%
  filter(racial_majority == "Non-White") %>%
  filter(!(is.na(avg_max_co)) & 
           !(is.na(avg_max_h2s)) &  
           !(is.na(avg_max_no2)) &  
           !(is.na(avg_max_temp)) &  
           !(is.na(avg_max_humid)) &  
           !(is.na(avg_max_press))
  ) %>%
  distinct(agg_date, tract_id)

poc_max_valid_time_perNbrhd <- poc_max_valid_time %>%
  group_by(agg_date) %>%
  count()

# min is present in all
poc_min_valid_time <- allCensus_noGeom3 %>%
  filter(racial_majority == "Non-White") %>%
  filter(!(is.na(avg_min_co)) & 
           !(is.na(avg_min_h2s)) &  
           !(is.na(avg_min_no2)) &  
           !(is.na(avg_min_temp)) &  
           !(is.na(avg_min_humid)) &  
           !(is.na(avg_min_press))
  ) %>%
  distinct(agg_date, tract_id)

poc_min_valid_time_perNbrhd <- poc_min_valid_time %>%
  group_by(agg_date) %>%
  count()


############  NON White  ############

white_valid_time <- allCensus_noGeom3 %>%
  filter(racial_majority == "White" & 
           !(is.na(avg_daily_co) | is.na(avg_min_co) | is.na(avg_max_co)) &
           !(is.na(avg_daily_h2s) | is.na(avg_min_h2s) | is.na(avg_max_h2s)) &
           !(is.na(avg_daily_no2) | is.na(avg_min_no2) | is.na(avg_max_no2)) &
           !(is.na(avg_daily_temp) | is.na(avg_min_temp) | is.na(avg_max_temp)) &
           !(is.na(avg_daily_humid) | is.na(avg_min_humid) | is.na(avg_max_humid)) &
           !(is.na(avg_daily_press) | is.na(avg_min_press) | is.na(avg_max_press)) 
  ) %>%
  distinct(agg_date, tract_id)

white_valid_time_perNbrhd <- allCensus_noGeom3 %>%
  filter(racial_majority == "White") %>%
  group_by(agg_date) %>%
  count()

# avg is present in all
white_avg_valid_time <- allCensus_noGeom3 %>%
  filter(racial_majority == "White") %>%
  filter(!(is.na(avg_daily_co)) & 
           !(is.na(avg_daily_h2s)) &  
           !(is.na(avg_daily_no2)) &  
           !(is.na(avg_daily_temp)) &  
           !(is.na(avg_daily_humid)) &  
           !(is.na(avg_daily_press))
  ) %>%
  distinct(agg_date, tract_id)

white_avg_valid_time_perNbrhd <- white_avg_valid_time %>%
  group_by(agg_date) %>%
  count()

# max is present in all
white_max_valid_time <- allCensus_noGeom3 %>%
  filter(racial_majority == "White") %>%
  filter(!(is.na(avg_max_co)) & 
           !(is.na(avg_max_h2s)) &  
           !(is.na(avg_max_no2)) &  
           !(is.na(avg_max_temp)) &  
           !(is.na(avg_max_humid)) &  
           !(is.na(avg_max_press))
  ) %>%
  distinct(agg_date, tract_id)

white_max_valid_time_perNbrhd <- white_max_valid_time %>%
  group_by(agg_date) %>%
  count()

# min is present in all
white_min_valid_time <- allCensus_noGeom3 %>%
  filter(racial_majority == "White") %>%
  filter(!(is.na(avg_min_co)) & 
           !(is.na(avg_min_h2s)) &  
           !(is.na(avg_min_no2)) &  
           !(is.na(avg_min_temp)) &  
           !(is.na(avg_min_humid)) &  
           !(is.na(avg_min_press))
  ) %>%
  distinct(agg_date, tract_id)

white_min_valid_time_perNbrhd <- white_min_valid_time %>%
  group_by(agg_date) %>%
  count()

#############  FIGURING OUT OVERLAP TIME #############
# Looking at the data, it seems like avg and max are present more often in both AH and non_AH

# Calculate overlap for avgs
avg_overlap_time <- poc_avg_valid_time_perNbrhd %>%
  inner_join(white_avg_valid_time_perNbrhd, by=c("agg_date"))
# Rename n.x and n.y cols
names(avg_overlap_time)[names(avg_overlap_time) == "n.x"] = "AH_avg_count"
names(avg_overlap_time)[names(avg_overlap_time) == "n.y"] = "non_AH_avg_count"
### VALID RANGE: 

# looking to see what IS present in August 2018 entries
test <- AH_noGeom_envJoin %>%
  filter(agg_date == "August 2018")

test %>%
  filter(is.na(avg_daily_temp)) %>%
  count() ## 0

test %>%
  filter(is.na(avg_daily_humid)) %>%
  count() ## 0

test %>%
  filter(is.na(avg_daily_press)) %>%
  count() ## 0

test %>%
  filter(is.na(avg_daily_co)) %>%
  count() ## 588 (all)

test %>%
  filter(is.na(avg_daily_h2s)) %>%
  count() ## 588 (all)

test %>%
  filter(is.na(avg_daily_no2)) %>%
  count() ## 0
### PRESENT: avg_daily: temp, humid, press, no2
### NOT PRESENT: co, h2s


# Calculate overlap for maxes
max_overlap_time <- poc_max_valid_time_perNbrhd %>%
  inner_join(white_max_valid_time_perNbrhd, by=c("agg_date"))
# Rename n.x and n.y cols
names(max_overlap_time)[names(max_overlap_time) == "n.x"] = "AH_max_count"
names(max_overlap_time)[names(max_overlap_time) == "n.y"] = "non_AH_max_count"
### VALID RANGE: Feb 2018-Feb 2019 (ALL)


