# Import libraries 
library(tidyverse)
library(dplyr)
library(sf) # for analyzing spatial data
library(tidycensus) # for analyzing census data
library(ggplot2)
library(showtext) # for importing fonts in graphs

setwd('/Users/rileyramos/Desktop/CapstoneDPSS')

# NOTES: 
#  - pressure is only measured from 2018-2019
#  - krigged data is already spatially interpolated to the census tract level (use krigged to matching nodes to tracts using tractce10 var)
#  - environmental permits data is not cleaned 
#  - Due to errors in the sensors, not all days have data, and not all sensors take data for all variables
#  - use complete.cases argument to remove rows that have NA values

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

############ Dividing Cleaned Census Data ############
# Calculate perc_white per tract
census3 <- census_green_merged %>%
  mutate(white = white_men+white_women,
         perc_white = white/total_population,
         maj_white = ifelse(perc_white < 0.5, 0, 1)) 

# Check if there are any incorrect total_population values
census3 %>%
  filter(white > total_population) %>%
  count() # 0

# Count how many census tracts have < 50% pop white (majority non-white)
census3 %>%
  filter(maj_white == 0) %>%
  count() # 464 census tracts (~59.33% of all tracts)

# Count how many census tracts have > 50% pop white (majority white)
census3 %>%
  filter(maj_white == 1) %>%
  count() # 318 (~40.66% of all tracts)

# Separate into majority white and non majority white
maj_white <- census3 %>%
  filter(maj_white == 1) 

maj_poc <- census3 %>%
  filter(maj_white == 0) 

### Count how many tracts intersect with a green area for each group
maj_white %>%
  select(conservation) %>%
  sum() # 123 (~39% of maj white pop)
maj_poc %>%
  select(conservation) %>%
  sum() # 23 (~5% of poc pop) 

### Count how many green roofs are in each group 
maj_white %>%
  select(roofs_2012) %>%
  sum() # 179 
maj_poc %>%
  select(roofs_2012) %>%
  sum() # 143  

### Create Bar Plot
# Rename maj_white col
census4 <- census3 %>%
  mutate(maj_white = ifelse(maj_white == 1, "White", "Non-White"))

# Get total count
census_agg <- census4 %>%
  group_by(maj_white) %>%
  count() %>%
  mutate(Variable = "Total Tracts")

# Get total count of access to GA per racial group maj
census_agg2 <- census4 %>%
  group_by(maj_white) %>%
  summarize(
    n = sum(conservation)
  ) %>%
  mutate(Variable = "Tracts With Access To\na Conservation Area")

# Bind 2 prev dfs for plotting
census_agg3 <- rbind(census_agg, census_agg2) %>%
  arrange(desc(Variable))

# Sum green roofs
census_agg4 <- census4 %>%
  group_by(maj_white) %>%
  summarize(
    n = sum(roofs_2012)
  ) %>%
  mutate(Variable = "Total Green Roofs")

# Bind dfs
census_agg5 <- rbind(census_agg, census_agg4)
census_agg6 <- rbind(census_agg3, census_agg4)

# Refactor so Total bar appears on the left
census_agg6$Variable <- factor(census_agg6$Variable, levels = c('Total Tracts', 'Total Green Roofs', 'Tracts With Access To\na Conservation Area'))

# Install UChicago fonts for plotting
font_add("GothamBold", "/Users/rileyramos/Desktop/CapstoneDPSS/Gotham-font-family/Gotham/Gotham-Bold.otf")
font_add("GothamBook", "/Users/rileyramos/Desktop/CapstoneDPSS/Gotham-font-family/Gotham/Gotham-Book.otf")
showtext_auto()

# Bar plot
counts_plot <- census_agg6 %>%
ggplot(aes(x = maj_white, y=n, fill=Variable)) +
  geom_bar(stat = "identity", position="dodge") +
  labs(title = "Counts By Racial Majority", x = "Racial Majority of Tract", y = "Count") + 
  scale_fill_manual(values = c("Total Tracts" = "#737373", "Tracts With Access To\na Conservation Area" = "#275D38", "Total Green Roofs" = "#789D4A")) +
  theme(
    legend.position = "right", 
    legend.title.align=0.5,
    legend.title=element_text(size=8, family="GothamBold"),
    legend.text=element_text(size=8, family = "GothamBook"),
    legend.key.width = unit(0.5, "inches"),
    plot.title = element_text(size = 18, family = "GothamBold"),
    axis.title.x = element_text(size = 14, family = "GothamBold", margin=margin(t=5, r=15, b=0, l=0)),
    axis.title.y = element_text(size = 14, family = "GothamBold"),
    axis.text.x = element_text(size = 10, family = "GothamBook"),
    axis.text.y = element_text(size = 10, family = "GothamBook")
  ) +
  geom_text(
    aes(y = n, label = n),
    position = position_dodge(width = 0.9),  # Adjust width for dodged bars
    vjust = -0.5,  # Vertical adjustment of text
    size = 3,  # Size of the text
    color = "black"  # Color of the text
  )

############  Merge census pops with shape file  ############  
# Read in tracts shape file
unzip("AOT MATERIALS/Spatial Data/Boundaries - Census Tracts - 2010.zip")
tracts <- st_read("AOT MATERIALS/Spatial Data/geo_export_08ee1dde-f77f-43d7-8624-09569366eec5.shp")

# Add id column to identify polygon geometry per census tract
tracts <- tracts %>%
  mutate(tract_id = row_number())

# Transform to crs
tracts <- st_transform(tracts, 4326)

# Merge point data with tracts
tracts_census_merged <- st_join(
  census_lonLat, # points
  tracts, # tract polygons
  join = st_within # join where point is WITHIN tract polygon
)

# Remove point data
tracts_census_merged$geometry = NULL

# Rejoin to get associated polygon geometries
tracts_census_merged <- inner_join(tracts_census_merged, tracts, by="tract_id")

# Convert back to sf object for plotting
tracts_census_merged <- st_as_sf(tracts_census_merged)

# Test plot
ggplot() + 
  geom_sf(
    data = tracts_census_merged,
    aes(color = white_men, fill = white_men)
  )

############  Merge Cleaned Census with Krigged ############  
names(census4)[names(census4) == "maj_white"] = "racial_majority"

census4_joinPrep <- census4 %>%
  select(tractce10, perc_white, racial_majority)

allCensus_noGeom <- census_allEnv_merged_df %>%
  left_join(census_allGas_merged_df, by=c("tractce10", "agg_date"))

# Drop all columns ending with .y
allCensus_noGeom2 <- allCensus_noGeom %>%
  select(-matches("\\.y$"))

# Remove .x suffix from column names
colnames(allCensus_noGeom2) <- str_replace(colnames(allCensus_noGeom2), "\\.x$", "")

allCensus_noGeom3 <- allCensus_noGeom2 %>%
  left_join(census4_joinPrep, by="tractce10")

nas <- allCensus_noGeom3 %>%
  filter(is.na(perc_white)) # 111 - all belong to scrubbed tractce10 ids

allCensus_noGeom3 <- allCensus_noGeom3 %>%
  filter(!(is.na(perc_white)))

allCensus_noGeom3 %>%
  filter(is.na(perc_white)) %>%
  count() # 0

maj_poc_envChars <- allCensus_noGeom3 %>%
  filter(racial_majority == "Non-White")

maj_white_envChars <- allCensus_noGeom3 %>%
  filter(racial_majority == "White")

poc_avgDailyCo <- maj_poc_envChars %>%
  filter(!(agg_date == "March 2020")) %>%
  select(agg_date, avg_daily_co) %>%
  group_by(agg_date) %>%
  summarize(
    mean_daily_co = mean(avg_daily_co, na.rm=T)
  )

poc_avgDailyCo %>% 
  ggplot(data=., aes(x=agg_date, y=mean_daily_co)) +
  geom_line() +
  geom_point()

white_avgDailyCo <- maj_white_envChars %>%
  filter(!(agg_date == "March 2020")) %>%
  select(agg_date, avg_daily_co) %>%
  group_by(agg_date) %>%
  summarize(
    mean_daily_co = mean(avg_daily_co, na.rm=T)
  )

white_avgDailyCo %>% 
  ggplot(data=., aes(x=agg_date, y=mean_daily_co)) +
  geom_line() +
  geom_point()



poc_avgDailyTemp <- maj_poc_envChars %>%
  select(agg_date, avg_daily_temp) %>%
  group_by(agg_date) %>%
  summarize(
    mean_daily_temp = mean(avg_daily_temp, na.rm=T)
  )

poc_avgDailyTemp %>% 
  ggplot(data=., aes(x=agg_date, y=mean_daily_temp)) +
  geom_line() +
  geom_point()

white_avgDailyTemp <- maj_white_envChars %>%
  select(agg_date, avg_daily_temp) %>%
  group_by(agg_date) %>%
  summarize(
    mean_daily_temp = mean(avg_daily_temp, na.rm=T)
  )

white_avgDailyTemp %>% 
  ggplot(data=., aes(x=agg_date, y=mean_daily_temp)) +
  geom_line() +
  geom_point()


allPop_avgMaxHumid <- allCensus_4 %>%
  select(agg_date, avg_max_humid, racial_majority) %>%
  group_by(agg_date, racial_majority) %>%
  summarize(
    mean_max_humid = mean(avg_max_humid, na.rm=T)
  )

allPop_avgMaxHumid %>% 
  ggplot(data=., aes(x=agg_date, y=mean_max_humid, group=racial_majority, color=racial_majority) )+
  geom_line() +
  geom_point()


allPop_avgMaxTemp <- allCensus_4 %>%
  select(agg_date, avg_max_temp, racial_majority) %>%
  group_by(agg_date, racial_majority) %>%
  summarize(
    mean_max_temp = mean(avg_max_temp, na.rm=T)
  )

allPop_avgMaxTemp %>% 
  ggplot(data=., aes(x=agg_date, y=mean_max_temp, group=racial_majority, color=racial_majority) )+
  geom_line() +
  geom_point()


allPop_avgMaxno2 <- allCensus_4 %>%
  select(agg_date, avg_max_no2, racial_majority) %>%
  group_by(agg_date, racial_majority) %>%
  summarize(
    mean_max_no2 = mean(avg_max_no2, na.rm=T)
  )

allPop_avgMaxno2 %>% 
  ggplot(data=., aes(x=agg_date, y=mean_max_no2, group=racial_majority, color=racial_majority) )+
  geom_line() +
  geom_point()

allPop_avgDailyno2 <- allCensus_4 %>%
  select(agg_date, avg_daily_no2, racial_majority) %>%
  group_by(agg_date, racial_majority) %>%
  summarize(
    mean_daily_no2 = mean(avg_daily_no2, na.rm=T)
  )

allPop_avgDailyno2 %>% 
  ggplot(data=., aes(x=agg_date, y=mean_daily_no2, group=racial_majority, color=racial_majority) )+
  geom_line() +
  geom_point()



allPop_avgMaxh2s <- allCensus_4 %>%
  filter(!(agg_date == "2020-02-01")) %>%
  select(agg_date, avg_max_h2s, racial_majority) %>%
  group_by(agg_date, racial_majority) %>%
  summarize(
    mean_max_h2s = mean(avg_max_h2s)
  )

allPop_avgMaxh2s %>% 
  ggplot(data=., aes(x=agg_date, y=mean_max_h2s, group=racial_majority, color=racial_majority) )+
  geom_line() +
  geom_point()

allCensus_4 <- allCensus_noGeom3

allCensus_4$agg_date <- my(allCensus_4$agg_date)

allCensus_5 <- allCensus_4

lonLat <- census %>%
  select(tractce10, lon, lat)

allCensus_5 <- allCensus_5 %>%
  left_join(lonLat, by="tractce10")

# Convert census data to lon lat format 
allCensus_5 <- st_as_sf(
  allCensus_5,
  coords = c("lon", "lat"),
  crs = 4326)

tracts <- st_transform(tracts, 4326)

# Merge point data with tracts
allCensus5_tractGeom <- st_join(
  allCensus_5, # points
  tracts, # tract polygons
  join = st_within # join where point is WITHIN tract polygon
)

# Remove point data
allCensus5_tractGeom$geometry = NULL

# Rejoin to get associated polygon geometries
allCensus5_tractGeom <- inner_join(allCensus5_tractGeom, tracts, by="tract_id")
colnames(allCensus5_tractGeom) <- str_replace(colnames(allCensus5_tractGeom), "\\.x$", "")

# Convert back to sf object for plotting
allCensus5_tractGeom <- st_as_sf(allCensus5_tractGeom)

# Test plot
allCensus6 <- allCensus5_tractGeom
allCensus6$geometry = NULL
test <- allCensus6 %>%
  distinct(agg_date, perc_white, tractce10, racial_majority, avg_daily_h2s, tract_id) %>%
  filter(agg_date >= "2018-02-01", agg_date <= "2019-02-01") %>%
  group_by(tractce10, perc_white, tract_id, racial_majority) %>%
  summarize(
    avg_daily_h2s_agg = mean(avg_daily_h2s, na.rm=T)
  )

test_lm <- lm(avg_daily_h2s_agg ~ perc_white, data=test)
summary(test_lm)

test %>% 
  ggplot(data=., aes(x=perc_white, y=avg_daily_h2s_agg) )+
  geom_point()

test$geometry = NULL

test <- inner_join(test, tracts, by="tract_id")

test <- st_as_sf(test)

ggplot() + 
  geom_sf(
    data = test,
    aes(color = avg_daily_h2s_agg, fill = avg_daily_h2s_agg)
  )

test3 <- allCensus6 %>%
  distinct(agg_date, perc_white, tractce10, racial_majority, avg_daily_temp, tract_id) %>%
  filter(agg_date >= "2018-02-01", agg_date <= "2019-02-01") %>%
  group_by(tractce10, perc_white, tract_id, racial_majority) %>%
  summarize(
    avg_daily_temp_agg = mean(avg_daily_temp, na.rm=T)
  )

test3_lm <- lm(avg_daily_temp_agg ~ perc_white, data=test3)
summary(test3_lm)

test3 %>% 
  ggplot(data=., aes(x=perc_white, y=avg_daily_temp_agg) )+
  geom_point()


test %>% 
  ggplot(data=., aes(x=perc_white, y=avg_daily_h2s_agg) )+
  geom_point()
###################################
# Test plot
allCensus6 <- allCensus5_tractGeom
allCensus6$geometry = NULL
test <- allCensus6 %>%
  distinct(agg_date, tractce10, racial_majority, tract_id) 

test$geometry = NULL

test <- inner_join(test, tracts, by="tract_id")

test <- st_as_sf(test)

ggplot() + 
  geom_sf(
    data = test,
    aes(color = racial_majority, fill = racial_majority) 
  ) + 
  scale_fill_manual(values = c("Non-White" = "#800000", "White" = "#a6a6a6")) + 
  scale_color_manual(values = c("Non-White" = "#000000", "White" = "#000000")) +
  labs(title="Racial Composition of Chicago Census Tracts",
       fill="Racial Majority", color="Racial Majority") +
  theme(
    legend.position = "right", 
    legend.title.align=0.5,
    legend.title=element_text(size=8, family="GothamBold"),
    legend.text=element_text(size=8, family = "GothamBook"),
    legend.key.width = unit(0.5, "inches"),
    plot.title = element_text(size = 14, family = "GothamBold"),
    axis.title.x = element_text(size = 14, family = "GothamBold", margin=margin(t=5, r=15, b=0, l=0)),
    axis.title.y = element_text(size = 14, family = "GothamBold"),
    axis.text.x = element_text(size = 10, family = "GothamBook"),
    axis.text.y = element_text(size = 10, family = "GothamBook")
  ) 

# Green roofs
test_map <- census_results4
test_map$geometry = NULL
test <- test_map %>%
  distinct(agg_date, tractce10, roofs_2012, tract_id) 

test$geometry = NULL

test <- inner_join(test, tracts, by="tract_id")

test <- st_as_sf(test)

ggplot() + 
  geom_sf(
    data = test,
    aes(color = roofs_2012, fill = roofs_2012), color="#d9d9d9"
  ) + 
  scale_fill_gradient(low="#a6a6a6", high="#789d4a") +
  labs(title="Green Roof Distribution\nAcross Chicago Census Tracts",
       fill="Number of Green Roofs", color="Number of Green Roofs") +
  theme(
    legend.position = "right", 
    legend.title.align=0.5,
    legend.title=element_text(size=8, family="GothamBold"),
    legend.text=element_text(size=8, family = "GothamBook"),
    legend.key.width = unit(0.5, "inches"),
    plot.title = element_text(size = 14, hjust=0.5, family = "GothamBold"),
    axis.title.x = element_text(size = 14, family = "GothamBold", margin=margin(t=5, r=15, b=0, l=0)),
    axis.title.y = element_text(size = 14, family = "GothamBold"),
    axis.text.x = element_text(size = 10, family = "GothamBook"),
    axis.text.y = element_text(size = 10, family = "GothamBook")
  ) 


# Access to Conservation Areas
test_map <- census_results4
test_map$geometry = NULL
test <- test_map %>%
  distinct(agg_date, tractce10, conservation, tract_id) %>%
  mutate(access_conservation = ifelse(conservation == 1, "Yes", "No"))

test$geometry = NULL

test <- inner_join(test, tracts, by="tract_id")

test <- st_as_sf(test)

ggplot() + 
  geom_sf(
    data = test,
    aes(color = access_conservation, fill = access_conservation), color="#d9d9d9"
  ) + 
  scale_fill_manual(values = c("No" = "#a6a6a6", "Yes" = "#275d38")) +
  labs(title="Conservation Area Distribution\nAcross Chicago Census Tracts",
       fill="Access to\nConservation Area", color="Access to\nConservation Area") +
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
    axis.text.y = element_text(size = 10, family = "GothamBook")
  ) 


# Median Home Value
test_map <- census_results5
test_map$geometry = NULL
test <- test_map %>%
  distinct(agg_date, tractce10, median_home_value, tract_id)

test$geometry = NULL

test <- inner_join(test, tracts, by="tract_id")

test <- st_as_sf(test)

legend_breaks = c(0, 250000, 500000, 750000, 1000000, 1250000, 1500000)

legend_labels = c("0", "250,000", "500,000", "750,000", "1,000,000", "1,250,000", "1,500,000")

ggplot() + 
  geom_sf(
    data = test,
    aes(color = median_home_value, fill = median_home_value), 
  ) + 
  scale_fill_gradient(low="#002a3a", high="#3eb1c8", breaks = legend_breaks, labels=legend_labels) +
  scale_color_gradient(low="#002a3a", high="#3eb1c8", guide = "none") +
  labs(title="Median Home Value Distribution\nAcross Chicago Census Tracts",
       fill="Median Home Value") +
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
    axis.text.y = element_text(size = 10, family = "GothamBook")
  ) 





