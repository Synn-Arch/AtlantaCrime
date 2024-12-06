setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load necessary libraries , Set Census API key
library(sf)
library(nominatimlite)
library(tidycensus)
library(tidyverse)
library(tmap)
library(tigris)
library(fastDummies)
library(dplyr)
census_api_key('7deb96b03ae11f23a2fb544839ee195ccd646ac3')


# 1. Socio-economic Data
## Get Census data for block groups
## Male under 5 : B01001_003E
## Male 5 - 9 : B01001_004E
## Male 10 - 14 : B01001_005E
## Male 15 - 17 : B01001_006E
## Female under 5 : B01001_027E
## Female 5 - 9 : B01001_028E
## Female 10 - 14 : B01001_029E
## Female 15 - 17 : B01001_030E


blockgroup <- get_acs(
  geography = "block_group", 
  variables = c(
    'tot_popE' = 'B01001_001E', 
    'male_popE' = 'B01001_002E', 
    'female_popE' = 'B01001_026E',
    'Male5E' = 'B01001_003E',
    'Male59E' = 'B01001_004E',
    'Male1014E' = 'B01001_005E',
    'Male1517E' = 'B01001_006E',
    'Female5E' = 'B01001_027E',
    'Female59E' = 'B01001_028E',
    'Female1014E' = 'B01001_029E',
    'Female1517E' = 'B01001_030E',
    'race_totalE' = 'B02001_001E', 
    'whiteE' = 'B02001_002E', 
    'blackE' = 'B02001_003E', 
    'nativeE' = 'B02001_004E', 
    'asianE' = 'B02001_005E', 
    'pacific_islanderE' = 'B02001_006E', 
    'otherE' = 'B02001_007E'
  ),
  year = 2022,
  state = "GA", 
  county = c("Fulton", "DeKalb", "Clayton"), 
  survey = "acs5", 
  geometry = TRUE, 
  output = "wide"
)

## Calculate race ratios and population density
blockgroup <- blockgroup %>%
  dplyr::select(GEOID, NAME, geometry, ends_with("E")) %>%
  mutate(
    pop_den = tot_popE / st_area(.),
    adult_popE = tot_popE - (Male5E + Male59E + Male1014E + Male1517E + Female5E + Female59E + Female1014E + Female1517E),
    white_ratio = whiteE / race_totalE,
    black_ratio = blackE / race_totalE,
    other_ratio = (nativeE + asianE + pacific_islanderE + otherE) / race_totalE,
  )

## Get tract-level data
## There are not data on education level and median income for blockgroup, thus Get tract level data and put it to corresponding block groups.
tract_data <- get_acs(
  geography = "tract", 
  variables = c(
    'median_incomeE' = 'B06011_001E', 
    'edu_totalE' = 'B06009_001E', 
    'less_than_hsE' = 'B06009_002E'
  ),
  year = 2022,
  state = "GA", 
  county = c("Fulton", "DeKalb", "Clayton", "Cobb"), 
  survey = "acs5", 
  output = "wide"
) %>%
  mutate(less_than_hs_ratio = less_than_hsE / edu_totalE) %>%
  dplyr::select(GEOID, median_incomeE, less_than_hs_ratio)

## Match and join to block group data
blockgroup <- blockgroup %>%
  mutate(tract_id = substr(GEOID, 1, 11)) %>%
  left_join(tract_data, by = c("tract_id" = "GEOID"))

## Clean up by dropping the helper tract_id column
blockgroup <- blockgroup %>%
  dplyr::select(-tract_id)



# 2. Filter block groups within Atlanta City Region

## Get City of Atlanta boundary
atlanta <- nominatimlite::geo_lite_sf('Atlanta, GA', points_only = FALSE)
atlanta <- st_transform(atlanta, st_crs(blockgroup))

## Filter block group data within Atlanta city region
blockgroup_atlanta <- blockgroup[st_intersects(blockgroup, atlanta, sparse = FALSE), ]





# 3. Crime Data

## Load crime data and categorize violent and non-violent crimes
crime.data <- read.csv("Data/08012020-07312022.csv")
crime.data <- crime.data %>%
  mutate(violent = ifelse(NIBRS.Code.Name %in% c(
    "Aggravated Assault", "Murder & Nonnegligent Manslaughter", "Sodomy", 
    "Animal Cruelty", "Statutory Rape", "Rape", "Fondling", "Arson", 
    "Kidnapping/Abduction", "Intimidation", "Simple Assault", 
    "Weapon Law Violations"), 1, 0))

## Convert crime data to sf object
crime.sf <- st_as_sf(crime.data, coords = c("Longitude", "Latitude"), crs = 4326)

## Ensure both objects have the same CRS
blockgroup_atlanta <- st_transform(blockgroup_atlanta, crs = st_crs(crime.sf))

## Perform spatial join
bg_crime <- st_join(blockgroup_atlanta, crime.sf %>% mutate(count = ifelse(violent == 0, 1, 10000)))

## Summarize crime data by block group
bg_crime_count <- bg_crime %>%
  group_by(GEOID) %>%
  summarise(count = sum(count, na.rm = TRUE)) %>% 
  st_drop_geometry()

blockgroup_atlanta <- blockgroup_atlanta %>%
  left_join(bg_crime_count, by = "GEOID") %>% 
  mutate(violent_count = count %/% 10000, nonviolent_count = count %% 10000)




# 4. Land-use Data

## Load zoning data and classify zones
zoning <- read.csv("Data/Official_Zoning_Districts_-_Open_Data.csv")
zoning <- zoning %>%
  mutate(class = case_when(
    str_detect(ZONECLASS, "^(R-(1|2|3|4|5)|PD-H|FCR-)") ~ "Low-density Residential",
    str_detect(ZONECLASS, "^(R-LC|MRC-|PD-MU)") ~ "Residential-Commercial",
    str_detect(ZONECLASS, "^(RG-|MR-)") ~ "High-density Residential",
    str_detect(ZONECLASS, "^I-") ~ "Industrial",
    str_detect(ZONECLASS, "^(C-|LD|LW|NC|PD-OC)") ~ "Commercial",
    str_detect(ZONECLASS, "^O-") ~ "Institutional",
    # Historical regions
    str_detect(ZONECLASS, "^(HC-20A SA1|HC-20C SA4|HC-20N|Poncey-Highland SA(3|4|5))") ~ "Residential-Commercial",
    str_detect(ZONECLASS, "^(HC-20A SA3|HC-20C SA(1|2)|HC-20B|Poncey-Highland SA(7))") ~ "Low-density Residential",
    str_detect(ZONECLASS, "^(HC-20A SA2|Poncey-Highland SA(1|2|6))") ~ "High-density Residential",
    str_detect(ZONECLASS, "^(HC-20A SA(4|5))") ~ "Commercial",
    str_detect(ZONECLASS, "^(HC-20C SA3|HC-20D|HC-20E)") ~ "Institutional",
    # Special Areas
    str_detect(ZONECLASS, "^(SPI-1 SA(1|2|3|7)|SPI-9(-C| SA1)|SPI-11 SA(2|9)|SPI-12 SA1|SPI-15 SA(1|2|3|4|9)|SPI-18 SA3|SPI-19 SA(2|10)|SPI-20 SA(1|2)|SPI-21 SA(1|2|3|4|10)|SPI-22 SA4)") ~ "Commercial",
    str_detect(ZONECLASS, "^(SPI-1 SA(5|6)|SPI-2 SA5|SPI-3 SA(4|5|6|7|8|9)|SPI-4 SA(3|4|7|10|13)|SPI-9 SA(2|3|4)|SPI-16 SA1|SPI-18 SA(1|2|10)|SPI-19 SA(1|4|8|9|11)|SPI-20 SA(3|4)|SPI-22 TSA)") ~ "Residential-Commercial",
    str_detect(ZONECLASS, "^(SPI-1 SA4|SPI-3 SA3|SPI-4 SA(2|5|11)|SPI-5 SA(2|3)|SPI-7 SA3|SPI-11 SA8|SPI-12 SA2|SPI-15 SA(5|6|7|8)|SPI-16 SA(2|3)|SPI-17|SPI-18 SA(4|9)|SPI-19 SA7|SPI-20 SA5|SPI-21 SA5|SPI-22 SA3)") ~ "High-density Residential",
    str_detect(ZONECLASS, "^(SPI-2 SA(1|2|3|4)|SPI-3 SA(1|2)|SPI-4 SA(6|9)|SPI-5 SA1|SPI-7 SA(1|2)|SPI-11 SA6|SPI-12 SA3|SPI-18 SA(5|6)|SPI-19 SA(5|6)|SPI-20 SA6|SPI-21 SA7|SPI-22 SA2)") ~ "Low-density Residential",
    str_detect(ZONECLASS, "^(SPI-4 SA8|SPI-18 SA(7|8)|SPI-21 SA9|SPI-22 SA1)") ~ "Industrial",
    str_detect(ZONECLASS, "^(SPI-4 SA12|SPI-12 SA4|SPI-19 SA3|SPI-21 SA8)") ~ "Institutional",
    str_detect(ZONECLASS, "^SPI-4 SA1") ~ "Low-density Residential",
    TRUE ~ ""
  ))

## Load land use data and calculate land use percentages by block group
### NOTE : It took too much time. We made geojson file already by this command.
'''
landuse <- st_read("Data/Current_Land_Use.geojson") %>%
  select(FID, ACRES, LandUse, geometry) %>%
  st_make_valid() %>%
  st_transform(st_crs(blockgroup))

landuse <- landuse %>%
  st_intersection(blockgroup) %>%
  group_by(GEOID) %>%
  summarise(area = sum(st_area(geometry)))
'''

landuse <- st_read("Data/bg_landuse.geojson")
columns_to_keep <- c("GEOID", "NAME", "area", "Commercial", "HighdensityResidential", 
                     "Industrial", "Institutional", "LowdensityResidential", 
                     "ResidentialCommercial", "Percentage.sum")
landuse <- landuse[, columns_to_keep]

landuse_dff <- landuse[landuse$Percentage.sum>80,]
landuse_isna <- !is.na(landuse_dff$Percentage.sum)
landuse <- landuse_dff[landuse_isna,]

## Remove spatial class for joining
blockgroup_atlanta_with_geometry <- blockgroup_atlanta
blockgroup_atlanta <- st_set_geometry(blockgroup_atlanta, NULL)
landuse_df <- st_set_geometry(landuse, NULL)

## Join data
blockgroup_joined <- blockgroup_atlanta %>%
  left_join(landuse_df %>% dplyr::select(-NAME), by = "GEOID")

## Filter out unmatched GEOIDs between blockgroup_atlanta and landuse to remove NaN values
missing_geoids <- setdiff(blockgroup_atlanta$GEOID, landuse_df$GEOID)

blockgroup_joined <- blockgroup_joined %>%
  filter(!GEOID %in% missing_geoids) %>%
  left_join(blockgroup_atlanta_with_geometry %>% dplyr::select(GEOID, geometry), by = "GEOID") %>%
  st_as_sf()




# 5. Police Station Data

## Calculate the distance from each block group centroid to nearest police station
stations <- read_csv("Data/police_stations.csv")
stations_sf <- st_as_sf(stations, coords = c("x", "y"), crs = 3857) #%>%
  #st_transform(crs = st_crs(blockgroup_joined))

block_centroids <- blockgroup_joined %>% st_centroid()
block_centroids <- st_transform(block_centroids, st_crs(stations_sf))

data_final <- blockgroup_joined %>%
  mutate(min_station_dist = apply(st_distance(block_centroids, stations_sf), 1, min))

distances <- st_distance(block_centroids, stations_sf)
units(distances)

# 6. Final Data Cleaning and Save

## Keep Columns we will use for regression (it can be changed later)
## Keep GEOID and Name to match for index
columns_to_keep <- c("GEOID", "NAME", "tot_popE", "adult_popE", "pop_den", "white_ratio", "black_ratio", "other_ratio", "median_incomeE", "less_than_hs_ratio", 
                     "Commercial", "HighdensityResidential", "Industrial", "Institutional", "LowdensityResidential", "ResidentialCommercial", "Percentage.sum", "min_station_dist",
                     "geometry", "violent_count", "nonviolent_count")
data_final <- data_final[, columns_to_keep]

## Remove Null Rows
data_final <- data_final %>%
  drop_na()

## Calculate Crime Rate per adult_pop
data_final2 <- data_final %>%
  mutate(
    vio_crimerate = violent_count / adult_popE,
    nonvio_crimerate = nonviolent_count / adult_popE
  )

## Export final dataset to GeoJSON
st_write(data_final2, "data_final.geojson", driver = "GeoJSON")

## Remove geometry
data_final_csv <- data_final2 %>%
  st_drop_geometry()

## Export to CSV
write.csv(data_final_csv, "data_final.csv", row.names = FALSE)

