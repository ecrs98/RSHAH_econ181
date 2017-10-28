##### Code for cleaning Metro data #####
## Ethan Rodriguez-Shah

##-----
## Necessary libraries
#install.packages("RCurl")
library(RCurl)
library(dplyr)
library(geosphere)
library(stringr)
library(lubridate)


##-----
## Set working directory

#getwd <- "~/Google Drive/6. ECON 181 - Expository Data Analysis w: R/Final Project"
#setwd(getwd)



##-----
## Read in data

# I was planning on uploading my data to github and reading it directly from the
# site using RCurl, but each time I click on "Upload files" the site tells me that
# uploads are disabled. Instead, I had to read the data in locally.

# Station data:
# Locations of each WMATA rail station by both physical address and
# longitude/latitude.
station_locations <- read.csv("Metro_station_locations.csv",
                              stringsAsFactors = F)

# Real estate data:
# I had to manually retrieve data in sets of less than 350 from the Redfin site. This data
# shows all properties sold in DC in the three months prior to October 2017.
studio <- read.csv("prelim_data/redfin_dc_studio.csv",
                   stringsAsFactors = F)
bed1_not_condo <- read.csv("prelim_data/redfin_dc_1bed_not_condo.csv",
                           stringsAsFactors = F)
bed1_condo_0_750 <- read.csv("prelim_data/redfin_dc_1bed_condo(0,750)sqft.csv",
                             stringsAsFactors = F)
bed1_condo_750_inf <- read.csv("prelim_data/redfin_dc_1bed_condo(750,inf)sqft.csv",
                               stringsAsFactors = F)
bed2_not_condo <- read.csv("prelim_data/redfin_dc_2bed_not_condo.csv",
                           stringsAsFactors = F)
bed2_condo_0_1250 <- read.csv("prelim_data/redfin_dc_2bed_condo(0,1250)sqft.csv",
                              stringsAsFactors = F)
bed2_condo_1250_inf <- read.csv("prelim_data/redfin_dc_2bed_condo(1250,inf).csv",
                                stringsAsFactors = F)
bed3_house_condo <- read.csv("prelim_data/redfin_dc_3bed_house_condo.csv",
                             stringsAsFactors = F)
bed3_townhouse_0_1500 <- read.csv("prelim_data/redfin_dc_3bed_townhouse(0,1500).csv",
                                  stringsAsFactors = F)
bed3_townhouse_1500_inf <- read.csv("prelim_data/redfin_dc_3bed_townhouse(1500,inf).csv",
                                    stringsAsFactors = F)
bed3_all_else <- read.csv("prelim_data/redfin_dc_3bed_all_else.csv",
                          stringsAsFactors = F)
bed4_not_townhouse <- read.csv("prelim_data/redfin_dc_4bed_not_townhouse.csv",
                               stringsAsFactors = F)
bed4_townhouse <- read.csv("prelim_data/redfin_dc_4bed_townhouse.csv",
                           stringsAsFactors = F)
bed5 <- read.csv("prelim_data/redfin_dc_5bed.csv",
                 stringsAsFactors = F)
bed6plus <- read.csv("prelim_data/redfin_dc_6plusbed.csv",
                     stringsAsFactors = F)


##-----
## Combine real estate data into single dataset

redfin_data <- studio %>%
  rbind(bed1_not_condo) %>%
  rbind(bed1_condo_0_750) %>%
  rbind(bed1_condo_750_inf) %>%
  rbind(bed2_not_condo) %>%
  rbind(bed2_condo_0_1250) %>%
  rbind(bed2_condo_1250_inf) %>%
  rbind(bed3_all_else) %>%
  rbind(bed3_house_condo) %>%
  rbind(bed3_townhouse_0_1500) %>%
  rbind(bed3_townhouse_1500_inf) %>%
  rbind(bed4_not_townhouse) %>%
  rbind(bed4_townhouse) %>%
  rbind(bed5) %>%
  rbind(bed6plus)


##-----
## Cleaning data

# Removing unnecessary columns from redfin_data
names(redfin_data)
redfin_data <- redfin_data %>% 
  select(-STATUS, -NEXT.OPEN.HOUSE.START.TIME,
         -NEXT.OPEN.HOUSE.END.TIME, -FAVORITE,
         -INTERESTED, -URL..SEE.http...www.redfin.com.buy.a.home.comparative.market.analysis.FOR.INFO.ON.PRICING.,
         -SOURCE, -MLS., -SALE.TYPE)

# Correct variable classes?
str(station_locations)
str(redfin_data)

# station_locations has all appropriate data classes. redfin_data needs work
# * do i need to change the classes from integer to numeric? whats the difference?
redfin_data <- redfin_data %>%
  mutate(SOLD.DATE = mdy(SOLD.DATE))


# Any missing values in station_locations?
na_rows_stat <- filter(station_locations,
                  is.na(station_locations$station) |
                    is.na(station_locations$address1) |
                    is.na(station_locations$address2) |
                    is.na(station_locations$Latitude) |
                    is.na(station_locations$Longitude))
na_rows_stat
# A total of 0 rows have missing values.

# *Replacing redfin_data empty cells with missing values



##-----
## Distance between two locations

# geosphere's distHaversine() takes two arguments, both of which are vectors or matrices
# consisting of longitude/latitude pairs in that order (NOTE: NOT LAT/LONG)
distHaversine(c(0, 0),
              c(90, 90))

# Testing distHaversine()
redfin_data[1, c("LONGITUDE", "LATITUDE")] %>% 
  distHaversine(station_locations[1, c("Longitude", "Latitude")])


# The distance between the first property in redfin_data and Addison Rd Station, the
# station described in the first row of station_locations, is 12652.35 meters. Testing the
# distance on an independent website yields the same result.
# Resource: https://www.mapdevelopers.com/distance_from_to.php


##-----
## Distance to nearest Metro station
dist_var <- Inf
test_dist <- 0
dist_vec <- c()

for(n in 1:nrow(station_locations)) {
  
  test_dist <- as.matrix(redfin_data[1, c("LONGITUDE", "LATITUDE")]) %>%
    distHaversine(as.matrix(station_locations[n, c("Longitude", "Latitude")]))
  
  # dist_vec <- c(dist_vec, as.numeric(test_dist))}
  
  if(test_dist < dist_var) {
    dist_var <- test_dist
  }
}
print(dist_var)
  


# end goal is a for loop within a for loop, one for each row in station_locations
# and the other for each row in redfin_data

##-----
## Defining a function for distance to closest Metro station.

# This function takes one argument, locations, which is either a vector or a matrix
# of long/lat coordinates.
# The function will return distance from the nearest DC Metro station in miles rather
# than meters.
Metro_dist <- function(locations) {
  dist_var <- Inf
  test_dist <- 0
  for(n in 1:nrow(station_locations)) {
    
    test_dist <- locations %>%
      distHaversine(as.matrix(station_locations[n, c("Longitude", "Latitude")]))
    
    dist_var <- ifelse(test_dist < dist_var, test_dist, dist_var)
  }
  dist_var <- dist_var / 1609.344    # 1 mi = 1609.344 m
  return(dist_var)
}
  

  

# Testing function
redfin_data[,c("LONGITUDE", "LATITUDE")] %>% Metro_dist() %>% head()
redfin_data[,c("LONGITUDE", "LATITUDE")] %>% Metro_dist() %>% tail()


##-----
## Adding distance column to real_estate data frame
redfin_data <- redfin_data %>%
  mutate(dist = Metro_dist(redfin_data[,c("LONGITUDE", "LATITUDE")]))


# Now redfin_data contains a column with the distance to the closest Metro station.

##-----
## Cutting distance into bins and transforming price into multiples of one million
redfin_data <- redfin_data %>%
  mutate(dist_bin = round(dist, digits = 1),
         new_price = PRICE / 1000000)

##-----
# Exporting the data frame for sharing
# write.csv(redfin_data, file = "redfin_data.csv")



# :-)