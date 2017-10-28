##### Summary Statistics #####
## Ethan Rodriguez-Shah

##-----
## Necessary libraries
library(ggplot2)
library(dplyr)
source("ETHAN_RSHAH_loading_cleaning_validation.R")

##-----
## Plotting property price vs. distance from nearest Metro station
# This plot shows the relationship between property prices and the distance from the
# nearest Metro station for properties sold in the Washington Metropolitan Area in the
# three months prior to October 2017.
price_dist <- redfin_data %>%  
  ggplot(aes(x = dist,
             y = new_price)) +
  geom_point(color = "green4") +
  labs(title = "Property Price vs. Distance from Metro Station",
       subtitle = "For properties sold in Washington, DC, in the three months\nprior to October 2017",
       x = "Distance from Nearest Metro Station (in miles)",
       y = "Property Price (in millions of USD)",
       caption = "Property data from Redfin") +
  theme_light()

price_dist

## Plotting property price vs. distance bins
# This plot cuts data into bins rounded to the nearest tenth of a mile. The price values
# are the average prices by bin.
price_distbin <- redfin_data %>%
  group_by(dist_bin) %>%
  summarise(avg_price = mean(new_price)) %>%
  ggplot(aes(x = dist_bin,
             y = avg_price)) +
  geom_line(color = "red") +
  labs(title = "Property Price vs. Distance from Metro Station",
       subtitle = "For properties sold in Washington, DC, in the three months\nprior to October 2017",
       x = "Rounded Distance from Metro Station (in miles)",
       y = "Average Property Price (in millions of USD)",
       caption = "Data from Redfin") +
  theme_light() 

price_distbin

##-----
## Plotting property price vs. square footage
# This plot shows the relation between property prices and square footage.
price_sqft <- redfin_data %>%  
  ggplot(aes(x = SQUARE.FEET,
             y = new_price)) +
  geom_point(color = "blue3") +
  labs(title = "Property Price vs. Square Footage",
       subtitle = "For properties sold in Washington, DC, in the three months\nprior to October 2017",
       x = "Square footage",
       y = "Property Price (in millions of USD)",
       caption = "Property data from Redfin") +
  theme_light()

price_sqft

## Plotting property price vs. square footage bins
# This plot cuts square footage into bins rounded to the nearest 100 square feet.
# The price values are the average prices by bin.
price_sqftbin <- redfin_data %>%
  mutate(sqft_bin = round(SQUARE.FEET, digits = -2)) %>% 
  group_by(sqft_bin) %>%
  summarise(avg_price = mean(new_price)) %>%
  ggplot(aes(x = sqft_bin,
             y = avg_price)) +
  geom_line(color = "blue2") +
  labs(title = "Property Price vs. Square Footage",
       subtitle = "For properties sold in Washington, DC, in the three months\nprior to October 2017",
       x = "Rounded Square Footage",
       y = "Average Property Price (in millions of USD)",
       caption = "Data from Redfin") +
  theme_light() 

price_sqftbin

##-----
## These plots will help me answer my economic question because I can use them to run 
## regressions and tease out the relationship between property prices and different
## property characteristics, including and most importantly proximity to Metro stations.
