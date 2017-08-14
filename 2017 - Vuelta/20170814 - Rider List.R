
# Script to extract and arrange the Velogames Rider List for the 2017 Vuleta Espana

# Load required libraries
library(rvest)
library(dplyr)

# Set working directory
setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.6 Projects/velogames/2017 - Vuelta/")

# Set URL, download and read html 
vuelta_rider_list_url <- "https://www.velogames.com/vuelta-a-espana/2017/riders.php"
download.file(vuelta_rider_list_url, "vuelta_rider_list_url.xml")
my_xml <- read_html("vuelta_rider_list_url.xml")


# Extract table from html
vuelta_rider_list_table <- my_xml %>% 
  html_node(xpath = "//table") %>% 
  html_table()

# Delete empty first column (that contained team jerseys)
vuelta_rider_list_table <- vuelta_rider_list_table[ , -1]

# Arrange Rider List by type, cost and group into teams where applicable.
riders_by_class_and_cost <- vuelta_rider_list_table %>% 
  select(Rider, Team, Class, Cost) %>% 
  # group_by(Team) %>%
  arrange(Class, desc(Cost), Team)
View(riders_by_class_and_cost)

# Write table to local .csv file
write.csv(riders_by_class_and_cost, file = "riders_by_class_and_cost.csv", row.names = FALSE)


