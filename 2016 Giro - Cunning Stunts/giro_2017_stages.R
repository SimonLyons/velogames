require(XML)
require(assertthat)
require(dplyr)
require(lubridate)


setwd("C://aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.1 Sandpit/Velogames/")  

# Download and html parse wikipedia table for 2017 Giro
giro_2017_stages_url <- "http://en.wikipedia.org/wiki/2017_Giro_d'Italia"
assert_that(download.file(giro_2017_stages_url, "giro_2017_stages_url.xml"))   
stages_html <- htmlParse("giro_2017_stages_url.xml")

# Table titles - aren't much use due to weird image column at column 5 and empty 'Winner' column at end
column_titles <- xpathApply(stages_html, "//table[@class='wikitable']/tr[1]/th", xmlValue)


# Convert html table into neat table
giro_stages <- readHTMLTable(stages_html)[2] %>% as.data.frame()
colnames(giro_stages) <- c("Stage", "Date", "Course", "Distance", "empty1", "Type", "empty2")
giro_stages <- giro_stages %>% filter(!is.na(distance)) %>% select(1:4,6)

# Convert dates into proper format using 'lubridate'
giro_stages$Date <- dmy(paste(giro_stages$Date, " 2017", sep = ""))

# Convert 'start-finish' column into 
grep("-", x = giro_stages$location[2])
grep(" ", x = giro_stages$`start-finish`[1], ignore.case = TRUE, value = TRUE )


View(giro_stages)


