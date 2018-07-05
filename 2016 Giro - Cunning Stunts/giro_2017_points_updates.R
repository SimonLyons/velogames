

setwd("C://aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.1 Sandpit")

require(XML)
require(dplyr)
require(assertthat)

league_code <- 3020825
setwd("C://aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.1 Sandpit/Velogames/")  

# Step 1: First do initial download of basic league and team data

# Set link to general league web page
velo_url <- paste("https://www.velogames.com/giro-ditalia/2016/leaguescores.php?league=", league_code, sep = "")
assert_that(RCurl::url.exists(velo_url))

# Using this as straight htmlParse of the link isn't working
assert_that(download.file(velo_url, "velo_url.xml"))   
race_html <- htmlParse("velo_url.xml")

# Extract 'points', 'team' and 'directeur' details from general webpage for league
points <- xpathApply(race_html, "//span/p/b", xmlValue)   # points extraction
points <- gsub(" points", "", points)
team <- xpathApply(race_html, "//h3/a", xmlValue)   # Team name extraction
directeur <- xpathApply(race_html, "//li/p", xmlValue)   # Extract name of the manager/directeur
team_weblink <- xpathApply(race_html, "//h3/a", xmlAttrs)
team_weblink <- gsub("href", "", team_weblink)
velo_table <- as.data.frame(cbind(points, team, directeur, team_weblink))   

velo_table[, 1] <- as.numeric(velo_table[,1])
velo_table[ ,2] <- as.character(velo_table[ ,2])
velo_table[ ,3] <- as.character(velo_table[ ,3])
velo_table[ ,4] <- as.character(velo_table[ ,4])

# Write 'velo_table' to local CSV file
write.csv(velo_table, file = paste("league_details", league_code, ".csv", sep = ""), row.names = FALSE)

# Step 2: Download league points for Stage 1
# url link for Stage 1
velo_url <- paste("https://www.velogames.com/giro-ditalia/2016/leaguescores.php?league=3020825&ga=13&st=", 1, sep = "")
# Using this as straight htmlParse of the link isn't working
assert_that()
assert_that(download.file(velo_url, "velo_url.xml"))   
race_html <- htmlParse("velo_url.xml")
# Extract 'points', 'team' and 'directeur' details from webpage for league
points <- xpathApply(race_html, "//span/p/b", xmlValue)   # points extraction
points <- gsub(" points", "", points)
team <- xpathApply(race_html, "//h3/a", xmlValue)   # Team name extraction
directeur <- xpathApply(race_html, "//li/p", xmlValue)   # Extract name of the team manager/directeur
team_weblink <- xpathApply(race_html, "//h3/a", xmlAttrs)
team_weblink <- gsub("href", "", team_weblink)
velo_table <- as.data.frame(cbind(points, team, directeur, team_weblink))   

velo_table[, 1] <- as.numeric(velo_table[,1])
velo_table[ ,2] <- as.character(velo_table[ ,2])
velo_table[ ,3] <- as.character(velo_table[ ,3])
velo_table[ ,4] <- as.character(velo_table[ ,4])
velo_table[ ,5] <- 1
colnames(velo_table)[5] <- "stage"
write.csv(velo_table, file = paste("league_points_master_", league_code, ".csv", sep = ""), row.names = FALSE)
View(velo_table)


###################################################
# Now that initial setup is complete, we'll now move
# to updating the points files based on the Stage in question
# 
# Step 3: Take specified Stage number and conduct updates to 
# league_points_master table and to giro_table_master table


# Enter in the relevant stage number here
stage_no <- 4

# Check for existence of 
if(file.exists("giro_table_master.csv")){
  giro_table_master <- read.csv("giro_table_master.csv")
  
  colnames(giro_table_master)
  max(giro_table_master$stage)
} 


else {print("No File")}
