##################################################
# This is the initial league and data setup script
# It takes details of the leagues and:
#    1. downloads the manager/team/weblink details for each teach
#    2. Build a master_table for each league with the first stage's results 

require(XML)
require(dplyr)
require(assertthat)
require(RCurl)

# Step 1: Load league details into R
# HP Laptop directory
setwd("/home/a_friend/data_analysis/projects/velogames/2018 - TDF")
# Work laptop directory
setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.6 Projects/velogames/")
league_codes <- read.csv("2018_TDF_velogames_leagues.csv")

# Step 2: For each league, extract the details for team, directeur and team_weblink
for(s in 1:2){   # Loop through each of the four (4) leagues
  league_code <- league_codes$league_no[s]
  velo_url <- paste("https://www.velogames.com/tour-de-france/2018/leaguescores.php?league=", league_code, sep = "")
  # assert_that(RCurl::url.exists(velo_url))

  # Using this as straight htmlParse of the link isn't working
  # assert_that(download.file(velo_url, "velo_url.xml"))   
  download.file(velo_url, "velo_url.xml")
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
  write.csv(velo_table, file = paste("league_details_", league_code, ".csv", sep = ""), row.names = FALSE)
  
}   # End loop through each league extracting head league details for each team.



############################
# DAILY UPDATE

# So far the below code will take a blanket approach to downloading stage results.
# I need to next look out how to append a master file with new data,
# to avoid having to download every stage, every day.

# Load required packages
require(XML)
require(dplyr)
require(assertthat)
require(RCurl)

# Step 1: Load league details into R
setwd("/home/a_friend/data_analysis/projects/velogames/2018 - TDF/")
league_codes <- read.csv("2018_TDF_velogames_leagues.csv")

for (l in 1:2){   # Loop through all two (2) leagues
  league_code <- league_codes$league_no[l]
  velo_table <- read.csv(paste("league_details_", league_code, ".csv", sep = ""))
  n_teams <- nrow(velo_table)
  TDF_table_master <- c()
  velo_table <- read.csv(paste("league_details_", league_code, ".csv", sep = ""), header = TRUE, sep = ",")
  
  team_table_master_02 <- c()
  
  for (t in 1:n_teams){   # Loop through each of the "t" teams
    team_link <- velo_table$team_weblink[t]
    team <- velo_table$team[t]
    directeur <- velo_table$directeur[t]
    team_table_master <- c()
    
    for (s in 1:1){   # Loop through all of the stages for each team. Stage 22 gives bonus points!
      team_url <- paste("https://www.velogames.com/tour-de-france/2018/", team_link, "&ga=13&st=", s, sep = "")
      download.file(team_url, "team_url.xml")
      team_html <- htmlParse("team_url.xml")
      # Extract table header data
      table_header <- gsub("\r\n", "", xpathApply(team_html, "//table[@class='responsive']/thead/th", xmlValue))
      table_header[[1]] <- "rider"   # Title for first column is missing. Insert 'rider' as title of column.
      # Extract body elements from the table
      rider <- gsub("\r\n", "", xpathApply(team_html, "//table[@class='responsive']/tbody/tr/td", xmlValue))
      
      # Create empty dataframe to receive table data.
      team_table <- data.frame(matrix(NA, nrow = 0, ncol = 13))
      colnames(team_table) <- table_header
      
      # Run loop to insert body elements into dataframe 
      for (r in 1:9){
        team_table[r, ] <- rider[(r*13-12):(r*13)]
      }   # End loop through the number of body elements to be inserted (r)
      
      # Add the stage number to the dataframe, so it is included in the master list.
      team_table$stage <- s
      team_table$team_link <- team_link
      team_table$directeur <- directeur
      team_table$team <- team
      # Add dataframe to master list
      team_table_master[[s]] <- team_table
      
    }   # End loop through the number of stage (s)
    
    # Combine stage tables into single master dataframe
    team_table_master_01 <- do.call(rbind, team_table_master)
    # Remove weird column with no values
    team_table_master_01 <- team_table_master_01[ , -4]
    # Convert numerical columns into class 'numeric'
    for (i in 3:13){
      team_table_master_01[ , i] <- as.numeric(team_table_master_01[ , i])
    }   # End loop through the number of numeric columns (i)
    team_table_master_02[[t]] <- team_table_master_01
    
  }   # End loop through the number of teams (t)
  TDF_table_master <- do.call(rbind, team_table_master_02)[ , -14]
  write.csv(TDF_table_master, file = paste("TDF_table_master_", league_code, ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)
  
  
}   # End loop through all "l" leagues
#####################



