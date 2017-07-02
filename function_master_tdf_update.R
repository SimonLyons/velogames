#########################################################
# Created to webscrape new stage data and combine with
# the exsiting master table

require(XML)
require(dplyr)

setwd("/home/a_friend/data_analysis/projects/velogames/")
league_codes <- read.csv("2017_tdf_velogames_leagues.csv")

# Ask for an input
# Should consider turning this file into a FUNCTION
# input_stage <- as.integer(readline(prompt = "Enter the latest stage you would like to webscrape:"))
input_stage <- 2

for(l in 1:3){
  # Download league specific tdf master table
  league_code <- league_codes$league_no[l]
  tdf_table_master <- read.csv(paste("tdf_table_master_", league_code, ".csv", sep = ""))
  
  # Check the latest stage number in the master table file
  latest_stage <- max(tdf_table_master$stage)

  #  Next - scrape data for new stages.
  # If the input from the user is larger than the maximum stage in the master table then assign
  # a list of stages to scrape
  if(latest_stage < input_stage){  
    stages_to_scrape <- (latest_stage+1):input_stage
    
    require(XML)
    n_teams <- n_distinct(tdf_table_master$directeur) 
    team_table_master_02 <- c()
    velo_table <- read.csv(paste("league_details_", league_code, ".csv", sep = ""), header = TRUE, sep = ",")
    
    for (t in 1:n_teams){   # Loop through each of the teams
      team_link <- velo_table$team_weblink[t]
      team <- velo_table$team[t]
      directeur <- velo_table$directeur[t]
      team_table_master <- c()
      
      for (s in stages_to_scrape){   # Loop through all of the new stages (up to )
        team_url <- paste("https://www.velogames.com/tour-de-france/2017/", team_link, "&ga=13&st=", s, sep = "")
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
        }
        # Add the stage number to the dataframe, so it is included in the master list.
        team_table$stage <- s
        team_table$team_link <- team_link
        team_table$directeur <- directeur
        team_table$team <- team
        # Add dataframe to master list
        team_table_master[[s]] <- team_table
        
      }
      
      # Combine stage tables into single master dataframe
      team_table_master_01 <- do.call(rbind, team_table_master)
      # Remove weird column with no values
      team_table_master_01 <- team_table_master_01[ , -4]
      # Convert numerical columns into class 'numeric'
      for (i in 3:13){
        team_table_master_01[ , i] <- as.numeric(team_table_master_01[ , i])
      }
      team_table_master_02[[t]] <- team_table_master_01
      
    }
    
    tdf_table_master_new <- do.call(rbind, team_table_master_02)[ , -14]
    colnames(tdf_table_master_new) <- gsub("\n", "", colnames(tdf_table_master_new))
    colnames(tdf_table_master) <- gsub("\\.", "", colnames(tdf_table_master))
    
    # Combine newly scraped stages with existing master table file
    tdf_table_master <- rbind(tdf_table_master, tdf_table_master_new)
    # Remove any duplicate rows
    tdf_table_master <- tdf_table_master %>%  filter(!duplicated(tdf_table_master))
    # View(tdf_table_master)
  }   # End of IF statement confirming input stage is larger than max giro table master stage
  
  write.csv(tdf_table_master, file = paste("tdf_table_master_", league_code, ".csv", sep = ""), 
            sep = ",", row.names = FALSE, col.names = TRUE)
  
}   # End of loop through 3 velogames leagues



