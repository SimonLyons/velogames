require(plotly)
require(XML)
require(dplyr)

setwd("/home/a_friend/data_analysis/projects/velogames/2016 Giro - Cunning Stunts/data/")
getwd()
velo_url <- "https://www.velogames.com/giro-ditalia/2016/leaguescores.php?league=2010629"

for (s in 1:22){   # The final bonus points are included on the fictional Stage 22
  # Access velogames league webpage for stages 1 through 21 
  # for the Cunning Stunts 2016 Giro competition
  velo_url <- paste("https://www.velogames.com/giro-ditalia/2016/leaguescores.php?league=2010629&ga=13&st=", s, sep = "")
  # Using this as straight htmlParse of the link isn't working
  download.file(velo_url, "velo_url.xml")   
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
write.csv(velo_table, file = paste("giro_stage_", formatC(s, width=2, flag="0"), ".csv", sep = ""), row.names = FALSE)
}   # End FOR loop for number of stages

################

require(XML)

n_teams <- nrow(velo_table)
giro_table_master <- c()
setwd("/home/a_friend/data_analysis/projects/velogames/2016 Giro - Cunning Stunts/data/")
velo_table <- read.csv("giro_stage_01.csv", header = TRUE, sep = ",")

team_table_master_02 <- c()

for (t in 1:n_teams){   # Loop through each of the 11 teams
  team_link <- velo_table$team_weblink[t]
  team <- velo_table$team[t]
  directeur <- velo_table$directeur[t]
  team_table_master <- c()
  
  for (s in 1:22){   # Loop through all of the stages for each team. Stage 22 gives bonus points!
    team_url <- paste("https://www.velogames.com/giro-ditalia/2016/", team_link, "&ga=13&st=", s, sep = "")
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

giro_table_master <- do.call(rbind, team_table_master_02)[ , -14]
write.csv(giro_table_master, file = "giro_table_master.csv", sep = ",", row.names = FALSE, col.names = TRUE)
View(giro_table_master)
#####################

# Open .csv file with Cunning Stunts Giro 2016 data
setwd("/home/a_friend/data_analysis/projects/velogames/2016 Giro - Cunning Stunts/data/")
giro_table_master <- read.csv(file = "giro_table_master.csv", sep = ",")
names(giro_table_master)
giro_table_master[giro_table_master$stage == 1, c("directeur", "Tot")]

# Perform the above filter using dPlyr
dtest <- giro_table_master %>% 
  filter(stage == 1) %>% 
  select(directeur, Tot) %>% 
  n()





# Build list of riders
uniq_riders <- unique(giro_table_master$rider)
length(uniq_riders)


which_team_rider <- giro_table_master %>% 
  filter(rider=="Johan Esteban Chaves") %>% 
  distinct(directeur)
which_team_rider


# Function to calculate the total score for a particular rider
rider_total <- function(rider){
  score <- sum(giro_table_master[giro_table_master$rider == rider, ][!duplicated(giro_table_master[giro_table_master$rider == rider, ]$stage) , ]$Tot)
  # rider_scores <- rbind(rider, score)
  # return(rider_scores)
}

# Create dataframe with rider names and their scores, ordered from highest score to lowest.
rider_scores <- as.numeric(lapply(uniq_riders, rider_total))
df <- as.data.frame(matrix(data = NA, nrow = 52, ncol = 2))
df[ ,1] <- uniq_riders
df[ ,2] <- rider_scores
colnames(df) <- c("rider", "total_score")
df <- df[rev(order(df$total_score)), ]
View(df)


rider_scores_dplyr <- giro_table_master %>% 
  group_by(rider) %>% 
  summarise(sum(Tot)) %>%
  arrange(desc(`sum(Tot)`)) 
View(rider_scores_dplyr)


require(ggplot2)
ggplot(data = rider_scores_dplyr[1:10, ], aes(x = factor(rider_scores_dplyr$rider[1:10]), y = rider_scores_dplyr$`sum(Tot)`[1:10])) + geom_bar(stat = "identity", fill = "blue") + scale_x_discrete(limits = rider_scores_dplyr$rider[1:10]) + xlab("Rider") + ylab("Score") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

rider_value_dplyr <- giro_table_master %>% 
  group_by(rider) %>% 
  mutate(ROI = Tot / Cost) %>% 
  summarise(sum(ROI)) %>% 
  arrange(desc(`sum(ROI)`))

rider_cost <- giro_table_master %>% 
  filter(stage == 1) %>% 
  group_by(rider) %>% 
  summarise(n()) %>% 
  arrange(desc(`n()`))
rider_cost[1:12,]

require(knitr)

ggplot(data = rider_value_dplyr[1:10, ], aes(x = factor(rider_value_dplyr$rider[1:10]), y = rider_value_dplyr$`sum(ROI)`[1:10])) + geom_bar(stat = "identity", fill = "blue") + scale_x_discrete(limits = rider_value_dplyr$rider[1:10]) + xlab("Rider") + ylab("ROI") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Team scores by stage
team_scores <- giro_table_master %>% 
  group_by(Stg, Team) %>% 
  # summarise(Tot)

knitr::kable(team_scores)
