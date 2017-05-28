require(dplyr)

setwd("/home/a_friend/data_analysis/projects/velogames/")
league_codes <- read.csv("2017_giro_velogames_leagues.csv")

l <- 4

for(l in 1:4){
  # Download league specific giro master table
  league_code <- league_codes$league_no[l]
  giro_table_master <- read.csv(paste("giro_table_master_", league_code, ".csv", sep = ""))
  giro_table_master <- giro_table_master %>% 
    arrange(stage) %>% 
    filter(stage %in% 1:6)
  write.csv(giro_table_master, file = paste("giro_table_master_", league_code, ".csv", sep = ""), 
           sep = ",", row.names = FALSE, col.names = TRUE)
  
  # View(giro_table_master)
}    # End FOR loop.