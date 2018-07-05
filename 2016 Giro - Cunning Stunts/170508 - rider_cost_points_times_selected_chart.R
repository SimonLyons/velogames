

rider_cost_total_times_selected <- function(giro_table_master){
  
  # Load required packages
  require(dplyr)
  require(ggplot2)
  
  #####################################################################
  # Build a table with unique riders, times selected, total points and cost
  
  # First build a table with individual rider totals
  new_table <- giro_table_master %>% 
    select(rider, Team,  stage, Cost, "Total" = 12) %>% 
    distinct() %>% 
    group_by(rider, Team, Cost) %>% 
    summarise("Total" = sum(Total)) %>% 
    arrange(rider)   # Arrange by rider so order is matched to next table
  # View(new_table)
  
  
  # Next build table with the number of times each rider has been selected
  rider_times_selected <- giro_table_master %>% 
    filter(stage == 1) %>% 
    group_by("Rider" = rider) %>% 
    summarise("Times Selected" = n()) %>% 
    arrange(Rider)   # Again arrange by rider to match order from previous table
  # View(rider_times_selected)
  
  # Add times selected column from 2nd table to 1st table
  new_table[ , 5] <- rider_times_selected$`Times Selected`
  colnames(new_table)[5] <- "Times_Selected"
  
  # Create plot showing Total Points, Cost and Times Selected for each rider
  new_plot <- ggplot(data = new_table, aes(x = Cost, y = Total)) +
    geom_point(aes(size = Times_Selected, colour = "blue")) + 
    geom_text(data = subset(new_table, (Total > 50) | (Times_Selected >2)) ,   aes(label=rider),hjust=0, vjust=0, size = 4) + 
    theme(legend.position="bottom") + scale_size(range = c(2,12)) + 
    labs(title = "Rider Points vs Cost",   y = "Total Rider Points") 
  return(new_plot)
  
  
  #####################################################################
  
}
