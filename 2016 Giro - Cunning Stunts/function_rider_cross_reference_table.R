
rider_cross_reference <- function(giro_table_master){
  # Load in dplyr
  require(dplyr)
  
  # Build a list of the riders selected by the mini-league and the number
  # of times they have been selected
  rider_list <- giro_table_master %>% 
    filter(stage == 1) %>% 
    group_by(rider) %>% 
    summarise("Times_Selected" = n()) %>% 
    arrange(desc(Times_Selected))
  
  # Build a list of the directeurs in the mini-league
  directeur_list <- giro_table_master %>% 
    select(directeur) %>% 
    distinct() %>% 
    arrange(directeur)
  
  # Now create the cross-reference matrix which shows which riders have been selected by which directeurs
  for (d in 1:length(directeur_list$directeur)){   # FOR loop to run through the list of team directeurs
    rider_list[ , d+2] <- NA
    colnames(rider_list)[(d+2)] <- toString(directeur_list$directeur[d])
    
    # Build list of team rider for specific team directeur 'd'
    team_list <- giro_table_master %>% 
      filter(directeur == directeur_list$directeur[d]) %>% 
      select(rider) %>% 
      distinct()
    
    # Next, for each directeur, run through the complete list of mini-league rideres
    # and check to see if they are in 
    for (r in 1:length(rider_list$rider)){   # FOR loop to run through the total list of riders in the mini-league
      if(unlist(rider_list[r, 1]) %in% unlist(team_list)){
        rider_list[r, d+2] <- "Yes"
      } else  # End IF statement checking to see if rider exists in the directeur's team
      {rider_list[r, d+2] <- ""}
    }   # End FOR loop 'r'
  }   # END FOR loop running through all of the directeurs
  
  return(rider_list)
  
}



