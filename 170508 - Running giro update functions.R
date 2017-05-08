

# Code for running giro update functions


# Set working directory
setwd("C://aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.1 Sandpit/Velogames/giro_master_tables/")

# Read in league master .csv file
giro_table_master <- read.csv("giro_table_master_760208202214.csv")



# Function to generate a plot of each rider for their Total Points, Cost and Times Selected (by our Velogames squads)
rider_cost_total_times_selected(giro_table_master)


cs_cross_ref <- rider_cross_reference(giro_table_master)
View(cs_cross_ref)


write.csv(cs_cross_ref, file = "cs_rider_cross_reference.csv")



