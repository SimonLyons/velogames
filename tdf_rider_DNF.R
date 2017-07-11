

require(rvest)
require(dplyr)

setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.6 Projects/velogames/")
setwd("/home/a_friend/data_analysis/projects/velogames/")

# Wikipedia list of 2017 TDF riders
wiki_tdf_2017 <- "https://en.wikipedia.org/wiki/List_of_teams_and_cyclists_in_the_2017_Tour_de_France"
# Scrape web data
download.file(wiki_tdf_2017, "wiki_tdf_2017.xml")
wiki_tdf_2017_parse <- read_html("wiki_tdf_2017.xml")

# Extract table with riders
riders_tdf <- wiki_tdf_2017_parse %>% 
  html_nodes(xpath = "//table") %>% 
  html_table(fill = TRUE)
riders_tdf <- as.data.frame(riders_tdf[3])

# Filter DNF riders
riders_DNF <- riders_tdf %>% 
  filter(!Pos. == "") %>% 
  select(Name, Nationality, Team, Pos.)

# Print (kable)
print(kable(riders_DNF), row.names = FALSE)



