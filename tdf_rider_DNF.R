


require(rvest)
require(dplyr)

setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.6 Projects/velogames/")

wiki_tdf_2017 <- "https://en.wikipedia.org/wiki/List_of_teams_and_cyclists_in_the_2017_Tour_de_France"


download.file(wiki_tdf_2017, "wiki_tdf_2017.xml")
wiki_tdf_2017_parse <- read_html("wiki_tdf_2017.xml")


riders_tdf <- wiki_tdf_2017_parse %>% 
  html_nodes(xpath = "//table") %>% 
  html_table(fill = TRUE)
riders_tdf <- riders_tdf[3]
glimpse(riders_tdf)



riders_DNF <- riders_tdf %>% 
  filter(Time == "")


riders_tdf$Pos.



