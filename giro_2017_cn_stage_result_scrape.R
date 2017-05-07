

# Code for downloading stage result data from Cycling News

require(XML)
setwd("/home/a_friend/data_analysis/projects/velogames/")

# Extract stage results from Cycling News
stage_no <- 3

stage_url <- paste("http://www.cyclingnews.com/races/giro-ditalia-2016/stage-", stage_no, "/results/", sep = "")
download.file(stage_url, "stage_url.xml")
stage_html <- htmlParse("stage_url.xml")

# Extract table names/captions
table_captions <- c("Stage Result", unlist(xpathApply(stage_html, "//table/caption", xmlValue)))
# Find location (number) of tables we need for summary (stage result, GC, stage points and overall points)
gc_table_location <- grep("^General Classification", table_captions, fixed = FALSE, ignore.case = TRUE)
points_table_location <- grep("^Points", table_captions, fixed = FALSE, ignore.case = TRUE)
target_tables <- c(1, gc_table_location, points_table_location)

for(tbl in target_tables){
  count <- count + 1
  print(table_captions[tbl])
  stage_table <- as.data.frame(readHTMLTable(stage_html)[tbl])
  colnames(stage_table) <-  as.factor(unlist(stage_table[1,]))
  stage_table <- stage_table[-1, ]
  stage_table <- stage_table[1:10, 1:3]
  print(knitr::kable(stage_table))
}

?print
# Stage finishing positions
stage_table <- as.data.frame(readHTMLTable(stage_html)[1])
colnames(stage_table) <-  as.factor(unlist(stage_table[1,]))
stage_table <- stage_table[-1, ]
stage_table <- stage_table[1:10, 1:3]
View(stage_table)

# Points
points_table <- as.data.frame(readHTMLTable(stage_html)[2])
colnames(points_table) <-  as.factor(unlist(points_table[1,]))
points_table <- points_table[-1, ]
points_table <- points_table[1:5, 1:3]
View(points_table)

# GC Positions
gc_table <- as.data.frame(readHTMLTable(stage_html)[3])
colnames(gc_table) <-  as.factor(unlist(gc_table[1,]))
gc_table <- gc_table[-1, ]
gc_table <- gc_table[1:10, 1:3]
View(gc_table)





