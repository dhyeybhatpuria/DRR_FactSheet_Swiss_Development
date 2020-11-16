# # library
# library(treemap)
# 
# # Build Dataset
# group <- c(rep("group-1",4),rep("group-2",2),rep("group-3",3))
# subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep="-")
# value <- c(13,5,22,12,11,7,3,1,23)
# data <- data.frame(group,subgroup,value)
# data
# # treemap
# treemap(data,
#         index=c("group","subgroup"),
#         vSize="value",
#         type="index"
# ) 



# Extract  total affected and total damage; 
# total (Southeast Asia) then Cambodia, Laos and Myanmar

if (!require("readxl")) {
  install.packages("readxl")
  library(readxl)
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("readr")) {
  install.packages("readr")
  library(readr)
}
library(treemap)

emdat_public <- read.csv("D:/Github_SEI/R/SDC_DRR_TEAM/emdat_public.csv", stringsAsFactors=TRUE)
emdat_public[is.na(emdat_public)] <- 0
names(emdat_public)[names(emdat_public) == "Country name"] <- "Country"
years = unique(emdat_public$Year)
decades <- seq(min(years)-10, max(years), by=10)
rm(years)
emdat_public$decade<- decades[findInterval(emdat_public$Year, decades)]

# countries = unique(emdat_public$Country)

emdat_public<-emdat_public[!(emdat_public$Country=="Timor-Leste"),]
emdat_public<-emdat_public[!(emdat_public$Country=="Brunei Darussalam"),]
emdat_public<-emdat_public[!(emdat_public$Country=="Singapore"),]
# emdat_public<-emdat_public[!(emdat_public$Country=="Viet Nam"),]
emdat_public<-emdat_public[!(emdat_public$Country=="Philippines (the)"),]
emdat_public<-emdat_public[!(emdat_public$Country=="Singapore"),]
emdat_public<-emdat_public[!(emdat_public$Country=="Indonesia"),]
# emdat_public<-emdat_public[!(emdat_public$Country=="Malaysia"),]
# emdat_public<-emdat_public[!(emdat_public$Country=="Thailand"),]

emdat_public$Country[emdat_public$Country == "Lao People's Democratic Republic (the)"] <- "Lao PDR"

output <- emdat_public %>% 
  group_by(Disaster.Subgroup, Country)%>% summarise(tot_death = sum(Total.Deaths)) 
  # %>% tally()
treemap(output,
        index=c("Country","Disaster.Subgroup"),
        vSize="tot_death",
        type="index",
        fontsize.labels=c(12,10),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("white","black"),    # Color of labels
        fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels=c("transparent"),              # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ),                                   # Where to place labels in the rectangle?
        overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
        border.col=c("black","white"),             # Color of borders of groups, of subgroups, of subsubgroups ....
        border.lwds=c(3,1.5),
        palette = "Set2",                        # Select your color palette from the RColorBrewer presets or make your own.
        title="Total Death",                      # Customize your title
        fontsize.title=12,                       # Size of the title
) 
