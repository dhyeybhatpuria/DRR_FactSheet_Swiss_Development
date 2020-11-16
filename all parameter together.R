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
library(reshape2)
library(ggplot2)

addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}

emdat_public <- read.csv("D:/Github_SEI/R/SDC_DRR_TEAM/emdat_public_1.csv", stringsAsFactors=TRUE)

emdat_public[is.na(emdat_public)] <- 0
names(emdat_public)[names(emdat_public) == "Country name"] <- "Country"
years = unique(emdat_public$Year)
decades <- seq(min(years)-10, max(years), by=10)
rm(years)
emdat_public$decade<- decades[findInterval(emdat_public$Year, decades)]
rm(decades)
# countries = unique(emdat_public$Country)
# row.names.remove <- c("Brunei Darussalam", "Timor-Leste", "Singapore")
emdat_public<-emdat_public[!(emdat_public$Country=="Timor-Leste"),]
emdat_public<-emdat_public[!(emdat_public$Country=="Brunei Darussalam"),]
emdat_public<-emdat_public[!(emdat_public$Country=="Singapore"),]
emdat_public<-emdat_public[!(emdat_public$Country=="Viet Nam"),]
emdat_public<-emdat_public[!(emdat_public$Country=="Philippines (the)"),]
emdat_public<-emdat_public[!(emdat_public$Country=="Singapore"),]
emdat_public<-emdat_public[!(emdat_public$Country=="Indonesia"),]
emdat_public<-emdat_public[!(emdat_public$Country=="Malaysia"),]
emdat_public<-emdat_public[!(emdat_public$Country=="Thailand"),]

emdat_public$Country[emdat_public$Country == "Lao People's Democratic Republic (the)"] <- "Lao PDR"

output <- emdat_public %>% 
  group_by(decade, Country) %>% 
  summarize_if(is.numeric, sum, na.rm=TRUE)

output$decade[output$decade == "1960"] <- "1960-1970"
output$decade[output$decade == "1970"] <- "1970-1980"
output$decade[output$decade == "1980"] <- "1980-1990"
output$decade[output$decade == "1990"] <- "1990-2000"
output$decade[output$decade == "2000"] <- "2000-2010"
output$decade[output$decade == "2010"] <- "2010-2020"
output$decade[output$decade == "2020"] <- "2020"

output = subset(output, select =-c(Total.Damages...000.US..,CPI, Year) )
output = subset(output, select =-c(Total.Deaths) )

output = output[-c(53:59), ]
# output = subset(output1, variable!="CPI") # Remove all unnecessary parts
ggplot(output, aes(fill=Disaster.Type, y=Total.Affected, x=decade)) + 
  geom_bar(position="stack", stat="identity") +
  scale_color_brewer(palette = "Paired") +
  # a_flat_fill() +
  scale_y_continuous(labels = addUnits) +
  labs(y='Number of people',
       x='Decades',
       title= 'No of People Affected in SE Asia (1960-2020)',
       caption='source: EMDAT',
       fill='') 




output1 <- melt(output, id.vars=c("Country","decade"))

ggplot(data = output1, aes(decade,value,color = variable)) +
  geom_line(size = 1) +
  geom_point() + 
  labs(title = "Decadal Impact of Natural Disasters",
       y = "Number", x = "Years in decades") + 
  facet_grid(. ~ Country) + scale_y_continuous(labels = addUnits)
