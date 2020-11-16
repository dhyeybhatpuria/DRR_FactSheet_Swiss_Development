
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

emdat_public <- read.csv("D:/Github_SEI/R/SDC_DRR_TEAM/emdat_public.csv", stringsAsFactors=TRUE)
emdat_public[is.na(emdat_public)] <- 0
names(emdat_public)[names(emdat_public) == "Country name"] <- "Country"
years = unique(emdat_public$Year)
decades <- seq(min(years)-10, max(years), by=10)
rm(years)
emdat_public$decade<- decades[findInterval(emdat_public$Year, decades)]

countries = unique(emdat_public$Country)
countries
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

output <- emdat_public %>% 
  group_by(decade, Country) %>% 
  summarise(tot_death = sum(No.Affected))

# newsel = subset(output, output$Country == 'Cambodia'|output$Country == "Lao People's Democratic Republic (the)")#|output$Country == 'Myanmar')

library(ggplot2)
theme_set(theme_classic())

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

# 1
ggplot(output, aes(x=decade, y=tot_death, fill=Country)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Population affected by Natural Disasters") +
  facet_wrap(~Country) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("Years") + ylab("Total Affected") + scale_y_continuous(labels = addUnits)



# p<-ggplot(output, aes(x=decade, y=tot_death, group=Country)) +
#   geom_line(aes(color=Country))+
#   geom_point(aes(color=Country)) + scale_y_continuous(labels = addUnits) 
# p
# 
# 
# ggplot(output, aes(x=decade, y=tot_death, fill=Country)) + 
#   scale_fill_brewer(palette = "Paired") +
#   geom_bar(position="stack", stat="identity") + scale_y_continuous(labels = addUnits)+
#   labs(title = "Deaths due to natural disaster") + xlab("Year") + ylab("Total Deaths")
# 
# 





# # Histogram on a Continuous (Numeric) Variable
# g <- ggplot(output, aes(decade)) + scale_fill_brewer(palette = "Spectral")
# 
# g + geom_histogram(aes(fill=tot_death), 
#                    bins=5, 
#                    col="black", 
#                    size=.1) +   # change number of bins
#   labs(title="Histogram with Fixed Bins", 
#        subtitle="Engine Displacement across Vehicle Classes")



# library(CGPfunctions)
# 
# newsel = subset(output, output$`Disaster Type` == 'Drought')
# newsel = subset(output, output$Country == 'Cambodia'|output$Country =='Laos')#|output$Country == 'Myanmar')
# newsel <- na.omit(output) 
# 
# newsel$decade = factor(newsel$decade, levels = decades, ordered = TRUE)
# newggslopegraph(newsel, decade, tot_death, Country) +
#   labs(title="Total Deaths by Country (Drought)", 
#        subtitle="South East Asia", 
#        caption="source: EMDAT")





# 
# conv_decade = output %>% 
#   mutate(decade = floor(Year/10)*10) %>% 
#   group_by(Year, `Disaster Type`, Country) %>% 
#   group_by(decade) %>% 
#   # summarise(deaths = sum(tot_death)) %>%
#   summarize_all(sum) %>%
#   select(-Year)
# 
# # years = unique(output$Year)
# decades <- seq(1960, 2020, by=10)
# output$decade<- decades[findInterval(output$Year, decades)]
# aa = aggregate(cbind(`Disaster Type`, Country, tot_death) ~ decade , data = output, sum)
# 
# 

