
source("Functions and Packages.R")

library("tidyverse")
library(scales)

# whole world excluding SEA
emdat_public_global <- read.csv("emdat_public_worldlevel.csv") 

emdat_public_global[is.na(emdat_public_global)] <- 0
names(emdat_public_global)[names(emdat_public_global) == "Country name"] <- "Country"
names(emdat_public_global)[names(emdat_public_global) == "ï..Year"] <- "Year"
years = unique(emdat_public_global$Year)
decades <- seq(min(years)-10, max(years), by=10)
# rm(years)
emdat_public_global$decade<- decades[findInterval(emdat_public_global$Year, decades)]
# rm(decades)
emdat_public_global<-emdat_public_global[!(emdat_public_global$Region=="South-Eastern Asia"),]

emdat_public_global$decade[emdat_public_global$decade == "1960"] <- "1960-1970"
emdat_public_global$decade[emdat_public_global$decade == "1970"] <- "1970-1980"
emdat_public_global$decade[emdat_public_global$decade == "1980"] <- "1980-1990"
emdat_public_global$decade[emdat_public_global$decade == "1990"] <- "1990-2000"
emdat_public_global$decade[emdat_public_global$decade == "2000"] <- "2000-2010"
emdat_public_global$decade[emdat_public_global$decade == "2010"] <- "2010-2020"
emdat_public_global$decade[emdat_public_global$decade == "2020"] <- "2020"
# whole world excluding SEA


emdat_public_global=emdat_public_global[!(emdat_public_global$decade=="2020"),]

output <- emdat_public_global %>% 
  group_by(decade) %>%  #group_by(decade, Country, Disaster.Type) %>% tally()
  summarize_if(is.numeric, sum, na.rm=TRUE)

output$year = output$decade
output[1, "year"] <- "1965"
output[2, "year"] <- "1975"
output[3, "year"] <- "1985"
output[4, "year"] <- "1995"
output[5, "year"] <- "2005"
output[6, "year"] <- "2015"
output$year = as.numeric(as.character(output$year))
gg = ggplot(output, aes(x=year, y=No.Affected), group = 1, color=continent) + #geom_line(data=dfr[!is.na(dfr$y),])
  geom_line(size = 1, color = "#D8A7B1") +
  geom_point(shape = 16, size = 2.5)+ ylab("number of persons") + xlab("Years") +
  scale_y_continuous(labels = addUnits) +
  scale_x_continuous(breaks = seq(1965,2020,by=10), labels = xaxislabels) +
  # scale_x_discrete(breaks = seq(1965,2020,by=10), labels = xaxislabels) +
  labs(title = "People affected due to Disasters (World excl. SE Asia)",
       subtitle = "People requiring immediate assistance during a period of emergency, i.e. requiring \nbasic survival needs such as food, water, shelter, sanitation and immediate medical assistance.",
       caption = "Data source: EMDAT") +
  geom_line( size = 1,color="#03cafc") +
  geom_point(shape=21, color="black", fill="#0390fc", size=6) +
  theme_minimal() +#t heme_light() #theme_minimal()
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 10)
  )
plot(gg)

gg = ggplot(output, aes(x=year, y=Total.Damages...000.US..), group = 1, color=continent) + #geom_line(data=dfr[!is.na(dfr$y),])
  # geom_line(size = 1, color = "#D8A7B1") +
  geom_point(shape = 16, size = 2.5)+ ylab("USD ('000)") + xlab("Years") +
  scale_y_continuous(labels = addUnits) +
  scale_x_continuous(breaks = seq(1965,2020,by=10), labels = xaxislabels) +
  # scale_x_discrete(breaks = seq(1965,2020,by=10), labels = xaxislabels) +
  labs(title = "Economic loss due to Disasters (World excl. SE Asia)",
       subtitle = "The amount of damage to property, crops, and livestock. In EM-DAT estimated damage are given in US$ (‘000). For each disaster, the \nregistered figure corresponds to the damage value at the moment of the event, i.e. the figures are shown true to the year of the event.",
       caption = "Data source: EMDAT") +
  geom_line( size = 1, color="#5aed5f") +
  geom_point(shape=21, color="black", fill="#08c40e", size=6) +
  theme_minimal() +#t heme_light() #theme_minimal()
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 10)
  )
plot(gg)
# WHOLE SOUTH EAST ASIA COUNTRIES START