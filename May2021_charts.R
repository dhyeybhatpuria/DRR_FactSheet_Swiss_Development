
source("Functions and Packages.R")
emdat_public <- read.csv("emdat_public_1.csv")
cleaned_emdat = basic_datacleaning(emdat_public)
options(scipen=10000)
plotheight = 4
plotwidth = 7
resolutionset = 200
library("tidyverse")
library(scales)

# xaxisbrraks = c(1965,1975,1985,1995,2005,2015)
xaxislabels <- c("1960-1970", "1970-1980", "1980-1990","1990-2000", "2000-2010", "2010-2020")

#####
cleaned_emdat1 = basic_datacleaning(emdat_public)
output=cleaned_emdat1[!(cleaned_emdat1$decade=="2020"),]

output <- output %>% 
  group_by(decade) %>%  #group_by(decade, Country, Disaster.Type) %>% tally()
  summarize_if(is.numeric, sum, na.rm=TRUE)

output$year = output$decade
output[1, "year"] <- "1965"
output[2, "year"] <- "1975"
output[3, "year"] <- "1985"
output[4, "year"] <- "1995"
output[5, "year"] <- "2005"
output[6, "year"] <- "2015"
########

# selected COUNTRIES START
cleaned_emdat1 = basic_datacleaning(emdat_public)
output=cleaned_emdat1[!(cleaned_emdat1$decade=="2020"),]

output <- output %>% 
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
  labs(title = "People affected due to Disasters (Myanmar, Cambodia & Lao PDR)",
       subtitle = "People requiring immediate assistance during a period of emergency, i.e. requiring \nbasic survival needs such as food, water, shelter, sanitation and immediate medical assistance.",
       caption = "Data source: EMDAT") +
  geom_line( size = 1,color="#ffa563") +
  geom_point(shape=21, color="black", fill="#ff7b1c", size=6) +
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
  labs(title = "Economic loss due to Disasters (Myanmar, Cambodia & Lao PDR)",
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
# selected COUNTRIES START




# WHOLE MEKONG COUNTRIES START
cleaned_emdat1 = basic_datacleaning_Mekong(emdat_public)
output=cleaned_emdat1[!(cleaned_emdat1$decade=="2020"),]

output <- output %>% 
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
  # geom_line(size = 1, color = "#D8A7B1") +
  geom_point(shape = 16, size = 2.5)+ ylab("number of persons") + xlab("Years") +
  scale_y_continuous(labels = addUnits) +
  scale_x_continuous(breaks = seq(1965,2020,by=10), labels = xaxislabels) +
  # scale_x_discrete(breaks = seq(1965,2020,by=10), labels = xaxislabels) +
  labs(title = "People affected due to Disasters (Lower Mekong Countries)",
       subtitle = "People requiring immediate assistance during a period of emergency, i.e. requiring \nbasic survival needs such as food, water, shelter, sanitation and immediate medical assistance.",
       caption = "Data source: EMDAT") +
  geom_line( size = 1, color="#b87ff5") +
  geom_point(shape=21, color="black", fill="#8526eb", size=6) +
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
  labs(title = "Economic loss due to Disasters (Lower Mekong Countries)",
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
# WHOLE MEKONG COUNTRIES START

# WHOLE SOUTH EAST ASIA COUNTRIES START
cleaned_emdat1 = basic_datacleaning_SEA(emdat_public)
output=cleaned_emdat1[!(cleaned_emdat1$decade=="2020"),]

output <- output %>% 
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
  labs(title = "People affected due to Disasters (South East Asia)",
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
  labs(title = "Economic loss due to Disasters (South East Asia)",
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


