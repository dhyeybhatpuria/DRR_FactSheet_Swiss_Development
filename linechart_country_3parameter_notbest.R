# Number of deaths by country
source("Functions and Packages.R")
emdat_public <- read.csv("emdat_public_1.csv")
cleaned_emdat = basic_datacleaning(emdat_public)
options(scipen=10000)
plotheight = 4
plotwidth = 7
resolutionset = 100
library("tidyverse")

output<-cleaned_emdat[(cleaned_emdat$Country=="Myanmar"),]
output=output[!(output$decade=="2020"),]
output <- output %>% 
  group_by(decade) %>%  #group_by(decade, Country, Disaster.Type) %>% tally()
  summarize_if(is.numeric, sum, na.rm=TRUE)


output <- output %>%
  select(decade, No.Homeless, No.Injured, Total.Deaths) %>% # , No.Affected,
  gather(key = "variable", value = "value", -decade)

gg = ggplot(output, aes(x=factor(decade), y=value,color=variable, group = variable)) + #geom_line(data=dfr[!is.na(dfr$y),])
  geom_line(size = 1) +
  geom_point(shape = 16, size = 2.5) + xlab("Years") + ylab("number of persons") +
  theme(legend.title=element_blank())

tiff("mmyr_line_chart.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()


output<-cleaned_emdat[(cleaned_emdat$Country=="Cambodia"),]
output=output[!(output$decade=="2020"),]
output <- output %>% 
  group_by(decade) %>%  #group_by(decade, Country, Disaster.Type) %>% tally()
  summarize_if(is.numeric, sum, na.rm=TRUE)


output <- output %>%
  select(decade,No.Injured, Total.Deaths) %>% # , No.Affected, No.Homeless,
  gather(key = "variable", value = "value", -decade)

gg = ggplot(output, aes(x=factor(decade), y=value,color=variable, group = variable)) + #geom_line(data=dfr[!is.na(dfr$y),])
  geom_line(size = 1) +
  geom_point(shape = 16, size = 2.5) + xlab("Years") + ylab("number of persons") +
  # scale_y_continuous(labels = addUnits, limits = c(0, 57000))
  theme(legend.title=element_blank())

tiff("cambodia_line_chart.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()


output<-cleaned_emdat[(cleaned_emdat$Country=="Lao PDR"),]
output=output[!(output$decade=="2020"),]
output <- output %>% 
  group_by(decade) %>%  #group_by(decade, Country, Disaster.Type) %>% tally()
  summarize_if(is.numeric, sum, na.rm=TRUE)


output <- output %>%
  select(decade, No.Homeless, No.Injured, Total.Deaths) %>% # , No.Affected,
  gather(key = "variable", value = "value", -decade)

gg = ggplot(output, aes(x=factor(decade), y=value,color=variable, group = variable)) + #geom_line(data=dfr[!is.na(dfr$y),])
  geom_line(size = 1) +
  geom_point(shape = 16, size = 2.5) + xlab("Years") + ylab("number of persons") +
  theme(legend.title=element_blank())

tiff("lao_line_chart.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()
