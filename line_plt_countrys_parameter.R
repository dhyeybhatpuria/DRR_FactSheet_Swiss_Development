# Number of deaths by country
source("Functions and Packages.R")
emdat_public <- read.csv("emdat_public_1.csv")
cleaned_emdat = basic_datacleaning(emdat_public)
options(scipen=10000)

output <- cleaned_emdat %>% 
  group_by(decade, Country) %>% 
  summarise(tot_death = sum(Total.Deaths))
output=output[!(output$decade=="2020"),]

ggplot(output, aes(x=factor(decade), y=tot_death,color=Country, group = Country)) + #geom_line(data=dfr[!is.na(dfr$y),])
  geom_line(size = 1, linetype="twodash") +
  geom_point(shape = 16, size = 2.5) +
  scale_y_continuous(labels = addUnits, limits = c(0, 1500)) +
  xlab("Years") + ylab("number of Deaths")
