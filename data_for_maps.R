# data for maps

source("Functions and Packages.R")
emdat_public <- read.csv("emdat_public_1.csv")
cleaned_emdat = basic_datacleaning_Mekong(emdat_public)

output <- cleaned_emdat %>% 
  group_by(Country) %>% #tally() #group_by(decade, Country, Disaster.Type) %>% tally()
summarize_if(is.numeric, sum, na.rm=TRUE)

write.csv(output,"country_wise_pie_affected_ALLYEARS.csv")


