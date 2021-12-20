# https://austinwehrwein.com 
# Extract  total affected and total damage; 
# total (Southeast Asia) then Cambodia, Laos and Myanmar

source("Functions and Packages.R")
emdat_public <- read.csv("emdat_public_1.csv")
cleaned_emdat = basic_datacleaning_SEA(emdat_public)

# VIS 1
output <- cleaned_emdat %>% 
  group_by(decade, Disaster.Subgroup) %>% tally() #group_by(decade, Country, Disaster.Type) %>% tally()
# summarize_if(is.numeric, sum, na.rm=TRUE)
# Removes all 2020 onwards data
output<-output[!(output$decade=="2020"),]

# Grouped
ggplot(output, aes(fill=Disaster.Subgroup, y=n, x=decade)) + 
  geom_bar(position=position_dodge(), stat="identity",color="black") +# ,position="stack"   #facet_grid(. ~ Country) +  
  scale_color_brewer(palette = "Paired") +
  # a_flat_fill() +
  labs(y='number of times',
       x='Decades',
       title= 'Disaster Occurance in SE Asia (1960-2020)',
       caption='source: EMDAT',
       fill='')


cleaned_emdat = basic_datacleaning_Mekong(emdat_public)
# VIS 1
output <- cleaned_emdat %>% 
  group_by(decade, Disaster.Subgroup) %>% tally() #group_by(decade, Country, Disaster.Type) %>% tally()
# summarize_if(is.numeric, sum, na.rm=TRUE)
# Removes all 2020 onwards data
output<-output[!(output$decade=="2020"),]

# Grouped
ggplot(output, aes(fill=Disaster.Subgroup, y=n, x=decade)) + 
  geom_bar(position=position_dodge(), stat="identity",color="black") +# ,position="stack"   #facet_grid(. ~ Country) +  
  scale_color_brewer(palette = "Paired") +
  # a_flat_fill() +
  labs(y='number of times',
       x='Decades',
       title= 'Disaster Occurance in Lower Mekong Countries (1960-2020)',
       caption='source: EMDAT',
       fill='')


cleaned_emdat = basic_datacleaning(emdat_public)
# VIS 1
output <- cleaned_emdat %>% 
  group_by(decade, Disaster.Subgroup) %>% tally() #group_by(decade, Country, Disaster.Type) %>% tally()
# summarize_if(is.numeric, sum, na.rm=TRUE)
# Removes all 2020 onwards data
output<-output[!(output$decade=="2020"),]

# Grouped
ggplot(output, aes(fill=Disaster.Subgroup, y=n, x=decade)) + 
  geom_bar(position=position_dodge(), stat="identity",color="black") +# ,position="stack"   #facet_grid(. ~ Country) +  
  scale_color_brewer(palette = "Paired") +
  # a_flat_fill() +
  labs(y='number of times',
       x='Decades',
       title= 'Disaster Occurance in Myanmar, Cambodia & Laos PDR (1960-2020)',
       caption='source: EMDAT',
       fill='')
# 
# 
# # World
# # whole world excluding SEA
# emdat_public_global <- read.csv("emdat_public_worldlevel.csv") 
# 
# emdat_public_global[is.na(emdat_public_global)] <- 0
# names(emdat_public_global)[names(emdat_public_global) == "Country name"] <- "Country"
# names(emdat_public_global)[names(emdat_public_global) == "ï..Year"] <- "Year"
# years = unique(emdat_public_global$Year)
# decades <- seq(min(years)-10, max(years), by=10)
# # rm(years)
# emdat_public_global$decade<- decades[findInterval(emdat_public_global$Year, decades)]
# # rm(decades)
# emdat_public_global<-emdat_public_global[!(emdat_public_global$Region=="South-Eastern Asia"),]
# 
# emdat_public_global$decade[emdat_public_global$decade == "1960"] <- "1960-1970"
# emdat_public_global$decade[emdat_public_global$decade == "1970"] <- "1970-1980"
# emdat_public_global$decade[emdat_public_global$decade == "1980"] <- "1980-1990"
# emdat_public_global$decade[emdat_public_global$decade == "1990"] <- "1990-2000"
# emdat_public_global$decade[emdat_public_global$decade == "2000"] <- "2000-2010"
# emdat_public_global$decade[emdat_public_global$decade == "2010"] <- "2010-2020"
# emdat_public_global$decade[emdat_public_global$decade == "2020"] <- "2020"
# # whole world excluding SEA
# output <- emdat_public_global %>% 
#   group_by(decade, Disaster.Subgroup) %>% tally() #group_by(decade, Country, Disaster.Type) %>% tally()
# # summarize_if(is.numeric, sum, na.rm=TRUE)
# # Removes all 2020 onwards data
# output<-output[!(output$decade=="2020"),]
# 
# # Grouped
# ggplot(output, aes(fill=Disaster.Subgroup, y=n, x=decade)) + 
#   geom_bar(position=position_dodge(), stat="identity",color="black") +# ,position="stack"   #facet_grid(. ~ Country) +  
#   scale_color_brewer(palette = "Paired") +
#   # a_flat_fill() +
#   labs(y='number of times',
#        x='Decades',
#        title= 'Disaster Occurance in Myanmar, Cambodia & Laos PDR (1960-2020)',
#        caption='source: EMDAT',
#        fill='')
