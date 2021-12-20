# https://austinwehrwein.com 
# Extract  total affected and total damage; 
# total (Southeast Asia) then Cambodia, Laos and Myanmar

source("Functions and Packages.R")
emdat_public <- read.csv("emdat_public_1.csv")
cleaned_emdat = basic_datacleaning(emdat_public)

# VIS 1
output <- cleaned_emdat %>% 
  group_by(decade, Disaster.Subgroup) %>% tally() #group_by(decade, Country, Disaster.Type) %>% tally()
  # summarize_if(is.numeric, sum, na.rm=TRUE)

# Removes all 2020 onwards data
output<-output[!(output$decade=="2020"),]

# If necessary
output = output[-c(55:58), ]

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

# VIS2
output <- cleaned_emdat %>% 
  group_by(decade,Disaster.Type) %>% #tally()
  summarize_if(is.numeric, sum, na.rm=TRUE)

output = subset(output, select =-c(Total.Damages...000.US..,CPI, Year, Total.Affected))  
output = output[-c(55:58), ]

ggplot(output, aes(fill=Disaster.Type, y=No.Injured, x=decade)) + 
  geom_bar(position="stack", stat="identity") +
  scale_color_brewer(palette = "Paired") +
  # a_flat_fill() +
  scale_y_continuous(labels = addUnits) +
  labs(y='Number of people',
       x='Decades',
       title= 'People Injured (1960-2020)',
       caption='source: EMDAT',
       fill='') 
ggplot(output, aes(fill=Disaster.Type, y=No.Affected, x=decade)) + 
  geom_bar(position="stack", stat="identity") +
  scale_color_brewer(palette = "Paired") +
  # a_flat_fill() +
  scale_y_continuous(labels = addUnits) +
  labs(y='Number of people',
       x='Decades',
       title= 'People Affected (1960-2020)',
       caption='source: EMDAT',
       fill='') 
ggplot(output, aes(fill=Disaster.Type, y=No.Homeless, x=decade)) + 
  geom_bar(position="stack", stat="identity") +
  scale_color_brewer(palette = "Paired") +
  # a_flat_fill() +
  scale_y_continuous(labels = addUnits) +
  labs(y='Number of people',
       x='Decades',
       title= 'People pushed towards homeless (1960-2020)',
       caption='source: EMDAT',
       fill='') 

# 
# output1 = subset(output, select =-c(Total.Damages...000.US..,CPI, Year, Total.Affected))  
# 
# # out + scale_color_lancet()
#   # scale_color_viridis(discrete=TRUE, option="viridis")
#   # scale_color_brewer(palette = "Set1")
# output1 <- melt(output1, id.vars=c("Disaster.Type","decade"))
# mydata2 = output1[-c(1:58), ] #select(output1, -1:-230)
# 
# ggplot(output1, aes(fill=Disaster.Type, y=value, x=decade)) + 
#   geom_bar(position="stack", stat="identity") + facet_grid( ~ variable) +
#   # a_flat_fill() +
#   labs(y='number of times',
#        x='Decades',
#        title= 'Disaster Occurance (1960-2020)',
#        caption='source: EMDAT',
#        fill='')
# 
# ggplot(data = mydata2, aes(decade,value,color = Disaster.Type)) +
#   geom_line(size = 1) +
#   geom_point() + 
#   labs(title = "Decadal Impact of Natural Disasters",
#        y = "Number", x = "Years in decades") +
#   facet_grid( ~ variable)
#   # scale_y_continuous(labels = addUnits)
# 
# ggplot(mydata2, aes(fill=Disaster.Type, y=value, x=decade)) + 
#   geom_bar(position="stack", stat="identity") + facet_grid(. ~ variable) +
# a_flat_fill() +
#   labs(y='number of times',
#        x='Decades',
#        title= 'Disaster Occurance (1960-2020)',
#        caption='source: EMDAT',
#        fill='')
# 
# 
# ggplot(output, aes(fill=Disaster.Type, y=n, x=decade)) + 
#   geom_bar(position="stack", stat="identity") #+ facet_grid(. ~ Country) +
# a_flat_fill() +
#   labs(y='number of times',
#        x='Decades',
#        title= 'Disaster Occurance (1960-2020)',
#        caption='source: EMDAT',
#        fill='')
