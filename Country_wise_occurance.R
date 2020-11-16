
source("Functions and Packages.R")
emdat_public <- read.csv("emdat_public_1.csv")
cleaned_emdat = basic_datacleaning(emdat_public)

plotheight = 4
plotwidth = 7
resolutionset = 100

output<-cleaned_emdat[(cleaned_emdat$Country=="Myanmar"),]
output <- output %>% 
  group_by(decade, Disaster.Type) %>% tally()

# Removes all 2020 onwards data
output<-output[!(output$decade=="2020"),]
# Grouped
gg = ggplot(output, aes(fill=Disaster.Type, y=n, x=decade)) + 
  geom_bar(position="stack", stat="identity") + #facet_grid(. ~ Country) +
  scale_color_brewer(palette = "Paired") +
  a_flat_fill() +
  labs(y='number of times',
       x='Years',
       title= 'Disaster Occurance, Myanmar (1960-2020)',
       caption='source: EMDAT',
       fill='')

tiff("disaster_occurance_myanmar.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()

############
output<-cleaned_emdat[(cleaned_emdat$Country=="Lao PDR"),]
output <- output %>% 
  group_by(decade, Disaster.Type) %>% tally()

# Removes all 2020 onwards data
output<-output[!(output$decade=="2020"),]
# Grouped
gg = ggplot(output, aes(fill=Disaster.Type, y=n, x=decade)) + 
  geom_bar(position="stack", stat="identity") + #facet_grid(. ~ Country) +
  scale_color_brewer(palette = "Paired") +
  a_flat_fill() +
  labs(y='number of times',
       x='Years',
       title= 'Disaster Occurance, Lao PDR (1960-2020)',
       caption='source: EMDAT',
       fill='')

tiff("disaster_occurance_laos.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()


############
output<-cleaned_emdat[(cleaned_emdat$Country=="Cambodia"),]
output <- output %>% 
  group_by(decade, Disaster.Type) %>% tally()

# Removes all 2020 onwards data
output<-output[!(output$decade=="2020"),]
# Grouped
gg = ggplot(output, aes(fill=Disaster.Type, y=n, x=decade)) + 
  geom_bar(position="stack", stat="identity") + #facet_grid(. ~ Country) +
  scale_color_brewer(palette = "Paired") +
  a_flat_fill() +
  labs(y='number of times',
       x='Years',
       title= 'Disaster Occurance, Cambodia (1960-2020)',
       caption='source: EMDAT',
       fill='')

tiff("disaster_occurance_cambodia.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()




