# Time series deaths by disaster

source("Functions and Packages.R")
emdat_public <- read.csv("emdat_public_1.csv")
cleaned_emdat = basic_datacleaning(emdat_public)
options(scipen=10000)
plotheight = 4
plotwidth = 7
resolutionset = 100

# Myanmar
output<-cleaned_emdat[(cleaned_emdat$Country=="Myanmar"),]
output <- output %>%
  group_by(Year,Disaster.Subgroup) %>% #tally()
  summarize_if(is.numeric, sum, na.rm=TRUE)

gga = ggplot(output, aes(x=Year,y=Disaster.Subgroup,color=Disaster.Subgroup))+
  ggbeeswarm::geom_quasirandom(
    alpha=.75,aes(size=No.Affected),
    groupOnX = FALSE,
    show.legend = TRUE) +
  a_plex_theme(plot_title_size = 16) + a_flat_color() +
  scale_x_discrete(name ="Year",
                   limits=c(1965,1975,1985,1995, 2005, 2015)) +
  labs(title='"Natural" Disasters in Myanmar',
       subtitle='The people affected by natural disasters from 1963 - 2020',
       y='', caption='Data:EMDAT')

gg <- gga + guides(color=guide_legend("Disaster Sub-group"), 
                   size=guide_legend("No of Affected")) #+ scale_y_continuous(labels = scales::comma)

# png(filename = "ts_mmyr_affected.png",
#     width = 725, height = 446, units = "px")
tiff("ts_mmyr_affected.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()


gga = ggplot(output, aes(x=Year,y=Disaster.Subgroup,color=Disaster.Subgroup))+
  ggbeeswarm::geom_quasirandom(
    alpha=.75,aes(size=Total.Deaths),
    groupOnX = FALSE,
    show.legend = TRUE) +
  a_plex_theme() + a_flat_color() +
  scale_x_discrete(name ="Year",
                   limits=c(1965,1975,1985,1995, 2005, 2015)) +
  labs(title='"Natural" Disasters in Myanmar',
       subtitle='Total Deaths by natural disasters from 1963 - 2020',
       y='', caption='Data:EMDAT')
gg <- gga + guides(color=guide_legend("Disaster Sub-group"), 
                   size=guide_legend("Total of Deaths"))

# png(filename = "ts_mmyr_deaths.png",
#     width = 838, height = 496, units = "px")
tiff("ts_mmyr_deaths.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()


gga = ggplot(output, aes(x=Year,y=Disaster.Subgroup,color=Disaster.Subgroup))+
  ggbeeswarm::geom_quasirandom(
    alpha=.75,aes(size=No.Homeless),
    groupOnX = FALSE,
    show.legend = TRUE) +
  a_plex_theme() + a_flat_color() +
  scale_x_discrete(name ="Year",
                   limits=c(1965,1975,1985,1995, 2005, 2015)) +
  labs(title='"Natural" Disasters in Myanmar',
       subtitle='Persons homeless due to natural disasters from 1963 - 2020',
       y='', caption='Data:EMDAT')
gg <- gga + guides(color=guide_legend("Disaster Sub-group"), 
                   size=guide_legend("No of Homeless"))

# png(filename = "ts_mmyr_homeless.png",
#     width = 838, height = 496, units = "px")
tiff("ts_mmyr_homeless.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()



# Lao PDR
output<-cleaned_emdat[(cleaned_emdat$Country=="Lao PDR"),]
output <- output %>%
  group_by(Year,Disaster.Subgroup) %>% #tally()
  summarize_if(is.numeric, sum, na.rm=TRUE)

gga = ggplot(output, aes(x=Year,y=Disaster.Subgroup,color=Disaster.Subgroup))+
  ggbeeswarm::geom_quasirandom(
    alpha=.75,aes(size=No.Affected),
    groupOnX = FALSE,
    show.legend = TRUE) +
  a_plex_theme() + a_flat_color() +
  scale_x_discrete(name ="Year",
                   limits=c(1965,1975,1985,1995, 2005, 2015)) +
  labs(title='"Natural" Disasters in Lao PDR',
       subtitle='The people affected by natural disasters from 1963 - 2020',
       y='', caption='Data:EMDAT')
gg <- gga + guides(color=guide_legend("Disaster Sub-group"), 
                   size=guide_legend("No of Affected"))

# png(filename = "ts_laos_affected.png",
    # width = 838, height = 496, units = "px")
tiff("ts_laos_affected.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()



gga = ggplot(output, aes(x=Year,y=Disaster.Subgroup,color=Disaster.Subgroup))+
  ggbeeswarm::geom_quasirandom(
    alpha=.75,aes(size=Total.Deaths),
    groupOnX = FALSE,
    show.legend = TRUE) +
  a_plex_theme() + a_flat_color() +
  scale_x_discrete(name ="Year",
                   limits=c(1965,1975,1985,1995, 2005, 2015)) +
  labs(title='"Natural" Disasters in Lao PDR',
       subtitle='Total Deaths by natural disasters from 1963 - 2020',
       y='', caption='Data:EMDAT')
gg <- gga + guides(color=guide_legend("Disaster Sub-group"), 
                   size=guide_legend("No of Deaths"))

# png(filename = "ts_laos_death.png",
# width = 838, height = 496, units = "px")
tiff("ts_laos_deaths.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()



gga = ggplot(output, aes(x=Year,y=Disaster.Subgroup,color=Disaster.Subgroup))+
  ggbeeswarm::geom_quasirandom(
    alpha=.75,aes(size=No.Homeless),
    groupOnX = FALSE,
    show.legend = TRUE) +
  a_plex_theme() + a_flat_color() +
  scale_x_discrete(name ="Year",
                   limits=c(1965,1975,1985,1995, 2005, 2015)) +
  labs(title='"Natural" Disasters in Lao PDR',
       subtitle='Persons homeless due to natural disasters from 1963 - 2020',
       y='', caption='Data:EMDAT')
gg <- gga + guides(color=guide_legend("Disaster Sub-group"), 
                   size=guide_legend("No of Homeless"))

# png(filename = "ts_laos_homeless.png",
#     width = 838, height = 496, units = "px")
tiff("ts_laos_homeless.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()



# Cambodia
output<-cleaned_emdat[(cleaned_emdat$Country=="Cambodia"),]
output <- output %>%
  group_by(Year,Disaster.Subgroup) %>% #tally()
  summarize_if(is.numeric, sum, na.rm=TRUE)

gga = ggplot(output, aes(x=Year,y=Disaster.Subgroup,color=Disaster.Subgroup))+
  ggbeeswarm::geom_quasirandom(
    alpha=.75,aes(size=No.Affected),
    groupOnX = FALSE,
    show.legend = TRUE) +
  a_plex_theme() + a_flat_color() +
  scale_x_discrete(name ="Year",
                   limits=c(1965,1975,1985,1995, 2005, 2015)) +
  labs(title='"Natural" Disasters in Cambodia',
       subtitle='The people affected by natural disasters from 1963 - 2020',
       y='', caption='Data:EMDAT')
gg <- gga + guides(color=guide_legend("Disaster Sub-group"), 
                   size=guide_legend("No of Affected"))

# png(filename = "ts_cambodia_affected.png",
#     width = 838, height = 496, units = "px")
tiff("ts_cambodia_affected.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()



gga = ggplot(output, aes(x=Year,y=Disaster.Subgroup,color=Disaster.Subgroup))+
  ggbeeswarm::geom_quasirandom(
    alpha=.75,aes(size=Total.Deaths),
    groupOnX = FALSE,
    show.legend = TRUE) +
  a_plex_theme() + a_flat_color() +
  scale_x_discrete(name ="Year",
                   limits=c(1965,1975,1985,1995, 2005, 2015)) +
  labs(title='"Natural" Disasters in Cambodia',
       subtitle='Total Deaths by natural disasters from 1963 - 2020',
       y='', caption='Data:EMDAT')
gg <- gga + guides(color=guide_legend("Disaster Sub-group"), 
                   size=guide_legend("No of Deaths"))

# png(filename = "ts_cambodia_deaths.png",
#     width = 838, height = 496, units = "px")
tiff("ts_cambodia_deaths.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()



gga = ggplot(output, aes(x=Year,y=Disaster.Subgroup,color=Disaster.Subgroup))+
  ggbeeswarm::geom_quasirandom(
    alpha=.75,aes(size=No.Homeless),
    groupOnX = FALSE,
    show.legend = TRUE) +
  a_plex_theme() + a_flat_color() +
  scale_x_discrete(name ="Year",
                   limits=c(1965,1975,1985,1995, 2005, 2015)) +
  labs(title='"Natural" Disasters in Cambodia',
       subtitle='Persons homeless due to natural disasters from 1963 - 2020',
       y='', caption='Data:EMDAT')
gg <- gga + guides(color=guide_legend("Disaster Sub-group"), 
                   size=guide_legend("No of Homeless"))

# png(filename = "ts_cambodia_homeless.png",
#     width = 838, height = 496, units = "px")
tiff("ts_cambodia_homeless.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()



# MEKONG
# output<-cleaned_emdat[(cleaned_emdat$Country=="Myanmar"),]
cleaned_emdat = basic_datacleaning_Mekong(emdat_public)
output <- cleaned_emdat %>%
  group_by(Year,Disaster.Subgroup) %>% #tally()
  summarize_if(is.numeric, sum, na.rm=TRUE)

gga = ggplot(output, aes(x=Year,y=Disaster.Subgroup,color=Disaster.Subgroup))+
  ggbeeswarm::geom_quasirandom(
    alpha=.75,aes(size=No.Affected),
    groupOnX = FALSE,
    show.legend = TRUE) +
  a_plex_theme(plot_title_size = 14) + 
  a_flat_color() +
  scale_x_discrete(name ="Year",
                   limits=c(1965,1975,1985,1995, 2005, 2015)) +
  labs(title='"Natural" Disasters in Mekong River Region',
       subtitle='The people affected by natural disasters from 1963 - 2020',
       y='', caption='Data:EMDAT')
gg <- gga + guides(color=guide_legend("Disaster Sub-group"), 
                   size=guide_legend("No of Affected"))

# png(filename = "ts_mekong_affected.png",
#     width = 838, height = 496, units = "px")
tiff("ts_mekong_affected.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()



gga = ggplot(output, aes(x=Year,y=Disaster.Subgroup,color=Disaster.Subgroup))+
  ggbeeswarm::geom_quasirandom(
    alpha=.75,aes(size=Total.Deaths),
    groupOnX = FALSE,
    show.legend = TRUE) +
  a_plex_theme(plot_title_size = 14) + a_flat_color() +
  scale_x_discrete(name ="Year",
                   limits=c(1965,1975,1985,1995, 2005, 2015)) +
  labs(title='"Natural" Disasters in Mekong River Region',
       subtitle='Total Deaths by natural disasters from 1963 - 2020',
       y='', caption='Data:EMDAT')
gg <- gga + guides(color=guide_legend("Disaster Sub-group"), 
                   size=guide_legend("No of Deaths"))

# png(filename = "ts_mekong_deaths.png",
#     width = 838, height = 496, units = "px")
tiff("ts_mekong_deaths.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()



gga = ggplot(output, aes(x=Year,y=Disaster.Subgroup,color=Disaster.Subgroup))+
  ggbeeswarm::geom_quasirandom(
    alpha=.75,aes(size=No.Homeless),
    groupOnX = FALSE,
    show.legend = TRUE) +
  a_plex_theme(plot_title_size = 14) +  
  a_flat_color() +
  scale_x_discrete(name ="Year",
                   limits=c(1965,1975,1985,1995, 2005, 2015)) +
  labs(title='"Natural" Disasters in Mekong River Region',
       subtitle='Persons homeless due to natural disasters from 1963 - 2020',
       y='', caption='Data:EMDAT')
gg <- gga + guides(color=guide_legend("Disaster Sub-group"), 
                   size=guide_legend("No of Homeless"))

# png(filename = "ts_mekong_homeless.png",
#     width = 838, height = 496, units = "px")
tiff("ts_mekong_homeless.tiff", units="in", 
     width=plotwidth, height=plotheight, res=resolutionset)
plot(gg)
dev.off()



