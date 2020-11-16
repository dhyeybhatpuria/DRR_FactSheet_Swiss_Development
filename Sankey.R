# Sankey of Disaster types
# https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html
# Static for all Scripts
source("Functions and Packages.R")
emdat_public <- read.csv("emdat_public_1.csv")
cleaned_emdat = basic_datacleaning(emdat_public)
#

output <- cleaned_emdat %>% 
  group_by(Disaster.Subgroup, Disaster.Type, Country) %>% tally() #group_by(decade, Country, Disaster.Type) %>% tally()
# summarize_if(is.numeric, sum, na.rm=TRUE)

ggplot(as.data.frame(output),
       aes(y = n, axis1 = Disaster.Type, axis2 = Country)) +
  geom_alluvium(aes(fill = Disaster.Subgroup), width = 1/16) +
  geom_stratum(width = 1/12, fill = "grey70", color = "grey10") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Disaster", "Country"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  scale_fill_discrete(name = "Disaster Category") +
  ggtitle("Natural Disaster occurances by Country")

ggplot(as.data.frame(output),
       aes(y = n, axis1 = Disaster.Subgroup, axis2 = Country)) +
  geom_alluvium(aes(fill = Disaster.Type), width = 1/16) +
  geom_stratum(width = 1/12, fill = "grey70", color = "grey10") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Category", "Country"), expand = c(.07, .05)) +
  scale_fill_brewer( palette = "Set1") +
  # scale_fill_discrete(name = "Disaster") + 
  ylab(NULL) +
  #a_plex_theme() + 
  # a_flat_color() +
  ggtitle("Natural Disaster occurances by Country")
