##################### EMDAT DATA MUNGING
#  06/042020


if (!require("readxl")) {
  install.packages("readxl")
  library(readxl)
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}

# Read Master Datafile
masterdata <- read_excel("Documents/emdat_public_2020_03_03.xlsx")

# # Select Asia
# selected_data <- subset(masterdata,Continent == "Asia") 
# OR 
selected_data <- subset(masterdata,Region == "South-Eastern Asia")

names(selected_data)[names(selected_data) == "Country name"] <- "Country"
# core Munging
output <- selected_data %>% group_by(Year, Type, Country) %>% tally()

# writes output to a csv file 
write.csv(output,'Documents/EMDAT_processed.csv')
