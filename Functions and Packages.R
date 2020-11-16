# Basic data cleaning and arrangement

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
if (!require("readr")) {
  install.packages("readr")
  library(readr)
}
if (!require("reshape2")) {
  install.packages("reshape2")
  library(reshape2)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

# library(ggsci)
# library(pals)
# library(awtools)
if (!require("ggsci")) {
  install.packages("ggsci")
  library(ggsci)
}
if (!require("pals")) {
  install.packages("pals")
  library(pals)
}
if (!require("devtools")) {
  install.packages("devtools")
  library(devtools)
}
if (!require("awtools")) {
  devtools::install_github('awhstin/awtools')
  library(awtools)
}
if (!require("ggalluvial")) {
  install.packages("ggalluvial")
  library(ggalluvial)
}
if (!require("ggforce")) {
  install.packages("ggforce")
  library(ggforce)
}
if (!require("ggbeeswarm")) {
  install.packages("ggbeeswarm")
  library(ggbeeswarm)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("rvest")) {
  install.packages("rvest")
  library(rvest)
}


addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}

basic_datacleaning = function(inputdata){
  inputdata[is.na(inputdata)] <- 0
  names(inputdata)[names(inputdata) == "Country name"] <- "Country"
  years = unique(inputdata$Year)
  decades <- seq(min(years)-10, max(years), by=10)
  # rm(years)
  inputdata$decade<- decades[findInterval(inputdata$Year, decades)]
  # rm(decades)
  inputdata<-inputdata[!(inputdata$Country=="Timor-Leste"),]
  inputdata<-inputdata[!(inputdata$Country=="Brunei Darussalam"),]
  inputdata<-inputdata[!(inputdata$Country=="Singapore"),]
  inputdata<-inputdata[!(inputdata$Country=="Viet Nam"),]
  inputdata<-inputdata[!(inputdata$Country=="Philippines (the)"),]
  inputdata<-inputdata[!(inputdata$Country=="Indonesia"),]
  inputdata<-inputdata[!(inputdata$Country=="Malaysia"),]
  inputdata<-inputdata[!(inputdata$Country=="Thailand"),]
  inputdata$Country[inputdata$Country == "Lao People's Democratic Republic (the)"] <- "Lao PDR"
  
  inputdata$decade[inputdata$decade == "1960"] <- "1960-1970"
  inputdata$decade[inputdata$decade == "1970"] <- "1970-1980"
  inputdata$decade[inputdata$decade == "1980"] <- "1980-1990"
  inputdata$decade[inputdata$decade == "1990"] <- "1990-2000"
  inputdata$decade[inputdata$decade == "2000"] <- "2000-2010"
  inputdata$decade[inputdata$decade == "2010"] <- "2010-2020"
  inputdata$decade[inputdata$decade == "2020"] <- "2020"
  return(inputdata)
}

basic_datacleaning_Mekong = function(inputdata){
  inputdata[is.na(inputdata)] <- 0
  names(inputdata)[names(inputdata) == "Country name"] <- "Country"
  years = unique(inputdata$Year)
  decades <- seq(min(years)-10, max(years), by=10)
  # rm(years)
  inputdata$decade<- decades[findInterval(inputdata$Year, decades)]
  # rm(decades)
  inputdata<-inputdata[!(inputdata$Country=="Timor-Leste"),]
  inputdata<-inputdata[!(inputdata$Country=="Brunei Darussalam"),]
  inputdata<-inputdata[!(inputdata$Country=="Singapore"),]
  # inputdata<-inputdata[!(inputdata$Country=="Viet Nam"),]
  inputdata<-inputdata[!(inputdata$Country=="Philippines (the)"),]
  # inputdata<-inputdata[!(inputdata$Country=="Singapore"),]
  inputdata<-inputdata[!(inputdata$Country=="Indonesia"),]
  inputdata<-inputdata[!(inputdata$Country=="Malaysia"),]
  # inputdata<-inputdata[!(inputdata$Country=="Thailand"),]
  inputdata$Country[inputdata$Country == "Lao People's Democratic Republic (the)"] <- "Lao PDR"
  
  inputdata$decade[inputdata$decade == "1960"] <- "1960-1970"
  inputdata$decade[inputdata$decade == "1970"] <- "1970-1980"
  inputdata$decade[inputdata$decade == "1980"] <- "1980-1990"
  inputdata$decade[inputdata$decade == "1990"] <- "1990-2000"
  inputdata$decade[inputdata$decade == "2000"] <- "2000-2010"
  inputdata$decade[inputdata$decade == "2010"] <- "2010-2020"
  inputdata$decade[inputdata$decade == "2020"] <- "2020"
  return(inputdata)
}