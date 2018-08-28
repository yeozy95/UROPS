# Yeo Zhi Yi 
# Yale-NUS
# Data Preparation

library(readxl)
library(dplyr)
library(reshape2)
setwd("~/../Desktop/Zhi Yi's Working Folder")
df <- read_excel("Data/Shallow Checked (0m CD)/Hantu (Shallow) 16.08.2017 (LN)/HA-W 2017_08_16 (House 14).xlsx",
           sheet = "Hantu-cleaned")

# For PRIMER Use
# Only take hard coral benthic communities
df <- df[df$`Benthic Cat.`== "HC",]
# Remove empty row between every transect
df <- df[-which(is.na(df$Site)),]
# Change tentative genus ID to 3-letter shortcuts
df$`TENTATIVE GENUS ID` <- ifelse(!is.na(df$Remarks), df$Remarks,
                            df$`TENTATIVE GENUS ID`)
sitename <- unique(df$Site)
date <- unique(df$`Date/Time`)
# Summarize the data needed
temp <- df %>%
  group_by(transect = `Transect #`, genus = `TENTATIVE GENUS ID`) %>%
  summarise(count = sum(length(`TENTATIVE GENUS ID`)))
# Melt it into a matrix
data <- dcast(temp, transect ~ genus, value.var = "count")
data$site <- sitename
data$date <- date
data[is.na(data)] <- ""
