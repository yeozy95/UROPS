# Yeo Zhi Yi 
# Yale-NUS
# Data Preparation

library(readxl)
library(xlsx)
library(dplyr)
library(reshape2)
# setwd("R")
file.name <- list.files()
# Need to manually input sheet names
sheet.name <- c("LW6", "LW1", "Semakau Seawall", "LN2", "LN2", "LN1", "LN3", "LN2", "Hantu-cleaned",
                "Kusu-cleaned", "LE1", "LN1-cleaned", "LW1", "Small Sisters-cleaned", "SSN-cleaned")
# Set up a genus key so as to convert genus names into their 3 letter short forms 
genus <- c("Acanthastrea (ACA)", "Acropora (ACR)", "Alveopora (ALV)", "Anacropora (ANA)", "Astreopora (AST)", 
           "Caulastrea (CAU)", "Corallimorph (COM)", "Coscinaraea (COS)", "Ctenactis (CTE)", "Cyphastrea (CYP)",
           "Diploastrea (DIP)", "Echinophyllia (ECL)", "Echinopora (ECH)", "Euphyllia (EUP)", "Favia (FAV)",
           "Favites (FVS)", "Fungia (FUN)", "Galaxea (GAL)", "Gardineroseris (GAR)", "Goniastrea (GOS)",
           "Goniopora (GON)", "Heliofungia (HEL)", "Herpolitha (HER)", "Hydnophora (HYD)", "Leptastrea (LER)",
           "Leptoseris (LES)", "Lithophyllon (LIT)", "Lobophyllia (LOB)", "Madracis (MAD)", "Manicina (MAN)",
           "Merulina (MER)", "Montipora (MON)", "Mycedium (MYC)", "Oulophyllia (OUL)", "Oulastrea (OUR)", 
           "Oxypora (OXY)", "Pachyseris (PAC)", "Pavona (PAV)", "Pectinia (PEC)", "Physogyra (PHY)", 
           "Platygyra (PLA)", "Plerogyra (PLE)", "Plesiastrea (PLS)", "Pocillopora (POC)", "Podabacia (POD)", 
           "Polyphyllia (POL)", "Porites (POR)", "Psammocora (PSA)", "Pseudosiderastrea (PSE)", "Stylocoeniella (STL)",
           "Symphyllia (SYM)", "Tubastraea (TUB)", "Turbinaria (TUR)", "Dipsastraea (DIS)", "Bernardpora (BER)",
           "Cycloseris (CYC)", "Coelastrea (COE)", "Fimbriaphyllia (FIM)")
genus.long <- gsub("(\\w*).\\((\\w*)\\)", "\\1", genus)
genus.short <- gsub("(\\w*).\\((\\w*)\\)", "\\2", genus)
genus.key <- data.frame(genus.long, genus.short, stringsAsFactors = FALSE)
# For PRIMER Use
# Species level data, including those that were not ID

primerdata.allspecies <- data.frame()
for (i in seq_along(file.name)) {
  df <- read_excel(file.name[i],
                   sheet = sheet.name[i])
  # Only take hard coral benthic communities
  df <- df[df$`Benthic Cat.`== "HC" | df$`Benthic Cat.` == "HC'",]
  # Remove empty row between every transect
  if (length(which(is.na(df$Site)))!= 0) {
    df <- df[-which(is.na(df$Site)),]
  }
  # Cleaning the new species ID into genus and species identifiers (xxx.xxx)
  tmp.gen <- gsub("(.*?) (.*)", "\\1", df$`New Species ID`)
  tmp.gen <- genus.key[match(toupper(tmp.gen), toupper(genus.key$genus.long)), "genus.short"]
  tmp.spc <- toupper(gsub("(.*?) (.{3}).*", "\\2", df$`New Species ID`))
  df$`New Species ID` <- paste0(tmp.gen, ".", tmp.spc)
  # Random data clean-ups go here
  if (grepl("Semakau", file.name[i])){
    df[is.na(df$`Transect #`), "Transect #"] <- 5
  }
  # Some files have inconsistent site names. Fix those
  if (length(unique(df$Site))!= 1){
    df$Site <- df$Site[1]
  }
  if (length(unique(df$Depth))!=1){
    df$Depth <- df$Depth[1]
  }
  sitename <- unique(df$Site)
  depth <- unique(df$Depth)
  # Summarize the data needed
  temp <- df %>%
    group_by(transect = `Transect #`, species = `New Species ID`) %>%
    summarise(length = sum(Length))
  # Melt it into a matrix
  data <- dcast(temp, transect ~ species, value.var = "length")
  data$site <- sitename
  data$depth <- depth
  data[is.na(data)] <- ""
  # For the first obs, use rbind. Subsequently merge the dataset
  if (i == 1) {
    primerdata.allspecies <- rbind(primerdata.allspecies,data)
  } else{
    primerdata.allspecies <- merge(primerdata.allspecies, data, all.x = TRUE, all.y = TRUE)
  }
}
primerdata.allspecies[is.na(primerdata.allspecies)] <- ""
# Add empty column and label column
primerdata.allspecies$` ` <- " "
primerdata.allspecies$"No." <- 1:nrow(primerdata.allspecies)
# Add Column for seawall and reef
primerdata.allspecies$type <- ifelse(grepl("Kusu", primerdata.allspecies$site) | grepl("Hantu", primerdata.allspecies$site) | 
                                   grepl("Sisters", primerdata.allspecies$site) | grepl("Sultan", primerdata.allspecies$site), "Reefs", 
                                 "Seawalls")
# Rearrange the columns
primerdata.allspecies.ord <- primerdata.allspecies %>% 
  select(No., "POR.SP.", POD.CRU:HYD.RIG, ` `, transect, site, depth, type)

primerdata.allspecies.ord <- primerdata.allspecies.ord[, -grep("NA", names(primerdata.allspecies.ord))]
# write.xlsx(primerdata.allspecies.ord, "../primerdata_allspecies.xlsx", row.names = FALSE)

# Rearranging for categories
primerdata.categories <- data.frame()
for (i in seq_along(file.name)) {
  df <- read_excel(file.name[i],
                   sheet = sheet.name[i])
  # Remove empty row between every transect
  if (length(which(is.na(df$Site)))!= 0) {
    df <- df[-which(is.na(df$Site)),]
  }
  # Random data clean-ups go here
  if (grepl("Semakau", file.name[i])){
    df[is.na(df$`Transect #`), "Transect #"] <- 5
  }
  # Some files have inconsistent site names. Fix those
  if (length(unique(df$Site))!= 1){
    df$Site <- df$Site[1]
  }
  if (length(unique(df$Depth))!=1){
    df$Depth <- df$Depth[1]
  }
  sitename <- unique(df$Site)
  depth <- unique(df$Depth)
  temp <- df %>% 
    group_by(transect = `Transect #`, categories = `Benthic Cat.`) %>%
    summarise(length = sum(Length))
  
  data <- dcast(temp, transect ~ categories, value.var = "length")
  data$site <- sitename
  data$depth <- depth
  data[is.na(data)] <- ""
  # For the first obs, use rbind. Subsequently merge the dataset
  if (i == 1) {
    primerdata.categories <- rbind(primerdata.categories,data)
  } else{
    primerdata.categories <- merge(primerdata.categories, data, all.x = TRUE, all.y = TRUE)
  }
}
primerdata.categories[is.na(primerdata.categories)] <- ""
# Add empty column and label column
primerdata.categories$` ` <- " "
primerdata.categories$"No." <- 1:nrow(primerdata.categories)
# Add Column for seawall and reef
primerdata.categories$type <- ifelse(grepl("Kusu", primerdata.categories$site) | grepl("Hantu", primerdata.categories$site) | 
                                       grepl("Sisters", primerdata.categories$site) | grepl("Sultan", primerdata.categories$site), "Reefs", 
                                     "Seawalls")
# Rearrange the columns
primerdata.categories.ord <- primerdata.categories %>% 
  select(No., DCA:TA, RC:TC, ` `, transect, site, depth, type)

primerdata.categories.ord <- primerdata.categories.ord[, -grep("NA", names(primerdata.categories.ord))]
# write.xlsx(primerdata.categories.ord, "../primerdata_categories.xlsx", row.names = FALSE)

# Separate by shallow and deep 
primerdata.shallow <- data.frame()
for (i in seq_along(file.name)) {
  df <- read_excel(file.name[i],
                   sheet = sheet.name[i])
  # Only take hard coral benthic communities
  df <- df[df$`Benthic Cat.`== "HC" | df$`Benthic Cat.` == "HC'",]
  # Remove empty row between every transect
  if (length(which(is.na(df$Site)))!= 0) {
    df <- df[-which(is.na(df$Site)),]
  }
  # Cleaning the new species ID into genus and species identifiers (xxx.xxx)
  tmp.gen <- gsub("(.*?) (.*)", "\\1", df$`New Species ID`)
  tmp.gen <- genus.key[match(toupper(tmp.gen), toupper(genus.key$genus.long)), "genus.short"]
  tmp.spc <- toupper(gsub("(.*?) (.{3}).*", "\\2", df$`New Species ID`))
  df$`New Species ID` <- paste0(tmp.gen, ".", tmp.spc)
  # Random data clean-ups go here
  if (grepl("Semakau", file.name[i])){
    df[is.na(df$`Transect #`), "Transect #"] <- 5
  }
  # Some files have inconsistent site names. Fix those
  if (length(unique(df$Site))!= 1){
    df$Site <- df$Site[1]
  }
  if (length(unique(df$Depth))!=1){
    df$Depth <- df$Depth[1]
  }
  sitename <- unique(df$Site)
  depth <- unique(df$Depth)
  # Take only shallow 
  if (depth != "Shallow"){
    next
  } 
  # Summarize the data needed
  
  temp <- df %>%
    group_by(transect = `Transect #`, species = `New Species ID`) %>%
    summarise(length = sum(Length))
  # Melt it into a matrix
  data <- dcast(temp, transect ~ species, value.var = "length")
  data$site <- sitename
  data$depth <- depth
  data[is.na(data)] <- ""
  # For the first obs, use rbind. Subsequently merge the dataset
  if (i == 1) {
    primerdata.shallow <- rbind(primerdata.shallow,data)
  } else{
    primerdata.shallow <- merge(primerdata.shallow, data, all.x = TRUE, all.y = TRUE)
  }
}
primerdata.shallow[is.na(primerdata.shallow)] <- ""
# Add empty column and label column
primerdata.shallow$` ` <- " "
primerdata.shallow$"No." <- 1:nrow(primerdata.shallow)
# Add Column for seawall and reef
primerdata.shallow$type <- ifelse(grepl("Kusu", primerdata.shallow$site) | grepl("Hantu", primerdata.shallow$site) | 
                                       grepl("Sisters", primerdata.shallow$site) | grepl("Sultan", primerdata.shallow$site), "Reefs", 
                                     "Seawalls")
# Rearrange the columns
primerdata.shallow.ord <- primerdata.shallow %>% 
  select(No., "POR.SP.", POD.CRU:LER.PRU, ` `, transect, site, depth, type)

primerdata.shallow.ord <- primerdata.shallow.ord[, -grep("NA", names(primerdata.shallow.ord))]
# write.xlsx(primerdata.shallow.ord, "../primerdata_shallow.xlsx", row.names = FALSE)


primerdata.deep <- data.frame()
for (i in seq_along(file.name)) {
  df <- read_excel(file.name[i],
                   sheet = sheet.name[i])
  # Only take hard coral benthic communities
  df <- df[df$`Benthic Cat.`== "HC" | df$`Benthic Cat.` == "HC'",]
  # Remove empty row between every transect
  if (length(which(is.na(df$Site)))!= 0) {
    df <- df[-which(is.na(df$Site)),]
  }
  # Cleaning the new species ID into genus and species identifiers (xxx.xxx)
  tmp.gen <- gsub("(.*?) (.*)", "\\1", df$`New Species ID`)
  tmp.gen <- genus.key[match(toupper(tmp.gen), toupper(genus.key$genus.long)), "genus.short"]
  tmp.spc <- toupper(gsub("(.*?) (.{3}).*", "\\2", df$`New Species ID`))
  df$`New Species ID` <- paste0(tmp.gen, ".", tmp.spc)
  # Random data clean-ups go here
  if (grepl("Semakau", file.name[i])){
    df[is.na(df$`Transect #`), "Transect #"] <- 5
  }
  # Some files have inconsistent site names. Fix those
  if (length(unique(df$Site))!= 1){
    df$Site <- df$Site[1]
  }
  if (length(unique(df$Depth))!=1){
    df$Depth <- df$Depth[1]
  }
  sitename <- unique(df$Site)
  depth <- unique(df$Depth)
  # Take only deep 
  if (depth != "Deep"){
    next
  } 
  # Summarize the data needed
  
  temp <- df %>%
    group_by(transect = `Transect #`, species = `New Species ID`) %>%
    summarise(length = sum(Length))
  # Melt it into a matrix
  data <- dcast(temp, transect ~ species, value.var = "length")
  data$site <- sitename
  data$depth <- depth
  data[is.na(data)] <- ""
  # For the first obs, use rbind. Subsequently merge the dataset
  if (i == 1) {
    primerdata.deep <- rbind(primerdata.deep,data)
  } else{
    primerdata.deep <- merge(primerdata.deep, data, all.x = TRUE, all.y = TRUE)
  }
}
primerdata.deep[is.na(primerdata.deep)] <- ""
# Add empty column and label column
primerdata.deep$` ` <- " "
primerdata.deep$"No." <- 1:nrow(primerdata.deep)
# Add Column for seawall and reef
primerdata.deep$type <- ifelse(grepl("Kusu", primerdata.deep$site) | grepl("Hantu", primerdata.deep$site) | 
                                    grepl("Sisters", primerdata.deep$site) | grepl("Sultan", primerdata.deep$site), "Reefs", 
                                  "Seawalls")
# Rearrange the columns
primerdata.deep.ord <- primerdata.deep %>% 
  select(No., DIS.FAV:TUR.MES, CYP.CHA:TUR.STE, ` `, transect, site, depth, type)

primerdata.deep.ord <- primerdata.deep.ord[, -grep("NA", names(primerdata.deep.ord))]

# write.xlsx(primerdata.deep.ord, "../primerdata_deep.xlsx", row.names = FALSE)

# Summarize by site instead of by transect
primerdata.site <- data.frame()
for (i in seq_along(file.name)) {
  df <- read_excel(file.name[i],
                   sheet = sheet.name[i])
  # Only take hard coral benthic communities
  df <- df[df$`Benthic Cat.`== "HC" | df$`Benthic Cat.` == "HC'",]
  # Remove empty row between every transect
  if (length(which(is.na(df$Site)))!= 0) {
    df <- df[-which(is.na(df$Site)),]
  }
  # Cleaning the new species ID into genus and species identifiers (xxx.xxx)
  tmp.gen <- gsub("(.*?) (.*)", "\\1", df$`New Species ID`)
  tmp.gen <- genus.key[match(toupper(tmp.gen), toupper(genus.key$genus.long)), "genus.short"]
  tmp.spc <- toupper(gsub("(.*?) (.{3}).*", "\\2", df$`New Species ID`))
  df$`New Species ID` <- paste0(tmp.gen, ".", tmp.spc)
  # Random data clean-ups go here
  if (grepl("Semakau", file.name[i])){
    df[is.na(df$`Transect #`), "Transect #"] <- 5
  }
  # Some files have inconsistent site names. Fix those
  if (length(unique(df$Site))!= 1){
    df$Site <- df$Site[1]
  }
  if (length(unique(df$Depth))!=1){
    df$Depth <- df$Depth[1]
  }
  depth <- unique(df$Depth)
  # Summarize the data needed
  temp <- df %>%
    group_by(site = `Site`, species = `New Species ID`) %>%
    summarise(length = sum(Length))
  # Melt it into a matrix
  data <- dcast(temp, site ~ species, value.var = "length")
  data$depth <- depth
  data[is.na(data)] <- ""
  # For the first obs, use rbind. Subsequently merge the dataset
  if (i == 1) {
    primerdata.site <- rbind(primerdata.site,data)
  } else{
    primerdata.site <- merge(primerdata.site, data, all.x = TRUE, all.y = TRUE)
  }
}
primerdata.site[is.na(primerdata.site)] <- ""
# Add empty column and label column
primerdata.site$` ` <- " "
primerdata.site$"No." <- 1:nrow(primerdata.site)
# Add Column for seawall and reef
primerdata.site$type <- ifelse(grepl("Kusu", primerdata.site$site) | grepl("Hantu", primerdata.site$site) | 
                                       grepl("Sisters", primerdata.site$site) | grepl("Sultan", primerdata.site$site), "Reefs", 
                                     "Seawalls")
# Rearrange the columns
primerdata.site.ord <- primerdata.site %>% 
  select(No., "POR.SP.", POD.CRU:HYD.RIG, ` `, site, depth, type)

primerdata.site.ord <- primerdata.site.ord[, -grep("NA", names(primerdata.site.ord))]
# write.xlsx(primerdata.site.ord, "../primerdata_site.xlsx", row.names = FALSE)


# Do some checks on original data 
# check <- mapply(read_excel, file.name, sheet.name)
# unique.species <- unique(do.call(rbind, lapply(check, function(x) {unique(x[, "New Species ID"])})))


##############################################OLD STUFF###################################
# primerdata <- data.frame()
# for (i in seq_along(file.name)) {
#   df <- read_excel(file.name[i],
#                    sheet = sheet.name[i])
#   # Only take hard coral benthic communities
#   df <- df[df$`Benthic Cat.`== "HC",]
#   # Remove empty row between every transect
#   if (length(which(is.na(df$Site)))!= 0) {
#     df <- df[-which(is.na(df$Site)),]
#   }
#   # Change tentative genus ID to 3-letter shortcuts
#   df$`TENTATIVE GENUS ID` <- ifelse(nchar(df$`TENTATIVE GENUS ID`) != 3, genus.key[match(toupper(df$`TENTATIVE GENUS ID`), toupper(genus.key$genus.long)), "genus.short"],
#                                     df$`TENTATIVE GENUS ID`)
#   # Random data clean-ups go here
#   if (grepl("Semakau", file.name[i])){
#     df[is.na(df$`Transect #`), "Transect #"] <- 5
#   }
#   # Some files have inconsistent site names. Fix those
#   if (length(unique(df$Site))!= 1){
#     df$Site <- df$Site[1]
#   }
#   if (length(unique(df$Depth))!=1){
#     df$Depth <- df$Depth[1]
#   }
#   sitename <- unique(df$Site)
#   depth <- unique(df$Depth)
#   # Summarize the data needed
#   temp <- df %>%
#     group_by(transect = `Transect #`, genus = `TENTATIVE GENUS ID`) %>%
#     summarise(count = sum(length(`TENTATIVE GENUS ID`)))
#   # Melt it into a matrix
#   data <- dcast(temp, transect ~ genus, value.var = "count")
#   data$site <- sitename
#   data$depth <- depth
#   data[is.na(data)] <- ""
#   # For the first obs, use rbind. Subsequently merge the dataset
#   if (i == 1) {
#     primerdata <- rbind(primerdata,data)
#   } else{
#     primerdata <- merge(primerdata, data, all.x = TRUE, all.y = TRUE)
#   }
# }
# # Convert NAs to blanks
# primerdata[is.na(primerdata)] <- ""
# # Add empty column and label column
# primerdata$` ` <- " "
# primerdata$"No." <- 1:nrow(primerdata)
# # Add Column for seawall and reef
# primerdata$type <- ifelse(grepl("Kusu", primerdata$site) | grepl("Hantu", primerdata$site) | 
#                             grepl("Sisters", primerdata$site) | grepl("Sultan", primerdata$site), "Reefs", 
#                           "Seawalls")
# # Rearrange the columns
# primerdata.ord <- primerdata %>% 
#   select(No., POR, POD:OUR, ` `, transect, site, depth, type)


# Export the file
# write.xlsx(primerdata.ord, "../primerdata.xlsx", row.names = FALSE)
# 
# # Primer data, length-wise (genus)
# primerdata.length <- data.frame()
# for (i in seq_along(file.name)) {
#   df <- read_excel(file.name[i],
#                    sheet = sheet.name[i])
#   # Only take hard coral benthic communities
#   df <- df[df$`Benthic Cat.`== "HC",]
#   # Remove empty row between every transect
#   if (length(which(is.na(df$Site)))!= 0) {
#     df <- df[-which(is.na(df$Site)),]
#   }
#   # Change tentative genus ID to 3-letter shortcuts
#   df$`TENTATIVE GENUS ID` <- ifelse(nchar(df$`TENTATIVE GENUS ID`) != 3, genus.key[match(toupper(df$`TENTATIVE GENUS ID`), toupper(genus.key$genus.long)), "genus.short"],
#                                     df$`TENTATIVE GENUS ID`)
#   # Random data clean-ups go here
#   if (grepl("Semakau", file.name[i])){
#     df[is.na(df$`Transect #`), "Transect #"] <- 5
#   }
#   # Some files have inconsistent site names. Fix those
#   if (length(unique(df$Site))!= 1){
#     df$Site <- df$Site[1]
#   }
#   if (length(unique(df$Depth))!=1){
#     df$Depth <- df$Depth[1]
#   }
#   sitename <- unique(df$Site)
#   depth <- unique(df$Depth)
#   # Summarize the data needed
#   temp <- df %>%
#     group_by(transect = `Transect #`, genus = `TENTATIVE GENUS ID`) %>%
#     summarise(count = sum(Length))
#   # Melt it into a matrix
#   data <- dcast(temp, transect ~ genus, value.var = "count")
#   data$site <- sitename
#   data$depth <- depth
#   data[is.na(data)] <- ""
#   # For the first obs, use rbind. Subsequently merge the dataset
#   if (i == 1) {
#     primerdata.length <- rbind(primerdata.length,data)
#   } else{
#     primerdata.length <- merge(primerdata.length, data, all.x = TRUE, all.y = TRUE)
#   }
# }
# 
# 
# # Convert NAs to blanks
# primerdata.length[is.na(primerdata.length)] <- ""
# # Add empty column and label column
# primerdata.length$` ` <- " "
# primerdata.length$"No." <- 1:nrow(primerdata.length)
# # Add Column for seawall and reef
# primerdata.length$type <- ifelse(grepl("Kusu", primerdata.length$site) | grepl("Hantu", primerdata.length$site) | 
#                                    grepl("Sisters", primerdata.length$site) | grepl("Sultan", primerdata.length$site), "Reefs", 
#                                  "Seawalls")
# # Rearrange the columns
# primerdata.length.ord <- primerdata.length %>% 
#   select(No., POR, POD:OUR, ` `, transect, site, depth, type)
# 
# # write.xlsx(primerdata.length.ord, "../primerdata_length.xlsx", row.names = FALSE)
