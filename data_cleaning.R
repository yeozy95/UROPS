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
primerdata <- data.frame()
for (i in seq_along(file.name)) {
  df <- read_excel(file.name[i],
                   sheet = sheet.name[i])
  # Only take hard coral benthic communities
  df <- df[df$`Benthic Cat.`== "HC",]
  # Remove empty row between every transect
  if (length(which(is.na(df$Site)))!= 0) {
    df <- df[-which(is.na(df$Site)),]
  }
  # Change tentative genus ID to 3-letter shortcuts
  df$`TENTATIVE GENUS ID` <- ifelse(nchar(df$`TENTATIVE GENUS ID`) != 3, genus.key[match(toupper(df$`TENTATIVE GENUS ID`), toupper(genus.key$genus.long)), "genus.short"],
                                    df$`TENTATIVE GENUS ID`)
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
    group_by(transect = `Transect #`, genus = `TENTATIVE GENUS ID`) %>%
    summarise(count = sum(length(`TENTATIVE GENUS ID`)))
  # Melt it into a matrix
  data <- dcast(temp, transect ~ genus, value.var = "count")
  data$site <- sitename
  data$depth <- depth
  data[is.na(data)] <- ""
  # For the first obs, use rbind. Subsequently merge the dataset
  if (i == 1) {
    primerdata <- rbind(primerdata,data)
  } else{
    primerdata <- merge(primerdata, data, all.x = TRUE, all.y = TRUE)
  }
}
# Convert NAs to blanks
primerdata[is.na(primerdata)] <- ""
# Add empty column and label column
primerdata$` ` <- " "
primerdata$"No." <- 1:nrow(primerdata)
# Add Column for seawall and reef
primerdata$type <- ifelse(grepl("Kusu", primerdata$site) | grepl("Hantu", primerdata$site) | 
                            grepl("Sisters", primerdata$site) | grepl("Sultan", primerdata$site), "Reefs", 
                          "Seawalls")
# Rearrange the columns
primerdata.ord <- primerdata %>% 
  select(No., POR, POD:OUR, ` `, transect, site, depth, type)


# Export the file
write.xlsx(primerdata.ord, "../primerdata.xlsx", row.names = FALSE)

# Primer data, length-wise (genus)
primerdata.length <- data.frame()
for (i in seq_along(file.name)) {
  df <- read_excel(file.name[i],
                   sheet = sheet.name[i])
  # Only take hard coral benthic communities
  df <- df[df$`Benthic Cat.`== "HC",]
  # Remove empty row between every transect
  if (length(which(is.na(df$Site)))!= 0) {
    df <- df[-which(is.na(df$Site)),]
  }
  # Change tentative genus ID to 3-letter shortcuts
  df$`TENTATIVE GENUS ID` <- ifelse(nchar(df$`TENTATIVE GENUS ID`) != 3, genus.key[match(toupper(df$`TENTATIVE GENUS ID`), toupper(genus.key$genus.long)), "genus.short"],
                                    df$`TENTATIVE GENUS ID`)
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
    group_by(transect = `Transect #`, genus = `TENTATIVE GENUS ID`) %>%
    summarise(count = sum(Length))
  # Melt it into a matrix
  data <- dcast(temp, transect ~ genus, value.var = "count")
  data$site <- sitename
  data$depth <- depth
  data[is.na(data)] <- ""
  # For the first obs, use rbind. Subsequently merge the dataset
  if (i == 1) {
    primerdata.length <- rbind(primerdata.length,data)
  } else{
    primerdata.length <- merge(primerdata.length, data, all.x = TRUE, all.y = TRUE)
  }
}


# Convert NAs to blanks
primerdata.length[is.na(primerdata.length)] <- ""
# Add empty column and label column
primerdata.length$` ` <- " "
primerdata.length$"No." <- 1:nrow(primerdata.length)
# Add Column for seawall and reef
primerdata.length$type <- ifelse(grepl("Kusu", primerdata.length$site) | grepl("Hantu", primerdata.length$site) | 
                            grepl("Sisters", primerdata.length$site) | grepl("Sultan", primerdata.length$site), "Reefs", 
                          "Seawalls")
# Rearrange the columns
primerdata.length.ord <- primerdata.length %>% 
  select(No., POR, POD:OUR, ` `, transect, site, depth, type)

write.xlsx(primerdata.length.ord, "../primerdata_length.xlsx", row.names = FALSE)

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
write.xlsx(primerdata.allspecies.ord, "../primerdata_allspecies.xlsx", row.names = FALSE)
# Do some checks on original data 
check <- mapply(read_excel, file.name, sheet.name)
unique.species <- unique(do.call(rbind, lapply(check, function(x) {unique(x[, "New Species ID"])})))
