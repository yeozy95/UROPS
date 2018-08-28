# Yeo Zhi Yi 
# Yale-NUS
# Data Preparation

library(readxl)
library(dplyr)
library(reshape2)
setwd("~/../Desktop/Zhi Yi's Working Folder/Data/R")
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
           "Merulina (MER)", "Montipora (MON)", "Mycedium (MYC)", "Oulaphyllia (OUL)", "Oulastrea (OUR)", 
           "Oxypora (OXY)", "Pachyseris (PAC)", "Pavona (PAV)", "Pectinia (PEC)", "Physogyra (PHY)", 
           "Platygyra (PLA)", "Plerogyra (PLE)", "Plesiastrea (PLS)", "Pocillopora (POC)", "Podabacia (POD)", 
           "Polyphyllia (POL)", "Porites (POR)", "Psammocora (PSA)", "Pseudosiderastrea (PSE)", "Stylocoeniella (STL)",
           "Symphyllia (SYM)", "Tubastrea (TUB)", "Turbinaria (TUR)")
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
# Rearrange the columns

# Export the file
write.csv(primerdata, "primerdata.csv", row.names = FALSE)
