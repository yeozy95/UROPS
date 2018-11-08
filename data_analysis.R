# Yeo Zhi Yi
# Yale-NUS 
# Analysis of coral community data

library(vegan)
library(dplyr)
library(readxl)
library(car)
library(WRS2)
library(ggpubr)
library(stargazer)
library(ggplot2)
categories <- read_excel("primerdata_categories.xlsx", sheet = "Sheet1")
allspecies <- read_excel("primerdata_allspecies.xlsx", sheet = "Sheet1")
categories[is.na(categories)] <- 0
allspecies[is.na(allspecies)] <- 0

# Shannon Diversity index for categories
categories[,2:22] <- apply(categories[,2:22], 2, as.numeric)
categories.diversity <- diversity(categories[,2:22], MARGIN = 1, index = "shannon")
which(categories.diversity == 0)
# View(categories[which(categories.diversity == 0),])

# Richness for categories
richness <- function(x){
  length(which(x != 0))
}
categories.richness <- apply(categories[,2:22], 1, richness)
c.sum  <- data.frame(site = categories$site, transect = categories$transect, depth = categories$depth,
                     type = categories$type, orientation = categories$orientation,
                     slope = categories$slope, diversity = categories.diversity,
                     richness = categories.richness, stringsAsFactors = FALSE)
cat.sum <- c.sum %>%
  group_by(site, depth) %>% 
  summarise(category.diversity = mean(diversity), category.richness = mean(richness))
c.sum$site <- gsub("LN3", "Lazarus North 3", c.sum$site)
c.sum$site <- gsub("LW1", "Lazarus West 1", c.sum$site)
c.sum$site <- gsub("LW6", "Lazarus West 6", c.sum$site)
c.sum$site <- gsub("SMK1", "Semakau 1", c.sum$site)
# Shannon Diversity index for species
allspecies[,2:117] <- apply(allspecies[,2:117], 2, as.numeric)
allspecies.diversity <- diversity(allspecies[,2:117], MARGIN = 1, index = "shannon")

# Check those that have 0 for diversity index
# which(allspecies.diversity == 0)
# View(allspecies[which(allspecies.diversity == 0),])
# For those that have diversity index 0, it's because there is only 1 species present


allspecies.richness <- apply(allspecies[,2:117], 1, richness)
as.richness.numbers <- data.frame(richness = allspecies.richness, depth = allspecies$depth,
                                  type = allspecies$type, slope = allspecies$slope, 
                                  orientation = allspecies$orientation)
as.sum <- data.frame(site = allspecies$site, transect = allspecies$transect, 
                     diversity = allspecies.diversity, richness = allspecies.richness,
                     depth = allspecies$depth, type = allspecies$type, 
                     orientation = allspecies$orientation,
                     slope = allspecies$slope, stringsAsFactors = FALSE)
as.sum$site <- gsub("LN3", "Lazarus North 3", as.sum$site)
as.sum$site <- gsub("LW1", "Lazarus West 1", as.sum$site)
as.sum$site <- gsub("LW6", "Lazarus West 6", as.sum$site)
as.sum$site <- gsub("SMK1", "Semakau 1", as.sum$site)
species.sum <- as.sum %>%
  group_by(site, depth) %>%
  summarize(species.diversity = mean(diversity), species.richness = mean(richness))
unique.sites <- unique(categories[, 25:29])
unique.sites$site <- gsub("LN3", "Lazarus North 3", unique.sites$site)
unique.sites$site <- gsub("LW1", "Lazarus West 1", unique.sites$site)
unique.sites$site <- gsub("LW6", "Lazarus West 6", unique.sites$site)
unique.sites$site <- gsub("SMK1", "Semakau 1", unique.sites$site)
unique.sites <- unique.sites[order(unique.sites$site), ]
unique.sites <- cbind(unique.sites, species.sum[,3:4], cat.sum[,3:4])
unique.sites$slope[unique.sites$slope == 0] <- NA
unique.sites$slope <- gsub("sloping", "Sloping", unique.sites$slope)
unique.sites$slope <- gsub("vertical", "Vertical", unique.sites$slope)
# stargazer(unique.sites, rownames = FALSE, summary = FALSE, type = "latex",
#           out = "site summary.tex")

# Get list of species table and presence
file.name <- list.files(path = "R", full.names = TRUE)
# Need to manually input sheet names
sheet.name <- c("LW6", "LW1", "Hantu", "Semakau Seawall", "Kusu", "SSN", "LN2", "LN2", "LN1", "LN3", "LN3", 
                "Hantu-cleaned", "Kusu-cleaned", "LE1", "LN1-cleaned", "LW1", "Small Sisters-cleaned", "SSN-cleaned")
spc.list <- data.frame()
for (i in seq_along(file.name)) {
  df <- read_excel(file.name[i],
                   sheet = sheet.name[i])
  df <- df[df$`Benthic Cat.`== "HC",]
  spc <- unique(df$`New Species ID`)
  if (length(unique(df$Site))!= 1){
    df$Site <- df$Site[1]
  }
  site <- unique(df$Site)
  spc <- cbind(spc, site)
  if (i == 1) {
    spc.list <- rbind(spc.list, spc)
  } else{
    spc.list <- merge(spc.list, spc, all.x = TRUE, all.y = TRUE)
  }
}
spc.list$site <- gsub("LN3", "Lazarus North 3", spc.list$site)
spc.list$site <- gsub("LW1", "Lazarus West 1", spc.list$site)
spc.list$site <- gsub("LW6", "Lazarus West 6", spc.list$site)
spc.list$site <- gsub("SMK1", "Semakau 1", spc.list$site)
spc.wide <- reshape2::dcast(spc.list, spc ~ site)
spc.wide <- spc.wide[-nrow(spc.wide), -ncol(spc.wide)]
names(spc.wide)[1] <- "Species"
spc.wide[, -1] <- replace(data.frame(lapply(spc.wide[,-1], as.character),
                                     stringsAsFactors = FALSE), spc.wide[, -1] != 0, "*")
spc.wide[, -1] <- replace(data.frame(lapply(spc.wide[,-1], as.character),
                                     stringsAsFactors = FALSE), spc.wide[, -1] == 0, NA)
spc.wide <- spc.wide[-grep("Juvenile", spc.wide$Species), ]
nrow(spc.wide)
# Top values
apply(spc.wide[,-1], 2, function(x){sum(!is.na(x))})

spc.wide$Species <- gsub("(.*)", "\\textit{\\1}", spc.wide$Species)
spc.wide.1 <- spc.wide[, 1:7]
spc.wide.2 <- spc.wide[, c(1, 8:ncol(spc.wide))]
stargazer(spc.wide.1, rownames = FALSE, summary = FALSE, type = "latex", out = "species_list1.tex")
stargazer(spc.wide.2, rownames = FALSE, summary = FALSE, type = "latex", out = "species_list2.tex")
head(unique.sites[order(unique.sites$species.diversity, decreasing = TRUE),], 3)
head(unique.sites[order(unique.sites$category.diversity, decreasing = TRUE),], 3)
# List of categories 
cat.list <- data.frame()
for (i in seq_along(file.name)) {
  df <- read_excel(file.name[i],
                   sheet = sheet.name[i])
  cat <- unique(df$`Benthic Cat.`)
  if (length(unique(df$Site))!= 1){
    df$Site <- df$Site[1]
  }
  site <- unique(df$Site)
  cat <- cbind(cat, site)
  if (i == 1) {
    cat.list <- rbind(cat.list, cat)
  } else{
    cat.list <- merge(cat.list, cat, all.x = TRUE, all.y = TRUE)
  }
}
cat.list$site <- gsub("LN3", "Lazarus North 3", cat.list$site)
cat.list$site <- gsub("LW1", "Lazarus West 1", cat.list$site)
cat.list$site <- gsub("LW6", "Lazarus West 6", cat.list$site)
cat.list$site <- gsub("SMK1", "Semakau 1", cat.list$site)
cat.wide <- reshape2::dcast(cat.list, cat ~ site)
cat.wide <- cat.wide[-nrow(cat.wide),]
names(cat.wide)[1] <- "Benthic Category"
cat.wide[, -1] <- replace(data.frame(lapply(cat.wide[,-1], as.character),
                                     stringsAsFactors = FALSE), cat.wide[, -1] != 0, "*")
cat.wide[, -1] <- replace(data.frame(lapply(cat.wide[,-1], as.character),
                                     stringsAsFactors = FALSE), cat.wide[, -1] == 0, NA)
# stargazer(cat.wide, rownames = FALSE, summary = FALSE, type = "latex", out = "category_list.tex")
nrow(cat.wide)

categories$site <- gsub("LN3", "Lazarus North 3", categories$site)
categories$site <- gsub("LW1", "Lazarus West 1", categories$site)
categories$site <- gsub("LW6", "Lazarus West 6", categories$site)
categories$site <- gsub("SMK1", "Semakau 1", categories$site)
cat.perc <- categories %>%
  select(DCA:AA, site) %>%
  group_by(site) %>%
  summarise_all(sum)
for (i in 1:nrow(cat.perc)){
  cat.perc[i, -1] <- cat.perc[i,-1]/sum(cat.perc[i,-1]) * 100
}
cat.percwide <- reshape2::melt(cat.perc, measure.vars = names(cat.perc)[2:ncol(cat.perc)],
                               variable.name = "category", value.name = "percentage")
cat.percwide <- reshape2::dcast(cat.percwide, category ~ site, value.var = "percentage")
cat.perwide.1 <- cat.percwide[, 1:7]
cat.perwide.2 <- cat.percwide[, c(1, 8:ncol(cat.percwide))]
# stargazer(cat.perwide.1, rownames = FALSE, summary = FALSE, type = "latex", out = "category_perc1.tex")
# stargazer(cat.perwide.2, rownames = FALSE, summary = FALSE, type = "latex", out = "category_perc2.tex")

# Boxplot for shannon indices and richness
bcd.box <- ggplot(c.sum, aes(x = site, y = diversity)) +
  geom_boxplot(fill = "steelblue") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Site") +
  ylab("Shannon Diversity Index") +
  ggtitle("Shannon Diversity Index for Benthic Categories")
ggsave("benthic_shannon.png", bcd.box, width = 7, height = 7)
ssd.box <- ggplot(as.sum, aes(x = site, y = diversity)) +
  geom_boxplot(fill = "steelblue") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Site") +
  ylab("Shannon Diversity Index") +
  ggtitle("Shannon Diversity Index for Scleractinia Species")
ggsave("coral_shannon.png", ssd.box, width = 7, height = 7)

bcs.box <- ggplot(c.sum, aes(x = site, y = richness)) +
  geom_boxplot(fill = "steelblue") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Site") +
  ylab("Richness") +
  ggtitle("Richness for Benthic Categories")
ggsave("benthic_richness.png", bcs.box, width = 7, height = 7)
sss.box <- ggplot(as.sum, aes(x = site, y = richness)) +
  geom_boxplot(fill = "steelblue") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Site") +
  ylab("Richness") +
  ggtitle("Richness for Scleractinia Species")
ggsave("coral_richness.png", sss.box, width = 7, height = 7)
# Check original data
table(categories$type, categories$depth, categories$orientation)
table(allspecies$type, allspecies$depth) # Pretty balanced design

# For category Shannon diversity Index
ggboxplot(c.diversity.numbers, x = "depth", y = "diversity", color = "type")
leveneTest(diversity ~ depth * type:slope * orientation, data = c.diversity.numbers)
cate.anov <- aov(diversity ~ depth * orientation * type:slope, data = c.diversity.numbers, 
                 contrasts = contr.sum(list(c.diversity.numbers$depth, c.diversity.numbers$type, c.diversity.numbers$orientation)))
summary(cate.anov)
TukeyHSD(cate.anov, which = "depth")
plot(residuals(cate.anov))
hist(residuals(cate.anov))
tmp <- c.diversity.numbers[-which(residuals(cate.anov) < -1.0), ]
tmp.anov <- aov(diversity ~ depth * orientation * type:slope, data = tmp,
                contrasts = contr.sum(list(tmp$depth, tmp$type, tmp$orientation)))
summary(tmp.anov)
TukeyHSD(tmp.anov, which = "depth")
plot(residuals(tmp.anov))
hist(residuals(tmp.anov))

# For species shannon index
hist(as.diversity.numbers$diversity)
ggboxplot(as.sum, x = "depth", y = "diversity", color = "type")
leveneTest(diversity ~ depth * orientation * type:slope, data = as.sum)
as.anov <- aov(diversity ~ depth * orientation * type:slope, data = as.sum, 
               contrasts = contr.sum(list(as.sum$depth, as.sum$type, as.sum$orientation)))
summary(as.anov)
TukeyHSD(as.anov, which = c("orientation", "type:slope"))
plot(residuals(as.anov))
hist(residuals(as.anov))

# test for species richness 
hist(as.richness.numbers$richness)
ggboxplot(as.richness.numbers, x = "depth", y = "richness", color = "type")
leveneTest(richness ~ depth * type:slope * orientation, data = as.richness.numbers)
as.richness <- aov(richness ~ depth * type:slope * orientation, data = as.richness.numbers, 
                   contrasts = contr.sum(list(as.richness.numbers$depth, as.richness.numbers$type, as.richness.numbers$orientation)))
summary(as.richness)
TukeyHSD(as.richness, which = c("orientation", "type:slope", "depth:type:slope"))
plot(residuals(as.richness))
hist(residuals(as.richness))

# Get similarity/dissimilarity matrix, then do permanova
# For Categories
categories.trans <- cbind(sqrt(categories[,2:29]), categories[, 31:36])
dissim.cate <- vegdist(categories.trans[, 1:28], method = "bray")
sim.cate <- 100 *(1 - dissim.cate)
perm.cat <- adonis(dissim.cate ~ type:slope * depth * orientation, data = categories.trans, 
                   permutations = 9999)
perm.cat
# For Species
allspecies.trans <- cbind(sqrt(allspecies[,2:117]), allspecies[,119:124]) 
dissimspecies.mat <- vegdist(allspecies.trans[,1:116], method = "bray")
simspecies.mat <- 100*(1 - dissimspecies.mat)
perm.sp <- adonis(dissimspecies.mat ~ type:slope * depth * orientation, data = allspecies.trans, permutations = 9999)
perm.sp

# Make table for results section
cat.sum <- categories %>% 
  group_by(site) %>%
  mutate(depth = depth, type = type, slope = slope, orientation = orientation,
         richness = apply(categories[,2:29], 1, richness))
###########################################OLD STUFF#######################################
# For Shannon diversity index
# Check to do t-test
leveneTest(diversity ~ type, data = diversity.numbers[diversity.numbers$depth == "Shallow",])
hist(diversity.numbers$diversity[diversity.numbers$depth == "Shallow"])
# t-test for difference between reefs and seawalls for Shallow 
t.test(diversity ~ type, data = diversity.numbers[diversity.numbers$depth == "Shallow",])

# Check to do t-test
leveneTest(diversity ~ depth, data = diversity.numbers[diversity.numbers$type == "Seawalls",])
hist(diversity.numbers$diversity[diversity.numbers$type == "Seawalls"])
# t-test for difference between depths for seawalls
t.test(diversity ~ depth, data = diversity.numbers[diversity.numbers$type == "Seawalls",])

# For Species richness
# Check to do t-test
leveneTest(log(richness) ~ type, data = richness.numbers[richness.numbers$depth == "Shallow",])
hist(log(richness.numbers$richness[richness.numbers$depth == "Shallow"]))
# t-test for difference between reefs and seawalls for Shallow 
t.test(log(richness) ~ type, data = richness.numbers[richness.numbers$depth == "Shallow",])

# Check to do t-test
leveneTest(richness ~ depth, data = richness.numbers[richness.numbers$type == "Seawalls",])
hist(richness.numbers$richness[richness.numbers$type == "Seawalls"])
# t-test for difference between depths for seawalls
t.test(richness ~ depth, data = richness.numbers[richness.numbers$type == "Seawalls",])

# Transform data, then calculate dissimilarity matrix
allspecies.trans <- cbind(sqrt(allspecies[,2:112]), allspecies[,114:117]) 
dissimspecies.mat <- vegdist(allspecies.trans[,1:111], method = "bray")
simspecies.mat <- 100*(1 - dissimspecies.mat)
perm <- adonis(dissimspecies.mat ~ type * depth, data = allspecies.trans)
adonis(dissimspecies.mat ~ type, data = allspecies.trans, permuatations = 9999)

# To check for stuff below -> since there are no Deep Reefs, then cannot test for interaction
# # Variance is not statistically different -> ANOVA assumption met
# leveneTest(diversity ~ depth * type, data = diversity.numbers)
# div.anv <- aov(diversity ~ depth * type, data = diversity.numbers)
# # Residuals are not normal -> assumption of normality violated
# plot(residuals(div.anv))
# shapiro.test(residuals(div.anv))
# plot(div.anv, 2)
# summary(div.anv)
# nonp.div <- t2way(diversity ~ depth * type, data = diversity.numbers)
