# Yeo Zhi Yi
# Yale-NUS 
# Analysis of coral community data

library(vegan)
library(dplyr)
library(readxl)
library(car)
library(WRS2)
library(ggpubr)

categories <- read_excel("primerdata_categories.xlsx", sheet = "Sheet1")
allspecies <- read_excel("primerdata_allspecies.xlsx", sheet = "Sheet1")
categories[is.na(categories)] <- 0
allspecies[is.na(allspecies)] <- 0

# Shannon Diversity index for categories
categories[,2:29] <- apply(categories[,2:29], 2, as.numeric)
categories.diversity <- diversity(categories[,2:29], MARGIN = 1, index = "shannon")
c.diversity.numbers <- data.frame(diversity = categories.diversity, depth = categories$depth,
                                  type = categories$type, orientation = categories$orientation)
which(categories.diversity == 0)
View(categories[which(categories.diversity == 0),])

# Richness for categories
richness <- function(x){
  length(which(x != 0))
}
categories.richness <- apply(categories[,2:29], 1, richness)
c.richness.numbers <- data.frame(richness = categories.richness, depth = categories$depth,
                                 type = categories$type)
# Shannon Diversity index for species
allspecies[,2:117] <- apply(allspecies[,2:117], 2, as.numeric)
allspecies.diversity <- diversity(allspecies[,2:117], MARGIN = 1, index = "shannon")
as.diversity.numbers <- data.frame(diversity = allspecies.diversity, depth = allspecies$depth,
                                   type = allspecies$type, orientation = allspecies$orientation)
# Check those that have 0 for diversity index
# which(allspecies.diversity == 0)
# View(allspecies[which(allspecies.diversity == 0),])
# For those that have diversity index 0, it's because there is only 1 species present


allspecies.richness <- apply(allspecies[,2:117], 1, richness)
as.richness.numbers <- data.frame(richness = allspecies.richness, depth = allspecies$depth,
                                  type = allspecies$type)
# Check original data
table(categories$type, categories$depth)
table(allspecies$type, allspecies$depth) # Pretty balanced design

# For category Shannon diversity Index
ggboxplot(c.diversity.numbers, x = "depth", y = "diversity", color = "type")
leveneTest(diversity ~ depth * type, data = c.diversity.numbers)
cate.anov <- aov(diversity ~ depth * type, data = c.diversity.numbers, 
                 contrasts = contr.sum(list(c.diversity.numbers$depth, c.diversity.numbers$type)))
summary(cate.anov)
plot(residuals(cate.anov))
shapiro.test(residuals(cate.anov))
hist(residuals(cate.anov))
which(residuals(cate.anov) < -1.0)
tmp <- c.diversity.numbers[-73, ]
tmp.anov <- aov(diversity ~ depth * type, data = tmp,
                contrasts = contr.sum(list(tmp$depth, tmp$type)))
summary(tmp.anov)
plot(residuals(tmp.anov))
shapiro.test(residuals(tmp.anov))
hist(residuals(tmp.anov))

# For species shannon index
hist(as.diversity.numbers$diversity)
ggboxplot(as.diversity.numbers, x = "depth", y = "diversity", color = "type")
leveneTest(diversity ~ depth * type, data = as.diversity.numbers)
as.anov <- aov(diversity ~ depth * type, data = as.diversity.numbers, 
               contrasts = contr.sum(list(as.diversity.numbers$depth, as.diversity.numbers$type)))
summary(as.anov)
plot(residuals(as.anov))
shapiro.test(residuals(as.anov))
hist(residuals(as.anov))

# test for species richness 
hist(as.richness.numbers$richness)
ggboxplot(as.richness.numbers, x = "depth", y = "richness", color = "type")
leveneTest(richness ~ depth * type, data = as.richness.numbers)
as.richness <- aov(richness ~ depth * type, data = as.richness.numbers, 
               contrasts = contr.sum(list(as.richness.numbers$depth, as.richness.numbers$type)))
summary(as.richness)
plot(residuals(as.richness))
hist(residuals(as.richness))

# Get similarity/dissimilarity matrix, then do permanova
# For Categories
categories.trans <- cbind(sqrt(categories[,2:29]), categories[, 31:35])
dissim.cate <- vegdist(categories.trans[, 1:28], method = "bray")
sim.cate <- 100 *(1 - dissimspecies.mat)
perm.cat <- adonis(dissim.cate ~ type * depth, data = categories.trans, 
                   permutations = 9999)
perm.cat
# For Species
allspecies.trans <- cbind(sqrt(allspecies[,2:117]), allspecies[,119:123]) 
dissimspecies.mat <- vegdist(allspecies.trans[,1:116], method = "bray")
simspecies.mat <- 100*(1 - dissimspecies.mat)
perm.sp <- adonis(dissimspecies.mat ~ type * depth, data = allspecies.trans, permutations = 9999)
perm.sp
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
