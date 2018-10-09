# Yeo Zhi Yi
# Yale-NUS 
# Analysis of coral community data

library(vegan)
library(dplyr)
library(readxl)
library(car)
library(WRS2)

categories <- read_excel("primerdata_categories.xlsx", sheet = "Sheet1")
allspecies <- read_excel("primerdata_allspecies.xlsx", sheet = "Sheet1")
categories[is.na(categories)] <- 0
allspecies[is.na(allspecies)] <- 0
allspecies[,2:112] <- apply(allspecies[,2:112], 2, as.numeric)
allspecies.diversity <- diversity(allspecies[,2:112], MARGIN = 1, index = "shannon")
diversity.numbers <- data.frame(diversity = allspecies.diversity, depth = allspecies$depth,
                                type = allspecies$type)
# Check those that have 0 for diversity index
# which(allspecies.diversity == 0)
# View(allspecies[which(allspecies.diversity == 0),])
# For those that have diversity index 0, it's because there is only 1 species present

richness <- function(x){
  length(which(x != 0))
}
allspecies.richness <- apply(allspecies[,2:112], 1, richness)
richness.numbers <- data.frame(richness = allspecies.richness, depth = allspecies$depth,
                               type = allspecies$type)
# Check original data
table(allspecies$type, allspecies$depth)

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
