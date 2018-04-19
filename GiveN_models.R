library(tidyverse)
library(ggplot2)

#Check and then set working dir
#getwd()
#setwd("[filepath]")

GiveN <- read.csv("GiveN_Coding_180405_AB.csv") #import the data file (which I already saved as a csv)
View(GiveN)

#data cleaning
GiveN_subset <- subset(GiveN, GiveN$Important.Notes=='')
View(GiveN_subset)
GiveN_subset <- GiveN_subset[1:101,1:17] #there were blank rows in the dataframe, and columns I didn't need

#visualize the data (task performance--Knower level by age, across different language groups)
Age <- GiveN_subset$Age
KL <- GiveN_subset$Knower.level_GiveN_Small

#reorder factor levels for Language Group so they show up in order I want them
GiveN_subset$Group_4cat <- factor(as.character(GiveN_subset$Group_4cat), levels = c("English Early", "ASL Early", "English Later", "ASL Later"))


p <- ggplot(GiveN_subset, aes(x=Age, y=KL)) + geom_point() + geom_smooth(method=lm)
p + facet_wrap(~GiveN_subset$Group_4cat) #facet_wrap doesn't work with assigned variables
#so if I do "LangGrp <- GiveN_subset$Group_4cat" I can use LangGrp in a model, but not in facet_wrap

cor.test(Age, KL)
cor.test(Age, KL)

#BEESWARM PLOTS!!!  Spectacular data visualization for GiveN data by Age

library(beeswarm)
beeswarm(Age~KL, data= GiveN_subset, subset= GiveN_subset$Group_4cat=='English Early', pch=16, col=rainbow((7)), xlab = "Give-N Small Knower Level", ylab= "Age", main="Knower Level by Age: 'English Early' Group")
beeswarm(Age~KL, data= GiveN_subset, subset= GiveN_subset$Group_4cat=='English Later', pch=16, col=rainbow((7)), xlab = "Give-N Small Knower Level", ylab= "Age", main="Knower Level by Age: 'English Later' Group")
beeswarm(Age~KL, data= GiveN_subset, subset= GiveN_subset$Group_4cat=='ASL Early', pch=16, col=rainbow((7)), xlab = "Give-N Small Knower Level", ylab= "Age", main="Knower Level by Age: 'ASL Early' Group")
beeswarm(Age~KL, data= GiveN_subset, subset= GiveN_subset$Group_4cat=='ASL Later', pch=16, col=rainbow((7)), xlab = "Give-N Small Knower Level", ylab= "Age", main="Knower Level by Age: 'ASL Later' Group")
