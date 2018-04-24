library(tidyverse)
library(ggplot2)
library(beeswarm)
library("colorspace")

#Check and then set working dir
#getwd()
#setwd([filepath])

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

#KL is an integer.  To use it in beeswarm plot (and have all possible values show up on all graphs--even values for which a particular group has no data points at that value--I have to turn it into a factor)
KL_factor <- as.factor(KL)

#add newly created, factorized KL variable to the dataset
GiveN_subset <- cbind(GiveN_subset, KL_factor)
View(GiveN_subset)

#re-assign variables now that I've changed the dataframe
Age <- GiveN_subset$Age
KL_factor <- GiveN_subset$KL_factor

#To get colors to match up to levels of Give-N variable, create variable for color (makes easier to insert into beeswarm code)
#(from https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf)

GiveN_col <- factor(KL_factor, levels=c(0, 1, 2, 3, 4, 5, 6), labels=c("red1", "orange1", "yellow2", "green1", "blue1", "plum1", "purple1"))


#allowing automatic x-axis labeling WITH FACTOR!!
#If try to coerce the x-axis here with axis() it messes things up

beeswarm(Age~KL_factor,
	data= GiveN_subset,
	subset= GiveN_subset$Group_4cat=='English Early',
	pch=16,
	pwcol=as.character(GiveN_col),
	ylim=c(3,8),
	yaxp = c(3, 8, 5),
	xlab = "Give-N Small Knower Level",
	ylab= "Age (years)",
	main="Knower Level by Age: 'English Early' Group")

beeswarm(Age~KL_factor,
	data= GiveN_subset,
	subset= GiveN_subset$Group_4cat=='English Later',
	pch=16,
	pwcol=as.character(GiveN_col),
	ylim=c(3,8),
	xaxp = c(0, 6, 6),
	yaxp = c(3, 8, 5), 
	xlab = "Give-N Small Knower Level", 
	ylab= "Age (years)", 
	main="Knower Level by Age: 'English Later' Group")


beeswarm(Age~KL_factor, 
	data= GiveN_subset, 
	subset= GiveN_subset$Group_4cat=='ASL Early', 
	method = "center", 
	pch=16, 
	pwcol=as.character(GiveN_col),
	ylim=c(3,8),  
	yaxp = c(3, 8, 5), 
	xlab = "Give-N Small Knower Level", 
	ylab= "Age (years)", 
	main="Knower Level by Age: 'ASL Early' Group")


beeswarm(Age~KL_factor, 
	data= GiveN_subset, 
	subset= GiveN_subset$Group_4cat=='ASL Later', 
	method = "center", 
	pch=16, 
	pwcol=as.character(GiveN_col),  
	ylim=c(3,8), 
	yaxp = c(3, 8, 5), 
	xlab = "Give-N Small Knower Level", 
	ylab= "Age (years)", 
	main="Knower Level by Age: 'ASL Later' Group")
