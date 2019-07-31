##Mapping Analyses

## Last edited 4/12/19

##If any of the following not already imported need import.packages("[packagename]")
library(tidyverse) # includes ggplot2??
library(ggplot2) # for general plots
library(beeswarm) # for beeswarm plots
library(colorspace) # for fixing colors in plots
library(stargazer) # for pretty regression output tables
library(MASS) # for polr package
library(generalhoslem) # for testing model fit (lipsitz test and two others)
library(qwraps2) # for summary_table use
library(quantreg) # testing quantile plots (geom_quantile) and quantile regressions
library(sure) # package for calculating residuals for ordinal logistic regression (https://journal.r-project.org/archive/2018/RJ-2018-004/RJ-2018-004.pdf)
library(mediation) # package for testing mediation effects


setwd("/Users/emilycarrigan/Dropbox/Data Analysis Work") ##this should be wherever your file is saved 

Mapping <- read.csv("Mapping_Coding_CH_190411.csv", na.strings = "N/A", skip = 1) #SKIP argument used to skip 1st line of data bc color coding in original excel file led to empty row and messed up column names

typeof(Mapping) # when importing using read.csv, resulting obj type is a list (data frame)
View(Mapping)

## FOR LANGFEST 2019: Subset hearing kids to look at their performance only 

Map_sub <- subset(Mapping, Mapping$Including_in_study=='Yes' & Mapping$Group_2cat == 'Hearing') # This can be amended as necessary for the specific analyses 
View(Map_sub)

##Trying to get only summary columns and not item colums--NOT WORKING YET

test <- subset(Map_sub, select=starts_with(c("Sum", "Avg", "Difference")))
View(test)

test <- select(Map_sub, -startsWith(c("Item"), prefix="I"))
View(test)


## Q1: What does mapping look like for kids @ this age?

# Prop. correct on each mapping type across all kids


qn <- ggplot(Map_sub, aes(x=Map_sub$Age, y=Map_sub$AvgCorrect_Quantity.Numeral)) + geom_point() + geom_smooth(method="loess", se = FALSE) + labs(y="Quantity-Numeral") + theme(text = element_text(size=11), axis.title.x=element_blank()) + coord_cartesian(xlim = c(5, 10), ylim= c(0.4,1)) + scale_y_continuous(breaks=c(0.5, 0.6, 0.7, 0.8, 0.9, 1)) + scale_x_continuous(breaks=c(5, 6, 7, 8, 9))

nq <- ggplot(Map_sub, aes(x=Map_sub$Age, y=Map_sub$AvgCorrect_Numeral.Quantity)) + geom_point() + geom_smooth(method="loess", se = FALSE) + labs(x="Age (Years)", y="Numeral-Quantity") + theme(text = element_text(size=11)) + coord_cartesian(xlim = c(5, 10), ylim= c(0.4,1)) + scale_y_continuous(breaks=c(0.5, 0.6, 0.7, 0.8, 0.9, 1)) + scale_x_continuous(breaks=c(5, 6, 7, 8, 9))
grid.arrange(qn, nq, ncol = 1)

# COMPARE each type to chance - Hurst et al used one-sample Wilcoxon Signed Rank tests (use median and assume roughly normal distribution around MEDIAN)
ggplot(Map_sub, aes(y=Map_sub$AvgCorrect_Quantity.Numeral)) + geom_boxplot()
ggplot(Map_sub, aes(y=Map_sub$AvgCorrect_Numeral.Quantity)) + geom_boxplot()

wilcox.test(Map_sub$AvgCorrect_Quantity.Numeral, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 

wilcox.test(Map_sub$AvgCorrect_Numeral.Quantity, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 

# ASYMMETRY? Quantity numeral better than numeral quantity?
wilcox.test(Map_sub$SumTotal_QuantityNumeral, Map_sub$SumTotal_NumeralQuantity, paired=TRUE)

qw <- ggplot(Map_sub, aes(x=Map_sub$Age, y=Map_sub$AvgCorrect_Quantity.Word)) + geom_point() + geom_smooth(method="loess", se = FALSE) + labs(y="Quantity-Word") + theme(text = element_text(size=11), , axis.title.x=element_blank()) + coord_cartesian(xlim = c(5, 10), ylim= c(0.4,1)) + scale_y_continuous(breaks=c(0.5, 0.6, 0.7, 0.8, 0.9, 1)) + scale_x_continuous(breaks=c(5, 6, 7, 8, 9))

wq <- ggplot(Map_sub, aes(x=Map_sub$Age, y=Map_sub$AvgCorrect_Word.Quantity)) + geom_point() + geom_smooth(method="loess", se = FALSE) + labs(x="Age (Years)", y="Word-Quantity") + theme(text = element_text(size=11)) + coord_cartesian(xlim = c(5, 10), ylim= c(0.4,1)) + scale_y_continuous(breaks=c(0.5, 0.6, 0.7, 0.8, 0.9, 1)) + scale_x_continuous(breaks=c(5, 6, 7, 8, 9))
grid.arrange(qw, wq, ncol = 1)


## Think we see an asymmetry btwn quantity word and word-quantity (this is what Hurst et al found using Wilcoxon signed rank tests)

wilcox.test(Map_sub$AvgCorrect_Quantity.Word, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 

wilcox.test(Map_sub$AvgCorrect_Word.Quantity, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 

# ASYMMETRY? Quantity word better than word quantity?
wilcox.test(Map_sub$SumTotal_QuantityWord, Map_sub$SumTotal_WordQuantity, paired=TRUE)


wn <- ggplot(Map_sub, aes(x=Map_sub$Age, y=Map_sub$AvgCorrect_Word.Numeral)) + geom_point() + geom_smooth(method="loess", se = FALSE) + labs(y="Word-Numeral") + theme(text = element_text(size=11), , axis.title.x=element_blank()) + coord_cartesian(xlim = c(5, 10), ylim= c(0.4,1)) + scale_y_continuous(breaks=c(0.5, 0.6, 0.7, 0.8, 0.9, 1)) + scale_x_continuous(breaks=c(5, 6, 7, 8, 9))

nw <- ggplot(Map_sub, aes(x=Map_sub$Age, y=Map_sub$AvgCorrect_Numeral.Word)) + geom_point() + geom_smooth(method="loess", se = FALSE) + labs(x="Age (Years)", y="Numeral-Word") + theme(text = element_text(size=11)) + coord_cartesian(xlim = c(5, 10), ylim= c(0.4,1)) + scale_y_continuous(breaks=c(0.5, 0.6, 0.7, 0.8, 0.9, 1)) + scale_x_continuous(breaks=c(5, 6, 7, 8, 9))
grid.arrange(wn, nw, ncol = 1)
# graphs look pretty similar

wilcox.test(Map_sub$AvgCorrect_Word.Numeral, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 

wilcox.test(Map_sub$AvgCorrect_Numeral.Word, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 

# ASYMMETRY?  Numeral word better than word numeral?
wilcox.test(Map_sub$SumTotal_NumeralWord, Map_sub$SumTotal_WordNumeral, paired=TRUE) #WEIRDLY THESE SHOW AN ASYMMETRY ALSO


### ALL GRAPHS ON A SINGLE PANEL (may be too small, but I LIKE IT)
grid.arrange(qn, qw, wn, nq, wq, nw, nrow=2, ncol=3)



# get average RAW NUMBER correct for each mapping type across all kids 
MapAvgCorr <- colMeans(subset(Map_sub, select = c(SumTotal_QuantityNumeral, SumTotal_NumeralQuantity, SumTotal_QuantityWord, SumTotal_WordQuantity, SumTotal_NumeralWord, SumTotal_WordNumeral)), na.rm = TRUE)
MapAvgCorr

# Mapping types are out of different numbers, so to calculate proportion correct need code below
MapMeans <- c((MapAvgCorr[1]/9), (MapAvgCorr[2]/8), (MapAvgCorr[3]/9), (MapAvgCorr[4]/8), (MapAvgCorr[5]/9), (MapAvgCorr[6]/8))
MapMeans


## WANT TO TRY LOOKING AT DATA FOR 5 YO vs 6YOs and up

#create a column for age group
Map_sub$AgeGrp <- ifelse(Map_sub$Age < 6, "5-year-olds", "6 and up")




# There has to be an easier way to get summary info for 5yos vs 6andups but for now I split into two data frames
Map5 <- subset(Map_sub, Map_sub$AgeGrp =="5-year-olds")
Map6up <- subset(Map_sub, Map_sub$AgeGrp =="6 and up")


Map5AvgCorr <- colMeans(subset(Map5, select = c(SumTotal_QuantityNumeral, SumTotal_NumeralQuantity, SumTotal_QuantityWord, SumTotal_WordQuantity, SumTotal_NumeralWord, SumTotal_WordNumeral)), na.rm = TRUE) 
Map5AvgCorr
Map5Means <- c((Map5AvgCorr[1]/9), (Map5AvgCorr[2]/8), (Map5AvgCorr[3]/9), (Map5AvgCorr[4]/8), (Map5AvgCorr[5]/9), (Map5AvgCorr[6]/8))
Map5Means

#ALL SIGNIFICANTLY DIFFERENT FROM CHANCE?
wilcox.test(Map5$AvgCorrect_Quantity.Numeral, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 
wilcox.test(Map5$AvgCorrect_Numeral.Quantity, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 
wilcox.test(Map5$SumTotal_QuantityNumeral, Map5$SumTotal_NumeralQuantity, paired=TRUE) # ASYMMETRY for 5yos? 
#YES

wilcox.test(Map5$AvgCorrect_Quantity.Word, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 
wilcox.test(Map5$AvgCorrect_Word.Quantity, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 
wilcox.test(Map5$SumTotal_QuantityWord, Map5$SumTotal_WordQuantity, paired=TRUE) # ASYMMETRY? Quantity word better than word quantity?
#YES

wilcox.test(Map5$AvgCorrect_Word.Numeral, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 
wilcox.test(Map5$AvgCorrect_Numeral.Word, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 
wilcox.test(Map5$SumTotal_NumeralWord, Map5$SumTotal_WordNumeral, paired=TRUE) # ASYMMETRY?  Numeral word better than word numeral?
#MARGINAL (p=.054)



Map6upAvgCorr <- colMeans(subset(Map6up, select = c(SumTotal_QuantityNumeral, SumTotal_NumeralQuantity, SumTotal_QuantityWord, SumTotal_WordQuantity, SumTotal_NumeralWord, SumTotal_WordNumeral)), na.rm = TRUE) 
Map6upAvgCorr
Map6Means <- c((Map6upAvgCorr[1]/9), (Map6upAvgCorr[2]/8), (Map6upAvgCorr[3]/9), (Map6upAvgCorr[4]/8), (Map6upAvgCorr[5]/9), (Map6upAvgCorr[6]/8))
MapMeans

wilcox.test(Map6up$AvgCorrect_Quantity.Numeral, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 
wilcox.test(Map6up$AvgCorrect_Numeral.Quantity, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 
wilcox.test(Map6up$SumTotal_QuantityNumeral, Map6up$SumTotal_NumeralQuantity, paired=TRUE) # ASYMMETRY for 5yos?
#YES

wilcox.test(Map6up$AvgCorrect_Quantity.Word, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 
wilcox.test(Map6up$AvgCorrect_Word.Quantity, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 
wilcox.test(Map6up$SumTotal_QuantityWord, Map6up$SumTotal_WordQuantity, paired=TRUE) # ASYMMETRY? Quantity word better than word quantity?
#YES

wilcox.test(Map6up$AvgCorrect_Word.Numeral, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 
wilcox.test(Map6up$AvgCorrect_Numeral.Word, mu = .25, alternative = "greater") # is median numeral correct significantly greater than chance (0.25) 
wilcox.test(Map6up$SumTotal_NumeralWord, Map6up$SumTotal_WordNumeral, paired=TRUE) # ASYMMETRY?  Numeral word better than word numeral?
#YES





## trying to see whether can get different slopes for different age groups but not working yet
#ggplot(Map_sub, aes(x=Map_sub$Age, y=Map_sub$AvgCorrect_Quantity.Numeral)) + geom_point() + geom_smooth(method="loess", se = FALSE, color = Map_sub$AgeGrp)
#ggplot(Map_sub, aes(x=Map_sub$Age, y=Map_sub$AvgCorrect_Numeral.Quantity)) + geom_point() + geom_smooth(method="loess", se = FALSE)

#ggplot(Map_sub, aes(x=Map_sub$Age, y=Map_sub$AvgCorrect_Quantity.Word)) + geom_point() + geom_smooth(method="loess", se = FALSE)
#ggplot(Map_sub, aes(x=Map_sub$Age, y=Map_sub$AvgCorrect_Word.Quantity)) + geom_point() + geom_smooth(method="loess", se = FALSE)

#ggplot(Map_sub, aes(x=Map_sub$Age, y=Map_sub$AvgCorrect_Word.Numeral)) + geom_point() + geom_smooth(method="loess", se = FALSE)
#ggplot(Map_sub, aes(x=Map_sub$Age, y=Map_sub$AvgCorrect_Numeral.Word)) + geom_point() + geom_smooth(method="loess", se = FALSE)


