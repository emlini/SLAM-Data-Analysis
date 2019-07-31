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

MagComp <- read.csv("MagnitudeComparison_Coding_GP_190416.csv", na.strings = c("N/A", "#DIV/0!")) #import the data file (which I already saved as a csv from excel file)
typeof(MagComp) # when importing using read.csv, resulting obj type is a list (data frame)
View(MagComp)

## FOR LANGFEST 2019: Subset hearing kids to look at their performance on ASL HANDSHAPE comparisons on trials with the same number of extended fingers versus different numbers of extended Fingers

# MagComp <- MagComp[1:42,1:23] # for Adjusted spreadsheet


MagComp_subset <- subset(MagComp, MagComp$Including_in_study=='Yes' & MagComp$Group_2cat == 'Hearing' & MagComp$Form...A.or.B.!='') # This can be amended as necessary for the specific analyses 
View(MagComp_subset)

## TRYING TO USE TO SELECT ONLY ASL HANDSHAPE COLUMNS - NOT WORKING YET
# dplyr::select(MagComp_subset, one_of(c(starts_with("Han"),starts_with("Matching"),starts_with("Nonmatching"))))

ggplot(MagComp_subset, aes(x=Matching_propcorr, y=..count..)) + geom_dotplot(method = "histodot") + labs(x="Proportion Correct", y="Count") + ggtitle("'Same Number of Fingers Extended' Trials") + theme(text = element_text(size=14), plot.title = element_text(hjust = 0.5)) + coord_cartesian(xlim = c(0.5, 1))
ggplot(MagComp_subset, aes(x=Nonmatching_propcorr, y=..count..)) + geom_dotplot(method = "histodot") +  ggtitle("'Different Number of Fingers Extended' Trials") + labs(x="Proportion Correct", y="Count") + theme(text = element_text(size=14), plot.title = element_text(hjust = 0.5)) + coord_cartesian(xlim = c(0.5, 1), ylim = c(0, 0.5))

ggplot(MagComp_subset, aes(x=Age, y=Nonmatching_propcorr)) + geom_point() +  ggtitle("'Different Number of Fingers Extended' Trials") + labs(y="Proportion Correct", x="Age (Years)") + theme(text = element_text(size=14), plot.title = element_text(hjust = 0.5)) + coord_cartesian(xlim = c(5, 9), ylim = c(0.5, 1))


ggplot(MagComp_subset, aes(x=Age, y=Matching_propcorr)) + geom_point() +  ggtitle("'Same Number of Fingers Extended' Trials") + labs(y="Proportion Correct", x="Age (Years)") + theme(text = element_text(size=14), plot.title = element_text(hjust = 0.5)) + coord_cartesian(xlim = c(5, 9), ylim = c(0.5, 1))

# Compare Matching to chance

t.test(MagComp_subset$Matching_propcorr, mu=0.5) #above chance on matching

 


# Compare nonmatching to chance
t.test(MagComp_subset$Nonmatching_propcorr, mu=0.5)

TEST HOMOGENEITY OF VARIANCE OF MATCHING VS NON-MATCHING
fligner.test(dataset~groups, data=dataframe) # fligner test supposed to be robust to departures from normality

#Compare Matching to Nonmatching - MIGHT NEED NONPARAMETRIC BC HOMOGENEITY OF VARIANCE NOT ASSUMED
t.test(x=MagComp_subset$Matching_propcorr, y=MagComp_subset$Nonmatching_propcorr, paired = TRUE)

#total # of trials answered
mean(MagComp_subset$Matching_Answered, na.rm = TRUE)
mean(MagComp_subset$Nonmatching_Answered, na.rm = TRUE)

# total # of trials correct
mean(MagComp_subset$Matching_Correct, na.rm = TRUE)
mean(MagComp_subset$Nonmatching_Correct, na.rm = TRUE)

# Average number of trials completed in entire ASL handshape set
mean(MagComp_subset$Total_Complete_Han, na.rm = TRUE)

#EXCEPT 


## Hearing kids on 3 vs. 6, 7, 8, 9
## COMPARISONS WE DON'T HAVE: 9 vs 6 or 6 vs 9

# DOESN'T HAVE ALL COMPARISONS three_comps <- c("SUBJECT_ID", "Han_3_vs._6.", "Han_3_vs._7.", "Han_3_vs._8.", "Han_3_vs._9.", "Han_6_vs._3.", "Han_7_vs._3.", "Han_8_vs._3.", "Han_9_vs._3.", "Han_6_vs._7.", "Han_6_vs._8.", "Han_7_vs._9.", "Han_8_vs._9.", "Han_9_vs._6.", "Han_9_vs._7.", "Han_9_vs._8.")

#DOES HAVE ALL THE SAME FINGER comparisons
ASLhand_threecomps <- MagComp_subset[, (names(MagComp_subset) %in% c("SUBJECT_ID", "Han_3_vs._7.", "Han_3_vs._9.", "Han_8_vs._3.", "Han_9_vs._3.", "Han_6_vs._3.", "Han_3_vs._8.", "Han_7_vs._3.", "Han_6_vs._8.", "Han_8_vs._7.", "Han_7_vs._9.", "Han_9_vs._7.", "Han_8_vs._9.", "Han_6_vs._7.", "Han_8_vs._6.", "Han_7_vs._6.", "Han_7_vs._8.", "Han_3_vs._6.",  "Han_9_vs._8."))]

View(ASLhand_threecomps)

ID_columns <- MagComp_subset[1:14]
View(ID_columns)

ASLhand_ID_threecomps <- merge(ID_columns, ASLhand_threecomps, by = "SUBJECT_ID")
View(ASLhand_ID_threecomps)

three_six <- ASLhand_ID_threecomps$Han_3_vs._6.
three_seven <- ASLhand_ID_threecomps$Han_3_vs._7.
three_eight <- ASLhand_ID_threecomps$Han_3_vs._8.
three_nine <- ASLhand_ID_threecomps$Han_3_vs._9.
six_three <- ASLhand_ID_threecomps$Han_6_vs._3.
seven_three <- ASLhand_ID_threecomps$Han_7_vs._3.
eight_three <- ASLhand_ID_threecomps$Han_8_vs._3.
nine_three <- ASLhand_ID_threecomps$Han_9_vs._3.
# six_nine <- ASLhand_ID_threecomps$Han_6_vs._9.
seven_nine <- ASLhand_ID_threecomps$Han_7_vs._9.
eight_nine <- ASLhand_ID_threecomps$Han_8_vs._9.
nine_seven <- ASLhand_ID_threecomps$Han_9_vs._7.
nine_eight <- ASLhand_ID_threecomps$Han_9_vs._8.


Column_Means <- colMeans(subset(ASLhand_ID_threecomps, select = c(Han_3_vs._7., Han_3_vs._9., Han_8_vs._3., Han_9_vs._3., Han_6_vs._3., Han_3_vs._8., Han_7_vs._3., Han_6_vs._8., Han_8_vs._7., Han_7_vs._9., Han_9_vs._7., Han_8_vs._9., Han_6_vs._7., Han_8_vs._6., Han_7_vs._6., Han_7_vs._8., Han_3_vs._6.,  Han_9_vs._8.)), na.rm = TRUE)


Column_Means_paired <- colMeans(subset(ASLhand_ID_threecomps, select = c(Han_3_vs._6., Han_6_vs._3., Han_3_vs._7., Han_7_vs._3., Han_3_vs._8., Han_8_vs._3., Han_3_vs._9., Han_9_vs._3., Han_6_vs._7., Han_7_vs._6., Han_6_vs._8., Han_8_vs._6., Han_7_vs._8., Han_8_vs._7., Han_7_vs._9., Han_9_vs._7., Han_8_vs._9., Han_9_vs._8.)), na.rm = TRUE)


#means for each of the comparisons where the number of selected fingers is the same (3)
MatchedMeans <- colMeans(subset(MagComp, select = c(Han_3_vs._6,Han_3_vs._7, Han_3_vs._8, Han_3_vs._9, Han_6_vs._7, Han_6_vs._8, Han_7_vs._8, Han_7_vs._9, Han_8_vs._9)), na.rm = TRUE)

#total number correct (sum of values in each column)
MatchedSums <- colSums(subset(MagComp, select = c(Han_3_vs._6,Han_3_vs._7, Han_3_vs._8, Han_3_vs._9, Han_6_vs._7, Han_6_vs._8, Han_7_vs._8, Han_7_vs._9, Han_8_vs._9)), na.rm = TRUE)

#total number of data points represented for each comparison
length(which(MagComp$Han_3_vs._6 !=''))
length(which(MagComp$Han_3_vs._7 !=''))
length(which(MagComp$Han_3_vs._8 !=''))
length(which(MagComp$Han_3_vs._9 !=''))
length(which(MagComp$Han_6_vs._7 !=''))
length(which(MagComp$Han_6_vs._8 !=''))
length(which(MagComp$Han_7_vs._8 !=''))
length(which(MagComp$Han_7_vs._9 !=''))
length(which(MagComp$Han_8_vs._9 !=''))


#TRY TO USE GATHER TO MAKE PAIRS and see what paired values are
gather(ASLhand_ID_threecomps, key = "", value = "value", ..., na.rm = FALSE,
  convert = FALSE, factor_key = FALSE)


rowMeans(subset(z, select = c(x, y)), na.rm = TRUE)


1) For all hearing kids, how well do they as a group do on each of the comparisons where the two handshape have the same number of selected fingers? (3 vs 6, 3 vs 7, etc?)
		EXPECT CHANCE VALUES FOR EACH 

average prop correct for each number type (dot)

2) How well does each hearing kid do on the comparisons where the two handshape have the same number of selected fingers overall? EXPECT kids at chance, does this vary by kid?


