# Mr. Elephant Coding sheet

#Last updated 4/16/19


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

MrE <- read.csv("MrE_Coding_EK_190415.csv", na.strings = c("N/A", "#DIV/0!")) #import the data file (which I already saved as a csv from excel file)
typeof(MrE) # when importing using read.csv, resulting obj type is a list (data frame)
View(MrE)

## FOR LANGFEST 2019: Subset hearing kids to look at their performance only 

MrE_subset <- subset(MrE, MrE$Including_in_Mr_E.=='Yes' & MrE$Date.Tested != '' & MrE$Average.Correct != '') # This can be amended as necessary for the specific analyses 
View(MrE_subset)
str(MrE_subset)



MrE_subset$LanguageGroup <- as.factor(factor(as.character(MrE_subset$Group_4cat), levels = c("English Early", "ASL Early", "English Later", "ASL Later"), exclude=NA))


MrE_subset$Language_Modality <- ifelse(MrE_subset$LanguageGroup == "English Early" | MrE_subset$LanguageGroup == "English Later", "English", "ASL")
MrE_subset$Language_Modality



MrE_subset$Language_Timing <- factor(as.character(MrE_subset$Group_2cat), levels = c("Early", "Later"), exclude="")



Age <- MrE_subset$Age
SES <- MrE_subset$SES..8.66.
Modality <- MrE_subset$Language_Modality
Timing <- MrE_subset$Language_Timing
ProCorr <- MrE_subset$Average.Correct
Stuck <- MrE_subset$Average.Correct_OneInside
Empty <- MrE_subset$Average.Correct_Empty
Small <- factor(as.character(MrE_subset$Average_Correct_Sm), levels = c("0.5", "1"), labels = c("Chance", "Correct"), exclude=NA) 
Large <- MrE_subset$Average_Correct_Lg


# Split data into Early vs. Later
MrE_LaterOnly <- subset(MrE_subset, MrE_subset$Language_Timing == "Later") # This can be amended as necessary for the specific analyses 
View(MrE_LaterOnly)
str(MrE_LaterOnly)

MrE_EarlyOnly <- subset(MrE_subset, MrE_subset$Language_Timing == "Early") # This can be amended as necessary for the specific analyses 
View(MrE_EarlyOnly)
str(MrE_EarlyOnly)


# Let's VISUALIZE!

# MrE by Age
jitter <- position_jitter(width = 0.1, height = 0.1) # create jitter object to manage jitter of points across graphs

ggplot(MrE_subset, aes(x=Age, y=ProCorr)) + geom_point(aes(shape=Timing, color=Timing), position = jitter, size=2.5) + geom_smooth(mapping=aes(group=Timing, color=Timing), method="lm", se=FALSE) + labs(x="Age", y="Mr. Elephant Overall Prop. Correct") + scale_shape_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c(17, 15)) + scale_color_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 8), ylim= c(0,1))


ggplot(MrE_subset, aes(x=Age, y=Small)) + geom_point(aes(shape=Timing, color=Timing), position = jitter, size=2.5) + geom_smooth(mapping=aes(group=Timing, color=Timing), method="lm", se=FALSE) + labs(x="Age", y="Mr. Elephant Overall Prop. Correct") + scale_shape_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c(17, 15)) + scale_color_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 8), ylim= c(0,1))




ggplot(MrE_subset, aes(x=Age, y=Large)) + geom_point(aes(shape=Timing, color=Timing), position = jitter, size=2.5) + geom_smooth(mapping=aes(group=Timing, color=Timing), method="lm", se=FALSE) + labs(x="Age", y="Mr. Elephant Large Prop. Correct") + scale_shape_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c(17, 15)) + scale_color_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 8), ylim= c(0,1))



ggplot(MrE_subset, aes(x=Timing, y=Small)) + geom_jitter() + labs(x="Timing of Language Exposure", y="Mr. E Small Trials Prop. Correct") + theme(text = element_text(size=16)) + scale_y_discrete(na.translate = FALSE)

timingx2 <- chisq.test(Timing, Small)
timingx2$observed
round(timingx2$expected,2)


ggplot(MrE_subset, aes(x=Timing, y=Large)) + geom_boxplot() + labs(x="Timing of Language Exposure", y="Mr. E Large Trials Prop. Correct") + theme(text = element_text(size=16))

timingLgx2 <- chisq.test(Timing, Large)
timingLgx2$observed
round(timingLgx2$expected,2)
# Try Cramer's V for STRENGTH statistic in Chi-Sq


# ggplot(MrE_subset, aes(x=Timing, y=Small)) + geom_jitter(aes(shape=Timing, color=Timing)) + labs(x="Timing of Language Exposure", y="Mr. Elephant Overall Prop. Correct") + scale_shape_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c(17, 15)) + scale_color_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c("forestgreen","tan2")) +theme(text = element_text(size=16))


ggplot(MrE_subset, aes(x=Modality, y=Small)) + geom_jitter() + labs(x="Modality of Language Exposure", y="Mr. E Small Trials Prop. Correct") + theme(text = element_text(size=16)) + scale_y_discrete(na.translate = FALSE)
modalityx2 <- chisq.test(Modality, Small)
modalityx2$observed
round(modalityx2$expected,2)




# Checking whether there is a modality difference for only Early exposed kids (this will be indicative of whether deafness somehow alters parallel individuation sytem or spontaneous focus on numerosity) 
Modality <- MrE_LaterOnly$Language_Modality
Small <- factor(as.character(MrE_LaterOnly$Average_Correct_Sm), levels = c("0.5", "1"), labels = c("Chance", "Correct"), exclude=NA) 
ggplot(MrE_LaterOnly, aes(x=Modality, y=Small)) + geom_jitter() + labs(x="Timing of Language Exposure", y="Mr. E Small Trials Prop. Correct") + theme(text = element_text(size=16)) + scale_y_discrete(na.translate = FALSE)

modalityx2 <- chisq.test(Modality, Small)
modalityx2$observed
round(modalityx2$expected,2)

wilcox.test(MrE_LaterOnly$Average_Correct_Sm, mu=0.5, alternative = "greater") # LATER EXPOSED ABOVE CHANCE ON SMALL NUMBERS
wilcox.test(MrE_EarlyOnly$Average_Correct_Sm, mu=0.5, alternative = "greater") # EARLY EXPOSED ABOVE CHANCE ON SMALL NUMBERS



Modality_early <- MrE_EarlyOnly$Language_Modality 
Small_early <- factor(as.character(MrE_EarlyOnly$Average_Correct_Sm), levels = c("0.5", "1"), labels = c("Chance", "Correct"), exclude=NA) 
Large_early <- $Average_Correct_Lg

ggplot(MrE_EarlyOnly, aes(x=Modality_early, y=Small_early)) + geom_jitter() + labs(x="Language Modality (Early-Exposed only)", y="Mr. E Small Trials Prop. Correct") + theme(text = element_text(size=16)) + scale_y_discrete(na.translate = FALSE)
modalityearlyx2 <- chisq.test(Modality_early, Small_early)
modalityearlyx2$observed
round(modalityearlyx2$expected,2)


# Scatterplot for overall proportion correct by Age
ggplot(MrE_subset, aes(x=Age, y=Large)) + geom_point(aes(shape=Timing, color=Timing), position = jitter, size=2.5) + geom_smooth(mapping=aes(group=Timing, color=Timing), method="lm", se=FALSE) + labs(x="Age", y="Mr. Elephant Overall Prop. Correct") + scale_shape_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c(17, 15)) + scale_color_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 8), ylim= c(0,1))




$$DO THE GRAPH BELOW$$

# ANNA WANTS: Botplot w knower level on x-axis and Proportion correct (or similar) Y axis w/paired early vs later

