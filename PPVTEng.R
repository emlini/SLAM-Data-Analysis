# Analysis of PPVT data from the SLAM number project
##LAST EDITED: 4/15/19


	# Analysis only includes a subset of groups tested, specifically a subset in two of the language groups under analysis ("English Early" and "English Later")

#	We'd like to know what predicts PPVT scores

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
library(car)

setwd("/Users/emilycarrigan/Dropbox/Data Analysis Work") ##this should be wherever your file is saved 

PPVT <- read.csv('PPVT_Data_190411_BS.csv', na.strings = "N/A")
View(PPVT)
typeof(PPVT) # when importing using read.csv, resulting obj type is a list (data frame)
View(PPVT)




#data cleaning & variable specification
PPVTEng <- subset(PPVT, PPVT$Including_in_study=='Yes' & PPVT$Language == 'English' & PPVT$Form != '')
PPVTEng_subset <- PPVTEng[,1:28] #remove extraneous columns
View(PPVTEng_subset)

PPVTEng_subset <- subset(PPVTEng_subset, PPVTEng_subset$Group != 'ASL Later' & PPVTEng_subset$Group != '' & PPVTEng_subset$Date.Tested != '' & PPVTEng_subset$Basal_Set_Calc_Correct == 'Yes' & PPVTEng_subset$Ceiling_Set_Calc_Correct == 'Yes' & PPVTEng_subset$Age < 7)


Age <- PPVTEng_subset$Age
Raw <- PPVTEng_subset$Raw_Score
SES <- PPVTEng_subset$SES_Score
LangGrp <- PPVTEng_subset$Group
ID <- PPVTEng_subset$SUBJECT_ID


# TESTING HOMOGENEITY OF VARIANCE in PPVT raw scores for two language timing groups:
leveneTest(y = Raw, group = LangGrp, center = mean) ##VARIANCES NOT SIGNIFICANTLY DIFFERENT!!!! (i was worried about this)

aggregate(formula = PPVTEng_subset$Age~PPVTEng_subset$Group, FUN = mean) # gives means by group!!!
aggregate(formula = PPVTEng_subset$SES_Score~PPVTEng_subset$Group, FUN = median) # gives means by group!!!
aggregate(formula = PPVTEng_subset$SES_Score~PPVTEng_subset$Group, FUN = min) # gives means by group!!!
aggregate(formula = PPVTEng_subset$SES_Score~PPVTEng_subset$Group, FUN = max) # gives means by group!!!

## Question 3: Do Early English kids have better PPVT scores than Later English kids?

LangScores <- lm(Raw~SES+Age+LangGrp)
summary(LangScores) # initial results seem significant 
par(mfrow = c(2, 2))
plot(LangScores)
stargazer(LangScores, title = "Linear Regression Results", align=TRUE, dep.var.labels=c("PPVT Raw Score"), covariate.labels=c("Socioeconomic Status (SES)", "Age (Years)", "Language Timing (Later)"),  single.row=TRUE, out= "PPVTRaw_SES_Age_Timing_190430.htm")


## QUESTION 2: Does RATE of language acquisition differ btwn early and later groups?

LangScoreRate <- lm(Raw~SES+LangGrp*Age)
summary(LangScoreRate) # initial results seem no significant effect of modality
stargazer(LangScoreRate, title = "Linear Regression Results", align=TRUE, dep.var.labels=c("PPVT Raw Score"), covariate.labels=c("Socioeconomic Status (SES)", "Language Timing (Later)", "Age (Years)", "Language Timing (Later) x Age"),  single.row=TRUE, out= "PPVTRaw_SES_TimingxAge_190415.htm")


jitter <- position_jitter(width = 0.1, height = 0.1) # create jitter object to manage jitter of points across graphs

ggplot(PPVTEng_subset, aes(x=Age, y=Raw)) + geom_point(aes(shape=LangGrp, color=LangGrp), position=jitter, size=2.5) + geom_smooth(mapping=aes(group=LangGrp, color=LangGrp), method="lm", se=FALSE) + labs(x="Age (years)", y="PPVT Raw Score") + scale_shape_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c(17, 15)) + scale_color_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,150)) + scale_y_continuous(breaks=c(0, 150, 25, 50, 75, 100, 125))



## TRYING TO INCORPORATE RANDOM EFFECTS FOR KIDS

LangScoresME <- lmer(formula = Raw~SES+LangGrp*Age + (1|ID), data = PPVTEng_subset) # this hasn't worked yet
Summary(LangScoresME)



## QUESTION 3: Does PPVT score predict Give-N (as we categorized), and does that differ for Early vs Later? 

#Need to import Give-N scores - see if Merge can do it

GiveN <- read.csv("MASTER_Coding_EC_190315.csv", na.strings = "N/A") #import the data file (which I already saved as a csv from excel file)
typeof(GiveN) # when importing using read.csv, resulting obj type is a list
View(GiveN)



GN <- dplyr::select(GiveN, SUBJECT.ID, GiveN_ALL_ceiling_conservative) #MASS has "select" function so have to specify which package using
View(GN)

#Gotta rename the SUBJECT.ID to be SUBJECT_ID (so the two dfs match)
colnames(GN)[colnames(GN)=="SUBJECT.ID"] <- "SUBJECT_ID"

PPVTEng_GiveN <- merge(PPVTEng_subset, GN, by = "SUBJECT_ID", all = FALSE)
View(PPVTEng_GiveN) ##DID IT!



#PLOT give-N by PPVT raw score

Age <- PPVTEng_GiveN$Age
Raw <- PPVTEng_GiveN$Raw_Score
SES <- PPVTEng_GiveN$SES_Score
LangGrp <- PPVTEng_GiveN$Group 
ID <- PPVTEng_GiveN$SUBJECT_ID
Give_N <- PPVTEng_GiveN$GiveN_ALL_ceiling_conservative


aggregate(formula = PPVTEng_GiveN$Age~PPVTEng_GiveN$Group, FUN = mean) # gives means by group!!!

summary(LangGrp) # gives number per group

ggplot(PPVTEng_GiveN, aes(x=Raw, y=Give_N)) + geom_point(aes(shape=LangGrp, color=LangGrp), position=jitter, size=2.5) + geom_smooth(mapping=aes(group=LangGrp, color=LangGrp), method="lm", se=FALSE) + labs(x="PPVT Raw Score", y="Give-N Highest Number Correct") + scale_shape_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c(17, 15)) + scale_color_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(ylim = c(0, 16), xlim= c(0,150)) + scale_x_continuous(breaks=c(0, 150, 25, 50, 75, 100, 125)) + scale_y_continuous(breaks=c(0, 16, 4, 8, 12))


# Question 3: Does PPVT predict Give-N scores? USING ORDINAL MODEL FOR THIS
#HAVE TO SUBSET AGAIN SO NO MISSING VALUES IN SES OR AGE
PPVTEng_GiveN <- subset(PPVTEng_GiveN, PPVTEng_GiveN$Age != '' & PPVTEng_GiveN$SES_Score != '' & PPVTEng_GiveN$Raw_Score != '' &PPVTEng_GiveN$Group != '')
View(PPVTEng_GiveN)


PPVTEng_GiveN$GN_fac <- as.factor(PPVTEng_GiveN$GiveN_ALL_ceiling_conservative)
GN_fac<- PPVTEng_GiveN$GN_fac

q <- polr(formula = GN_fac ~ SES + Age + Raw + LangGrp, data = PPVTEng_GiveN, Hess = TRUE, method = "probit")
summary(q) #AIC as of 4/17/19:  Res. Dev: 
lipsitz.test(q)
logitgof(PPVTEng_GiveN$GN_fac, fitted(q))
pulkrob.chisq(q, c("LangGrp")) 


sres <- resids(q)

p1 <- autoplot(sres, what = "covariate", x = PPVTEng_GiveN$SES_Score, xlab = "SES")
p2 <- autoplot(sres, what = "qq", distribution = qnorm)
grid.arrange(p1, p2, ncol = 2)  # code that produces Figure 2 in Greenwell et al ppr


autoplot(sres, what = "covariate", x = PPVTEng_GiveN$Age, xlab = "Age")
autoplot(sres, what = "covariate", x = PPVTEng_GiveN$Group, xlab = "Timing")
autoplot(sres, what = "covariate", x = PPVTEng_GiveN$Raw_Score, xlab = "Number Knowledge")



# checking the proportionality assumption
fit1 <- polr(formula = GiveN_subset[1:66, ]$GN_conserv_fac ~ GiveN_subset[1:66, ]$SES..8.66. + GiveN_subset[1:66, ]$Age + GiveN_subset[1:66, ]$Language_Modality + GiveN_subset[1:66, ]$Language_Timing + GiveN_subset[1:66, ]$Highest_Count_noobj, data = GiveN_subset[1:66, ], Hess = TRUE, method = "probit") 
             
fit2 <- update(fit1, data = GiveN_subset[67:131, ])

s1 <- surrogate(fit1)
s2 <- surrogate(fit2)

ggplot(data.frame(D = s1 - s2, x = GiveN_subset[1:66, ]$SES..8.66.) , aes(x = x, y = D)) +
  geom_point(color = "#444444", shape = 19, size = 2) +
  geom_smooth(se = FALSE, size = 1.2, color = "red")

ggplot(data.frame(D = s1 - s2, x = GiveN_subset[1:66, ]$Age) , aes(x = x, y = D)) +
  geom_point(color = "#444444", shape = 19, size = 2) +
  geom_smooth(se = FALSE, size = 1.2, color = "red")

ggplot(data.frame(D = s1 - s2, x = GiveN_subset[1:66, ]$Highest_Count_noobj) , aes(x = x, y = D)) +
  geom_point(color = "#444444", shape = 19, size = 2) +
  geom_smooth(se = FALSE, size = 1.2, color = "red")

ggplot(data.frame(D = s1 - s2, x = GiveN_subset[1:66, ]$Language_Timing) , aes(x = x, y = D)) +
  geom_point(color = "#444444", shape = 19, size = 2) +
  geom_smooth(se = FALSE, size = 1.2, color = "red")

ggplot(data.frame(D = s1 - s2, x = GiveN_subset[1:66, ]$Language_Modality) , aes(x = x, y = D)) +
  geom_point(color = "#444444", shape = 19, size = 2) +
  geom_smooth(se = FALSE, size = 1.2, color = "red")



GiveNscores <- glm(Give_N ~ SES + Age + Raw + LangGrp) ##PROBABLY BAD MODEL BC LINEAR
summary(GiveNscores) 
par(mfrow = c(2, 2))
plot(GiveNscores)


stargazer(GiveNscores, title = "Question 3: Does PPVT predict Give-N scores?\nLinear Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Number Correct"), covariate.labels=c("Socioeconomic Status (SES)", "Age (Years)", "PPVT Raw Score", "Language Timing (Later)"),  single.row=TRUE, out= "GN_EngOnly_SES_Age_PPVT_Timing_190415.htm")


## QUESTION 4: Does prediction of Give-N by PPVT DIFFER depending on Language Timing?

GiveNscoresTiming <- glm(Give_N ~ SES + Age + Raw * LangGrp)
summary(GiveNscoresTiming) # initial results seem no significant effect of modality
stargazer(GiveNscoresTiming, title = "Question 4: Does PPVT-GiveN Relationship vary by Language Timing?", align=TRUE, dep.var.labels=c("Give-N Highest Number Correct"), covariate.labels=c("Socioeconomic Status (SES)", "Age (Years)", "PPVT Raw Score", "Language Timing (Later)", "Language Timing (Later) x PPVT Raw Score"),  single.row=TRUE, out= "GN_EngOnly_SES_Age_PPVT_x_Timing_190415.htm")





##### ###### ##### ##### #####

##OLD CODE

#Data Visualization
ggplot(PPVTEng_subset, aes(x=Age, y=Raw)) + geom_point() + geom_smooth(method='lm')
#Relationship btwn Age and Raw Scores
#looks like might have clustering of ages btw 4.5-6
cor.test(Age, Raw)

#NOW BREAK DOWN BY LANGUAGE GROUP using facets
r <- ggplot(PPVTEng_subset, aes(x=Age, y=Raw)) + geom_point() + geom_smooth(method='lm')
r + facet_wrap(~PPVTEng_subset$Group, nrow=2)





#check NORMALITY of Age, HearingAge, SES, Raw, and Std
ggplot(PPVTEng_subset, aes(x=Age, y=..count..)) + geom_density()
shapiro.test(Age) # marginal 
ggplot(PPVTEng_subset, aes(x=HearingAge, y=..count..)) + geom_density()
shapiro.test(HearingAge) # HEARING AGE NORMALLY DISTRIBUTED 
ggplot(PPVTEng_subset, aes(x=SES, y=..count..)) + geom_density()
shapiro.test(SES) # SKEWED (more kids at higher SES levels) 
ggplot(PPVTEng_subset, aes(x=Raw, y=..count..)) + geom_density()
shapiro.test(Raw) #skewed
ggplot(PPVTEng_subset, aes(x=Std, y=..count..)) + geom_density() 
shapiro.test(Std) #NORMAL (should be!)

ggplot(PPVTEng_subset, aes(x=Age, y=Std)) + geom_point() + geom_smooth(method='lm')
#Relationship btwn Age and Std Scores
cor.test(Age, Std)
#THIS SHOULDN'T BE SIGNIFICANT (standard scores are supposed to be the same across ages)

#NOW BREAK DOWN BY LANGUAGE GROUP using facets
r <- ggplot(PPVTEng_subset, aes(x=Age, y=Raw)) + geom_point() + geom_smooth(method='lm')
r + facet_wrap(~PPVTEng_subset$Group, nrow=2)
#Relationship btwn age and Raw score by Language Grp

s <- ggplot(PPVTEng_subset, aes(x=Age, y=Std)) + geom_point() + geom_smooth(method='lm')
t<- s + labs(title="PPVT Scores by Age",  
	x="Age at Test (Years)",
	y="Standardized PPVT Score")
t + theme(plot.title = element_text(hjust = 0.5))
s + facet_wrap(~PPVTEng_subset$Group, nrow=2)
#Relationship btwn age and Std score by Language Grp

	##WHY DOES IT LOOK LIKE FACETED GRAPH PULLING FROM DIFFERENT DATA THAN COMBINED??????

#trying another way to look at the data (on same graph w diff color dots for diff Lng Grp)
ggplot(PPVTEng_subset, aes(x=Age, y=Std)) + geom_point(color=factor(as.integer(PPVTEng_subset$Group))) + geom_smooth(method='lm')
	#worked but don't know which color is which (also doesn't seem to match up with FACETED data)

##Seems like age distribution difference for different language groups, so split them up and check for each individually
#There is likely a more elegant way to do this, but I haven't determined what that is yet 
PPVTEng_Lat <- subset(PPVTEng_subset, PPVTEng_subset$Group=="English Later")
View(PPVTEng_Lat)

PPVTEng_Ear <- subset(PPVTEng_subset, PPVTEng_subset$Group=="English Early")
View(PPVTEng_Ear)

shapiro.test(PPVTEng_Lat$Age) #NORMAL
shapiro.test(PPVTEng_Ear$Age) # NOT NORMAL
cor.test(PPVTEng_Lat$Age, PPVTEng_Lat$Standard_Score)
cor.test(PPVTEng_Ear$Age, PPVTEng_Ear$Standard_Score)

h<-ggplot(PPVTEng_Ear, aes(x=PPVTEng_Ear$Age, y=PPVTEng_Ear$Standard_Score)) + geom_point()
j<- h + labs(title="'English Early' Group: PPVT Scores by Age",  
	x="Age at Test (Years)",
	y="Standardized PPVT Score")
j + theme(plot.title = element_text(hjust = 0.5))

k<- ggplot(PPVTEng_Lat, aes(x=PPVTEng_Lat$Age, y=PPVTEng_Lat$Standard_Score)) + geom_point()
l<- k + labs(title="'English Later' Group: PPVT Scores by Age",  
	x="Age at Test (Years)",
	y="Standardized PPVT Score")
l + theme(plot.title = element_text(hjust = 0.5))

# AGE IS GIVING US PROBLEMS, and variance for Age diff across language groups, so let's check hearing age 

ggplot(PPVTEng_subset, aes(x=HearingAge, y=Std)) + geom_point() + geom_smooth(method='lm')
#Relationship btwn HearingAge and Std Scores
cor.test(HearingAge, Std) #NOT SIG


ggplot(PPVTEng_subset, aes(x=SES, y=Std)) + geom_point() + geom_smooth(method='lm')
#Relationship btwn SES and Std Scores
cor.test(SES, Std, method="kendall") # Kendall bc SES not normally distributed
#SES NOT SIGNIFICANTLY RELATED TO STD SCORES


#Boxplot comparing std scores by lang group
m<- ggplot(PPVTEng_subset, aes(x=LangGrp, y=Std)) + geom_boxplot()
n<- m + labs(title="PPVT Scores by Language Group",  
	x="Language Group",
	y="Standardized PPVT Score")
n + theme(plot.title = element_text(hjust = 0.5))
t.test(Std~LangGrp, var.equal=FALSE) #<-- using Welch t-test bc I THINK variance not equal (but can't tell for sure)




#Quick models using Standard scores


#Model with Age and Language Group
PPVTLanAge <- glm(Std~Age+LangGrp)
summary(PPVTLanAge)

#Model with only Language Group (not a good model, since Age significant in previous model, just wanted to see for comparisons purposes)
PPVTLangGrp <- glm(Std~LangGrp)
summary(PPVTLangGrp)

#Future models will include SES, possible also "HearingAge" (for English Later kids, this is the age at which they got their first assistive device)

PPVTAgeLangGrpSES <- glm(Std~Age+LangGrp+SES)
summary(PPVTAgeLangGrpSES)

PPVTAgeLangGrpint <- glm(Std~Age*LangGrp)
summary(PPVTAgeLangGrpint)

PPVTAgeLangGrp <- glm(Std~Age+LangGrp)
summary(PPVTAgeLangGrp) # BEST MODEL

 PPVTHAgeLangGrpSES <- glm(Std~HearingAge+LangGrp+SES)
> summary(PPVTHAgeLangGrpSES)
#Also going to explore whether it might be possible to use Raw scores or if we can standardize the scores in some way (so we don't lose as many data points)
