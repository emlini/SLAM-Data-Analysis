#LAST EDITED 3/17/19 (using data collected end of 2018 and early 2019)
#ALL SETUP IN "GiveN_setup_descriptives_plots.R" file



##Keeping variable name assignments in here for ease of switching between different dataframes

#ASSIGN shorter VARIABLE names
GN_sm_int <- GiveN_subset$Knower.level_GiveN_Small #KL drawing only from Give-N small data (which all children completed)
GN_ALL_int <- GiveN_subset$GiveN_ALL_ceiling
GN_conserv_int <- GiveN_subset$GiveN_ALL_ceiling_conservative
GN_sm_fac <- GiveN_subset$GN_sm_fac
GN_ALL_fac <- GiveN_subset$GN_ALL_fac
GN_conserv_fac <- GiveN_subset$GN_conserv_fac
GN_lg_fac <- GiveN_subset$GN_lg_fac

#predictors
Age <- GiveN_subset$Age
SES <- GiveN_subset$SES..8.66.
LangGrp <- GiveN_subset$LanguageGroup
Count_NObj <- GiveN_subset$Highest_Count_noobj
Count_WObj <- GiveN_subset$Highest_Count_wobj
MAX_Count_Seq <- GiveN_subset$MAX_COUNT_SEQ
Modality <- GiveN_subset$Language_Modality
Timing <- GiveN_subset$Language_Timing

# SCATTERPLOT GIVE-N by AGE (TIMING) - EARLY AND LATER EXPOSED

jitter <- position_jitter(width = 0.1, height = 0.1) # create jitter object to manage jitter of points across graphs

#PLOT ONLY
ggplot(GiveN_subset, aes(x=Age, y=GN_conserv_int)) + geom_point(aes(shape=Timing, color=Timing), position=jitter, size=2.5) + labs(x="Age (years)", y="Give-N Highest Quantity Correct") + scale_shape_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c(17, 15)) + scale_color_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c("forestgreen","tan2")) + theme(text = element_text(size=18)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,16)) + scale_y_continuous(breaks=c(0,16,4,8,12)) + geom_quantile(quantiles=c(.5))

#PLOT w/LINES by group (method="lm" for straight, "loess" for curve) - se=FALSE means no gray area around lines
ggplot(GiveN_subset, aes(x=Age, y=GN_conserv_int)) + geom_point(aes(shape=Timing, color=Timing), position=jitter, size=2.5) + geom_smooth(mapping=aes(group=Timing, color=Timing), method="loess", se=FALSE) + labs(x="Age (years)", y="Give-N Highest Quantity Correct") + scale_shape_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c(17, 15)) + scale_color_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,16)) + scale_y_continuous(breaks=c(0,16,4,8,12))

#PLOT w/quartile regression lines (for all data)
ggplot(GiveN_subset, aes(x=Age, y=GN_conserv_int)) + geom_point(aes(shape=Timing, color=Timing), position="jitter", size=2.5) + labs(x="Age (years)", y="Give-N highest quantity correct") + scale_shape_manual(name="Language\nTiming", labels=c("Early", "Later"), values=c(17, 15)) +scale_color_manual(name="Language\nTiming", labels=c("Early", "Later"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,16)) + geom_quantile(quantiles=c(.25, .5, .75))
    #geom_quantile requires package quantreg and calculates regression lines for the specified quantiles





#ORDINAL REGRESSION MODELS LOOKING ONLY AT LATER-EXPOSED GROUPS
#Instructions for this from: https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/

#SUBSET THE DATA for this specific analysis
GiveNLater_exp <- subset(GiveN_subset, GiveN_subset$Group_4cat=="English Later" | GiveN_subset$Group_4cat=="ASL Later")
View(GiveNLater_exp)


#LATER EXPOSED GROUPS VARIABLE
GN_sm_int <- GiveNLater_exp$Knower.level_GiveN_Small #KL drawing only from Give-N small data (which all children completed)
GN_lg_int <- GiveNLater_exp$GiveN_Large_Ceiling
GN_ALL_int <- GiveNLater_exp$GiveN_ALL_ceiling
GN_conserv_int <- GiveNLater_exp$GiveN_ALL_ceiling_conservative
GN_sm_fac <- GiveNLater_exp$GN_sm_fac
GN_ALL_fac <- GiveNLater_exp$GN_ALL_fac
GN_conserv_fac <- GiveNLater_exp$GN_conserv_fac


#predictors
Age <- GiveNLater_exp$Age
SES <- GiveNLater_exp$SES..8.66.
Modality <- GiveNLater_exp$Language_Modality # this only has the modality not the timing
LangGrp <- GiveNLater_exp$LanguageGroup # this has both language modality and timing
Count_NObj <- GiveNLater_exp$Highest_Count_noobj
Count_WObj <- GiveNLater_exp$Highest_Count_wobj
MAX_count_seq <- GiveNLater_exp$MAX_COUNT_SEQ
Timing <- GiveNLater_exp$Language_Timing


ggplot(GiveNLater_exp, aes(x=Count_NObj, y=GN_conserv_int)) + geom_point(aes(shape=Modality, color=Modality), position="jitter", size=2.5) + geom_smooth(mapping=aes(group=Modality, color=Modality), method="lm", se=FALSE) + labs(x="Highest Count (no objects)", y="Give-N Highest Quantity Correct") + scale_shape_manual(name="Language\nModality", labels=c("ASL", "English"), values=c(19, 17)) + scale_color_manual(name="Language\nModality", labels=c("ASL", "English"), values=c("grey67", "mediumvioletred")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(0, 20), ylim= c(0,16)) + scale_y_continuous(breaks=c(0,16,4,8,12))


GiveNLater_exp <- subset(GiveN_subset, GiveN_subset$Group_4cat=="English Later" | GiveN_subset$Group_4cat=="ASL Later")
View(GiveNLater_exp)


##GET NUMBERS & demographics (as of 3/18/19)
length(which(Modality=="ASL")) #25
length(which(Modality=="English")) #50

LaterASL <- subset(GiveNLater_exp, Modality=="ASL") #to get mean age
summary(LaterASL$Age) # 5.58 (5;7)


LaterEng <- subset(GiveNLater_exp, Modality=="English")
summary(LaterEng$Age) # 5.08 (5;7)


# Age of exposure needs to come from Background questionnaire BUT HOLY BANANAS THE VERSION I HAVE (190207) IS NO GOOD.  


##ORDINAL MODEL with LATER EXPOSED GROUPS ONLY

j <- polr(formula = GN_conserv_fac ~ SES + Age + Modality + Count_NObj, data = GiveNLater_exp, Hess = TRUE, method = "probit")
summary(j)


j$AIC <- AIC(j) #needed to get AIC in output table
stargazer(j, title = "Question 1: Ordinal Logistic Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct"), covariate.labels=c("Socioeconomic Status (SES)", "Age (Years)", "Language Modality (Spoken English)"), single.row=TRUE, keep.stat=c("n", "aic"), out= "GN_all_conserv_Modality_Age_SES_181002.htm")
##haven't yet figured out how to get res.dev and null.dev to show up in output


## OUTPUT TABLE THAT INCLUDES t and p VALUES FOR EACH COEFFICIENT - bit awkward in formatting execution but good to have
stargazer(j, title = "Question 1: Ordinal Logistic Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct"), covariate.labels=c("Socioeconomic Status (SES)", "Age (Years)", "Language Modality (Spoken English)"), single.row=TRUE, keep.stat=c("n", "aic"), report=('vc*stp'), out= "GN_all_conserv_Modality_Age_SES_t_p_180726.htm")


#TEST MODEL FIT USING generalhoslem package (three tests): https://cran.r-project.org/web/packages/generalhoslem/generalhoslem.pdf

lipsitz.test(j)
logitgof(GiveN_subset$GN_conserv_fac, fitted(h))
pulkrob.chisq(h, c("Modality")) 





##EXAMINING EARLY VS LATE in terms of quantiles, trying to see what features distinguish the two groups

#Scatterplot above with regression line at 50th quantile shows that is a pretty good split (shows good division of data above/below that line).
#NEXT need to figure out how to split the data (those above the 50th quantile and those below)

quantile(GN_conserv_int) # tells me that (as of 3/17/19), quantiles for data are below:
#  0%  25%  50%  75% 100% 
#   0    4    7   16   16  

# Decide how to split data - see how many are at 50th quantile value of 7
length(which(GN_conserv_int==7)) # 17
length(which(GN_conserv_int>7)) #69 values GREATER than 7
length(which(GN_conserv_int<7)) #66 values LESS than 7
nrow(GiveN_subset) #152 observations in the dataset (which matches summing results of three length() functions above)

##This creates a factor variable that splits the data by whether it is above/below a particular value (but NOT THE SAME as the above/below regression line) 
# decision to use 8 as split point may seem somewhat arbitrary, given 7 is the 50th quantile, but looking at scatterplot convinced me that this split is justified in terms of data clustering
##THIS IS ESSENTIALLY a MEDIAN SPLIT
qsplit.50 <- cut2(x=GN_conserv_int, cuts=8)
 table(qsplit.50)
#z
#[ 0, 8) [ 8,16] 
#     80      62 
str(qsplit.50) # check what the split looks like 

GiveN_subset$Quantile_50_split <- factor(qsplit.50, levels=c(0,1)) # add quantile split info to dataframe

q.50 <- GiveN_subset$Quantile_50_split

#model looking at membership in above or below quantile split by various predictors
TimQuartLogit <- glm(q.50 ~ Timing + Modality + Age + SES, data= GiveN_subset, family=binomial)


#Checking to make sure the "1," "2" designation given by cut2() doesn't bias the coefficient estimates
test <- factor(qsplit.50, labels=c(0,1)) # turn qsplit.50 into factor that has 0/1 for labels
GiveN_subset$test <- test # add quantile split info to dataframe

q.50_test <- GiveN_subset$test
TimQuartLogit <- glm(q.50_test ~ Timing + Modality + Age + SES, data= GiveN_subset, family=binomial) # EVERYTHING IS THE SAME


#another way to split data
#x <- rnorm(100)
#qx <- quantile(x)
#ind <- cut(x, qx, include.lowest = TRUE)
#split(x, ind)

## OKAY NOW ACTUALLY TRYING TO SPLIT by under/over QR line
##THIS IS BETTER THAN a simple median split bc this takes into account the fact that kids improve with age (and doesn't just look at the median split collapsed across all ages)

fit50 <- rq(formula = GN_conserv_int ~ Age, data = GiveN_subset, tau = 0.5)

##adds fitted values to dataframe and creates short variable name to refer to that column
GiveN_subset$fitval50 <- fit50$fitted.values
fitval50 <-GiveN_subset$fitval50

GiveN_subset$Regline.50_split <- ifelse(GN_conserv_int > fitval50, "Higher", "Lower") # returns a factor indicating whether participant's Give-N value is in the Upper or Lower 50th quantile of the data

GiveN_subset$Regline.50_split <- as.factor(factor(GiveN_subset$Regline.50_split, levels = c("Lower", "Higher"))) #factorizes the variable

#NOW SPLIT OFF later-exposed kids so we can look at, for that group, WHAT VARIABLES PREDICT membership in above or below regression line group

Later_exposed <- subset(GiveN_subset, Language_Timing=="Later") #75 data points as of 3/17/19, 50 English and 25 ASL (LOT OF MISSING SES data from ASL participants!!)

srcd_depvar <- Later_exposed$Regline.50_split
Modality <-Later_exposed$Language_Modality
Age <- Later_exposed$Age
SES <- Later_exposed$SES..8.66.
Sex <- Later_exposed$Sex

Count_NObj <- Later_exposed$Highest_Count_noobj
Count_WObj <- Later_exposed$Highest_Count_wobj
MAX_count_seq <- Later_exposed$MAX_COUNT_SEQ

ggplot(data=Later_exposed, aes(x=srcd_depvar, y=Age)) + geom_boxplot() # don't seem to differ in terms of age
wilcox.test(Age~srcd_depvar, Later_exposed) ##AGE not normally distributed #NOT SIG but this is a sig predictor in model--need violin plot
ggplot(data=Later_exposed, aes(x=srcd_depvar, y=Age)) + geom_violin() + labs(x="Give-N Group (determined by 50th quartile regression split)", y="Age (years)") #distribution at specific ages doesn't look much different for Higher vs Lower grps (but LOWER is a bit top-heavy--more older kids in Lower groups)

ggplot(data=Later_exposed, aes(x=Age, y=..count..)) + geom_density() ##HAVE MORE OLDER KIDS

ggplot(data=Later_exposed, aes(x=srcd_depvar, y=SES)) + geom_boxplot()
t.test(SES~srcd_depvar, var.equal=FALSE)  #t = -1.7992, df = 65, p-value = 0.07663

ggplot(data=Later_exposed, aes(x=srcd_depvar, y=Sex)) + geom_jitter() # don't seem to differ in terms of sex

ggplot(data=Later_exposed, aes(x=srcd_depvar, y=MAX_count_seq)) + geom_boxplot()
t.test(MAX_count_seq~srcd_depvar, var.equal=FALSE)  #t = -1.8687, df = 65.931, p-value = 0.0661

ggplot(data=Later_exposed, aes(x=srcd_depvar, y=Count_NObj)) + geom_boxplot() # not sig diff in terms of Count word knowledge

ggplot(data=Later_exposed, aes(x=srcd_depvar, y=Count_WObj)) + geom_boxplot()  ##ABILITY TO COUNT DIFFERS
t.test(Count_WObj~srcd_depvar, var.equal=FALSE) ###t = -2.1062, df = 65.548, p-value = 0.03902




Timing_split <- glm(formula=srcd_depvar ~ Modality + SES + Age + Count_NObj, data= Later_exposed, family=binomial)
summary(Timing_split)

Timing_split$AIC <- AIC(Timing_split)
stargazer(Timing_split, align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct"), covariate.labels=c("Language Modality (Spoken English)", "Socioeconomic Status (SES)", "Age (Years)", "Count List Knowledge", "Intercept"), keep.stat=c("n", "aic"), single.row=TRUE, out= "GNconserv_Later_Mod_SES_Age_Count_SRCD_190318.htm")

stargazer(Timing_split, align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct Odds Ratio"), covariate.labels=c("Language Modality (Spoken English)", "Socioeconomic Status (SES)", "Age (Years)", "Count List Knowledge", "Intercept"), apply.coef=exp, keep.stat=c("n", "aic"), single.row=TRUE, out= "GNconserv_Later_Mod_SES_Age_Count_SRCD_ODDSRATIO_190318.htm")


exp(coef(Timing_split)) #odds ratios for predictors

ci <- confint(Timing_split) #confidence intervals for coefficients
exp(coef(Timing_split)) ##ODDS RATIO for predictors
exp(cbind(OR = coef(Timing_split), CI = ci)) #Odds ratios for coefficients and confidence intervals of coefficients

                       OR       2.5 %     97.5 %
(Intercept)     0.8788679 0.009332284 76.7859537
ModalityEnglish 1.0316283 0.301502410  3.5783127
Age             0.2786398 0.097054149  0.6772869
SES             1.0232279 0.984270129  1.0704607
Count_WObj      1.3941549 1.150486616  1.7742783

##CHECKING MODEL FIT (for logistic regression):
# https://www.r-bloggers.com/evaluating-logistic-regression-models/

#Likelihood ratio test comparing preferred model to one with fewer predictors

#run a second model with one fewer predictors
Timing_split_nomod <- glm(formula=srcd_depvar ~  Age + SES + Count_NObj, data= Later_exposed, family=binomial)
summary(Timing_split_nomod) # not hugely better in terms of AIC, like keeping modality in (model above) just for partialing out independent contributions of different predictors
anova(Timing_split, Timing_split_nomod, test= "Chisq") #there is actually no difference btwn the model with more and fewer predictors (which is not what we love to see--but maybe not damning either)

Timing_split_Count_Age <- glm(formula=srcd_depvar ~  Age + Count_NObj, data= Later_exposed, family=binomial)
summary(Timing_split_Count_Age)
anova(Timing_split, Timing_split_nomod, test= "Chisq") #also no sig difference between this model and one with all predictors

Timing_split_Count_x_Age <- glm(formula=srcd_depvar ~  Age * Count_NObj, data= Later_exposed, family=binomial)
summary(Timing_split_Count_x_Age)


#Trying Pseudo R squared
install.packages("pscl")
library(pscl)
pR2(Timing_split) # McFadden is the one I want, values closer to zero indicating that the model has NO predictive power
##McFadden for this model (as of 8/16/18) was 0.1846993--not sure what the overall metric for this is
pR2(Timing_split_nomod) #same McFadden as Timing_split model
pR2(Timing_split_Count_Age) #worse (closer to zero) McFadden than Timing_split model

#Wald test for inidividual predictors - using Timing_split model

library(survey)
regTermTest(Timing_split, "Modality") # p= 0.96 (not sig contributor to model fit)
regTermTest(Timing_split, "Age") # p = 0.01
regTermTest(Timing_split, "SES") # p = 0.27
regTermTest(Timing_split, "Count_WObj") # p = 0.003

#Importance of specific variables in the model
library(caret) #doesn't install
varImp(Timing_split)


#generalhoslem package logitgof (STILL NOT WORKING)
GN_Later_noNA <- subset(Later_exposed$GiveN_ALL_ceiling_conservative, !is.na(Later_exposed$Language_Modality) & !is.na(Later_exposed$Highest_Count_wobj) & !is.na(Later_exposed$SES_range_8_to_66) & !is.na(Later_exposed$Age))
logitgof(GN_Later_noNA, fitted(Timing_split))

Later_exposed <- subset(Later_exposed, !is.na(Later_exposed$SES_range_8_to_66)) #tried a different way to make sure nrows equal (have one missing val in dataframe relative to model bs one participant missing SES score)

logitgof(Later_exposed$GiveN_ALL_ceiling_conservative, fitted.values(Timing_split)) # STILL NOT WORKING (Error: Observed or expected values < 0. Check that observed and fitted values entered correctly.) Haven't figured out how to sort this, but notes below:

##this is the code from the generalhoslem packagage logitgof test where my attempt to run logitgof is failing--not sure why
    if (any(observed[, 2:ncol(observed)] < 0)  || any(expected[, 2:ncol(expected)] < 0))

#trying to check if in fact I do have values <0 - this also returns an error bc there is only one column
    ifelse(any(Later_exposed$GiveN_ALL_ceiling_conservative[,2:ncol(Later_exposed$GiveN_ALL_ceiling_conservative)] <0), "Yep. There are negative values", "CALUMNY AND LIES!")

#Also need to look into what the || notation is (ORELSE, but want to undertand it more)



# NOT NECESSARY TO MODEL BUT GOOD CODE TO HAVE: separate into two dataframes (or lists) with those above the expected 50th quartile fitline and those below
above.reg.line <- GiveN_subset %>% 
    filter(GN_conserv_int > fitval50)

below.reg.line <- GiveN_subset %>%
    filter(GN_conserv_int < fitval50)



# NOTE: We know that "timing" variable (early vs late) is imprecise, and we suspect that there is a difference in timing btwn English Later & ASL Later that is not captured by current timing variable, but MAY be INCORRECTLY captured by Modality variable.

## Give-n SMALL ##
j <- polr(formula = GN_sm_fac ~ Age + Timing + Modality + SES, data = GiveN_subset, Hess = TRUE) #AIC: 312
j <- polr(formula = GN_sm_fac ~ Age + Timing + SES, data = GiveN_subset, Hess = TRUE) # AIC: 215 BUT THIS MODEL AND THE ONE ABOVE make the most sense from a theoretical perspective
j <- polr(formula = GN_sm_fac ~ Age + Timing + Modality, data = GiveN_subset, Hess = TRUE)
j <- polr(formula = GN_sm_fac ~ Age + Timing * Modality, data = GiveN_subset, Hess = TRUE)
j <- polr(formula = GN_sm_fac ~ Age + Timing * Modality + SES, data = GiveN_subset, Hess = TRUE) #AIC 210
j <- polr(formula = GN_sm_fac ~ Age * Modality + Timing + SES, data = GiveN_subset, Hess = TRUE) #AIC: 215
j <- polr(formula = GN_sm_fac ~ Age + Timing * SES, data = GiveN_subset, Hess = TRUE) #AIC: 216
j <- polr(formula = GN_sm_fac ~ Age + Timing, data = GiveN_subset, Hess = TRUE)
j <- polr(formula = GN_sm_fac ~ Age * Timing * Modality + SES, data = GiveN_subset, Hess = TRUE) # <-- best model AIC 209
j <- polr(formula = GN_sm_fac ~ Age + Timing * Modality * SES, data = GiveN_subset, Hess = TRUE) # not better than above model
j <- polr(formula = GN_sm_fac ~ Age * Timing * Modality * SES, data = GiveN_subset, Hess = TRUE) #won't run
summary(j)




##TESTING ASSUMPTIONS
## test assumptions of ordinal model (that slope is same for all y1-y2 outcome levels)
## from https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/

sf <- function(y) {
  c('Y>=0' = qlogis(mean(y >= 0)),
    'Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)),
    'Y>=5' = qlogis(mean(y >= 5)),
    'Y>=6' = qlogis(mean(y >= 6)))}

(s <- with(GiveN_subset, summary(as.numeric(GN_sm_fac) ~ Age + Timing + SES, fun=sf)))

# glm(I(as.numeric(GN_sm_fac) >= 1) ~ Timing, family="binomial", data = GiveN_subset) #can't figure this part out




##TESTING MODEL FIT

 # logitgof(GiveN_subset$Knower.level_GiveN_Small, fitted(j)) # this code doesn't work bc first thing includes all 101 values (incl NAs) and 2nd includes only 86 fitted values

#Generate a vector of observations that would have been included in the above models (that is, that aren't missing values in the predictor variables)
GN_sm_noNA <- subset(GiveN_subset$Knower.level_GiveN_Small, !is.na(GiveN_subset$Knower.level_GiveN_Small) & !is.na(GiveN_subset$SES) & !is.na(GiveN_subset$Age) & !is.na(GiveN_subset$Group_2cat))
logitgof(GN_sm_noNA, fitted(j))

pulkrob.chisq(j, c("Timing", "SES", "Age")) #this worked!


ggplot(GiveN_subset, aes(x=Timing, y=GN_sm_fac)) + geom_boxplot()

jtable <- coef(summary(j))
p <- pnorm(abs(jtable[, "t value"]), lower.tail = FALSE) * 2
jtable <- cbind(jtable, "p value" = p)
stargazer(jtable, out= "GN_sm_Timing_x_Modality_x_Age_SES.htm")




## Give-n LARGE ##
j <- polr(formula = GN_lg_fac ~ Age + Timing + Modality + SES, data = GiveN_subset, Hess = TRUE)
summary(j)

j <- polr(formula = GN_lg_fac ~ Age + Timing + SES, data = GiveN_subset, Hess = TRUE) # AIC: 163
summary(j)

j <- polr(formula = GN_lg_fac ~ Age + Timing + Modality, data = GiveN_subset, Hess = TRUE)
summary(j)

j <- polr(formula = GN_lg_fac ~ Age + Timing * Modality, data = GiveN_subset, Hess = TRUE)
summary(j)

j <- polr(formula = GN_lg_fac ~ Age + Timing * SES, data = GiveN_subset, Hess = TRUE) # <-- BEST MODEL AIC 162
summary(j)

j <- polr(formula = GN_lg_fac ~ Age * Timing + SES, data = GiveN_subset, Hess = TRUE) # AIC: 165
summary(j)

jtable <- coef(summary(j))
p <- pnorm(abs(jtable[, "t value"]), lower.tail = FALSE) * 2
jtable <- cbind(jtable, "p value" = p)
stargazer(jtable, out= "GN_lg_Age_Timing_x_SES.htm")

lipsitz.test(j)

GN_lg_noNA <- subset(GiveN_subset$GN_lg_fac, !is.na(GiveN_subset$GN_lg_fac) & !is.na(GiveN_subset$SES) & !is.na(GiveN_subset$Age) & !is.na(GiveN_subset$Group_2cat))
logitgof(GN_lg_noNA, fitted(j))




## GIVE-n ALL (conservative) ## (working with 130 total data points as of 10/2/18)
j <- polr(formula = GN_conserv_fac ~ Age + Timing + Modality + SES, data = GiveN_subset, Hess = TRUE)
summary(j) #AIC: 562, 12 obs deleted

j <- polr(formula = GN_conserv_fac ~ Age + Timing + Modality, data = GiveN_subset, Hess = TRUE)
summary(j) #AIC: 616, NO obs deleted

j <- polr(formula = GN_conserv_fac ~ Age + Timing * Modality, data = GiveN_subset, Hess = TRUE)
summary(j) #AIC: 617, NO obs deleted

j <- polr(formula = GN_conserv_fac ~ Age + Timing * Modality + SES, data = GiveN_subset, Hess = TRUE)
summary(j) #AIC: 563, 12 obs deleted

j <- polr(formula = GN_conserv_fac ~ Age + Timing * SES + Modality, data = GiveN_subset, Hess = TRUE)
summary(j) #AIC: 564, 12 obs deleted

j <- polr(formula = GN_conserv_fac ~ SES + Age + Timing + Modality + Age*Timing, data = GiveN_subset, Hess = TRUE) # <--  BEST MODEL according to AIC
    # CONFIRM WHTHER TRUE: BEST other measures of model fit (lipsitz and logitgof)
summary(j) #AIC: 554, 12 obs deleted

j <- polr(formula = GN_conserv_fac ~ Age + Timing + SES, data = GiveN_subset, Hess = TRUE) 
summary(j) #AIC: 561, 12 obs deleted

j <- polr(formula = GN_conserv_fac ~ Age + Timing * SES, data = GiveN_subset, Hess = TRUE)
summary(j) #AIC: 563, 12 obs deleted

j <- polr(formula = GN_conserv_fac ~ Age * Timing + SES, data = GiveN_subset, Hess = TRUE) 
summary(j) #AIC: 553, 12 obs deleted
    # TECHNICALLY best model according to AIC and other measures of model fit (as of 10/2/18) BUT this doesn't include modality, which we want)

#--> also the 2nd best model (last run 10/2/18) makes it look like the later exposed kids do better (for the Timing alone predictor), but I suspect that is because there are more younger kids in the early explosed groups (bc of hearing kids)

    #above suspicion confirmed with:
    ggplot(GiveN_subset, aes(x=Timing, y=Age)) + geom_violin()
    #and
    wilcox.test(Age~Timing, GiveN_subset)
    #more later exposed kids than early exposed kids




#Make nice table for best model
j$AIC <- AIC(j)
stargazer(j, title = "Question 2: Ordinal Logistic Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct"), covariate.labels=c("Socioeconomic Status (SES)", "Age (Years)", "Timing of Language Exposure (Later)", "Language Modality (Spoken English)", "Age x Timing Interaction"), keep.stat=c("n", "aic"), single.row=TRUE, out= "GN_all_SES_Mod_Timing_x_Age_int_181002.htm")





# Test model fit using generalholselm package:
    # https://cran.r-project.org/web/packages/generalhoslem/generalhoslem.pdf
    # https://github.com/matthewjay15/generalhoslem


lipsitz.test(j)

# for "BEST MODEL" and "2nd BEST MODEL" above I get the following error for lipsitz test: 
    #In lipsitz.test(j) :
        # g >= n/5c. Running this test when g >= n/5c is not recommended.

GN_all_noNA <- subset(GiveN_subset$GN_conserv_fac, !is.na(GiveN_subset$SES) & !is.na(GiveN_subset$Age))
lipsitz.test(j)
logitgof(GN_all_noNA, fitted(j), ord=TRUE)
    #ALSO ERROR: In logitgof(GN_all_noNA, fitted(j)) :
        #At least one cell in the expected frequencies table is < 1. Chi-square approximation may be incorrect.
pulkrob.chisq(j, c("Timing", "Modality"))
    # second arg should be ONLY categorical variables, but I keep getting: Error in FUN(left, right) : non-numeric argument to binary operator






### QUESTION 3: DOES NUMBER LIST KNOWLEDGE (elicited counting performance) MATTER? ###

##Make sure to RE-assign variables if running this after analyses for Q2 (see code @ top of this file)


#on give-n SMALL
j <- polr(formula = GN_sm_fac ~ Age + LangGrp + SES + MAX_Count_Seq, data = GiveN_subset, Hess = TRUE) 
summary(j) # AIC: 293.96


j <- polr(formula = GN_sm_fac ~ Age + Timing + Modality + SES + MAX_Count_Seq, data = GiveN_subset,  Hess = TRUE) 
summary(j) #AIC: 296.8553


j <- polr(formula = GN_sm_fac ~ Age + Timing + SES + MAX_Count_Seq, data = GiveN_subset, Hess = TRUE) 
summary(j) #AIC: 295.15

j <- polr(formula = GN_sm_fac ~ Age + Timing * SES + MAX_Count_Seq, data = GiveN_subset, Hess = TRUE)
summary(j) #AIC: 296.54

#~~#

j <- polr(formula = GN_sm_fac ~ Age + LangGrp + SES + Count_NObj, data = GiveN_subset, Hess = TRUE)
summary(j) # AIC: 301.16


j <- polr(formula = GN_sm_fac ~ Age + Timing + Modality + SES + Count_NObj, data = GiveN_subset, Hess = TRUE)
summary(j) #AIC: 304.1985


j <- polr(formula = GN_sm_fac ~ Age + Timing + SES + Count_NObj, data = GiveN_subset, Hess = TRUE)
summary(j)  #AIC: 302.50


#~~#



j <- polr(formula = GN_sm_fac ~ Age + LangGrp + SES + Count_WObj, data = GiveN_subset, Hess = TRUE)
summary(j) # AIC: 287.77 

j <- polr(formula = GN_sm_fac ~ Age + Timing + Modality + SES + Count_WObj, data = GiveN_subset, Hess = TRUE)
summary(j) #AIC: 291.8755


j <- polr(formula = GN_sm_fac ~ Age + Timing + SES + Count_WObj, data = GiveN_subset, Hess = TRUE) 
summary(j) #AIC: 289.9786

#~~#



## on give-N LARGE ##
#not doing this bc not everyone did Give-N Large


#on give-n ALL (conservative)

#I'd like check what the null deviance is (on a model with no predictors), but code below doens't do that
#j <- polr(y = GN_conserv_fac, data = GiveN_subset, Hess = TRUE)
#summary(j)

j <- polr(formula = GN_conserv_fac ~ Age * Timing + Modality + SES + Count_WObj, data = GiveN_subset, Hess = TRUE) 
summary(j) #AIC: 541 <--BEST MODEL OF THE FIRST THREE HERE (that is, without the three-way interaction)

#same as above, using "probit" instead of default method  
j <- polr(formula = GN_conserv_fac ~ Age * Timing + Modality + SES + Count_WObj, data = GiveN_subset, Hess = TRUE, method= "probit") 
summary(j) # AIC: 533

j$AIC <- AIC(j)

# MODEL OUTPUT WITH ODDS RATIOS - CAUTION IF YOU DO THIS (apply.coef) THE P-VALUES WILL BE WRONG
 stargazer(j, title = "Question 3: Ordinal Logistic Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct"), covariate.labels=c("Age (Years)", "Timing of Language Exposure (Later)", "Language Modality (Spoken English)", "Socioeconomic Status (SES)", "Elicited Counting Task (with objects): Highest Number Correct", "Age x Timing interaction"), apply.coef=exp, single.row=TRUE, keep.stat=c("n", "aic"), p=NULL, out= "GN_all_Age_x_Timing_Modality_SES_CountWObj_OR_181030.htm")

# Model output no OR
stargazer(j, title = "Question 3: Ordinal Logistic Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct (Odds Ratios)"), covariate.labels=c("Age (Years)", "Timing of Language Exposure (Later)", "Language Modality (Spoken English)", "Socioeconomic Status (SES)", "Elicited Counting Task (with objects): Highest Number Correct", "Age x Timing interaction"), single.row=TRUE, keep.stat=c("n", "aic"),  out= "GN_all_Age_x_Timing_Modality_SES_CountWObj_181031.htm")



stargazer(j, title = "Question 3: Ordinal Probit Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct"), covariate.labels=c("Age (Years)", "Timing of Language Exposure (Later)", "Language Modality (Spoken English)", "Socioeconomic Status (SES)", "Elicited Counting Task (with objects): Highest Number Correct", "Age x Timing interaction"), single.row=TRUE, keep.stat=c("n", "aic"), report=('vc*stp'), out= "GN_all_Age_x_Timing_Modality_SES_CountWObj_probit_180801.htm")

k <- polr(formula = GN_conserv_fac ~ Age * Timing + Modality + SES + Count_NObj, data = GiveN_subset, 
    Hess = TRUE) #AIC: 548
summary(k)

stargazer(k, title = "Question 3: Ordinal Logistic Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct"), covariate.labels=c("Age (Years)", "Timing of Language Exposure (Later)", "Language Modality (Spoken English)", "Socioeconomic Status (SES)", "Elicited Counting Task (NO objects): Highest Number Correct", "Age x Timing interaction"), single.row=TRUE, out= "GN_all_Age_x_Timing_Modality_SES_CountNObj.htm")

p <- polr(formula = GN_conserv_fac ~ Age * Timing + Modality + SES + MAX_Count_Seq, data = GiveN_subset, 
    Hess = TRUE) #AIC: 542
summary(p)

stargazer(p, title = "Question 3: Ordinal Logistic Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct"), covariate.labels=c("Age (Years)", "Timing of Language Exposure (Later)", "Language Modality (Spoken English)", "Socioeconomic Status (SES)", "Elicited Counting Task: Highest Number Correct (of both tasks)", "Age x Timing interaction"), single.row=TRUE, out= "GN_all_Age_x_Timing_Modality_SES_MAXCount.htm")


j <- polr(formula = GN_conserv_fac ~ Modality + SES + Count_WObj*Timing*Age, data = GiveN_subset, 
    Hess = TRUE) #AIC: 531 <--BEST MODEL OF all, but has 3-way interaction which can be tough to interpret
summary(j)


# POSSIBLY CHECK stepAIC() for comparing models stepwise and chosing one with best AIC (but also remember interpretability and theoretical justification for including specific predictors/interactions)


jtable <- coef(summary(j))
p <- pnorm(abs(jtable[, "t value"]), lower.tail = FALSE) * 2
jtable <- cbind(jtable, "p value" = p)


#output table with COEFFICIENTS
stargazer(j, title = "Ordinal Logistic Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct"), covariate.labels=c("Age (Years)", "Timing of Language Exposure (Later)", "Language Modality (Spoken English)", "Socioeconomic Status (SES)", "Elicited Counting Task: Highest Number Correct", "Age x Timing Interaction"), keep.stat=c("n", "aic", "res.dev"), omit.stat="ser", single.row=TRUE, out= "GN_all_SES_Mod_CountWObj_Timing_x_Age_int.htm")


## TABLE WITH ODDS RATIOS INSTEAD OF COEFFICIENTS
stargazer(j, title = "Ordinal Logistic Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct"), covariate.labels=c("Age (Years)", "Timing of Language Exposure (Later)", "Language Modality (Spoken English)", "Socioeconomic Status (SES)", "Elicited Counting Task: Highest Number Correct", "Age x Timing Interaction"), apply.coef=exp, keep.stat=c("n", "aic", "res.dev"), omit.stat="ser", single.row=TRUE, out= "GN_all_SES_Mod_CountWObj_Timing_x_Age_int_OR.htm")


# GET ODDS RATIOS and confidence intervals for coefficients

ci <- confint(j) #confidence intervals for coefficients
exp(coef(j)) ##ODDS RATIO for predictors
exp(cbind(OR = coef(j), ci)) #Odds ratio and confidence intervals for coefficients



#### BEST MODEL FOR GIVE-N ALL (conservative) w/LangGrp ###
#AGE, LngGrp, SES, and COUNT-NObj (counting WITHOUT objects)###
b <- polr(formula = GN_conserv_fac ~ Age + SES + LangGrp + Count_NObj, data = GiveN_subset, Hess = TRUE)
btable <- coef(summary(b))
p <- pnorm(abs(btable[, "t value"]), lower.tail = FALSE) * 2 #THINK THIS IS ONE-TAILED
btable <- cbind(btable, "p value" = p)
stargazer(btable, out= "KL_conserv_Age_SES_LngGrp_Count_NObj.htm")

#stargazer(htable, summary.stat = c("mean", "sd", "n"), out = "KL_conserv_Age_SES_LngGrp_MAX_count_seq_summary.htm")

ci <- confint(b) #confidence intervals for coefficients
exp(coef(b)) ##ODDS RATIO for predictors
exp(cbind(OR = coef(b), CI = ci)) #Odds ratios for coefficients and confidence intervals of coefficients

summary(b) 



## TESTING MODEL FIT
GN_all_noNA <- subset(GiveN_subset$GN_conserv_fac, !is.na(GiveN_subset$GN_conserv_fac) & !is.na(GiveN_subset$SES) & !is.na(GiveN_subset$Age) & !is.na(GiveN_subset$Group_2cat) & !is.na(GiveN_subset$Language_Modality))
lipsitz.test(j)
logitgof(GN_all_noNA, fitted(j))
Interaction
pulkrob.chisq(j, c("Timing", "Modality")) # 

(some )
setup1 <- data.frame(x1=rep(mean(mydata$x1),2),
x2=rep(mean(mydata$x2),2), x3=c(1,2))




##OTHER MODELS TRIED w/GIVE-N ALL (conservative)
#Age, LngGrp and SES as predictors
m<- polr(formula = GN_conserv_fac ~ Age + LangGrp + SES, data = GiveNEarly_exp, Hess = TRUE)
ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
stargazer(ctable, out= "KL_conserv_Age_SES_LngGrp.htm")
ci <- confint(m) #confidence intervals for coefficients
exp(coef(m)) ##ODDS RATIO for predictors
summary(m)

#Age, LngGrp and SES and Age x LngGrp interaction as predictors
n<- polr(formula = GN_conserv_fac ~ Age + SES + LangGrp + Age*LangGrp, data = GiveN_subset, Hess = TRUE)
ntable <- coef(summary(n))
p <- pnorm(abs(ntable[, "t value"]), lower.tail = FALSE) * 2
ntable <- cbind(ntable, "p value" = p)
stargazer(ntable, out= "KL_conserv_Age_SES_LngGrp_int.htm")
ci <- confint(n)
exp(coef(n))
summary(n)


#Age, LngGrp, SES, and Count w/objects as predictors
q <- polr(formula = KL_conserv_fac ~ Age + SES + LangGrp + Count_WObj, data = GiveN_subset, 
    Hess = TRUE)
qtable <- coef(summary(q))
p <- pnorm(abs(qtable[, "t value"]), lower.tail = FALSE) * 2
qtable <- cbind(qtable, "p value" = p)
stargazer(qtable, out= "KL_conserv_Age_SES_LngGrp_CountWObj.htm")
ci <- confint(q)
exp(coef(q))
summary(q)

#Age, LngGrp, SES, and MAX_count_seq as predictors
h <- polr(formula = KL_conserv_fac ~ Age + SES + LangGrp + MAX_count_seq, data = GiveN_subset, 
    Hess = TRUE)
htable <- coef(summary(h))
p <- pnorm(abs(htable[, "t value"]), lower.tail = FALSE) * 2
htable <- cbind(htable, "p value" = p)
stargazer(htable, out= "KL_conserv_Age_SES_LngGrp_MAX_count_seq.htm")

#stargazer(htable, summary.stat = c("mean", "sd", "n"), out = "KL_conserv_Age_SES_LngGrp_MAX_count_seq_summary.htm")

ci <- confint(h) #confidence intervals for coefficients
exp(coef(h)) ##ODDS RATIO for predictors
exp(cbind(OR = coef(h), ci)) #Odds ratio and confidence intervals for odds ratio

summary(h)








#Age, LngGrp (2-CATEGORY), SES, and MAX_count_seq as predictors
LangTiming <- GiveN_subset$Group_2cat
g <- polr(formula = KL_conserv_fac ~ Age + SES + LangTiming + MAX_count_seq, data = GiveN_subset, 
    Hess = TRUE)
gtable <- coef(summary(g))
p <- pnorm(abs(gtable[, "t value"]), lower.tail = FALSE) * 2
gtable <- cbind(gtable, "p value" = p)
stargazer(gtable, out= "KL_conserv_Age_SES_LngTiming_MAX_count_seq.htm")

#stargazer(htable, summary.stat = c("mean", "sd", "n"), out = "KL_conserv_Age_SES_LngGrp_MAX_count_seq_summary.htm")

ci <- confint(g) #confidence intervals for coefficients
exp(coef(g)) ##ODDS RATIO for predictors
exp(cbind(OR = coef(g), ci)) #Odds ratio and confidence intervals for odds ratio
summary(g)










#OLD/BROKEN CODE:

#Scatterplot faceted graphs don't work well with FACTORIZED vars, so MUST USE INTEGER VERSIONS OF KL
#BUT THE FACET/WRAP DOES SOMETHING STRANGE WITH LABELING/DATA POINTS - makes up values that don't exist

#GiveN small
p <- ggplot(GiveN_subset, aes(x=GiveN_subset$Age, y=GiveN_subset$Knower.level_GiveN_Small)) + geom_point() + geom_smooth(method=lm)
p + facet_wrap(~GiveN_subset$Group_4cat) #facet_wrap doesn't work with assigned variables 
#if I do "LangGrp <- GiveN_subset$Group_4cat" I can use LangGrp in a model, but not in facet_wrap


#GiveN_ALL_ceiling
p <- ggplot(GiveN_subset, aes(x=Age, y=KL_ALL_int)) + geom_point() + geom_smooth(method=lm) + labs(x="Age at Test", y="Knower Level (non-conservative)") + coord_cartesian(xlim = c(3,8), ylim=c(0,17))
p + facet_wrap(~GiveN_subset$Group_4cat) 


#GiveN_ALL_ceiling_conservative
p <- ggplot(GiveN_subset, aes(x=GiveN_subset$Age, y=GiveN_subset$GiveN_ALL_ceiling_conservative)) + geom_point() + geom_smooth(method=lm) + labs(x="Age at Test", y="Knower Level (conservative calculation)") + coord_cartesian(xlim = c(3,8), ylim=c(0,17))
p + facet_wrap(~GiveN_subset$Group_4cat)


#LINEAR MODELS (really not appropriate for our data)
glm(formula = give_N ~ GiveN_subset$Age + GiveN_subset$Group_2cat)
glm(formula = give_N ~ Age + TimeGrp + Sex)
glm(formula = give_N ~ Age + TimeGrp + SES)
glm(formula = give_N ~ Age + LangGrp + SES)
