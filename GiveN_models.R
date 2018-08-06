#LAST EDITED 8/2/18 (using data collected as recently as June 2018) - only updated AICs for Give-N Conservative models

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
SES <- GiveN_subset$SES
LangGrp <- GiveN_subset$LanguageGroup
Count_NObj <- GiveN_subset$Highest_Count_noobj
Count_WObj <- GiveN_subset$Highest_Count_wobj
MAX_Count_Seq <- GiveN_subset$MAX_COUNT_SEQ
Modality <- GiveN_subset$Language_Modality
Timing <- GiveN_subset$Language_Timing

##MODELS

#ORDINAL REGRESSION MODELS
#Instructions for this from: https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/

### QUESTION 1: DOES LANGUAGE MODALITY MATTER ###

#for this we look only at the early-exposed groups

#SUBSET THE DATA for this specific analysis
GiveNEarly_exp <- subset(GiveN_subset, GiveN_subset$Group_4cat=="English Early" | GiveN_subset$Group_4cat=="ASL Early")
View(GiveNEarly_exp)

#THE CODE BELOW RECAPITULATED work I did in the setup, and is unecessary now that I've got the setup properly organized
#GiveNEarly_exp <- GiveNEarly_exp[,1:23]


#reorder factor levels for Language Group so they show up in order I want them
#LanguageGroup <- as.factor(factor(as.character(GiveNEarly_exp$Group_4cat), levels = c("English Early", "ASL Early"), labels = c("English", "ASL"), exclude=NA))

#create factorized versions of variables of interest and add them to df
#GN_sm_fac <- as.factor(GiveNEarly_exp$GiveN_Small_Ceiling)
#GN_ALL_fac <- as.factor(GiveNEarly_exp$GiveN_ALL_ceiling)
#GN_conserv_fac <- as.factor(GiveNEarly_exp$GiveN_ALL_ceiling_conservative)
#GN_lg_fac <- as.factor(GiveNEarly_exp$GiveN_Large_Ceiling)

#ADD factorized COLUMNS TO dataframe

#GiveNEarly_exp <- cbind(GiveNEarly_exp, LanguageGroup, GN_sm_fac, GN_lg_fac, GN_ALL_fac, GN_conserv_fac)
#View(GiveNEarly_exp)

#NEED TO RE-NAME the variables (have to redo bc working with different df now)
GN_sm_int <- GiveNEarly_exp$Knower.level_GiveN_Small #KL drawing only from Give-N small data (which all children completed)
GN_lg_int <- GiveNEarly_exp$GiveN_Large_Ceiling
GN_ALL_int <- GiveNEarly_exp$GiveN_ALL_ceiling
GN_conserv_int <- GiveNEarly_exp$GiveN_ALL_ceiling_conservative
GN_sm_fac <- GiveNEarly_exp$GN_sm_fac
GN_ALL_fac <- GiveNEarly_exp$GN_ALL_fac
GN_conserv_fac <- GiveNEarly_exp$GN_conserv_fac


#predictors
Age <- GiveNEarly_exp$Age
SES <- GiveNEarly_exp$SES
Modality <- GiveNEarly_exp$Language_Modality # this only has the modality not the timing
LangGrp <- GiveNEarly_exp$LanguageGroup # this has both language modality and timing
Count_NObj <- GiveNEarly_exp$Highest_Count_noobj
Count_WObj <- GiveNEarly_exp$Highest_Count_wobj
MAX_count_seq <- GiveNEarly_exp$MAX_Count_Seq
Timing <- GiveNEarly_exp$Language_Timing


# SCATTERPOT SHOWING TWO EARLY GROUPS on Give-N by Age (separated by different marker shapes)

GN_sm_int <- GiveNEng_Ear$Knower.level_GiveN_Small #KL drawing only from Give-N small data (which all children completed)
GN_lg_int <- GiveNEng_Ear$GiveN_Large_Ceiling
GN_ALL_int <- GiveNEng_Ear$GiveN_ALL_ceiling
GN_conserv_int <- GiveNEng_Ear$GiveN_ALL_ceiling_conservative
GN_sm_fac <- GiveNEng_Ear$GN_sm_fac
GN_ALL_fac <- GiveNEng_Ear$GN_ALL_fac
GN_conserv_fac <- GiveNEng_Ear$GN_conserv_fac


#predictors
Age <- GiveNEng_Ear$Age
SES <- GiveNEng_Ear$SES
Modality <- GiveNEng_Ear$Language_Modality
Count_NObj <- GiveNEng_Ear$Highest_Count_noobj
Count_WObj <- GiveNEng_Ear$Highest_Count_wobj
MAX_count_seq <- GiveNEng_Ear$MAX_Count_Seq

#PLOT WITH ONLY ENGLISH EARLY--uses data from Eng_Ear df
ggplot(GiveNEng_Ear, aes(x=Age, y=GN_conserv_int)) + geom_point(aes(shape=Modality, color=Modality), position="jitter", size=2.5) + labs(x="Age (years)", y="Give-N highest quantity correct") + scale_shape_manual(name="Language\nGroup", labels=c("English Early"), values=c(19)) + scale_color_manual(name="Language\nGroup", labels=c("English Early"), values=c("grey39")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,16))

#PLOT WITH BOTH early-exposed GROUPS--uses data from Early_exp df, so NEED TO MAKE SURE TO RE-NAME variables to reference correct df
ggplot(GiveNEarly_exp, aes(x=Age, y=GN_conserv_int)) + geom_point(aes(shape=LangGrp, color=LangGrp), position="jitter", size=2.5) + labs(x="Age (years)", y="Give-N highest quantity correct") + scale_shape_manual(name="Language\nGroup", labels=c("ASL Early", "English Early"), values=c(17, 19)) + scale_color_manual(name="Language\nGroup", labels=c("ASL Early", "English Early"), values=c("mediumvioletred","grey67")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,16))

# trying to make the plots interative using https://www.rdocumentation.org/packages/ggiraphExtra/versions/0.1.0
install.packages("devtools")
devtools::install_github("cardiomoon/ggiraphExtra")

#ggPoints(j,se=TRUE,interactive=TRUE)

#CANNOT use SES in models as of 5/9/18 bc have 5 SES missing from ASL Early group (which only has 13 participants right now)
	# Also I suspect that more of the missing SES are from lower SES (BUT NO WAY TO TEST THIS)

# BELOW trying to use VIM to plot missing data, but package not loading right now (on my machine)
#md.pattern(GiveNEarly_exp)
# miss_plot <- aggr(GiveNEarly_exp, col=c('navyblue', 'red'), numbers=TRUE, sortVars=TRUE, labels=names(LangGrp), ylab=c("Histogram of missing data", "Pattern"))
# graphical way of looking at missing data - confirms "md.pattern" output (using VIM package) 

#check whether missing SES differs by language group
SES_miss <- ifelse(is.na(GiveNEarly_exp$SES), c("1"), c("0"))
SES_miss
GiveNEarly_exp <- cbind(GiveNEarly_exp, SES_miss)
View(GiveNEarly_exp)
ggplot(GiveNEarly_exp, aes(x=LangGrp, y=SES_miss)) + geom_jitter() #missing SES not different across two Lang Grps


#MODEL for give-n SMALL
j <- polr(formula = GN_sm_fac ~ Age + LangGrp, data = GiveNEarly_exp, Hess = TRUE)
summary(j)
j <- polr(formula = GN_sm_fac ~ SES+ Age * LangGrp, data = GiveNEarly_exp, Hess = TRUE)
summary(j)


## MODEL FOR give-n LARGE (only) ##
j <- polr(formula = GN_lg_fac ~ Age + Modality, data = GiveNEarly_exp, Hess = TRUE)
summary(j)



## MODELS FOR give-N ALL (conservative) ##
j <- polr(formula = GN_conserv_fac ~ SES + Age + Modality, data = GiveNEarly_exp, Hess = TRUE)
j <- clm(formula = GN_conserv_fac ~ SES + Age + Modality, data = GiveNEarly_exp, na.action='na.exclude')
summary(j) #AIC 233 <--BEST MODEL




##Older, clunkier way of getting t and p-values that messed up stargazer output formatting (solved below with "report" argument in Startgazer package)
#jtable <- coef(summary(j))
#p <- pnorm(abs(jtable[, "t value"]), lower.tail = FALSE) * 2
#jtable <- cbind(jtable, "p value" = p)
#stargazer(jtable, out= "Q1_GN_all_cons_Modality_Age_SES.htm")

j <- polr(formula = GN_conserv_fac ~ Modality + SES*Age, data = GiveNEarly_exp, Hess = TRUE)
summary(j) #AIC: 235, not the better model (Modality*SES + Age had same AIC as this model)

j$AIC <- AIC(j) #needed to get AIC in output table
j$res.dev <- j$deviance #THIS DOESN'T WORK
stargazer(j, title = "Question 1: Ordinal Logistic Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct"), covariate.labels=c("Socioeconomic Status (SES)", "Age (Years)", "Language Modality (Spoken English)"), single.row=TRUE, keep.stat=c("n", "aic", "res.dev"), out= "GN_all_conserv_Modality_Age_SES_180726.htm")
##haven't yet figured out how to get res.dev and null.dev to show up in output


## OUTPUT TABLE THAT INCLUDES t and p VALUES FOR EACH COEFFICIENT - bit awkward in formatting execution but good to have
stargazer(j, title = "Question 1: Ordinal Logistic Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct"), covariate.labels=c("Socioeconomic Status (SES)", "Age (Years)", "Language Modality (Spoken English)"), single.row=TRUE, keep.stat=c("n", "aic"), report=('vc*stp'), out= "GN_all_conserv_Modality_Age_SES_t_p_180726.htm")


#TEST MODEL FIT USING generalhoslem package (three tests): https://cran.r-project.org/web/packages/generalhoslem/generalhoslem.pdf

##THIS WORKS (for similar model in Question 2):  GN_all_noNA <- subset(GiveN_subset$GN_conserv_fac, !is.na(GiveN_subset$SES) & !is.na(GiveN_subset$Age))

#THIS DOESN'T WORK for model in Q1:
GN_all_noNA <- subset(GiveNEarly_exp, !is.na(GiveNEarly_exp$SES) & !is.na(GiveNEarly_exp$Age))


lipsitz.test(j)
logitgof(GN_all_noNA$GN_conserv_fac, fitted(j), g=5, ord=TRUE)
    # Error in Ops.data.frame(observed[, 2:ncol(observed)], expected[, 2:ncol(expected)]) : 
    # ‘-’ only defined for equally-sized data frames
    #ALSO ERROR: In logitgof(GN_all_noNA, fitted(j)) :
        #At least one cell in the expected frequencies table is < 1. Chi-square approximation may be incorrect.

##TRYING TO RUN HOMER-LENSLOW TEST of model fit, but something about fitted() either returns too many values (so compared data frames are not equal) or returns NAs (which system can't handle)
##Tried using clm() from ordinal package, get error:
    predprob <- data.frame(SES = GiveNEarly_exp$SES, Age = GiveNEarly_exp$Age, Modality = GiveNEarly_exp$Language_Modality)
    fv <- predict(j, newdata = predprob, type = "prob")$fit
    logitgof(GiveNEarly_exp$GiveN_ALL_ceiling_conservative, fv, g = 5, ord = TRUE)
    #Error in quantile.default(yhat, probs = seq(0, 1, 1/g)) : missing values and NaN's not allowed if 'na.rm' is FALSE

## tried using hoslem.test() in ResourceSelection package
hoslem.test(GiveNEarly_exp$GiveN_ALL_ceiling_conservative, fitted(j))
#Error in quantile.default(yhat, probs = seq(0, 1, 1/g)) : missing values and NaN's not allowed if 'na.rm' is FALSE

pulkrob.chisq(j, c("Modality"))




### QUESTION 2: DOES LANGUAGE TIMING MATTER? ###

#Create Modality and Timing variables and add to dataframe
    ##THIS SHOULD HAVE BEEN DONE IN SETUP



## Early exposed only
ggplot(GiveNEarly_exp, aes(x=Age, y=GN_conserv_int)) + geom_point(aes(shape=Timing, color=Timing), position="jitter", size=2.5) + labs(x="Age (years)", y="Give-N highest quantity correct") + scale_shape_manual(name="Language\nTiming", labels=c("Early"), values=c(17)) + scale_color_manual(name="Language\nTiming", labels=c("Early"), values=c("forestgreen")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,16))

# SCATTERPLOT GIVE-N by AGE (TIMING)
ggplot(GiveN_subset, aes(x=Age, y=GN_conserv_int)) + geom_point(aes(shape=Timing, color=Timing), position="jitter", size=2.5) + labs(x="Age (years)", y="Give-N highest quantity correct") + scale_shape_manual(name="Language\nTiming", labels=c("Early", "Later"), values=c(17, 15)) +scale_color_manual(name="Language\nTiming", labels=c("Early", "Later"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,16)) + geom_quantile(quantiles=c(.25, .5, .75))
    #geom_quantile requires package quantreg and calculates regression lines for the specified quantiles

    ## WOULD LIKE TO be able to determine what proportion of each group (early vs later) are in which quantiles...see screenshots J sent (saved in Data analysis folder)
        # Like would be good to know proportion of later exposed kids vs early exposed kids who are above and below 50th percentile

# + geom_smooth(aes(group=Timing, color=Timing),method="loess")

# NOTE: We know that "timing" variable (early vs late) is imprecise, and we suspect that there is a difference in timing btwn English Later & ASL Later that is not captured by current timing variable, but MAY be INCORRECTLY captured by Modality variable.

## Give-n SMALL ##
j <- polr(formula = GN_sm_fac ~ Age + Timing + Modality + SES, data = GiveN_subset, Hess = TRUE) #AIC: 214.98
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




## GIVE-n ALL (conservative) ## (working with 129 total data points as of 6/14/18)
j <- polr(formula = GN_conserv_fac ~ Age + Timing + Modality + SES, data = GiveN_subset, Hess = TRUE)
summary(j) #AIC: 550, 12 obs deleted

j <- polr(formula = GN_conserv_fac ~ Age + Timing + Modality, data = GiveN_subset, Hess = TRUE)
summary(j) #AIC: 608, NO obs deleted

j <- polr(formula = GN_conserv_fac ~ Age + Timing * Modality, data = GiveN_subset, Hess = TRUE)
summary(j) #AIC: 609, NO obs deleted

j <- polr(formula = GN_conserv_fac ~ Age + Timing * Modality + SES, data = GiveN_subset, Hess = TRUE)
summary(j) #AIC: 551, 12 obs deleted

j <- polr(formula = GN_conserv_fac ~ SES + Age + Timing + Modality + Age*Timing, data = GiveN_subset, Hess = TRUE) # <-- 2nd BEST MODEL according to AIC, but better res.dev than model below with lowest AIC
    # BEST other measures of model fit (lipsitz and logitgof)
summary(j) #AIC: 545, 12 obs deleted

j <- polr(formula = GN_conserv_fac ~ Age + Timing + SES, data = GiveN_subset, Hess = TRUE) 
summary(j) #AIC: 549, 12 obs deleted

j <- polr(formula = GN_conserv_fac ~ Age + Timing * SES, data = GiveN_subset, Hess = TRUE)
summary(j) #AIC: 551, 12 obs deleted

j <- polr(formula = GN_conserv_fac ~ Age * Timing + SES, data = GiveN_subset, Hess = TRUE) 
summary(j) #AIC: 544, 12 obs deleted
    # TECHNICALLY best model BUT other measures of model fit not great (and this doesn't include modality, which we want)

#--> also the 2nd best model (run 7/26/18) makes it look like the later exposed kids do better (for the Timing alone predictor), but I suspect that is because there are more younger kids in the early explosed groups (bc of hearing kids)

    #above suspicion confirmed with:
    ggplot(GiveN_subset, aes(x=Timing, y=Age)) + geom_violin()
    #and
    wilcox.test(Age~Timing, GiveN_subset)
    #more later exposed kids than early exposed kids


j <- polr(formula = GN_conserv_fac ~ Age + Timing * SES + Modality, data = GiveN_subset, Hess = TRUE)
summary(j) #AIC: 552, 12 obs deleted

#Make nice table for best model
j$AIC <- AIC(j)
stargazer(j, title = "Question 2: Ordinal Logistic Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct"), covariate.labels=c("Socioeconomic Status (SES)", "Age (Years)", "Timing of Language Exposure (Later)", "Language Modality (Spoken English)", "Age x Timing Interaction"), keep.stat=c("n", "aic"), single.row=TRUE, out= "GN_all_SES_Mod_Timing_x_Age_int_180726.htm")





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

j <- polr(formula = GN_conserv_fac ~ Age * Timing + Modality + SES + Count_WObj, data = GiveN_subset, Hess = TRUE, method= "probit") 
summary(j) #AIC: 526.2156 <--BEST MODEL OF THE FIRST THREE HERE (that is, without the three-way interaction)

#same as above, using "probit" instead of default method  
j <- polr(formula = GN_conserv_fac ~ Age * Timing + Modality + SES + Count_WObj, data = GiveN_subset, Hess = TRUE, method= "probit") 
summary(j)

j$AIC <- AIC(j)
stargazer(j, title = "Question 3: Ordinal Logistic Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct"), covariate.labels=c("Age (Years)", "Timing of Language Exposure (Later)", "Language Modality (Spoken English)", "Socioeconomic Status (SES)", "Elicited Counting Task (with objects): Highest Number Correct", "Age x Timing interaction"), single.row=TRUE, keep.stat=c("n", "aic"), report=('vc*stp'), out= "GN_all_Age_x_Timing_Modality_SES_CountWObj_180801.htm")

stargazer(j, title = "Question 3: Ordinal Probit Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct"), covariate.labels=c("Age (Years)", "Timing of Language Exposure (Later)", "Language Modality (Spoken English)", "Socioeconomic Status (SES)", "Elicited Counting Task (with objects): Highest Number Correct", "Age x Timing interaction"), single.row=TRUE, keep.stat=c("n", "aic"), report=('vc*stp'), out= "GN_all_Age_x_Timing_Modality_SES_CountWObj_probit_180801.htm")

k <- polr(formula = GN_conserv_fac ~ Age * Timing + Modality + SES + Count_NObj, data = GiveN_subset, 
    Hess = TRUE) #AIC: 538.45
summary(k)

stargazer(k, title = "Question 3: Ordinal Logistic Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct"), covariate.labels=c("Age (Years)", "Timing of Language Exposure (Later)", "Language Modality (Spoken English)", "Socioeconomic Status (SES)", "Elicited Counting Task (NO objects): Highest Number Correct", "Age x Timing interaction"), single.row=TRUE, out= "GN_all_Age_x_Timing_Modality_SES_CountNObj.htm")

p <- polr(formula = GN_conserv_fac ~ Age * Timing + Modality + SES + MAX_Count_Seq, data = GiveN_subset, 
    Hess = TRUE) #AIC: 531.4756
summary(p)

stargazer(p, title = "Question 3: Ordinal Logistic Regression Results", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct"), covariate.labels=c("Age (Years)", "Timing of Language Exposure (Later)", "Language Modality (Spoken English)", "Socioeconomic Status (SES)", "Elicited Counting Task: Highest Number Correct (of both tasks)", "Age x Timing interaction"), single.row=TRUE, out= "GN_all_Age_x_Timing_Modality_SES_MAXCount.htm")


j <- polr(formula = GN_conserv_fac ~ Modality + SES + Count_WObj*Timing*Age, data = GiveN_subset, 
    Hess = TRUE) #AIC: 520.4946 <--BEST MODEL OF all, but has 3-way interaction which can be tough to interpret
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
p <- pnorm(abs(btable[, "t value"]), lower.tail = FALSE) * 2
btable <- cbind(btable, "p value" = p)
stargazer(btable, out= "KL_conserv_Age_SES_LngGrp_Count_NObj.htm")

#stargazer(htable, summary.stat = c("mean", "sd", "n"), out = "KL_conserv_Age_SES_LngGrp_MAX_count_seq_summary.htm")

ci <- confint(b) #confidence intervals for coefficients
exp(coef(b)) ##ODDS RATIO for predictors
exp(cbind(OR = coef(b), ci)) #Odds ratio and confidence intervals for coefficients

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





#BEESWARM PLOTS!!!  Spectacular data visualization for GiveN data by Age

#KL is an integer.  To use it in beeswarm plot (and have all possible values show up on all graphs--even values for which a particular group has no data points at that value--I have to turn it into a factor)

KL_factor <- as.factor(KL_sm)

#add newly created, factorized KL variable to the dataset
GiveN_subset <- cbind(GiveN_subset, KL_factor)
View(GiveN_subset)

#re-assign variables now that I've changed the dataframe
Age <- GiveN_subset$Age
KL_factor <- GiveN_subset$KL_factor


#To get colors to match up to levels of Give-N variable, create variable for color (makes easier to insert into beeswarm code)
#(from https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf)

GiveN_col <- factor(GiveNEng_Ear$KL_conserv_fac, 
	levels=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
	labels=c("red1", "orange1", "yellow2", "green1", "blue1", "plum1", "purple1", "coral1", "hotpink1", "wheat1", "springgreen1", "steelblue1", "chocolate1"))


#allowing automatic x-axis labeling WITH FACTOR!!
#If try to coerce the x-axis here with axis() it messes things up

beeswarm(GiveNEng_Ear$Age~GiveNEng_Ear$KL_conserv_int,
	data= GiveNEng_Ear,
	pch=16,
	pwcol=as.character(GiveN_col),
	ylim=c(3,8),
	yaxp = c(3, 8, 5),
	xlab = "Give-N ALL (conservative) Knower Level",
	ylab= "Age (years)",
	main="Knower Level by Age: 'English Early' Group")

# something off about above data - x-axis not lining up with data points






#BEESWARMS WITH GIVEN_small
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
