## TISLR submission analyses: looking at iconicity effects (signed vs. spoken) on age of acquisition of quantities 1-5

##LAST EDITED

## MAIN QUESTION:  Do signing kids (maybe early only groups, maybe all) acquire 1-5 earlier than hearing kids

    # use Nicoladis paper on iconicity hearing kids gesture (framing for Iconicity TISLR abstract) 

#NEED Specific variable looking at only 1-5
#two ways to do this:
# 1) create binary variable with 'if reached 5, "yes"/1 otherwise "no"/0'
	## This will lead to proportion of kids who have reached 5 at different ages on scatterplot If I use this
# 2) create variable that has only performance on 1-5 (can start with Give-N small and convert 6 to five)

# Copy GiveN_small column and convert 6 values to 5

GN_upto_5 <- GiveN_subset$GN_sm_fac
GiveN_subset <- cbind(GiveN_subset, GN_upto_5)
View(GiveN_subset)

## RECODE
install.packages("car")

#not sure which of the first two lines actually worked, but IT DID
GiveN_subset$GN_upto_5 <- recode(GN_upto_5, "6" = "5")
GN_upto_5 <-GiveN_subset$GN_upto_5
View(GiveN_subset)
GiveN_subset$GN_upto_5 <- factor(as.factor(GN_upto_5), levels) ## STILL AN INTEGER NOT GOOD
GN_upto_5 <-GiveN_subset$GN_upto_5
summary(GN_upto_5)

##VARIABLES FOR ALL PARTICIPANTS
Age <- GiveN_subset$Age
SES <- GiveN_subset$SES_range_8_to_66
LangGrp <- GiveN_subset$LanguageGroup
Count_NObj <- GiveN_subset$Highest_Count_noobj
Count_WObj <- GiveN_subset$Highest_Count_wobj
MAX_count_seq <- GiveN_subset$MAX_COUNT_SEQ
Modality <- GiveN_subset$Language_Modality
Timing <- GiveN_subset$Language_Timing
GN_upto_5 <-GiveN_subset$GN_upto_5

#VARIABLES FOR English Early
GN_conserv_int <- GiveNEng_Ear$GiveN_ALL_ceiling_conservative
GN_conserv_fac <- GiveNEng_Ear$GN_conserv_fac
Age <- GiveNEng_Ear$Age
SES <- GiveNEng_Ear$SES
Modality <- GiveNEng_Ear$Language_Modality
Count_NObj <- GiveNEng_Ear$Highest_Count_noobj
Count_WObj <- GiveNEng_Ear$Highest_Count_wobj
MAX_count_seq <- GiveNEng_Ear$MAX_Count_Seq



#VARIABLES FOR Engligh Later
GN_conserv_int <- GiveNEng_Lat$GiveN_ALL_ceiling_conservative
GN_conserv_fac <- GiveNEng_Lat$GN_conserv_fac
Age <- GiveNEng_Lat$Age
SES <- GiveNEng_Lat$SES
Modality <- GiveNEng_Lat$Language_Modality
Count_NObj <- GiveNEng_Lat$Highest_Count_noobj
Count_WObj <- GiveNEng_Lat$Highest_Count_wobj
MAX_count_seq <- GiveNEng_Lat$MAX_Count_Seq

#VARIABLES FOR ASL Early
GN_conserv_int <- GiveNASL_Ear$GiveN_ALL_ceiling_conservative
GN_conserv_fac <- GiveNASL_Ear$GN_conserv_fac
Age <- GiveNASL_Ear$Age
SES <- GiveNASL_Ear$SES
Modality <- GiveNASL_Ear$Language_Modality
Count_NObj <- GiveNASL_Ear$Highest_Count_noobj
Count_WObj <- GiveNASL_Ear$Highest_Count_wobj
MAX_count_seq <- GiveNASL_Ear$MAX_Count_Seq

#VARIABLES FOR ASL Later 
GN_conserv_int <- GiveNASL_Lat$GiveN_ALL_ceiling_conservative
GN_conserv_fac <- GiveNASL_Lat$GN_conserv_fac
Age <- GiveNASL_Lat$Age
SES <- GiveNASL_Lat$SES
Modality <- GiveNASL_Lat$Language_Modality
Count_NObj <- GiveNASL_Lat$Highest_Count_noobj
Count_WObj <- GiveNASL_Lat$Highest_Count_wobj
MAX_count_seq <- GiveNASL_Lat$MAX_Count_Seq



# scatterplot of give-N by age separated by modality (is slope steeper for ASL than for spoken English?)

# ALL TIMING GROUPS (early and later exposed)

ggplot(GiveN_subset, aes(x=Age, y=GN_upto_5)) + geom_point(aes(shape=Modality, color=Modality), position=jitter, size=2.5) + geom_smooth(mapping=aes(group=Modality, color=Modality), method="loess", se=FALSE) + labs(x="Age at Test (years)", y="Give-N Score (up to 5)") + scale_shape_manual(name="Language\nModality", labels=c("ASL", "English"), values=c(17, 15)) + scale_color_manual(name="Language\nModality", labels=c("ASL", "English"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3,7), ylim= c(0,6))


##WITH THE STANDARD ERROR showing
ggplot(GiveN_subset, aes(x=Age, y=GN_upto_5)) + geom_point(aes(shape=Modality, color=Modality), position=jitter, size=2.5) + geom_smooth(mapping=aes(group=Modality, color=Modality), method="loess") + labs(x="Age at Test (years)", y="Give-N Score (up to 5)") + scale_shape_manual(name="Language\nModality", labels=c("ASL", "English"), values=c(17, 15)) + scale_color_manual(name="Language\nModality", labels=c("ASL", "English"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3,7), ylim= c(0,6))


##EARLY-ONLY GROUPS
GiveNEarly_exp <- subset(GiveN_subset, GiveN_subset$Group_4cat=="English Early" | GiveN_subset$Group_4cat=="ASL Early")
View(GiveNEarly_exp)

Age <- GiveNEarly_exp$Age
SES <- GiveNEarly_exp$SES_range_8_to_66
LangGrp <- GiveNEarly_exp$LanguageGroup
Count_NObj <- GiveNEarly_exp$Highest_Count_noobj
Count_WObj <- GiveNEarly_exp$Highest_Count_wobj
MAX_count_seq <- GiveNEarly_exp$MAX_COUNT_SEQ
Modality <- GiveNEarly_exp$Language_Modality
Timing <- GiveNEarly_exp$Language_Timing
GN_upto_5 <-GiveNEarly_exp$GN_upto_5


ggplot(GiveNEarly_exp, aes(x=Age, y=GN_upto_5)) + geom_point(aes(shape=Modality, color=Modality), position=jitter, size=2.5) + geom_smooth(mapping=aes(group=Modality, color=Modality), method="loess", se=FALSE) + labs(x="Age at Test (years)", y="Give-N Score (up to 5)") + scale_shape_manual(name="Language\nModality", labels=c("ASL", "English"), values=c(17, 15)) + scale_color_manual(name="Language\nModality", labels=c("ASL", "English"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3,7), ylim= c(0,6))


##WITH THE STANDARD ERROR showing
ggplot(GiveNEarly_exp, aes(x=Age, y=GN_upto_5)) + geom_point(aes(shape=Modality, color=Modality), position=jitter, size=2.5) + geom_smooth(mapping=aes(group=Modality, color=Modality), method="loess") + labs(x="Age at Test (years)", y="Give-N Score (up to 5)") + scale_shape_manual(name="Language\nModality", labels=c("ASL", "English"), values=c(17, 15)) + scale_color_manual(name="Language\nModality", labels=c("ASL", "English"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3,7), ylim= c(0,6))


# At first thought Logistic regression for “achieve 5-knower status” looking for age and modality interaction (see if signing kids get there faster)
GN_reach5 <- recode(GN_upto_5, "5" = 1, "1"=0, "")


# Maybe just a regular regression with 1-5 as outcomes and see if devt faster for ASL kids



INCLUDING NUMBER LANGUAGE AS A PREDICTOR BC WE KNOW THAT IMPACTS Give-N (and maybe the kids who get more exposure to the number words in sign are better able to use it)
	Nicoladis et al. used school/country/educ. program as a very broad-brush proxy for how much number language experience kids gesture


## ALL GROUPS
j <- polr(formula = as.factor(GN_upto_5) ~ SES + Count_NObj+ Age * Modality, data = GiveN_subset, Hess = TRUE)
summary(j)
j$AIC <- AIC(j) #needed to get AIC in output table (240)
stargazer(j, title = "Do 'Iconic' Number Symbols Facilitate Number Knowledge? (All Groups)", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct (up to 5)"), covariate.labels=c("Socioeconomic Status (SES)", "Number Language (Count List)", "Age (Years)", "Language Modality (Spoken English)", "Age x Modality Interaction"), single.row=TRUE, keep.stat=c("n", "aic"), out= "GN_to5_SES_Count_Modality*Age_ALLGRPS_181123.htm")

lipsitz.test(j)

GN_upto5_noNA <- subset(GiveN_subset$GN_upto_5, !is.na(GiveN_subset$GN_upto_5) & !is.na(GiveN_subset$SES) & !is.na(GiveN_subset$Age) & !is.na(GiveN_subset$Language_Modality) & !is.na(GiveN_subset$Highest_Count_noobj))

logitgof(GN_upto5_noNA, fitted(j), ord = TRUE)
pulkrob.chisq(j, c("Modality"))


##ALL GROUPS + TIMING
k <- polr(formula = as.factor(GN_upto_5) ~ SES + Count_NObj + Timing + Age * Modality, data = GiveN_subset, Hess = TRUE)
summary(k)
k$AIC <- AIC(k) #needed to get AIC in output table
stargazer(k, title = "Do 'Iconic' Number Symbols Facilitate Number Knowledge? (All Groups)", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct (up to 5)"), covariate.labels=c("Socioeconomic Status (SES)", "Number Language (Count List)", "Age (Years)", "Timing of Language Exposure (Later)", "Language Modality (Spoken English)", "Age x Modality Interaction"), single.row=TRUE, keep.stat=c("n", "aic"), out= "GN_to5_SES_Count_Timing_Modality*Age_ALLGRPS_181128.htm")


lipsitz.test(k)
GN_upto5_noNA <- subset(GiveN_subset$GN_upto_5, !is.na(GiveN_subset$GN_upto_5) & !is.na(GiveN_subset$SES) & !is.na(GiveN_subset$Age) & !is.na(GiveN_subset$Language_Modality) & !is.na(GiveN_subset$Highest_Count_noobj) & !is.na(GiveN_subset$Language_Timing))

logitgof(GN_upto5_noNA, fitted(k), ord = TRUE)
pulkrob.chisq(k, catvars=c("Timing", "Modality"))

##FINDING: NO AGE x MODALITY INTERACTION (although age plays a role, not the case that signing kids faster than spoken language kids)



##EARLY-ONLY GROUPS
$$REMEMBER TO CHANGE VARIABLE REFERENTS$$


j <- polr(formula = GN_upto_5 ~ SES + Count_NObj + Age * Modality, data = GiveNEarly_exp, Hess = TRUE)
summary(j)
j$AIC <- AIC(j) #needed to get AIC in output table
stargazer(j, title = "Do 'Iconic' Number Symbols Facilitate Number Knowledge? (Early Only Groups)", align=TRUE, dep.var.labels=c("Give-N Highest Quantity Correct (up to 5)"), covariate.labels=c("Socioeconomic Status (SES)", "Number Language (Count List)", "Age (Years)", "Language Modality (Spoken English)", "Age x Modality Interaction"), single.row=TRUE, keep.stat=c("n", "aic"), out= "GN_to5_SES_Count_Modality*Age__EARLYONLY_181121.htm")


##INTERESTINGLY, WHEN YOU LOOK AT EARLY ONLY, AGE IS A SIGNIFICANT PREDICTOR
## BUT WHEN LOOKING AT ALL GROUPS, WHAT BECOMES MORE IMPORTANT IS HOW MANY NUMBER WORDS THEY KNOW (AND AGE IS NO LONGER SIGNIFICANT)



# WHAT ABOUT BEESWARM PLOTS??



GiveN_col <- factor(GiveN_subset$LanguageGroup, 
	levels=c("English Early", "ASL Early", "English Later", "ASL Later"), 
	labels=c("darkorchid4", "darkgreen", "firebrick4", "chocolate2"))




GiveN_sm_col <- factor(GiveN_subset$GN_sm_fac, 
	levels=c(0, 1, 2, 3, 4, 5, 6), 
	labels=c("red1", "orange1", "yellow2", "green1", "blue1", "plum1", "purple1"))

GiveN_sm_col_grey <- factor(GiveN_subset$GN_sm_fac, 
	levels=c(0, 1, 2, 3, 4, 5, 6), 
	labels=c("gray30", "gray69", "gray75", "gray43", "grey39", "grey23", "grey13"))

beeswarm(Age~GN_sm_fac,
	data= GiveN_subset,
	subset= GiveN_subset$Group_4cat=='English Early',
	pch=16,
	pwcol=as.character(GiveN_sm_col),
	ylim=c(3,7),
	yaxp = c(3, 7, 4),
	xlab = "Give-N Ceiling (to 5)",
	ylab= "Age (years)",
	main="Give-N Ceiling by Age: 'English Early' Group")

ggplot(GiveN_subset, aes(x=Age, y=GN_conserv_int)) + geom_point(position=jitter, size=2.5) + geom_smooth(method="loess", se=FALSE) + labs(x="Age at Test (years)", y="Number Knowledge Max (Give-N)")  + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,16)) + scale_y_continuous(breaks=c(0,16,4,8,12))


#