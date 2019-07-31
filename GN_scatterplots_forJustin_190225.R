### SCATTERPLOTS FOR GIVE-N by AGE representing TIMING as different shapes/colors
## created: 02/25/19
## Last edited: 02/25/19

library(ggplot2) # for general plots
library(colorspace) # for fixing colors in beeswarm plots
library(quantreg)

**SET WORKING DIRECTORY AND IMPORT MASTER CODING SPREADSHEET**

#SETUP OF DATA FRAME (import, make necessary columns, add to df)
GiveN_subset <- subset(GiveN, GiveN$Including_in_study=='Yes' & GiveN$GiveN_Coded.=='Yes'& GiveN$EC_Coded.=='Yes' & GiveN$Age < 7 & GiveN$GiveN_ALL_ceiling_conservative!='')

LanguageGroup <- as.factor(factor(as.character(GiveN_subset$Group_4cat), levels = c("English Early", "ASL Early", "English Later", "ASL Later"), exclude=NA))

GN_sm_fac <- as.factor(GiveN_subset$GiveN_Small_Ceiling)
GN_ALL_fac <- as.factor(GiveN_subset$GiveN_ALL_ceiling)
GN_conserv_fac <- as.factor(GiveN_subset$GiveN_ALL_ceiling_conservative)

Language_Modality <- ifelse(GiveN_subset$Group_4cat == "English Early" | GiveN_subset$Group_4cat == "English Later", "English", "ASL")
Language_Modality

Language_Timing <- factor(as.character(GiveN_subset$Group_2cat), levels = c("Early", "Later"), exclude="")

GiveN_subset <- cbind(GiveN_subset, LanguageGroup, GN_sm_fac, GN_ALL_fac, GN_conserv_fac, Language_Modality, Language_Timing)
View(GiveN_subset)



# SET JITTER value for all graphs (so points don't go outside set axis values)
jitter <- position_jitter(width=0.2, height=0.01)


# SETUP FOR EARLY AND LATER EXPOSED (var names)

GN_sm_int <- GiveN_subset$GiveN_Small_Ceiling #KL drawing only from Give-N small data (which all children completed)
GN_ALL_int <- GiveN_subset$GiveN_ALL_ceiling
GN_conserv_int <- GiveN_subset$GiveN_ALL_ceiling_conservative
GN_sm_fac <- GiveN_subset$GN_sm_fac
GN_ALL_fac <- GiveN_subset$GN_ALL_fac
GN_conserv_fac <- GiveN_subset$GN_conserv_fac

#predictors
Age <- GiveN_subset$Age
SES <- GiveN_subset$SES_range_8_to_66
LangGrp <- GiveN_subset$LanguageGroup
Count_NObj <- GiveN_subset$Highest_Count_noobj
Count_WObj <- GiveN_subset$Highest_Count_wobj
MAX_count_seq <- GiveN_subset$MAX_COUNT_SEQ
Modality <- GiveN_subset$Language_Modality
Timing <- GiveN_subset$Language_Timing


**THIS IS THE CODE FOR THE PLOT I WANT YOU TO MAKE**

#SRCD PLOT GIVE-N BY TIMING w/quartile regression lines (for all data)

**CHANGE VARIABLE NAME FOR GIVE-N (y-axis) TO "Number Knowledge Max (Give-N)"**
**I WANT YOU TO ONLY INCLUDE THE 50th quartile line--see if you can figure out what to change here**

ggplot(GiveN_subset, aes(x=Age, y=GN_conserv_int)) + geom_point(aes(shape=Timing, color=Timing), position="jitter", size=2.5) + labs(x="Age (years)", y="Give-N highest quantity correct") + scale_shape_manual(name="Language\nTiming", labels=c("Early", "Later"), values=c(17, 15)) +scale_color_manual(name="Language\nTiming", labels=c("Early", "Later"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,16)) + geom_quantile(quantiles=c(.25, .5, .75))
    #geom_quantile requires package quantreg and calculates regression lines for the specified quantiles



## OTHER PLOTS OF THE GIVE-N DATA WITH DIFFERENT FEATURES


## SCATTERPLOT (NO LINE) OF GIVE-N WITH ALL KIDS SEPARATED BY MODALITY
ggplot(GiveN_subset, aes(x=Age, y=GN_conserv_int)) + geom_point(aes(shape=Modality, color=Modality), position=jitter, size=2.5) + geom_smooth(mapping=aes(group=Modality, color=Modality), method="loess", se=FALSE) + labs(x="Age at Test (years)", y="Number Knowledge Max (Give-N)") + scale_shape_manual(name="Language\nModality", labels=c("English", "ASL"), values=c(19, 17)) + scale_color_manual(name="Language\nModality", labels=c("English", "ASL"), values=c("grey67", "mediumvioletred")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,16)) + scale_y_continuous(breaks=c(0,16,4,8,12))


## GRAPH (w/line) GIVE-N by TIMING (EARLY vs. LATER EXPOSED) (no standard error shading)
ggplot(GiveN_subset, aes(x=Age, y=GN_conserv_int)) + geom_point(aes(shape=Timing, color=Timing), position=jitter, size=2.5) + geom_smooth(mapping=aes(group=Timing, color=Timing), method="loess", se=FALSE) + labs(x="Age at Test (years)", y="Number Knowledge Max (Give-N)") + scale_shape_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c(17, 15)) + scale_color_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,16)) + scale_y_continuous(breaks=c(0,16,4,8,12))


##GRAPH (w/line) for EARLY AND LATER EXPOSED WITH THE STANDARD ERROR shading
ggplot(GiveN_subset, aes(x=Age, y=GN_conserv_int)) + geom_point(aes(shape=Timing, color=Timing), position=jitter, size=2.5) + geom_smooth(mapping=aes(group=Timing, color=Timing), method="loess") + labs(x="Age at Test (years)", y="Give-N Highest Quantity Correct") + scale_shape_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c(17, 15)) + scale_color_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,16)) + scale_y_continuous(breaks=c(0,16,4,8,12))







##THESE ARE OTHER GRAPHS YOU CAN PLAY AROUND WITH - some variable names might need to be adjusted



# SETUP FOR EARLY EXPOSED only (new df and var names)
GiveNEarly_exp <- subset(GiveN_subset, GiveN_subset$Group_4cat=="English Early" | GiveN_subset$Group_4cat=="ASL Early")
View(GiveNEarly_exp)

GN_sm_int <- GiveNEarly_exp$Knower.level_GiveN_Small #KL drawing only from Give-N small data (which all children completed)
GN_lg_int <- GiveNEarly_exp$GiveN_Large_Ceiling
GN_ALL_int <- GiveNEarly_exp$GiveN_ALL_ceiling
GN_conserv_int <- GiveNEarly_exp$GiveN_ALL_ceiling_conservative
GN_sm_fac <- GiveNEarly_exp$GN_sm_fac
GN_ALL_fac <- GiveNEarly_exp$GN_ALL_fac
GN_conserv_fac <- GiveNEarly_exp$GN_conserv_fac


#predictors
Age <- GiveNEarly_exp$Age
SES <- GiveNEarly_exp$SES_range_8_to_66
Modality <- GiveNEarly_exp$Language_Modality # this only has the modality not the timing
LangGrp <- GiveNEarly_exp$LanguageGroup # this has both language modality and timing
Count_NObj <- GiveNEarly_exp$Highest_Count_noobj
Count_WObj <- GiveNEarly_exp$Highest_Count_wobj
MAX_count_seq <- GiveNEarly_exp$MAX_Count_Seq
Timing <- GiveNEarly_exp$Language_Timing



#VARIABLE SETUP FOR GIVE-N BY AGE ONLY ENGLISH EARLY GROUP 

GiveNEng_Ear <- subset(GiveN_subset, GiveN_subset$Group_4cat=="English Early")
View(GiveNEng_Ear)

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


##EARLY-EXPOSED GROUPS ONLY

#English Early only
ggplot(GiveNEng_Ear, aes(x=Age, y=GN_conserv_int)) + geom_point(aes(shape=Modality, color=Modality), position="jitter", size=2.5) + labs(x="Age at Test (years)", y="Number Knowledge Max (Give-N)") + scale_shape_manual(name="Language\nGroup", labels=c("English Early"), values=c(19)) + scale_color_manual(name="Language\nGroup", labels=c("English Early"), values=c("grey39")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,16))

#English Early & ASL Early
ggplot(GiveNEarly_exp, aes(x=Age, y=GN_conserv_int)) + geom_point(aes(shape=LangGrp, color=LangGrp), position="jitter", size=2.5) + labs(x="Age at Test (years)", y="Number Knowledge Max (Give-N)") + scale_shape_manual(name="Language\nGroup", labels=c("English Early", "ASL Early"), values=c(19, 17)) + scale_color_manual(name="Language\nGroup", labels=c("English Early", "ASL Early"), values=c("grey67", "mediumvioletred")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,16))



## GRAPH (w/line) for Early exposed only (no standard error shading)
ggplot(GiveNEarly_exp, aes(x=Age, y=GN_conserv_int)) + geom_point(aes(shape=Timing, color=Timing), position=jitter, size=2.5) + geom_smooth(mapping=aes(group=Timing, color=Timing), method="loess", se=FALSE) + labs(x="Age at Test (years)", y="Number Knowledge Max (Give-N)") + scale_shape_manual(name="Timing of\nLanguage\nExposure", labels=c("Early"), values=c(17)) + scale_color_manual(name="Timing of\nLanguage\nExposure", labels=c("Early"), values=c("forestgreen")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,16)) + scale_y_continuous(breaks=c(0,16,4,8,12))

##WITH THE STANDARD ERROR showing
ggplot(GiveNEarly_exp, aes(x=Age, y=GN_conserv_int)) + geom_point(aes(shape=Timing, color=Timing), position=jitter, size=2.5) + geom_smooth(mapping=aes(group=Timing, color=Timing), method="loess") + labs(x="Age at Test (years)", y="Number Knowledge Max (Give-N)")  + scale_shape_manual(name="Timing of\nLanguage\nExposure", labels=c("Early"), values=c(17)) + scale_color_manual(name="Timing of\nLanguage\nExposure", labels=c("Early"), values=c("forestgreen")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,16)) + scale_y_continuous(breaks=c(0,16,4,8,12))



## SCATTERPLOT FOR ELICITED COUNTING VS AGE

#Separated by Timing (difference)
ggplot(GiveN_subset, aes(x=Age, y=Count_NObj)) + geom_point(aes(shape=Timing, color=Timing), position=jitter, size=2.5) + geom_smooth(mapping=aes(group=Timing, color=Timing), method="loess", se=FALSE) + labs(x="Age at Test (years)", y="Number Language (highest number correct)") + scale_shape_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c(17, 15)) + scale_color_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,20)) + scale_y_continuous(breaks=c(0,20,4,8,12, 16))

#Separated by Modality (no difference)
ggplot(GiveN_subset, aes(x=Age, y=Count_WObj)) + geom_point(aes(shape=Modality, color=Modality), position=jitter, size=2.5) + geom_smooth(mapping=aes(group=Modality, color=Modality), method="loess", se=FALSE) + labs(x="Age (years)", y="Elicited Counting Highest Quantity Correct") + scale_shape_manual(name="Language\nModality", labels=c("English", "ASL"), values=c(19, 17)) + scale_color_manual(name="Language\nModality", labels=c("English", "ASL"), values=c("grey67", "mediumvioletred")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(3, 7), ylim= c(0,20)) + scale_y_continuous(breaks=c(0,20,4,8,12, 16))


## SCATTERPLOT FOR ELICITED COUNTING BY GIVE-N
ggplot(GiveN_subset, aes(x=Count_NObj, y=GN_conserv_int)) + geom_point(position="jitter", size=2.5, shape=17) + geom_smooth(method="loess", se=FALSE) + labs(x="Number Language Max (count list)", y="Number Knowledge Max (Give-N)") +  theme(text = element_text(size=16)) + coord_cartesian(xlim = c(0, 20), ylim= c(0,16)) + scale_x_continuous(breaks=c(0,20,4,8,12,16)) + scale_y_continuous(breaks=c(0,16,4,8,12))

# By language timing
ggplot(GiveN_subset, aes(x=Count_WObj, y=GN_conserv_int)) + geom_point(aes(shape=Timing, color=Timing), position=jitter, size=2.5) + geom_smooth(mapping=aes(group=Timing, color=Timing), method="loess", se=FALSE) + labs(x="Elicited Counting Highest Quantity Correct", y="Give-N Highest Quantity Correct") + scale_shape_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c(17, 15)) + scale_color_manual(name="Timing of\nLanguage\nExposure", labels=c("Early", "Later"), values=c("forestgreen","tan2")) + theme(text = element_text(size=16)) + coord_cartesian(xlim = c(0, 20), ylim= c(0,16)) + scale_x_continuous(breaks=c(0,20,4,8,12,16)) + scale_y_continuous(breaks=c(0,16,4,8,12))