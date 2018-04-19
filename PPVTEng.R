# Analysis of PPVT data from the SLAM number project
	# Analysis only includes a subset of groups tested, specifically a subset in two of the language groups under analysis ("English Early" and "English Later")

#	We'd like to know what predicts PPVT scores

#setwd("/Volumes/YARN STASH/Dropbox/SLAM - Number Project/SLAM Analysis/PPVT analyses")
library(ggplot2)
PPVTEng <- read.csv('PPVT_Data_180418_EC.csv')
View(PPVTEng)


#data cleaning & variable specification
PPVTEng_subset <- subset(PPVTEng, PPVTEng$IncludeLF_Eng_only=='x')
PPVTEng_subset <- PPVTEng_subset[,1:26] #remove extraneous columns
View(PPVTEng_subset)
PPVTEng_subset$Group <- factor(as.character(PPVTEng_subset$Group), levels = c("English Early", "ASL Early", "English Later", "ASL Later"))
Age <- PPVTEng_subset$Age
Raw <- PPVTEng_subset$Raw_Score
Std <- PPVTEng_subset$Standard_Score
HearingAge <- PPVTEng_subset$Hearing_Age
SES <- PPVTEng_subset$SES_Score
LangGrp <- PPVTEng_subset$Group

#Data Visualization
ggplot(PPVTEng_subset, aes(x=Age, y=Raw)) + geom_point() + geom_smooth(method='lm')
#Relationship btwn Age and Raw Scores
#looks like might have clustering of ages btw 4.5-6
cor.test(Age, Raw)

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
