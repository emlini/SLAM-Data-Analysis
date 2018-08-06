library(tidyverse) # includes ggplot2??
library(ggplot2) # for general plots
library(beeswarm) # for beeswarm plots
library(colorspace) # for fixing colors in beeswarm plots
library(stargazer) # for pretty regression output tables
library(MASS) # for polr package
library(generalhoslem) # for testing model fit (lipsitz test and two others)
library(qwraps2) # for summary_table use
library(quantreg) # testing quantile plots (geom_quantile) and quantile regressions

#Check and then set working dir
#getwd() #to see 
setwd("/Volumes/YARN STASH/Dropbox/Dropbox/SLAM - Number Project/SLAM Analysis") #filepath should be your own, wherever your file lives  #mine is double Dropbox bc STUPID

GiveN <- read.csv("MASTER_Coding_180802_AB.csv", na.strings = "N/A") #import the data file (which I already saved as a csv from excel file)
View(GiveN)

###DATA CLEANING/PREP/CHECK

## Get data ready for analysis (remove blank data points, ones that haven't yet been coded, kids above the age of 7, etc)

# GiveN_subset <- GiveN_subset[1:101,1:23] #if you need to remove blank rows/unecessary columns in the dataframe, subset by [row,column] ranges

GiveN_subset <- subset(GiveN, GiveN$GiveN_Coded.=='Yes'& GiveN$EC_Coded.=='Yes' & GiveN$Age < 7 & GiveN$GiveN_ALL_ceiling_conservative!='') # This can be amended as necessary for the specific analyses 
View(GiveN_subset)

str(GiveN_subset) # check the data
#Age and SES SHOULD import as 'num' type (bc decimal points)
# All GiveN values should import as integer type (but we will convert them to factor below)
	## If they import as something other than integer, that typically means there is text in one of the cells in that column OTHER than the "N/A" string


#reorder factor levels for Language Group so they show up in order I want them
LanguageGroup <- as.factor(factor(as.character(GiveN_subset$Group_4cat), levels = c("English Early", "ASL Early", "English Later", "ASL Later"), exclude=NA))

#ALSO NEED TO FACTORIZE ALL THE KNOWER LEVEL VARS (bc they are technically factors, for the purposes of the models we do)

#BELOW uses names in MASTER spreadsheet
GN_sm_fac <- as.factor(GiveN_subset$GiveN_Small_Ceiling)
GN_ALL_fac <- as.factor(GiveN_subset$GiveN_ALL_ceiling)
GN_conserv_fac <- as.factor(GiveN_subset$GiveN_ALL_ceiling_conservative)


#Create Modality and Timing variables and add to dataframe

Language_Modality <- ifelse(GiveN_subset$Group_4cat == "English Early" | GiveN_subset$Group_4cat == "English Later", c("English"), c("ASL"))
Language_Modality



Language_Timing <- as.factor(factor(as.character(GiveN_subset$Group_2cat), levels = c("Early", "Later"), exclude=""))
##ORDER MATTERS - I initially acidentally ordered things "Later", "Early", which actually reversed the automatically alphabetically ordered variables in the original Group2Cat (and labeled the opposite of what they actually were--didn't realize until I ran the models and was confused about why it seemed like Early exposed kids were doing worse--YIKES!)


#(Below uses names in Give-N only spreadsheet)
#GN_sm_fac <- as.factor(GiveN_subset$Knower.level_GiveN_Small)
#GN_ALL_fac <- as.factor(GiveN_subset$GiveN_ALL_ceiling)
#GN_conserv_fac <- as.factor(GiveN_subset$GiveN_ALL_ceiling_conservative)

#ADD factorized COLUMNS TO dataframe

GiveN_subset <- cbind(GiveN_subset, LanguageGroup, GN_sm_fac, GN_ALL_fac, GN_conserv_fac, Language_Modality, Language_Timing)
View(GiveN_subset)


#ASSIGN shorter VARIABLE names
GN_sm_int <- GiveN_subset$GiveN_Small_Ceiling #KL drawing only from Give-N small data (which all children completed)
GN_ALL_int <- GiveN_subset$GiveN_ALL_ceiling
GN_conserv_int <- GiveN_subset$GiveN_ALL_ceiling_conservative
GN_sm_fac <- GiveN_subset$GN_sm_fac
GN_ALL_fac <- GiveN_subset$GN_ALL_fac
GN_conserv_fac <- GiveN_subset$GN_conserv_fac

#predictors
Age <- GiveN_subset$Age
SES <- GiveN_subset$SES
LangGrp <- GiveN_subset$LanguageGroup
Count_NObj <- GiveN_subset$Highest_Count_noobj
Count_WObj <- GiveN_subset$Highest_Count_wobj
MAX_count_seq <- GiveN_subset$MAX_COUNT_SEQ
Modality <- GiveN_subset$Language_Modality
Timing <- GiveN_subset$Language_Timing


#CHECK NORMALITY of integer Give-N ceiling data, age, SES ETC

# This set of tests is for the data as a whole (not specific language groups)
shapiro.test(GN_ALL_int)
shapiro.test(GN_conserv_int)

#neither conservative nor non-conservative KL are normally distributed (when considering them integers)
#supports decision to use them as factors

shapiro.test(Age) #don't expect normal dist bc we want even numbers of all ages
shapiro.test(SES) # DO expect normal dist, need to check how skewed
shapiro.test(Count_NObj) # not sure what kind of dist to expect
shapiro.test(Count_WObj) # not sure what kind of dist to expect
#none of these variables are normally distributed - confirmed as of data 7/23/18 (STILL not normally distributed)



##VISUALIZE THE DATA (task performance--Knower level by age, across different language groups)

	##DON'T USE FACET_WRAP BC DATA ARE WEIRD

GiveNEng_Ear <- subset(GiveN_subset, GiveN_subset$Group_4cat=="English Early")
View(GiveNEng_Ear)

shapiro.test(GiveNEng_Ear$Age) # AGE IS normally distributed for this group (which is odd bc we want even numbers of all ages, right?)
ggplot(GiveNEng_Ear, aes(x=GiveNEng_Ear$Age, y=..count..)) + geom_density()

shapiro.test(GiveNEng_Ear$GiveN_ALL_ceiling_conservative)
ggplot(GiveNEng_Ear, aes(x=GiveNEng_Ear$GiveN_ALL_ceiling_conservative, y=..count..)) + geom_density() # BIMODAL distribution (at 6 and 16)

shapiro.test(GiveNEng_Ear$SES)
ggplot(GiveNEng_Ear, aes(x=GiveNEng_Ear$SES, y=..count..)) + geom_density() #definitely skewed toward higher end of value range

shapiro.test(GiveNEng_Ear$Highest_Count_noobj)
ggplot(GiveNEng_Ear, aes(x=GiveNEng_Ear$Highest_Count_noobj, y=..count..)) + geom_density() # skewed high

shapiro.test(GiveNEng_Ear$Highest_Count_wobj)
ggplot(GiveNEng_Ear, aes(x=GiveNEng_Ear$Highest_Count_wobj, y=..count..)) + geom_density() # skewed high

#Give N by Age for English Early group
ggplot(GiveNEng_Ear, aes(x=GiveNEng_Ear$Age, y=GiveNEng_Ear$GiveN_ALL_ceiling_conservative)) + geom_jitter()  + geom_smooth(method=lm) + coord_cartesian(xlim = c(3,7), ylim=c(0,17))

cor.test(x=GiveNEng_Ear$Age, y=GiveNEng_Ear$GiveN_ALL_ceiling_conservative, method=c("kendall"))


GiveNEng_Lat <- subset(GiveN_subset, GiveN_subset$Group_4cat=="English Later")
View(GiveNEng_Lat)

shapiro.test(GiveNEng_Lat$Age)
ggplot(GiveNEng_Lat, aes(x=GiveNEng_Lat$Age, y=..count..)) + geom_density()

shapiro.test(GiveNEng_Lat$SES)
ggplot(GiveNEng_Lat, aes(x=GiveNEng_Lat$SES, y=..count..)) + geom_density()

shapiro.test(GiveNEng_Lat$Highest_Count_noobj)
ggplot(GiveNEng_Lat, aes(x=GiveNEng_Lat$Highest_Count_noobj, y=..count..)) + geom_density()

shapiro.test(GiveNEng_Lat$Highest_Count_wobj)
ggplot(GiveNEng_Lat, aes(x=GiveNEng_Lat$Highest_Count_wobj, y=..count..)) + geom_density()

#Give N by Age for English LATER group
ggplot(GiveNEng_Lat, aes(x=GiveNEng_Lat$Age, y=GiveNEng_Lat$GiveN_ALL_ceiling_conservative)) + geom_jitter()  + geom_smooth(method=lm) + coord_cartesian(xlim = c(3,7), ylim=c(0,17))

#trying the plot with a logarithmic curve fit (bc relationship really looks curvilinear)
ggplot(GiveNEng_Lat, aes(x=GiveNEng_Lat$Age, y=GiveNEng_Lat$GiveN_ALL_ceiling_conservative)) + geom_point()  + geom_smooth(method=lm, formula = y~log(x)) + coord_cartesian(xlim = c(3,7), ylim=c(0,17))
#doesn't improve curve fit much 

cor.test(x=GiveNEng_Lat$Age, y=GiveNEng_Lat$GiveN_ALL_ceiling_conservative, method=c("kendall"))


GiveNASL_Ear <- subset(GiveN_subset, GiveN_subset$Group_4cat=="ASL Early")
View(GiveNASL_Ear)


shapiro.test(GiveNASL_Ear$Age)
ggplot(GiveNASL_Ear, aes(x=GiveNASL_Ear$Age, y=..count..)) + geom_density()

shapiro.test(GiveNASL_Ear$SES)
ggplot(GiveNASL_Ear, aes(x=GiveNASL_Ear$SES, y=..count..)) + geom_density()

shapiro.test(GiveNASL_Ear$Highest_Count_noobj)
ggplot(GiveNASL_Ear, aes(x=GiveNASL_Ear$Highest_Count_noobj, y=..count..)) + geom_density()

shapiro.test(GiveNASL_Ear$Highest_Count_wobj)
ggplot(GiveNASL_Ear, aes(x=GiveNASL_Ear$Highest_Count_wobj, y=..count..)) + geom_density()


ggplot(GiveNASL_Ear, aes(x=GiveNASL_Ear$Age, y=GiveNASL_Ear$GiveN_ALL_ceiling_conservative)) + geom_jitter()  + geom_smooth(method=lm) + coord_cartesian(xlim = c(3,7), ylim=c(0,17))

cor.test(x=GiveNASL_Ear$Age, y=GiveNASL_Ear$GiveN_ALL_ceiling_conservative, method=c("kendall"))


GiveNASL_Lat <- subset(GiveN_subset, GiveN_subset$Group_4cat=="ASL Later")
View(GiveNASL_Lat)


shapiro.test(GiveNASL_Lat$Age)
ggplot(GiveNASL_Lat, aes(x=GiveNASL_Lat$Age, y=..count..)) + geom_density() # MORE OLDER KIDS IN ASL Later group

shapiro.test(GiveNASL_Lat$SES)
ggplot(GiveNASL_Lat, aes(x=GiveNASL_Lat$SES, y=..count..)) + geom_density()

shapiro.test(GiveNASL_Lat$Highest_Count_noobj)
ggplot(GiveNASL_Lat, aes(x=GiveNASL_Lat$Highest_Count_noobj, y=..count..)) + geom_density()

shapiro.test(GiveNASL_Lat$Highest_Count_wobj)
ggplot(GiveNASL_Lat, aes(x=GiveNASL_Lat$Highest_Count_wobj, y=..count..)) + geom_density()


ggplot(GiveNASL_Lat, aes(x=GiveNASL_Lat$Age, y=GiveNASL_Lat$GiveN_ALL_ceiling_conservative)) + geom_jitter() + geom_smooth(method=lm) + coord_cartesian(xlim = c(3,7), ylim=c(0,17))

cor.test(GiveNASL_Lat$Age, GiveNASL_Lat$GiveN_ALL_ceiling_conservative, method=c("kendall"))



#To get colors to match up to levels of Give-N variable, create variable for color (makes easier to insert into beeswarm code)
#(from https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf)

GiveN_col <- factor(GiveN_subset$GN_conserv_fac, 
	levels=c(0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 12, 16), 
	labels=c("red1", "orange1", "yellow2", "green1", "blue1", "plum1", "purple1", "coral1", "hotpink1", "springgreen1", "steelblue1", "chocolate1"))



GiveN_sm_col <- factor(GiveN_subset$GN_sm_fac, 
	levels=c(0, 1, 2, 3, 4, 5, 6), 
	labels=c("red1", "orange1", "yellow2", "green1", "blue1", "plum1", "purple1"))

GiveN_sm_col_grey <- factor(GiveN_subset$GN_sm_fac, 
	levels=c(0, 1, 2, 3, 4, 5, 6), 
	labels=c("gray30", "gray69", "gray75", "gray43", "grey39", "grey23", "grey13"))

#allowing automatic x-axis labeling WITH FACTOR!!
#If try to coerce the x-axis here with axis() it messes things up


#BEESWARMS WITH GIVE-N ALL (conservative) then Give-N SMALL
beeswarm(Age~GN_conserv_fac,
	data= GiveN_subset,
	subset= GiveN_subset$Group_4cat=='English Early',
	pch=16,
	pwcol=as.character(GiveN_col),
	ylim=c(3,7),
	yaxp = c(3, 7, 4),
	xlab = "Give-N Ceiling",
	ylab= "Age (years)",
	main="Give-N Ceiling by Age: 'English Early' Group")


beeswarm(Age~GN_conserv_fac,
	data= GiveN_subset,
	subset= GiveN_subset$Group_4cat=='English Later',
	pch=16,
	pwcol=as.character(GiveN_col),
	ylim=c(3,7),
	yaxp = c(3, 7, 4), 
	xlab = "Give-N Ceiling", 
	ylab= "Age (years)", 
	main="Give-N Ceiling by Age: 'English Later' Group")


beeswarm(Age~GN_conserv_fac, 
	data= GiveN_subset, 
	subset= GiveN_subset$Group_4cat=='ASL Early', 
	method = "center", 
	pch=16, 
	pwcol=as.character(GiveN_col),
	ylim=c(3,7),  
	yaxp = c(3, 7, 4), 
	xlab = "Give-N Ceiling", 
	ylab= "Age (years)", 
	main="Give-N Ceiling by Age: 'ASL Early' Group")


beeswarm(Age~GN_conserv_fac, 
	data= GiveN_subset, 
	subset= GiveN_subset$Group_4cat=='ASL Later', 
	method = "center", 
	pch=16, 
	pwcol=as.character(GiveN_col),  
	ylim=c(3,7), 
	yaxp = c(3, 7, 4), 
	xlab = "Give-N Ceiling", 
	ylab= "Age (years)", 
	main="Give-N Ceiling by Age: 'ASL Later' Group")


#Give N Small

beeswarm(Age~GN_sm_fac,
	data= GiveN_subset,
	subset= GiveN_subset$Group_4cat=='English Early',
	pch=16,
	pwcol=as.character(GiveN_sm_col_grey),
	ylim=c(3,7),
	yaxp = c(3, 7, 4),
	xlab = "Give-N Small Knower Level",
	ylab= "Age (years)",
	main="Give-N Small Ceiling by Age: 'English Early' Group")

beeswarm(Age~GN_sm_fac, 
	data= GiveN_subset, 
	subset= GiveN_subset$Group_4cat=='ASL Early', 
	method = "center", 
	pch=16, 
	pwcol=as.character(GiveN_sm_col),
	ylim=c(3,7),  
	yaxp = c(3, 7, 5), 
	xlab = "Give-N Small Ceiling", 
	ylab= "Age (years)", 
	main="Give-N Small Ceiling by Age: 'ASL Early' Group")

beeswarm(Age~GN_sm_fac, 
	data= GiveN_subset, 
	subset= GiveN_subset$Group_4cat=='ASL Early', 
	method = "center", 
	pch=16, 
	col="darkorange",
	ylim=c(3,8),  
	yaxp = c(3, 8, 5), 
	add=TRUE)






##FOR ICSLA 2018:

beeswarm(Age~GN_conserv_fac,
	data= GiveN_subset,
	subset= GiveN_subset$Group_4cat=='English Early',
	pch=16,
	col="grey39",
	ylim=c(3,7),
	yaxp = c(3, 7, 4),
	xlab = "Give-N Ceiling",
	ylab= "Age (years)",
	main="Give-N Ceiling by Age: 'English Early' Group")

beeswarm(Age~GN_conserv_fac,
	data= GiveN_subset,
	subset= GiveN_subset$Group_4cat=='English Early',
	pch=16,
	col="grey39",
	ylim=c(3,7),
	yaxp = c(3, 7, 4),
	xlab = "Give-N Ceiling",
	ylab= "Age (years)",
	main="Give-N Ceiling by Age: 'Early Language' Groups")

beeswarm(Age~GN_conserv_fac, 
	data= GiveN_subset, 
	subset= GiveN_subset$Group_4cat=='ASL Early', 
	method = "center", 
	pch=16, 
	col="mediumvioletred",
	ylim=c(3,7),  
	yaxp = c(3, 7, 4),
	add=TRUE)


legend("topleft", pch=16, col=c("mediumvioletred", "grey39"),
	legend=c("ASL", "English"), 
	text.font = 2, 
	text.col=c("mediumvioletred", "grey39"))


#Give-N small ICSLA 2018
beeswarm(Age~GN_sm_fac,
	data= GiveN_subset,
	subset= GiveN_subset$Group_4cat=='English Early',
	pch=16,
	col="grey39",
	ylim=c(3,7),
	yaxp = c(3, 7, 4),
	xlab = "Give-N Ceiling",
	ylab= "Age (years)",
	main="Give-N Ceiling by Age: 'English Early' Group")

beeswarm(GN_sm_fac~Age,
	data= GiveN_subset,
	subset= GiveN_subset$Group_4cat=='English Early',
	pch=16,
	col="grey39",
	xlab = "Give-N Ceiling",
	ylab= "Age (years)",
	main="Give-N Ceiling by Age: 'English Early' Group")

beeswarm(Age~GN_sm_fac,
	data= GiveN_subset,
	subset= GiveN_subset$Group_4cat=='English Early',
	pch=16,
	col="grey39",
	ylim=c(3,7),
	yaxp = c(3, 7, 4),
	xlab = "Give-N Ceiling",
	ylab= "Age (years)",
	main="Give-N Ceiling by Age: 'Early Language' Groups")

beeswarm(Age~GN_sm_fac, 
	data= GiveN_subset, 
	subset= GiveN_subset$Group_4cat=='ASL Early', 
	method = "center", 
	pch=16, 
	col="mediumvioletred",
	ylim=c(3,7),  
	yaxp = c(3, 7, 4),
	add=TRUE)


legend("topleft", pch=16, col=c("mediumvioletred", "grey39"),
	legend=c("ASL", "English"), 
	text.font = 2, 
	text.col=c("mediumvioletred", "grey39"))



#BOXPLOT of Give-N by lang grp (4 category)
ggplot(data=subset(GiveN_subset, !is.na(LngGrp)), aes(x=LangGrp, y=KL_conserv_int)) + geom_boxplot()

data=subset(iris, !is.na(Sepal.Length)

# tried using drop_na() function, but had trouble passing correct argument referencing column names/position (both df$colname and df[col3] returned the wrong kind of object for the function (1st returned an integer vector, second returned a list)
#drop_na(GiveN_subset) %>% 

##GENERATING A SUMMARY DESCRIPTIVES TABLE
#https://cran.r-project.org/web/packages/qwraps2/vignettes/summary-statistics.html
#library(qwraps2)

#mean_sd(mtcars2$mpg, denote_sd = "paren") #mean and SD

#n_perc(mtcars2$cyl == 4) #N and percent of total cases 

GiveNEng_Ear <- subset(GiveN_subset, GiveN_subset$Group_4cat=="English Early")
View(GiveNEng_Ear)

GiveNEng_Lat <- subset(GiveN_subset, GiveN_subset$Group_4cat=="English Later")
View(GiveNEng_Lat)

GiveNASL_Ear <- subset(GiveN_subset, GiveN_subset$Group_4cat=="ASL Early")
View(GiveNASL_Ear)

GiveNASL_Lat <- subset(GiveN_subset, GiveN_subset$Group_4cat=="ASL Later")
View(GiveNASL_Lat)

# need ~s for use in summary_table (qwraps2)
	descriptives_list <-
	  list("English Early (Hearing)" =
	       list("N (% total)" = ~ n_perc(GiveN_subset$Group_4cat == "English Early"),
	            "Age" =  ~ mean_sd(GiveNEng_Ear$Age, na_rm = TRUE, denote_sd = "paren"),
	            "SES" =  ~ mean_sd(GiveNEng_Ear$SES, na_rm = TRUE, denote_sd = "paren")),
	       "ASL Early (Deaf/Hard of Hearing)" =
	       list("N (% total)" =  ~ n_perc(GiveN_subset$Group_4cat == "ASL Early"),
	            "Age" =  ~ mean_sd(GiveNASL_Ear$Age, na_rm = TRUE, denote_sd = "paren"),
	            "SES" =  ~ mean_sd(GiveNASL_Ear$SES, na_rm = TRUE, denote_sd = "paren")),
	       "English Later (Deaf/Hard of Hearing) " =
	       list("N (% total)" = ~ n_perc(GiveN_subset$Group_4cat == "English Later"),
	            "Age" = ~ mean_sd(GiveNEng_Lat$Age, na_rm = TRUE, denote_sd = "paren"),
	            "SES" = ~ mean_sd(GiveNEng_Lat$SES, na_rm = TRUE, denote_sd = "paren")),
	       "ASL Later (Deaf/Hard of Hearing)" =
	       list("N (% total)" = ~ n_perc(GiveN_subset$Group_4cat == "ASL Later"),
	            "Age" = ~ mean_sd(GiveNASL_Lat$Age, na_rm = TRUE, denote_sd = "paren"),
	            "SES" = ~ mean_sd(GiveNASL_Lat$SES, na_rm = TRUE, denote_sd = "paren"))
	       )

GN_descr <- summary_table(GiveN_subset, descriptives_list)

write.table(GN_descr, file = "GN_descriptives_180615.htm", quote=FALSE, col.names=TRUE)

## TO GET THIS INTO EASY EXPORTABLE FORMAT (html), need to import some more libraries ##
library(knitr)
library(pandoc)
library(kableExtra)
library(magick)

kable(descriptives_list, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

#can't use ~ stat for use in stargazer'
	descriptives_list <-
	  list("English Early (Hearing)" =
	       list("N (% total)" =  n_perc(GiveN_subset$Group_4cat == "English Early"),
	            "Age" =   mean_sd(GiveNEng_Ear$Age, na_rm = TRUE, denote_sd = "paren"),
	            "SES" =   mean_sd(GiveNEng_Ear$SES, na_rm = TRUE, denote_sd = "paren"),
	       		"Give-N" = mean_sd(GiveNEng_Ear$GiveN_ALL_ceiling_conservative, na_rm = TRUE, denote_sd = "paren"),
	       		"Count Objects" = mean_sd(GiveNEng_Ear$Highest_Count_wobj, na_rm = TRUE, denote_sd = "paren")),
	       "ASL Early (Deaf/Hard of Hearing)" =
	       list("N (% total)" =  n_perc(GiveN_subset$Group_4cat == "ASL Early"),
	            "Age" =   mean_sd(GiveNASL_Ear$Age, na_rm = TRUE, denote_sd = "paren"),
	            "SES" =   mean_sd(GiveNASL_Ear$SES, na_rm = TRUE, denote_sd = "paren"),
	            "Give-N" = mean_sd(GiveNASL_Ear$GiveN_ALL_ceiling_conservative, na_rm = TRUE, denote_sd = "paren"),
	       		"Count Objects" = mean_sd(GiveNASL_Ear$Highest_Count_wobj, na_rm = TRUE, denote_sd = "paren")),
	       "English Later (Deaf/Hard of Hearing) " =
	       list("N (% total)" =  n_perc(GiveN_subset$Group_4cat == "English Later"),
	            "Age" =  mean_sd(GiveNEng_Lat$Age, na_rm = TRUE, denote_sd = "paren"),
	            "SES" =  mean_sd(GiveNEng_Lat$SES, na_rm = TRUE, denote_sd = "paren"),
	            "Give-N" = mean_sd(GiveNEng_Lat$GiveN_ALL_ceiling_conservative, na_rm = TRUE, denote_sd = "paren"),
	       		"Count Objects" = mean_sd(GiveNEng_Lat$Highest_Count_wobj, na_rm = TRUE, denote_sd = "paren")),
	       "ASL Later (Deaf/Hard of Hearing)" =
	       list("N (% total)" =  n_perc(GiveN_subset$Group_4cat == "ASL Later"),
	            "Age" =  mean_sd(GiveNASL_Lat$Age, na_rm = TRUE, denote_sd = "paren"),
	            "SES" =  mean_sd(GiveNASL_Lat$SES, na_rm = TRUE, denote_sd = "paren"),
	            "Give-N" = mean_sd(GiveNASL_Lat$GiveN_ALL_ceiling_conservative, na_rm = TRUE, denote_sd = "paren"),
	       		"Count Objects" = mean_sd(GiveNASL_Lat$Highest_Count_wobj, na_rm = TRUE, denote_sd = "paren"))
	       )

do.call(rbind, lapply(seq_along(descriptives_list), function(i){
  data.frame(Group=i, descriptives_list[[i]])
}))

# NO GOOD:  stargazer(descriptives_list, type = "html", out = "BUCLD_Descriptives.htm")
