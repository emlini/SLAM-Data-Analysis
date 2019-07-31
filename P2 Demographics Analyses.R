P2 Demographics Analyses

library(tidyverse) # includes ggplot2??
library(ggplot2) # for general plots
library(qwraps2) # for summary_table use
library(reshape2)

setwd("/Users/emilycarrigan/Dropbox/Data Analysis Work") ##this should be wherever your file is saved 

DemP2 <- read.csv("SLaMProject2BackgroundQuestionnaire_KW_190701.csv", na.strings = c("N/A", "#VALUE!", ""))
DemP2 <- subset(DemP2, DemP2$Including.in.Study=="Yes" & DemP2$Age_Rounded!="" & DemP2$DOB!="")
View(DemP2)

# Look at demographics by technology type (for Corina)
##CIcol <- dplyr::select(Dem, SUBJ.ID, Age_Rounded, Hearing_Tech_5cat, Age_of_Implantation_1st_CI_in_MONTHS)

##CIcol <- dplyr::filter(CIcol, Hearing_Tech_5cat =="CI")

##CIcol$Age_2cat <- ifelse(CIcol$Age_Rounded <5, "3 to 5", "5 and up")

##dplyr::count(CIcol, Age_2cat, sort = TRUE)

##ggplot(CIcol, aes(x=CIcol$Age_Rounded, y=CIcol$Age_of_Implantation_1st_CI_in_MONTHS)) + geom_point(aes(color = CIcol$Age_2cat))


DP2Short <- dplyr::select(DemP2, SUBJ.ID, Testing.Location, Program.Language.Use, Grade., P1.Tested., Including.in.Study, Tested, Group_4cat, Early_Later, Child_Language, Age_Rounded, Hearing_Tech_5cat, M.F, Child.Hearing.Status, Age.of.Exposure..mo., Age.of.Exposure.Comments, P1.P2_SES)


DP2Short$LanguageGroup <- as.factor(factor(as.character(DP2Short$Group_4cat), levels = c("English Early", "ASL Early", "English Later", "ASL Later"), exclude=NA))



DP2Short %>% group_by(LanguageGroup) %>% tally() # gives count by group!!!

aggregate(formula = DP2Short$Age_Rounded~DP2Short$LanguageGroup, FUN = mean) # gives means by group!!!

ggplot(DP2Short, aes(x=DP2Short$Age_Rounded, y=..count..)) + geom_density(aes(color = DP2Short$LanguageGroup))


aggregate(formula = DP2Short$Age_Rounded~DP2Short$LanguageGroup, FUN = median) # gives median by group!!!
aggregate(formula = DP2Short$Age_Rounded~DP2Short$LanguageGroup, FUN = min) # gives min by group!!!
aggregate(formula = DP2Short$Age_Rounded~DP2Short$LanguageGroup, FUN = max) # gives max by group!!!


ggplot(DP2Short, aes(x=DP2Short$P1.P2_SES, y=..count..)) + geom_density(aes(color = DP2Short$LanguageGroup))

shapiro.test(DP2Short$P1.P2_SES) #  NOT  NORMALLY DISTRIBUTED but expected


ggplot(DP2Short, aes(fill = DP2Short$M.F, x=DP2Short$LanguageGroup, y=..count..)) + geom_bar() 




# Answering question about age of implantation

# For all kids with CIs (English, ASL, Early, Later)
CIs <- dplyr::filter(Dem, Hearing_Tech_5cat == "CI")
View(CIs) #19 kids with CIs

HAs <- dplyr::filter(Dem, Hearing_Tech_5cat =="HA")
View(HAs) #55 with HAs


#SPLIT into separate df for each and 

Eng_Ear <- subset(Dem2, Dem2$Group_4cat=="English Early")
View(Eng_Ear)


ggplot(Eng_Ear, aes(x=Eng_Ear$Age_Rounded, y=..count..)) + geom_density()
shapiro.test(Eng_Ear$Age_Rounded) # SHOULD *NOT* BE NORMALLY DISTRIBUTED but IS




shapiro.test(Eng_Ear$SES) # SHOULD NOT BE NORMALLY DISTRIBUTED ? and is not
ggplot(Eng_Ear, aes(x=Eng_Ear$SES, y=..count..)) + geom_density()


summary <-
  list("Age" =
       list("min" = ~ min(.data$Age_Rounded),
            "max" = ~ max(.data$Age_Rounded),
            "median" = ~ median(.data$Age_Rounded),
            "mean (sd)" = ~ qwraps2::mean_sd(.data$Age_Rounded)),
       "SES" =
       list("min" = ~ min(.data$SES),
            "max" = ~ max(.data$SES),
            "median" = ~ median(.data$SES),
            "mean (sd)" = ~ qwraps2::mean_sd(.data$SES)),
       "Number (% of total)" =
       list(~ qwraps2::n_perc0(Dem2$LanguageGroup == "English Early"))
       )
whole <- summary_table(Eng_Ear, summary)
whole




Eng_Lat <- subset(Dem2, Dem2$Group_4cat=="English Later")
View(Eng_Lat)

shapiro.test(Eng_Lat$Age_Rounded) # SHOULD *NOT* BE NORMALLY DISTRIBUTED


ggplot(Eng_Lat, aes(x=Eng_Lat$Age_Rounded, y=..count..)) + geom_density()

shapiro.test(Eng_Lat$SES)
ggplot(Eng_Lat, aes(x=Eng_Lat$SES, y=..count..)) + geom_density()

summary <-
  list("Age" =
       list("min" = ~ min(.data$Age_Rounded),
            "max" = ~ max(.data$Age_Rounded),
            "median" = ~ median(.data$Age_Rounded),
            "mean (sd)" = ~ qwraps2::mean_sd(.data$Age_Rounded)),
       "SES" =
       list("min" = ~ min(.data$SES),
            "max" = ~ max(.data$SES),
            "median" = ~ median(.data$SES),
            "mean (sd)" = ~ qwraps2::mean_sd(.data$SES)),
       "Number (% of total)" =
       list("meow" = ~ qwraps2::n_perc0(Dem2$LanguageGroup == "English Later"))
       )
whole2 <- summary_table(Eng_Lat, summary)
whole2



ASL_Ear <- subset(Dem2, Dem2$Group_4cat=="ASL Early")
View(ASL_Ear)

shapiro.test(ASL_Ear$Age_Rounded) # SHOULD *NOT* BE NORMALLY DISTRIBUTED

ggplot(ASL_Ear, aes(x=ASL_Ear$Age_Rounded, y=..count..)) + geom_density()

shapiro.test(ASL_Ear$SES)
ggplot(ASL_Ear, aes(x=ASL_Ear$SES, y=..count..)) + geom_density()


summary <-
  list("Age" =
       list("min" = ~ min(.data$Age_Rounded),
            "max" = ~ max(.data$Age_Rounded),
            "median" = ~ median(.data$Age_Rounded),
            "mean (sd)" = ~ qwraps2::mean_sd(.data$Age_Rounded)),
       "SES" =
       list("min" = ~ min(.data$SES),
            "max" = ~ max(.data$SES),
            "median" = ~ median(.data$SES),
            "mean (sd)" = ~ qwraps2::mean_sd(.data$SES)),
       "Number (% of total)" =
       list("meow" = ~ qwraps2::n_perc0(Dem2$LanguageGroup == "ASL Early"))
       )
whole3 <- summary_table(ASL_Ear, summary)
whole3





ASL_Lat <- subset(Dem2, Dem2$Group_4cat=="ASL Later")
View(ASL_Lat)

shapiro.test(ASL_Lat$Age_Rounded) # SHOULD *NOT* BE NORMALLY DISTRIBUTED

ggplot(ASL_Lat, aes(x=ASL_Lat$Age_Rounded, y=..count..)) + geom_density() # MORE OLDER KIDS IN ASL Later group

shapiro.test(ASL_Lat$SES)
ggplot(ASL_Lat, aes(x=ASL_Lat$SES, y=..count..)) + geom_density()

summary <-
  list("Age" =
       list("min" = ~ min(.data$Age_Rounded),
            "max" = ~ max(.data$Age_Rounded),
            "median" = ~ median(.data$Age_Rounded),
            "mean (sd)" = ~ qwraps2::mean_sd(.data$Age_Rounded)),
       "SES" =
       list("min" = ~ min(.data$SES),
            "max" = ~ max(.data$SES),
            "median" = ~ median(.data$SES),
            "mean (sd)" = ~ qwraps2::mean_sd(.data$SES)),
       "Number (% of total)" =
       list("meow" = ~ qwraps2::n_perc0(Dem2$LanguageGroup == "ASL Later"))
       )
whole4 <- summary_table(ASL_Lat, summary)
whole4

# VIOLIN PLOTS ARE BEST PLOTS!

ggplot(Dem2, aes(x=Dem2$LanguageGroup, y=Dem2$Age_Rounded)) + geom_violin()

ggplot(Dem2, aes(x=Dem2$LanguageGroup, y=Dem2$SES)) + geom_violin()


## LOOKING AT DESCRIPTIVES BY LANGUAGE MODALITY AND TIMING

Language_Modality <- ifelse(Dem2$Group_4cat == "English Early" | GiveN_subset$Group_4cat == "English Later", "English", "ASL")
Language_Modality



Dem2 <- cbind(Dem2, Language_Modality)


kruskal.test(Age_Rounded ~ LanguageGroup, data = Dem2)
kruskal.test(SES ~ LanguageGroup, data = Dem2)



ggplot(Dem2, aes(x=Dem2$Language_Modality, y=Dem2$Age_Rounded)) + geom_violin()

wilcox.test(Dem2$Age_Rounded~Dem2$Language_Modality) # NOT DIFFERENT

ggplot(Dem2, aes(x=Dem2$Language_Modality, y=Dem2$SES)) + geom_violin()
wilcox.test(Dem2$SES~Dem2$Language_Modality) # ALSO NOT DIFFERENT

ggplot(Dem2, aes(x=Dem2$Early_Later, y=Dem2$Age_Rounded)) + geom_violin()

wilcox.test(Dem2$Age_Rounded~Dem2$Early_Later)

ggplot(Dem2, aes(x=Dem2$Early_Later, y=Dem2$SES)) + geom_violin()
wilcox.test(Dem2$SES~Dem2$Early_Later)

