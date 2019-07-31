##  PROJECT 2 DEMOGRAPHICS

library(tidyverse) # includes ggplot2??
library(ggplot2) # for general plots
library(qwraps2) # for summary_table use
library(reshape2)

setwd("/Users/emilycarrigan/Dropbox/Data Analysis Work") ##this should be wherever your file is saved 

P2Dem <- read.csv("00_SLaMProject2BackgroundQuestionnaire_CD_050219.csv", na.strings = c("N/A", "#VALUE!", ""))
P2Dem <- subset(P2Dem, P2Dem$Including.in.Study=="Yes" & P2Dem$Age_Rounded!="" & P2Dem$DOB!="")
View(P2Dem)

P2CIcol <- dplyr::select(P2Dem, SUBJ.ID, P1.Tested., Age_Rounded, Hearing_Tech_5cat, Age.of.Implantation..CI.kids.only.)

P2CIcol <- dplyr::filter(P2CIcol, Hearing_Tech_5cat =="CI")
P2CIcol <- dplyr::filter(P2CIcol, P1.Tested. =="No")

ggplot(P2CIcol, aes(x=P2CIcol$Age_Rounded, y=P2CIcol$Age.of.Implantation..CI.kids.only.)) + geom_point() + geom_text(aes(label= paste("(", P2CIcol$Age_Rounded, " Years Old, CI @ ", P2CIcol$Age.of.Implantation..CI.kids.only., " Months)", sep = ""), hjust="inward", vjust="inward"))


p + geom_text(aes(label = paste(wt, "^(", cyl, ")", sep = "")),
  parse = TRUE)