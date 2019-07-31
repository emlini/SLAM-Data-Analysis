
## ANALYSES FOR BUCLD 2018 (MODEL SWITH & WITHOUT COUNTING)


#LAST EDITED 11/2/18 (using data collected as recently as June 2018)

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


#DEMOGRAPHICS ONLY MODEL
j <- polr(formula = GN_conserv_fac ~ SES + Age, data = GiveN_subset, Hess = TRUE, method = "probit")
summary(j) #AIC as of 7/9/19: 695, Res. Dev: 665

j$AIC <- AIC(j)

stargazer(j, align=TRUE, dep.var.labels=c("Number Knowledge Task Performance"), covariate.labels=c("Socioeconomic Status (SES)", "Age (Years)"), single.row=TRUE, keep.stat=c("n", "aic"), out= "GN_all_conserv_Age_SES_190709.htm")




#Demographics + Modality
j <- polr(formula = GN_conserv_fac ~ SES + Age + Modality, data = GiveN_subset, Hess = TRUE, method = "probit")
summary(j) #AIC as of 7/9/19: 694, Res. Dev: 664

j$AIC <- AIC(j)

stargazer(j, align=TRUE, dep.var.labels=c("Number Knowledge Task Performance"), covariate.labels=c("Socioeconomic Status (SES)", "Age (Years)", "Language Modality (Spoken English)"), single.row=TRUE, keep.stat=c("n", "aic"), out= "GN_all_conserv_Age_SES_Modality_190709.htm")


#Demographics + Modality + Timing
j <- polr(formula = GN_conserv_fac ~ SES + Age + Modality + Timing, data = GiveN_subset, Hess = TRUE, method = "probit")
summary(j) #AIC as of 7/9/19: 681, Res. Dev: 649

j$AIC <- AIC(j)

stargazer(j, align=TRUE, dep.var.labels=c("Number Knowledge Task Performance"), covariate.labels=c("Socioeconomic Status (SES)", "Age (Years)", "Language Modality (Spoken English)", "Timing of Language Exposure (Later)"), single.row=TRUE, keep.stat=c("n", "aic"), out= "GN_all_conserv_SES_Age_Modality_Timing_190709.htm")



#Demographics + Modality + Timing + Number Language (Elicited Counting)

t <- polr(formula = GN_conserv_fac ~ SES + Age + Modality + Timing + Count_NObj, data = GiveN_subset, Hess = TRUE, method = "probit")
summary(t) #AIC as of 7/9/19: 671, Res. Dev: 637

t$AIC <- AIC(t)

stargazer(t, align=TRUE, dep.var.labels=c("Number Knowledge Task Performance"), covariate.labels=c("Socioeconomic Status (SES)", "Age (Years)", "Language Modality (Spoken English)", "Timing of Language Exposure (Later)", "Number Language (Count List)"), single.row=TRUE, keep.stat=c("n", "aic"), out= "GN_all_conserv_Age_SES_Modality_Timing_NumLang_190709.htm")


f <- polr(formula = GN_lg_fac ~ SES + Age + Modality + Timing + Count_NObj, data = GiveN_subset, Hess = TRUE, method = "probit")
summary(f) #AIC as of 7/9/19: 275 Res. dev: 255 (COUNT LIST DOES NOT SIGNIFICANTLY PREDICT GIVE-N LARGE alone; only age does) 

f$AIC <- AIC(f)


h <- polr(formula = GN_sm_fac ~ SES + Age + Modality + Timing + Count_NObj, data = GiveN_subset, Hess = TRUE, method = "probit")
summary(h) #COUNT LIST KNOWLEDGE DOES PREDICT GIVE-N SMALL (only), along with Age
  #AIC as of 7/9/19: 331, Res. Dev: 309

h$AIC <- AIC(h)


## Residuals plot ("Theoretical Quantile") for above model showed that model link method (LOGISTIC WITHOUT SPECIFYING) may not have been accurate, so trying other types

h <- polr(formula = GN_conserv_fac ~ SES + Age + Modality + Timing + Count_NObj, data = GiveN_subset, Hess = TRUE, method = "probit")
summary(h) # BEST MODEL LINK METHOD: model using probit method has better AIC even than model using logit method

h <- polr(formula = GN_conserv_fac ~ SES + Age + Modality + Timing + Count_NObj, data = GiveN_subset, Hess = TRUE, method = "loglog")
summary(h) # AWFUL RESIDUALS - NOT RIGHT FIT

h <- polr(formula = GN_conserv_fac ~ SES + Age + Modality + Timing + Count_NObj, data = GiveN_subset, Hess = TRUE, method = "cloglog")
summary(h) # AIC 635, Res Dev 601 (not that different from model with "probit" method).  Residuals don't look great with this link method but we DO have a higher proportion of data at higher values, which is what this link method is supposed to handle (but if residuals don't look better then this isn't a better model, right?

h <- polr(formula = GN_conserv_fac ~ SES + Age + Modality + Timing + Count_NObj, data = GiveN_subset, Hess = TRUE, method = "cauchit")
summary(h) # TRRble


##FINAL MODEL I GO WITH IS PROBIT METHOD - BELOW REFERS TO THAT

#quickly redoing the demographics only and middle model with probit link method - estimation not great for these two with probit but not worse than with any other link method
j <- polr(formula = GN_conserv_fac ~ SES + Age, data = GiveN_subset, Hess = TRUE, method = "probit")
summary(j)
j$AIC <- AIC(j)
stargazer(j, align=TRUE, dep.var.labels=c("Number Knowledge Task Performance"), covariate.labels=c("Socioeconomic Status (SES)", "Age (Years)"), single.row=TRUE, keep.stat=c("n", "aic"), out= "GN_all_conserv_Age_SES_190304.htm")



j <- polr(formula = GN_conserv_fac ~ SES + Age + Modality + Timing, data = GiveN_subset, Hess = TRUE, method = "probit")
summary(j)
j$AIC <- AIC(j)
stargazer(j, align=TRUE, dep.var.labels=c("Number Knowledge Task Performance"), covariate.labels=c("Socioeconomic Status (SES)", "Age (Years)", "Language Modality (Spoken English)", "Timing of Language Exposure (Later)"), single.row=TRUE, keep.stat=c("n", "aic"), out= "GN_all_conserv_SES_Age_Modality_Timing_190304.htm")



## TESTING MODEL FIT - ALL THREE WORKED!!!!!!!!

lipsitz.test(h)

## Lipsitz goodness of fit test for ordinal response models (for probit method)
	# data:  formula:  GN_conserv_fac ~ SES + Age + Modality + Timing + Count_NObj
	# LR statistic = 14.801, df = 9, p-value = 0.09655    ## as of 4/9/19

	# Warning message:
		# In lipsitz.test(h) :
  		# g >= n/5c. Running this test when g >= n/5c is not recommended.



logitgof(GiveN_subset$GN_conserv_fac, fitted(h))

# Hosmer and Lemeshow test (multinomial model) (for probit method)
	# data:  GiveN_subset$GN_conserv_fac, fitted(h)
	# X-squared = 112.34, df = 96, p-value = 0.1218    ## as of 4/9/19

	# Warning message:
		# In logitgof(GiveN_subset$GN_conserv_fac, fitted(h)) :
  		# At least one cell in the expected frequencies table is < 1. Chi-square approximation may be incorrect.



pulkrob.chisq(h, c("Timing", "Modality")) 

# Pulkstenis-Robinson chi-squared test (for probit model)
	# data:  formula:  GN_conserv_fac ~ SES + Age + Modality + Timing + Count_NObj
	# -squared = 96.008, df = 81, p-value = 0.122    ## as of 4/9/19

	# Warning message:
		# In pulkrob.chisq(h, c("Timing", "Modality")) :
  		# At least one cell in the expected frequencies table is < 1. Chi-square approximation may be incorrect.




## CALCULATING surrogate RESIDUALS (using sure package)

sres <- resids(h)

p1 <- autoplot(sres, what = "covariate", x = GiveN_subset$SES..8.66., xlab = "SES")
p2 <- autoplot(sres, what = "qq", distribution = qnorm)
grid.arrange(p1, p2, ncol = 2)  # code that produces Figure 2 in Greenwell et al ppr


p1 <- autoplot(sres, what = "covariate", x = GiveN_subset$Age, xlab = "Age")
p2 <- autoplot(sres, what = "qq", distribution = qnorm)
grid.arrange(p1, p2, ncol = 2)  # code that produces Figure 2 in Greenwell et al ppr


p1 <- autoplot(sres, what = "covariate", x = GiveN_subset$Language_Timing, xlab = "Timing")
p2 <- autoplot(sres, what = "qq", distribution = qnorm)
grid.arrange(p1, p2, ncol = 2)  # code that produces Figure 2 in Greenwell et al ppr


p1 <- autoplot(sres, what = "covariate", x = GiveN_subset$Language_Modality, xlab = "Modality")
p2 <- autoplot(sres, what = "qq", distribution = qnorm)
grid.arrange(p1, p2, ncol = 2)  # code that produces Figure 2 in Greenwell et al ppr


p1 <- autoplot(sres, what = "covariate", x = GiveN_subset$Highest_Count_noobj, xlab = "Number Knowledge")
p2 <- autoplot(sres, what = "qq", distribution = qnorm)
grid.arrange(p1, p2, ncol = 2)  # code that produces Figure 2 in Greenwell et al ppr


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


# checking for missing interaction terms 

# DOES EFFECT OF *HIGHEST COUNT* DIFFER DEPENDING ON TIMING OR MODALITY?
fit1 <- polr(GiveN_subset[GiveN_subset$Language_Timing == "Early", ]$GN_conserv_fac ~ GiveN_subset[GiveN_subset$Language_Timing == "Early", ]$Highest_Count_noobj, data = GiveN_subset[GiveN_subset$Language_Timing == "Early", ], Hess = TRUE, method = "probit")

fit2 <- polr(GiveN_subset[GiveN_subset$Language_Timing == "Later", ]$GN_conserv_fac ~ GiveN_subset[GiveN_subset$Language_Timing == "Later", ]$Highest_Count_noobj, data = GiveN_subset[GiveN_subset$Language_Timing == "Later", ], Hess = TRUE, method = "probit")

GiveN_subset$s <- c(surrogate(fit1), surrogate(fit2))

ggplot(GiveN_subset, aes(x = GiveN_subset$Highest_Count_noobj, y = s)) +
    geom_point(color = "#444444", shape = 19, size = 2, alpha = 0.5) +
    geom_smooth(se = FALSE, size = 1.2, color = "red2") +
    facet_wrap( ~ GiveN_subset$Language_Timing) +
    ylab("Surrogate response") +
    xlab(expression(x[1]))

fit1 <- polr(GiveN_subset[GiveN_subset$Language_Modality == "English", ]$GN_conserv_fac ~ GiveN_subset[GiveN_subset$Language_Modality == "English", ]$Highest_Count_noobj, data = GiveN_subset[GiveN_subset$Language_Modality == "English", ], Hess = TRUE, method = "probit")

fit2 <- polr(GiveN_subset[GiveN_subset$Language_Modality == "ASL", ]$GN_conserv_fac ~ GiveN_subset[GiveN_subset$Language_Modality == "ASL", ]$Highest_Count_noobj, data = GiveN_subset[GiveN_subset$Language_Modality == "ASL", ], Hess = TRUE, method = "probit")

GiveN_subset$s <- c(surrogate(fit1), surrogate(fit2))

ggplot(GiveN_subset, aes(x = GiveN_subset$Highest_Count_noobj, y = s)) +
    geom_point(color = "#444444", shape = 19, size = 2, alpha = 0.5) +
    geom_smooth(se = FALSE, size = 1.2, color = "red2") +
    facet_wrap( ~ GiveN_subset$Language_Modality) +
    ylab("Surrogate response") +
    xlab(expression(x[1]))


# DOES EFFECT OF *AGE* DIFFER DEP ON TIMING OR MODALITY?
fit1 <- polr(GiveN_subset[GiveN_subset$Language_Timing == "Early", ]$GN_conserv_fac ~ GiveN_subset[GiveN_subset$Language_Timing == "Early", ]$Age, data = GiveN_subset[GiveN_subset$Language_Timing == "Early", ], Hess = TRUE, method = "probit")

fit2 <- polr(GiveN_subset[GiveN_subset$Language_Timing == "Later", ]$GN_conserv_fac ~ GiveN_subset[GiveN_subset$Language_Timing == "Later", ]$Age, data = GiveN_subset[GiveN_subset$Language_Timing == "Later", ], Hess = TRUE, method = "probit")

GiveN_subset$s <- c(surrogate(fit1), surrogate(fit2))

ggplot(GiveN_subset, aes(x = GiveN_subset$Age, y = s)) +
    geom_point(color = "#444444", shape = 19, size = 2, alpha = 0.5) +
    geom_smooth(se = FALSE, size = 1.2, color = "red2") +
    facet_wrap( ~ GiveN_subset$Language_Timing) +
    ylab("Surrogate response") +
    xlab(expression(x[1]))

fit1 <- polr(GiveN_subset[GiveN_subset$Language_Modality == "English", ]$GN_conserv_fac ~ GiveN_subset[GiveN_subset$Language_Modality == "English", ]$Age, data = GiveN_subset[GiveN_subset$Language_Modality == "English", ], Hess = TRUE, method = "probit")

fit2 <- polr(GiveN_subset[GiveN_subset$Language_Modality == "ASL", ]$GN_conserv_fac ~ GiveN_subset[GiveN_subset$Language_Modality == "ASL", ]$Age, data = GiveN_subset[GiveN_subset$Language_Modality == "ASL", ], Hess = TRUE, method = "probit")

GiveN_subset$s <- c(surrogate(fit1), surrogate(fit2))

ggplot(GiveN_subset, aes(x = GiveN_subset$Age, y = s)) +
    geom_point(color = "#444444", shape = 19, size = 2, alpha = 0.5) +
    geom_smooth(se = FALSE, size = 1.2, color = "red2") +
    facet_wrap( ~ GiveN_subset$Language_Modality) +
    ylab("Surrogate response") +
    xlab(expression(x[1]))


# DOES EFFECT OF SES DIFFER DEP ON TIMING OR MODALITY?
fit1 <- polr(GiveN_subset[GiveN_subset$Language_Timing == "Early", ]$GN_conserv_fac ~ GiveN_subset[GiveN_subset$Language_Timing == "Early", ]$SES_range_8_to_66, data = GiveN_subset[GiveN_subset$Language_Timing == "Early", ], Hess = TRUE, method = "probit")

fit2 <- polr(GiveN_subset[GiveN_subset$Language_Timing == "Later", ]$GN_conserv_fac ~ GiveN_subset[GiveN_subset$Language_Timing == "Later", ]$SES_range_8_to_66, data = GiveN_subset[GiveN_subset$Language_Timing == "Later", ], Hess = TRUE, method = "probit")

GiveN_subset$s <- c(surrogate(fit1), surrogate(fit2))

ggplot(GiveN_subset, aes(x = GiveN_subset$SES_range_8_to_66, y = s)) +
    geom_point(color = "#444444", shape = 19, size = 2, alpha = 0.5) +
    geom_smooth(se = FALSE, size = 1.2, color = "red2") +
    facet_wrap( ~ GiveN_subset$Language_Timing) +
    ylab("Surrogate response") +
    xlab(expression(x[1]))

fit1 <- polr(GiveN_subset[GiveN_subset$Language_Modality == "English", ]$GN_conserv_fac ~ GiveN_subset[GiveN_subset$Language_Modality == "English", ]$SES_range_8_to_66, data = GiveN_subset[GiveN_subset$Language_Modality == "English", ], Hess = TRUE, method = "probit")

fit2 <- polr(GiveN_subset[GiveN_subset$Language_Modality == "ASL", ]$GN_conserv_fac ~ GiveN_subset[GiveN_subset$Language_Modality == "ASL", ]$SES_range_8_to_66, data = GiveN_subset[GiveN_subset$Language_Modality == "ASL", ], Hess = TRUE, method = "probit")

GiveN_subset$s <- c(surrogate(fit1), surrogate(fit2))

ggplot(GiveN_subset, aes(x = GiveN_subset$SES_range_8_to_66, y = s)) +
    geom_point(color = "#444444", shape = 19, size = 2, alpha = 0.5) +
    geom_smooth(se = FALSE, size = 1.2, color = "red2") +
    facet_wrap( ~ GiveN_subset$Language_Modality) +
    ylab("Surrogate response") +
    xlab(expression(x[1]))


## So effect of Age, SES, and Highest count seems to differ for timing groups--does this mean I create a model with all those variables interacting?
intmod <- polr(formula = GN_conserv_fac ~ Age * SES * Timing * Count_NObj  + Modality, data = GiveN_subset, Hess = TRUE, method = "probit") # THIS MODEL DOESN'T WORK
summary(intmod)

intmod <- polr(formula = GN_conserv_fac ~ SES + Modality + Age * Timing * Count_NObj, data = GiveN_subset, Hess = TRUE, method = "probit") # THIS MODEL WORKs and AIC is better than model below, but none of the individual terms are significant
summary(intmod)

intmod$AIC <- AIC(intmod)

stargazer(intmod, align=TRUE, dep.var.labels=c("Number Knowledge Task Performance"), covariate.labels=c("Socioeconomic Status (SES)", "Language Modality (Spoken English)", "Age (Years)", "Timing of Language Exposure (Later)", "Number Language (Count List)", "Age x Timing", "Age x Number Language", "Timing X Number Language", "Age x Timing x Number Language"), single.row=TRUE, keep.stat=c("n", "aic"), out= "GN_all_conserv_SES_Modality_Age_x_Timing_x_NumLang_190305.htm")



intmod <- polr(formula = GN_conserv_fac ~ SES * Timing * Count_NObj + Age + Modality, data = GiveN_subset, Hess = TRUE, method = "probit") # THIS MODEL WORKs but is not as good as the one above
summary(intmod)

#other model tried, but not better than above model according to AIC from run on 3/4/19
q <- polr(formula = GN_conserv_fac ~ Age+ SES + Modality + Timing * Count_NObj, data = GiveN_subset, Hess = TRUE, method = "probit")
summary(q)

q$AIC <- AIC(q)

stargazer(q, align=TRUE, dep.var.labels=c("Number Knowledge Task Performance"), covariate.labels=c("Socioeconomic Status (SES)", "Age (Years)", "Language Modality (Spoken English)", "Timing of Language Exposure (Later)", "Number Language (Count List)", "Timing X Number Language Interaction"), single.row=TRUE, keep.stat=c("n", "aic"), out= "GN_all_conserv_Age_SES_Modality_Timing_x_NumLang_190304.htm")



## SIMPLE MEDIATION TEST (but now thinking I'm going to have to go SEM route to accurately capture model)
## need package "mediation" for this, and VGAM bc of need for tobit model with MAX_count


med <- vglm(Count_NObj ~ Timing + SES + Age + Modality, family = "tobit", data = GiveN_subset)
# med <- glm(Count_NObj ~ Timing + SES + Age + Modality, data = GiveN_subset, )
out <- polr(formula = GN_conserv_fac ~ Count_NObj + Timing + SES + Age + Modality, data = GiveN_subset, Hess = TRUE, method = "probit")

medcomp <- mediate(med, out, treat = c("Timing"), mediator = c("Count_NObj"), covariates = c("SES", "Age", "Modality"), boot = TRUE, sims = 100, control.value = c("Early"), treat.value = c("Later"))
summary(medcomp)