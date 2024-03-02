library(MASS); library(car); library(olsrr)

setwd("C:/Users/jorda/OneDrive/Documents/R-Studio/STA6443")  # need to change this path

?UScrime  # data in packages "MASS"
str(UScrime)
dev.new(width = 1000, height = 1000, unit = "px")
pairs(UScrime, pch = 19) # 16 by 16 scatter plot matrix

pairs(UScrime[,c(1:9,16)], pch = 19) # output in lecture note

######################################################################
# simulation study
#####################################################################
drinking=read.csv("drinking.csv", header=TRUE)

# simulation data generation - predictors with perfect or high correlation
set.seed(5678)
alcohol2= 3* drinking$alcohol   # alcohol & alcohol2 : perfect linear relationship
alcohol3= 3*drinking$alcohol+rnorm(15,0,0.5) # alcohol & alcohol3 : almost linear but not perfectly linear

# new dataframe with simulated predictors - alcohol2 & alcohol3
drinking.new=data.frame(cbind(drinking, alcohol2, alcohol3))

# check correlation through pairwise scatter plot and correation
pairs(drinking.new[ ,c(2:5)])

cor(drinking.new$alcohol, drinking.new$alcohol2)  
cor(drinking.new$alcohol, drinking.new$alcohol3)

plot(drinking.new$alcohol, drinking.new$alcohol2)
plot(drinking.new$alcohol, drinking.new$alcohol3)

# run linear regressions with multicollinearity issue
# perfect correlation 
lm.multicol1 = lm(cirrhosis~alcohol+alcohol2, data=drinking.new)

# high correlation but not perfect
lm.multicol2 = lm(cirrhosis~alcohol+alcohol3, data=drinking.new)

summary(lm.multicol1)
summary(lm.multicol2)

# run simple linear regression
lm.multicol1 = lm(cirrhosis~alcohol, data=drinking.new)
summary(lm.multicol1)

lm.multicol2 = lm(cirrhosis~alcohol2, data=drinking.new)
summary(lm.multicol2)

lm.multicol3 = lm(cirrhosis~alcohol3, data=drinking.new)
summary(lm.multicol3)

###############################################################################

lm.crime <- lm(y~., data=UScrime)   # run the model with all variables in data set
summary(lm.crime) 

summary(lm.crime)$r.squared  # R-square: percentage of variation of y explained by the model
summary(lm.crime)$adj.r.squared  # adjusted R-square: with penalty for large (large p) model


# VIF to check multicollinearity
vif(lm.crime)   # in package "car"

lm.crime2=lm(y~.-Po2, data=UScrime)   # all predictors except Po2
vif(lm.crime2)

lm.crime3=lm(y~.-Po2-GDP, data=UScrime)   # all predictors except Po2 and GDP
vif(lm.crime3)

par(mfrow=c(2,2))
plot(lm.crime3, which=1:4)  # diagnostics plot 


######################################################################
# model selection
#######################################################################
# 1. automatic selection (forward, backward, stepwise selection)
########################################################################
# stepwise selection
?ols_step_both_p
model.stepwise<-ols_step_both_p(lm.crime3, pent = 0.10, prem = 0.10, details = FALSE)
# pent = .1 means that our p-value for removing variables is .1 
model.stepwise
plot(model.stepwise)

lm.step=lm(y~Po1+Ineq+Ed+M+Prob+U2, data=UScrime)
summary(lm.step)

# forward selection
model.forward<-ols_step_forward_p(lm.crime3, penter = 0.10, details = F) # penter: threshold p-value for enter
model.forward    # final model summary
plot(model.forward)

# backward selection
model.backward<-ols_step_backward_p(lm.crime3, prem = 0.10, details = F) # prem: threshold p-value for removal
model.backward 
plot(model.backward)


###############################################
# 2. best subset selection (AIC, SBC, adjusted R-squre, C(p) etc.)
model.best.subset<-ols_step_best_subset(lm.crime3) # takes some of time 5-10 min. Be patient :)
model.best.subset

#########################################################
# regression model with categorical variable
########################################################
professor=read.csv("Professor.csv",header=TRUE)
str(professor)
is.factor(professor$Gender); 
is.character(professor$Gender)

# if categorical variable is not a factor/ character
# professor$Gender = as.factor(professor$Gender)
# or
# lm(SALARY~factor(Gender)+PUBS, data=professor)

#############################################################
# # main effect model
##############################################################
#################################################################
# regressuion with factor/ character "Gender"
summary(lm(SALARY~Gender+PUBS, data=professor))  


#################################################################
# regression with numerically coded Gender.num
Gender.num=ifelse(professor$Gender=="Female",0,1) # ref group = female
Gender.num2=ifelse(professor$Gender=="Female",1,0) # ref group = male
professor=data.frame(professor,Gender.num,Gender.num2)
View(professor)

summary(lm(SALARY~Gender.num+PUBS, data=professor))
summary(lm(SALARY~Gender.num2+PUBS, data=professor))


 #############################################################
# # interaction effect model
##############################################################

summary(lm(SALARY~Gender*PUBS, data=professor))  # main effect and interaction model

############################################################################
# regression model with categorical variable with more than three levels
############################################################################
tooth <- read.csv("ToothGrowth.csv") # data analyzed in balanced ANOVA
str(tooth)
tooth$Dose= as.factor(tooth$Dose)   # three levels: 0.5/ 1/ 2
str(tooth)

summary(lm(Toothlength ~ Dose, data=tooth))

###################################################
# log transformation
##################################################
athletes=read.table("athletes.txt", header=TRUE)
names(athletes)[1]="Sex"
str(athletes)

lm.athletes=lm(Ferr~.-Sport, data=athletes)
model.stepwise<-ols_step_both_p(lm.athletes, pent = 0.05, prem = 0.05, details = FALSE)
model.stepwise

lm.step=lm(Ferr~Sex+BMI+LBM, data=athletes)
summary(lm.step)

par(mfrow=c(2,2))
plot(lm.step, which=1:4)  # diagnostics plot 

#####################################################
lm.log.athletes=lm(log(Ferr)~.-Sport, data=athletes)
model.log.stepwise<-ols_step_both_p(lm.log.athletes, pent = 0.05, prem = 0.05, details = FALSE)
model.log.stepwise

lm.log.step=lm(log(Ferr)~Sex+BMI+LBM, data=athletes)
summary(lm.log.step)

plot(lm.log.step, which=1:4)  # diagnostics plot 
