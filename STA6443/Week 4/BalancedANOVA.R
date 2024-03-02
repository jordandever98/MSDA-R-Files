library(DescTools); library(MASS)

setwd("C:/Users/jorda/OneDrive/Documents/R-Studio/STA6443")  # need to change this path

######################################
## import data
######################################
tooth <- read.csv("ToothGrowth.csv")
View(tooth)
str(tooth)

###################################
## change the format of "Dose"
###################################
tooth$Dose= as.factor(tooth$Dose)
str(tooth)

###################################
## balanced or unbalanced?
###################################
table(tooth$Dose); table(tooth$Supplement)

####################################
## one-way ANOVA (Toothlength ~ Dose)
####################################
#
library(car)
aov.tooth= aov(Toothlength~Dose, data=tooth)   # aov() in package "car"

LeveneTest(aov.tooth)  # If levene test is significant (null: homoscedasticity), use  Welch correction

summary(aov.tooth)      # ANOVA result

plot(aov.tooth)          # diagnostics plot - Normality check

#dev.new(width = 100, height = 100, unit = "px")
par(mfrow=c(2,2))      # diagnostics plot - in one 
plot(aov.tooth)
par(mfrow=c(1,1))

plot(aov.tooth, 1); # display each plot separate
plot(aov.tooth, 2)  

#################################################
## One-way ANOVA result in slide
##################################################
boxplot(Toothlength ~ Dose, data=tooth, main="distribution of tooth length by dose")

summary(aov.tooth)

###########################################################
## R-square : variation of response variable explained by model
##############################################################
# lm() and aov() resulst are identical but lm() is more generalized function
lm.res= lm(Toothlength ~ Dose, data=tooth)
anova(lm.res)
summary(lm.res)$r.squared  # R-square 

#######################################
## welch test for unequal variance case
#########################################
oneway.test(Toothlength ~ Dose, data=tooth, var.equal=FALSE)  #oneway.test has option for Welch's anova to account for unequal variances

############################
# post-hoc test
###########################
ScheffeTest(aov.tooth)   # Scheffe test
TukeyHSD(aov.tooth)      # Tukey test


####################################
## Two-way ANOVA (Toothlength ~ Dose * Supplement)
####################################

aov.tooth2 <- aov(Toothlength ~ Dose * Supplement , data = tooth) #these are the same
aov.tooth3 <- aov(Toothlength ~ Dose + Supplement + Dose*Supplement , data = tooth)#just different code  

summary(aov.tooth2)
summary(aov.tooth3)

LeveneTest(aov.tooth2)  # equal variance check - error with leveneTest(aov.tooth3)

dev.new(width = 100, height = 100, unit = "px")
par(mfrow=c(2,2))   # normality check 
plot(aov.tooth2)

summary(aov.tooth2)  # ANOVA result

lm.res2= lm(Toothlength ~ Dose * Supplement, data=tooth)
summary(lm.res2)$r.squared  # R-square 

# post-hoc test
ScheffeTest(aov.tooth2)
TukeyHSD(aov.tooth2)

##############################
#interaction plot
##############################
# plot in lecture note
par(mfrow=c(1,1))
interaction.plot(tooth$Dose, tooth$Supplement, tooth$Toothlength,
                 leg.bty="o", leg.bg="beige", lwd=2, 
                 type="b", col=c(1:2),  ### Colors for levels of trace var.
                 pch=c(18,24),            ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 xlab="Dose",
                 trace.label="Supplement",
                 ylab="Mean of tooth length",
                 main="Interaction plot")

interaction.plot(tooth$Supplement, tooth$Dose, tooth$Toothlength,
                 leg.bty="o", leg.bg="beige", lwd=2, 
                 type="b", col=c(1:3),  ### Colors for levels of trace var.
                 pch=c(18,24,22),            ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 xlab="Supplement",
                 trace.label="Dose",
                 ylab="Mean of tooth length",
                 main="Interaction plot")



#################################
## practice 
################################
grass=read.csv("Grass.csv", header=TRUE)
str(grass)

# change format to factor
grass$Method = as.factor(grass$Method)
grass$variety=as.factor(grass$variety)
grass$Group=as.factor(grass$Group)
str(grass)

table(grass$Method) # A, B, C
table(grass$variety) # 1 -5 
table(grass$Group) # 1-3
##############################################
# Backward elimination example (manually)
##############################################
# 1. find the least significant variable (with largest p-value) to remove
summary(aov(Yield ~ Method + variety + Group, data=grass)) # Remove Group 

# 2. remove the next insignificant variable
summary(aov(Yield ~ Method + variety, data=grass)) #Remaining variables significant

# 3. until there is no more insignificant variable to remove


#######################################
# forward selection example (manually)
########################################
# 1. find the most significant variable (with smallest p-value) to add
summary(aov(Yield ~ Method, data=grass)) #Most Significant - add method
summary(aov(Yield ~ variety, data=grass))
summary(aov(Yield ~ Group, data=grass))

# 2. add the next significant variable
summary(aov(Yield ~ Method + variety, data=grass)) # Variety most significant - add variety
summary(aov(Yield ~ Method + Group, data=grass))

# 3. until there is no more significant variable to add
summary(aov(Yield ~ Method + variety + Group, data=grass)) #Stopping Rule Group > .05 - dont add group

# final model from forward selection with main effects only: Yield ~ Method + variety

###############################
# stepwise selection  (automatic) 
###############################

# stepwise selection with main effects model first
full.main=lm(Yield ~ Method + variety + Group, data=grass)
step.main <- stepAIC(full.main, direction = "both", trace = FALSE) # based on AIC not on p-vlaue, but same idea
anova(step.main)

summary(aov(Yield ~ Method*variety, data=grass)) #interaction effect of method*variety


# stepwise selection with all interaction terms
full.intr=lm(Yield ~ Method*variety*Group, data=grass)
aov(full.intr)
step.intr <- stepAIC(full.intr, direction = "both", trace = FALSE) # based on AIC not on p-vlaue, but same idea
anova(step.intr)
