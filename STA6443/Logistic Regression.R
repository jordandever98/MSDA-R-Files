library(DescTools); 
library(ResourceSelection)

setwd("C:/Users/jorda/OneDrive/Documents/R-Studio/STA6443/")  # need to change this path

#################################################
# Example 1: plasma data
#################################################
plasma=read.csv("plasma.csv",header=TRUE)
str(plasma)


##############################################
## logistic regression 
glm.plasma0 = glm(esr ~ fibrinogen+gamma, data = plasma, family = "binomial")
summary(glm.plasma0)

## odds ratios 
OR=exp(glm.plasma0$coefficients)
round(OR, 3)

## Remove insignificant terms, refit and interpret
glm.plasma = glm(esr ~ fibrinogen, data = plasma, family = "binomial")
summary(glm.plasma)

OR2=exp(glm.plasma$coefficients)
round(OR2, 3)

###########################################
## goodness-of-fit 

## Hosmer Lemeshow test
hoslem.test(glm.plasma$y, fitted(glm.plasma), g=10)  # function in package "ResourceSelection"

## psuedo R^2
pseudo.r2<-PseudoR2(glm.plasma, which =  c("McFadden", "Nagel", "CoxSnell"))
round(pseudo.r2, 3)

############################################
## diagnostics
###########################################
## 1. Residual plots
resid.d<-residuals(glm.plasma, type = "deviance")
resid.p<-residuals(glm.plasma, type = "pearson")
std.res.d<-residuals(glm.plasma, type = "deviance")/sqrt(1 - hatvalues(glm.plasma)) # standardized deviance residuals
std.res.p <-residuals(glm.plasma, type = "pearson")/sqrt(1 - hatvalues(glm.plasma)) # standardized pearson residuals

dev.new(width = 1000, height = 1000, unit = "px")
par(mfrow=c(1,2))
plot(std.res.d[glm.plasma$model$esr==0], col = "red", 
     ylim = c(-3.5,3.5), ylab = "std. deviance residuals", xlab = "ID")
points(std.res.d[glm.plasma$model$esr==1], col = "blue")

plot(std.res.p[glm.plasma$model$esr==0], col = "red", 
     ylim = c(-3.5,3.5), ylab = "std. Pearson residuals", xlab = "ID")
points(std.res.p[glm.plasma$model$esr==1], col = "blue")

#############################################
## 2. Influence dianostics - cook's distance

dev.new(width = 1000, height = 1000, unit = "px")
plot(glm.plasma, which = 4, id.n = 5)  # visual inspection

# which observation has cook's d larger than 0.4?
(inf.id=which(cooks.distance(glm.plasma)>0.4))

# ## fit model without observation with cook's d larger than 0.4 
 glm.plasma = glm(esr ~ fibrinogen, data = plasma[-inf.id, ], family = "binomial")
 summary(glm.plasma)


#################################################
# Example 2: amputation data
#################################################

amputation=read.csv("amputation.csv",header = TRUE)
str(amputation)
glm.amputation <- glm(AMPUTATION ~ factor(ILLNESS_SEVERITY)+factor(diabetes)+factor(Ulcers), data = amputation, family = "binomial")
summary(glm.amputation) #Converting categorical variables into factors

############################################
# Odds Ratio
round(exp(glm.amputation$coefficients),3)

##############################################
## model selection - setpwise selection

amputation$hypertension = as.factor(amputation$hypertension)
amputation$Ulcers = as.factor(amputation$Ulcers)
model.null = glm(AMPUTATION ~ 1, data=amputation, family = binomial) # null model : no predictor
model.full = glm(AMPUTATION ~ ., data=amputation, family = binomial) # full model: all predictors
# Null model doesnt include any x



# stepwise selection with AIC
step.models.AIC<-step(model.null, scope = list(upper=model.full), # create an upper bound for the model
                  direction="both",test="Chisq", trace = F) # Can also add a lower bound
 # test = 'Chisq' is not needed?
# Error + Penalty * 2
# stepwise selection with BIC
step.models.BIC<-step(model.null, scope = list(upper=model.full),
                  direction="both",test="Chisq", trace = F, k=log(nrow(amputation))) # Diff 
#log(3000) for BIC


summary(step.models.AIC)   # summary of stepwise selection with AIC
summary(step.models.BIC)   # summary of stepwise selection with BIC


#####################################################
## classification  - plasma data
#####################################################
## 1. predict the probability (p) of unhealthy status
glm.plasma = glm(esr ~ fibrinogen, data = plasma, family = "binomial")
summary(glm.plasma)
fit.prob <- predict(glm.plasma, type = "response") # estimated (fitted) probabilities of unhealthy status
head(fit.prob) # Similar to y hat from linear reg ^
## 2. classification with threshold

pred.class.1 <- ifelse(fit.prob > 0.5, 1, 0) # classification with 0.5 threshold

cbind(fit.prob, pred.class.1)
head(pred.class.1, 10)

# output in slide - not necessary but just for the display
combined.dat = cbind(plasma[,c("fibrinogen","esr")], fit.prob, pred.class.1)
combined.dat[12:20,]

sample.prop=mean(plasma$esr) # sample proportion of unhealthy status, getting the 1s to 0s proportion
sample.prop # Can use this as a cutoff as well
pred.class.2 <- ifelse(fit.prob > sample.prop, 1, 0) # classification with sample proportion threshold
#using .5 as our cut off
head(pred.class.2, 10)

cbind(fit.prob, pred.class.1, pred.class.2)

## 3. calculate misclassification rate for each threshold
mean(plasma$esr != pred.class.1)  # misclassification rate from 0.5 threshold
mean(plasma$esr != pred.class.2)  # misclassification rate from sample proportion threshold

# Our data is quite unbalanced because we only had .1875 1s, so if we are focusing on FN
# this model is potentially good'