rm(list = ls())

# load necessary libraries
library(tidyverse)
library(car)
library(broom)
library(DescTools)
library(ROCR)
library(lmtest)

# read in dataset
df_fraud = read_csv("/Users/arkaroy1/Dropbox/MS 3073/MS 3073 Course notes/L9. Logistic regression/Fraud.csv")

str(df_fraud) #look at data structures

#need to convert most of the character variables to factor variables
df_fraud$Fraud = as.factor(df_fraud$Fraud)
df_fraud$Sex = as.factor(df_fraud$Sex)
df_fraud$Age_group = as.factor(df_fraud$Age_group)
df_fraud$Race = as.factor(df_fraud$Race)
df_fraud$Fraud_Risk = as.factor(df_fraud$Fraud_Risk)
df_fraud$Missed_Payment = as.factor(df_fraud$Missed_Payment)
df_fraud$Usual_spending = as.factor(df_fraud$Usual_spending)
df_fraud$Location = as.factor(df_fraud$Location)
df_fraud$Type_of_purchase = as.factor(df_fraud$Type_of_purchase)
df_fraud$Purchase_size = as.factor(df_fraud$Purchase_size)

#look at the cell counts of the variables we're interested in
table(df_fraud$Fraud)
table(df_fraud$Fraud_Risk)

#change the reference level to low
df_fraud$Fraud_Risk = relevel(df_fraud$Fraud_Risk, ref = "Low")

# build a quick first model with logistic regression. #the binmial family is needed to let the glm function know 
# that is is logistic regression
m1 = glm(Fraud ~ Fraud_Risk, data = df_fraud, family = binomial)
summary(m1) # see results

# compute the odds rations using the exponential function
OR = exp(m1$coefficients) 
round(OR,3)

# some model fit statisitcs
AIC = AIC(m1)
BIC = BIC(m1)
cbind(AIC,BIC)

# model statistics
LR = Anova(m1, test = "LR")
Wald = Anova(m1, test = "Wald")
score = anova(m1, test = "Rao")
data.frame(LR = LR[,1], Score = score$Rao[2], Wald = Wald$Chisq)
data.frame(p.LR = LR$`Pr(>Chisq)`, p.Score = score$`Pr(>Chi)`[2], p.Wald = Wald$`Pr(>Chisq)`)

# model statistics for type 3 ANOVA
Wald3<-Anova(m1,type = 3, test = "Wald")
Wald3

###Since the Fraud data does not have any numerical variables, we'll use another (HBAT) data to discuss about model assumptions. 
rm(list = ls())

#read in data
df = read_csv("/Users/arkaroy1/Dropbox/MS 3073/MS 3073 Course notes/L9. Logistic regression/HBAT.csv")

str(df) # see data structures

#convert some numeric variables to factor variables
df$x1custype = as.factor(df$x1custype)
df$x2industype = as.factor(df$x2industype)
df$x3firmsize = as.factor(df$x3firmsize)
df$x4region = as.factor(df$x4region)
df$x5distsys = as.factor(df$x5distsys)
df$x23strat = as.factor(df$x23strat)

df <- na.omit(df) #any missing values?

# Fit the logistic regression model
m2 <- glm(x23strat ~. -id, data = df, family = binomial)
summary(m2)

##Checking first assumption: linearity of x with logit of y.
# Predict the probability (p) of x23strat
probabilities <- predict(m2, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Select only numeric predictors
mydata = dplyr::select_if(df, is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata = mutate(mydata, logit = log(probabilities/(1-probabilities))) 
mydata = gather(mydata, key = "predictors", value = "predictor.value", -logit)

#making the plot
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#Check for influential variables, only highlighting 2
plot(m2, which = 4, id.n = 2) # cook's distance

# Extract model results to compute std. residuals
m2.data <- augment(m2)
top_n(m2.data, 2, .cooksd)

#plot the std. residuals
ggplot(m2.data, aes(id, .std.resid)) + 
  geom_point(aes(color = x23strat), alpha = .5) +
  theme_bw()

# find data points with an absolute standardized residuals above 3: outliers 
filter(m2.data, abs(.std.resid) > 3) # none exists

#check for multicollinearity
vif(m2) #x17pricflex high vif so remove

df2 = select(df, -x17pricflex)
m3 <- glm(x23strat ~., data = df2, family = binomial)
vif(m3) #x1custype high vif so remove

df3 = select(df2, -x1custype)
m4 <- glm(x23strat ~., data = df3, family = binomial)
vif(m4) # under 10; you could keep going to under 5 if you like, but I'll stop here.
summary(m4)


#ROC Curve and AUC
pred <- prediction(predict(m4, df3, type = "response"),df3$x23strat) #Predicted Probability and True Classification

# area under curve
auc <- round(as.numeric(performance(pred, measure = "auc")@y.values),3)

# some important statistics
false.rates <-performance(pred, "fpr","fnr")
accuracy <-performance(pred, "acc","err")
perf <- performance(pred, "tpr","fpr")

#plotting the ROC curve and computing AUC
plot(perf,colorize = T, main = "ROC Curve")
text(0.5,0.5, paste("AUC:", auc))

# computing threshold for cutoff to best trade off sensitivity and specificity
#first sensitivity
plot(unlist(performance(pred, "sens")@x.values), unlist(performance(pred, "sens")@y.values), 
     type="l", lwd=2, 
     ylab="Sensitivity", xlab="Cutoff", main = paste("Maximized Cutoff\n","AUC: ",auc))

par(new=TRUE) # plot another line in same plot

#second specificity
plot(unlist(performance(pred, "spec")@x.values), unlist(performance(pred, "spec")@y.values), 
     type="l", lwd=2, col='red', ylab="", xlab="")
axis(4, at=seq(0,1,0.2)) #specificity axis labels
mtext("Specificity",side=4, col='red')

#find where the lines intersect
min.diff <-which.min(abs(unlist(performance(pred, "sens")@y.values) - unlist(performance(pred, "spec")@y.values)))
min.x<-unlist(performance(pred, "sens")@x.values)[min.diff]
min.y<-unlist(performance(pred, "spec")@y.values)[min.diff]
optimal <-min.x #this is the optimal points to best trade off sensitivity and specificity

abline(h = min.y, lty = 3)
abline(v = min.x, lty = 3)
text(min.x,0,paste("optimal threshold=",round(optimal,2)), pos = 4)

pr = predict(m4, df3, type = "response") # make prediction with last built model
pr_class = ifelse(pr>0.49, 1,0) #use the optimal cutoff to classify
caret::confusionMatrix(as.factor(pr_class),as.factor(df3$x23strat)) #this function and package auto computes a lot of the metrics

### Going back to the Fraud data and redoing the classification steps again
#build the final model with fraud data
m5 = glm(Fraud ~ ., data = df_fraud, family = binomial)

#ROC Curve and AUC
pred <- prediction(predict(m5, df_fraud, type = "response"),
                   df_fraud$Fraud) #Predicted Probability and True Classification
auc <- round(as.numeric(performance(pred, measure = "auc")@y.values),3)

false.rates <-performance(pred, "fpr","fnr")
accuracy <-performance(pred, "acc","err")

perf <- performance(pred, "tpr","fpr")
plot(perf,colorize = T, main = "ROC Curve")
text(0.5,0.5, paste("AUC:", auc))

plot(unlist(performance(pred, "sens")@x.values), unlist(performance(pred, "sens")@y.values), 
     type="l", lwd=2, 
     ylab="Sensitivity", xlab="Cutoff", main = paste("Maximized Cutoff\n","AUC: ",auc))
par(new=TRUE)
plot(unlist(performance(pred, "spec")@x.values), unlist(performance(pred, "spec")@y.values), 
     type="l", lwd=2, col='red', ylab="", xlab="")
axis(4, at=seq(0,1,0.2))
mtext("Specificity",side=4, padj=-2, col='red')

min.diff <-which.min(abs(unlist(performance(pred, "sens")@y.values) - unlist(performance(pred, "spec")@y.values)))
min.x<-unlist(performance(pred, "sens")@x.values)[min.diff]
min.y<-unlist(performance(pred, "spec")@y.values)[min.diff]
optimal <-min.x

abline(h = min.y, lty = 3)
abline(v = min.x, lty = 3)
text(min.x,0,paste("optimal threshold=",round(optimal,5)), pos = 4)

pr_fraud = predict(m5, df_fraud, type = "response")
pr_fraud_class = ifelse(pr_fraud>0.01, 1,0)
caret::confusionMatrix(as.factor(pr_fraud_class),as.factor(df_fraud$Fraud))

#doing the classification at arbitrary 0.5 instead of the optimal threshold
pr_fraud_class_notopt = ifelse(pr_fraud>0.5, 1,0)
caret::confusionMatrix(as.factor(pr_fraud_class_notopt),as.factor(df_fraud$Fraud))

### HBAT example code with reduced data
rm(list = ls())
#read in data
df = read_csv("/Users/arkaroy1/Dropbox/MS 3073/MS 3073 Course notes/L9. Logistic regression/HBAT.csv")

str(df) # see data structures

#convert some numeric variables to factor variables
df$x1custype = as.factor(df$x1custype)
df$x2industype = as.factor(df$x2industype)
df$x3firmsize = as.factor(df$x3firmsize)
df$x4region = as.factor(df$x4region)
df$x5distsys = as.factor(df$x5distsys)
df$x23strat = factor(df$x23strat, ordered = F)

df_reduced = select(df, c(id, x1custype, x19satis, x20recomm, x21futpur, x23strat))

form = x23strat ~ x1custype + x19satis + x20recomm + x21futpur + x1custype*x19satis + x1custype*x21futpur # formula
#including+ x1custype*x20recomm creates perfect separation
m6 = glm(formula = form, data = df_reduced, family = binomial) 
summary(m6) #see results of regression

m7 = glm(x23strat ~ x19satis + x20recomm + x21futpur, data = df_reduced, family = binomial)
summary(m7)

# Individua categorical terms signficance
Anova(m7, type = 3, test = "Wald")

#overall model
lrtest(m7) #LR test

##Checking first assumption: linearity of x with logit of y.
# Predict the probability (p) of x23strat
probabilities <- predict(m7, type = "response")

# Select only numeric predictors
mydata = dplyr::select_if(df_reduced, is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata = mutate(mydata, logit = log(probabilities/(1-probabilities))) 
mydata = gather(mydata, key = "predictors", value = "predictor.value", -logit)

#making the plot
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) + geom_smooth(method = "loess") + theme_bw() + facet_wrap(~predictors, scales = "free_y")

#Check for influential variables, only highlighting 2
plot(m7, which = 4, id.n = 2) # cook's distance

# Extract model results to compute std. residuals
m7.data <- augment(m7)
m7.data = cbind(df$id, m7.data)
colnames(m7.data)[1] = "id"

#plot the std. residuals
ggplot(m7.data, aes(id, .std.resid)) + geom_point(aes(color = x23strat), alpha = .5) + theme_bw()

# find data points with an absolute standardized residuals above 3: outliers 
filter(m7.data, abs(.std.resid) > 3) # none exists

#check for multicollinearity
vif(m7) #x17pricflex high vif so remove

#ROC Curve and AUC
pred <- prediction(predict(m7, df_reduced, type = "response"),
                   df_reduced$x23strat) #Predicted Probability and True Classification
auc <- round(as.numeric(performance(pred, measure = "auc")@y.values),3)

false.rates <-performance(pred, "fpr","fnr")
accuracy <-performance(pred, "acc","err")

perf <- performance(pred, "tpr","fpr")
plot(perf,colorize = T, main = "ROC Curve")
text(0.5,0.5, paste("AUC:", auc))

predicted.classes <- ifelse(probabilities > 0.49, 1, 0)
table(as.factor(predicted.classes), as.factor(df$x23strat))

sensit = table(as.factor(predicted.classes), as.factor(df$x23strat))[2,2]/sum(table(as.factor(predicted.classes), as.factor(df$x23strat))[,2])
specif = table(as.factor(predicted.classes), as.factor(df$x23strat))[1,1]/sum(table(as.factor(predicted.classes), as.factor(df$x23strat))[,1])
