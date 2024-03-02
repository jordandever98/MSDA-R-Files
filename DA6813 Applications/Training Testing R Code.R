library(ROCR)
library(car)
library(tidyverse)

setwd("C:/Users/jorda/OneDrive/Documents/R-Studio/DA6813 Applications/")

KC_HOUSE = read_csv("KC_HOUSE.csv")

str(KC_HOUSE)

# Creating training and testing samples
set.seed(1)
index = sample(1:nrow(KC_HOUSE), 0.7*nrow(KC_HOUSE))
KCtrain = KC_HOUSE[index,]
KCtest = KC_HOUSE[-index,]

m3 = lm(price ~ sqft_living + bedrooms + bathrooms, data = KCtrain)
summary(m3)
predictions = predict(m3, newdata = KCtest, type = "response")

#Measures
mse = mean((KCtest$price - predictions)^2)
mae = mean(abs(KCtest$price - predictions))
me = mean(KCtest$price - predictions)
mape =  mean(abs(KCtest$price - predictions)/KCtest$price)*100


#read in data
df = read_csv("HBAT.csv")

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

#check for multicollinearity
vif(m2) #x17pricflex high vif so remove

df2 = select(df, -x17pricflex)
m3 <- glm(x23strat ~., data = df2, family = binomial)
vif(m3) #x1custype high vif so remove

# VIF under 5; so I'll stop here.
summary(m3)

# Predict the probability (p) of x23strat
probabilities <- predict(m3, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

caret::confusionMatrix(as.factor(predicted.classes),as.factor(df2$x23strat)) #this function and package auto computes a lot of the metrics

#### end of default cutoff of 0.5
#### optimal cutoff

#ROC Curve and AUC
pred <- prediction(probabilities,df2$x23strat) #Predicted Probability and True Classification

# area under curve
auc <- round(as.numeric(performance(pred, measure = "auc")@y.values),3)

#plotting the ROC curve and computing AUC
perf <- performance(pred, "tpr","fpr")
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
text(min.x,0,paste("optimal threshold=",round(optimal,2)), pos = 3)

pr_class = ifelse(probabilities>0.47, 1,0) #use the optimal cutoff to classify
caret::confusionMatrix(as.factor(pr_class),as.factor(df2$x23strat)) #this function and package auto computes a lot of the metrics
