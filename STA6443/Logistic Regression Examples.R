library(DescTools); library(ResourceSelection)

setwd("C:/Users/jorda/OneDrive/Documents/R-Studio/STA6443/")
resp = read.csv("resp.csv", header=TRUE)
str(resp)

resp$Cent = as.factor(resp$Cent)
resp$BL = as.factor(resp$BL)

#####################################################################################
# backward model selection with BIC criteria

model.full = glm(V1 ~ .-V4, data=resp, family = binomial) # full model: all predictors

glm.resp <- step(model.full, 
               direction="backward",test="Chisq", trace = F, k=log(nrow(resp)))

summary(glm.resp)

exp(glm.resp$coefficients)

###########################################
## goodness-of-fit check:  Hosmer Lemeshow test
hoslem.test(glm.resp$y, fitted(glm.resp), g=10)  # function in package "ResourceSelection"

#############################################
## residual plots
resid.d<-residuals(glm.resp, type = "deviance")
resid.p<-residuals(glm.resp, type = "pearson")
std.res.d<-residuals(glm.resp, type = "deviance")/sqrt(1 - hatvalues(glm.resp)) # standardized deviance residuals
std.res.p <-residuals(glm.resp, type = "pearson")/sqrt(1 - hatvalues(glm.resp)) # standardized pearson residuals

dev.new(width = 1000, height = 1000, unit = "px")
par(mfrow=c(1,2))
plot(std.res.d[glm.resp$model$V1==0], col = "red", 
     ylim = c(-3.5,3.5), ylab = "std. deviance residuals", xlab = "ID")
points(std.res.d[glm.resp$model$V1==1], col = "blue")

plot(std.res.p[glm.resp$model$V1==0], col = "red", 
     ylim = c(-3.5,3.5), ylab = "std. Pearson residuals", xlab = "ID")
points(std.res.p[glm.resp$model$V1==1], col = "blue")

#############################################
## Detect infludential points (cook's d > 0.05)

dev.new(width = 1000, height = 1000, unit = "px")
plot(glm.resp, which = 4, id.n = 5)  # visual inspection

(inf.id=which(cooks.distance(glm.resp)>0.05))


#####################################################
## classification 

## 1. predict the probability (p) of good status

fit.prob <- predict(glm.resp, type = "response") # estimated (fitted) probabilities 

## 2. classification with threshold

pred.class.1 <- ifelse(fit.prob > 0.5, 1, 0) # classification with 0.5 threshold
head(pred.class.1, 10)
cbind(resp[c("Treat", "BL", "V1")], fit.prob, pred.class.1)

(sample.prop=mean(resp$V1)) # sample proportion 
pred.class.2 <- ifelse(fit.prob > sample.prop, 1, 0) # classification with sample proportion threshold
cbind(resp[c("Treat", "BL", "V1")], fit.prob, pred.class.2)


## 3. calculate misclassification rate for each threshold
mean(resp$V1 != pred.class.1)  # misclassification rate from 0.5 threshold
mean(resp$V1 != pred.class.2)  # misclassification rate from sample proportion threshold
