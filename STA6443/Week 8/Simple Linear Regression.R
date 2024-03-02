
setwd("C:/Users/jorda/OneDrive/Documents/R-Studio/STA6443")

drinking=read.csv("drinking.csv", header=TRUE)
str(drinking)
#View(drinking)

##############################################################
# scatter plot and correltion between alcohol and cirrhosis
##############################################################
# scatter plot
with(drinking, plot(alcohol, cirrhosis))
plot(drinking$alcohol, drinking$cirrhosis, xlab="alcohol", ylab="cirrhosis")

# correlation
cor(drinking$alcohol, drinking$cirrhosis, method="pearson")
cor(drinking$alcohol, drinking$cirrhosis, method="spearman")


################################
# fit linear regression 
###############################
lm.drinking <- lm(cirrhosis~alcohol, data=drinking)  
summary(lm.drinking)

###################################
# scatter plot - alcohol vs. cirrhosis
###################################
with(drinking, plot(alcohol, cirrhosis))
abline(lm.drinking, col="red")

################################
# diagnostics plot
###############################
par(mfrow=c(2,2))
plot(lm.drinking, which=c(1:4))    # default diagnostics plots
#dev.off()

# manual generation of diagnostics plot
std.res=rstandard(lm.drinking)    # fitted vs. standardized residual plot
Y.hat=fitted(lm.drinking)
plot(Y.hat, std.res); abline(h=0,col="red")

#############################
# other saved results
############################ 
(Y.hat = fitted(lm.drinking))   #Predicted value 
(residuals = residuals(lm.drinking))   # residual = Y - fitted values (Y-hat)

names(lm.drinking) # other saved results
lm.drinking$coefficients
lm.drinking$fitted.values

names(summary(lm.drinking)) 
summary(lm.drinking)$r.squared

#############################
# influential points
#############################
cook.d = cooks.distance(lm.drinking)
round(cook.d, 2)

(dfbeta=dfbetas(lm.drinking)) # dfbeta for measuring influence of each obs
(diffit=dffits(lm.drinking)) # diffit 

plot(cook.d,col="pink", pch=19, cex=1)
text(cooks.distance(lm.drinking),labels = rownames(drinking))

###########################################################
# 1. detect observations with cooks distance greater than 1
# 2. run the model without influential observations
###########################################################

###############################################
# find observation id with large cook's d 
inf.id = which(cooks.distance(lm.drinking)>1)
drinking[inf.id, ]
lm.drinking2=lm(cirrhosis ~ alcohol, data=drinking[-inf.id, ])

with(drinking, plot(alcohol, cirrhosis))
abline(lm.drinking, col="red")
abline(lm.drinking2,col="blue") # without france
legend("bottomright",col=c("red","blue"),legend=c("w/ France", "w/out France"), cex=0.8, title.adj=0.15, lty=1)

# compare two regression results 
summary(lm.drinking)   # w/ France 
summary(lm.drinking2)  # w/out France

par(mfrow=c(2,2))
plot(lm.drinking2, which=c(1:4))    # default diagnostics plots

###############################################
# find observations id with large cook's d in the plot
# cutoff = 1   # or 4/(nrow(drinking)) 
plot(lm.drinking, which=4,  col = "navyblue")

#######################################################
# example in slide
######################################################
crime=read.csv("crime.csv",header=TRUE)
str(crime)
head(crime)

lm.crime <- lm(crime ~ poverty, data=crime)
summary(lm.crime)

par(mfrow=c(2,2))
plot(lm.crime, which=c(1:4))    # default diagnostics plots

inf.id=which(cooks.distance(lm.crime)>1)
crime[inf.id, ]
lm.crime2=lm(crime ~ poverty, data=crime[-inf.id, ])

with(crime, plot(poverty, crime))
abline(lm.crime, col="red")
abline(lm.crime2,col="blue")

summary(lm.crime)
summary(lm.crime2)

######################################################################
# simulation study 
# what happens on diagnostics plots when assumptions are not valid?
#######################################################################

sim=read.csv("SimData.csv", header=TRUE)
View(sim)

# data generation 
set.seed(12345678)
X = rnorm(100,3,1)   # random generation of independent variable
Y = 1.5+3*X           # E(Y) = 1.5 + 3X

##################
# scenario 1
##################
e1 = rnorm(100,0,0.5) # normally distributed error
Y1 = Y + e1          # observed Y1 with noise e1

##################
# scenario 2
##################
e2 = rlnorm(100,0,1)  # error from right skewed and heavy tailed distribution (log normal)
Y2 = Y + e2         # observed Y2 with noise e2

##################
# scenario 3
##################
e3 = X^2*e1      # heteroscediaticity (variance of error is proportional to X)
Y3 = Y + e3      # observed Y3 from noise e3

##################
# scenario 4
##################
Y4 = 1.5+X^2+e1  # non-linear relationship 

# simulated data from each scenario 
#dev.new(width = 100, height = 100, unit = "px")
par(mfrow=c(2,2))
plot(X,Y1, main="Scenario 1"); lines(X, Y, col="red")
plot(X,Y2, main="Scenario 2"); lines(X, Y, col="red")
plot(X,Y3, main="Scenario 3"); lines(X, Y, col="red")
plot(X,Y4, main="Scenario 4"); lines(X, Y, col="red")

# fit the linear regression and check diagnostics plot
res1=lm(Y1~X); 
par(mfrow=c(2,2))
plot(res1, which=1:4)

res2=lm(Y2~X); 
par(mfrow=c(2,2))
plot(res2, which=1:4)

res3=lm(Y3~X); 
par(mfrow=c(2,2))
plot(res3, which=1:4)

res4=lm(Y4~X); 
par(mfrow=c(2,2))
plot(res4, which=1:4)
