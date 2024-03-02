# HW2
---
  title: "STA6443 HW2"
author: "Jordan Dever"
format: docx
editor: visual
---
  
library(DescTools); library(MASS)
library(BSDA)
library(car)
setwd("C:/Users/jorda/OneDrive/Documents/R-Studio/STA6443/HW/HW2 files")
  
######################################################
# Exercise 1
######################################################
health <- read.csv('heartbpchol.csv')
View(health)
str(health)
health$Cholesterol = as.integer(health$Cholesterol)
#Assuming Normality based on directions
aov.health = aov(Cholesterol~BP_Status, data = health)
summary(aov.health) # P = 0.00192

LeveneTest(aov.health) #p = 0.9083

par(mfrow=c(2,2))
plot(aov.health)

lm.health = lm(Cholesterol~BP_Status, data = health)
anova(lm.health)
summary(lm.health)$r.square # 0.02297824

#A
# BP_Status has a p-value of 0.00192 which is less than our alpha level of 0.05,
# so we will reject the null hypothesis and state that there is at least one
# BP_Status group with a different cholesterol mean. Our r-squared is 0.02297824, so our
# model only describes about ~2.3% of the variation. Using the Levene Test and
# looking at our figure "Residuals vs Fitted" we can trust our equal variance 
# assumption because our Levene's p-value was .9083 so we fail to reject the
# null hypothesis and state that there is equal variance among groups.
# The figure also shows that the lines are about equal lengths.

ScheffeTest(aov.health) #N-H 0.0218 , O-H 0.0103               H > N > O
TukeyHSD(aov.health) # N-H 0.0156851 , O-H0.0070073

#B
# After running a post-hoc test, the Normal-High (p = 0.0218) and Optimal-High
# (p =  0.0103) groups both had significantly different cholesterol means. 
# Both groups had a negative difference, so we know that the High group had the
# highest mean. The group with the lowest mean is the Optimal group because when
# we difference it with the Normal group, the difference is negative. So the
# High group has the highest cholesterol mean across BP status groups, the 
# Normal group has the second highest cholesterol mean across BP status groups,
# and the Optimal group has the lowest cholesterol mean across BP status groups.
# High > Normal > Optimal





######################################################
# Exercise 2
######################################################
liver <- read.csv('bupa.csv')
View(liver)
str(liver)

#Assume normality and independence assumptions
liver$drinkgroup = as.factor(liver$drinkgroup)
str(liver)

aov.mcv = aov(mcv~drinkgroup, data = liver)
summary(aov.mcv)#drinkgroup p = 7.43e-08 (was 2.09e-08 previously)


LeveneTest(aov(mcv~drinkgroup, data = liver)) # p = 0.8744

par(mfrow=c(2,2))
plot(aov.mcv)

lm.mcv = lm(mcv~drinkgroup, data = liver)
anova(lm.mcv) # 7.429e-08
summary(lm.mcv)$r.squared # 0.1077214

#A
# The p-value for the drink group in this model is 7.43e-08 so we will reject 
# the null and state that there is at least one drink group with a different
# mean corpuscular volume. About ~10.8% is how much variation can be described 
# by our model. The Levene test gave a p-value of 0.8744 so we will fail to 
# reject the null and state that there is equal variance across groups so our
# assumption can be trusted. The Residuals vs Fitted is about equal as well.


aov.alkphos = aov(alkphos~drinkgroup, data = liver)
summary(aov.alkphos)#drinkgroup p = 0.00495


LeveneTest(aov(alkphos~drinkgroup, data = liver)) # p = 0.5201

par(mfrow=c(2,2))
plot(aov.alkphos) #About equal

lm.alkphos = lm(alkphos~drinkgroup, data = liver)
anova(lm.alkphos) # 0.00495
summary(lm.alkphos)$r.squared # 0.04270721

#B
# In this model with alkphos, the drink group had a p-value of 0.00495, so we 
# will reject the null and state that there is at least one group with a 
# different alkphos mean across the drink groups. About 4.3% of the variation
# can be explained by our model. The residuals vs fitted figure looks about
# equal but we can get a better insight into our equal variance with our Levene
# test which gave us a p-value of 0.5201. We will fail to reject the null and
# state that there is equal variance across groups.

ScheffeTest(aov.mcv) # 4-1 1.9e-6, 5-1 0.0081, 4-2 0.0380, 4-3 0.0025 4>1 5>1 4>2 4>3
ScheffeTest(aov.alkphos) # 5-2 0.0329, 5-3 0.0069 5-1 & 5-4 almost

#C
# For the mcv model (A), we got 4 pairs with significant means. 4-1 with a p of
# 1.9e-6, 5-1 with p=0.0081, 4-2 p=0.0380, and 4-3 p=0.0025. 
# For the alkphos model (B), we got 2 pairs with significant means. 5-2 with
# p=0.0329 and 5-3 with p=0.0069.
# Is is not a significant common pair between these two models but all of our
# significant pairs had a positive difference. Another difference is that in 
# model A there were pairs significant containing group 4, but in model B the 
# only significant pairs contained group 5. Another difference is that in model
# B there were some pairs that were close to being significant while model A
# did not have other pairs that were close to being significant.




######################################################
# Exercise 3
######################################################
psy <- read.csv('psych.csv')
View(psy)
str(psy)
table(psy$sex);table(psy$rank)

psych1 = aov(salary ~ rank + sex + rank*sex, data = psy) 
psych2 = aov(salary ~ sex + rank + rank*sex, data = psy)
psych3 = Anova(psych1, type = 3)

summary((psych1))#RANKp = 5.33e-5 , SEXp = 0.0112, RANK*SEXp = 0.7951
summary(psych2)#RANKp = 0.000637 , SEXp = 0.000417, RANK*SEXp = 0.795101
psych3#RANKp = 0.001240 , SEXp = 0.09671, RANK*SEXp= 0.79519
summary(aov(salary ~ rank * sex, data = psy)) #0.7951

LeveneTest(aov(salary ~ rank * sex, data = psy)) #0.7241
par(mfrow=c(2,2))
plot(psych1)

summary(lm(salary ~ rank + sex + rank*sex, data = psy))$r.squared #0.6647566


#A
# The Type 1 and Type 3 Sum of Squares tell us that rank and sex are significant
# to the mean salary of psychology faculty.
# Type 1 RANKp = 5.33e-5 , SEXp = 0.0112, RANK*SEXp = 0.7951
# Type 1 RANKp = 0.000637 , SEXp = 0.000417, RANK*SEXp = 0.795101
# Type 3 RANKp = 0.001240 , SEXp = 0.09671, RANK*SEXp= 0.79519
# For Type 1, we reject the null for rank and sex then state that there is a 
# rank effect and sex effect but we fail to reject the null for the interaction
# effect so there is no interaction effect for both Type 1s.
# For Type 3, we reject the null for rank but fail to reject the null for sex
# and the interaction so there is a rank effect but no sex effect and no
# interaction effect. Type 1 and Type 3 sum of squares tell us that the 
# significance of effects shifts when we exclude overlapping in Type 3 to where
# sex is no longer significant which is important to our research question of
# female works being paid differently. 
# Our explainable variance from the model is ~66.5%.



#b) Refit the model without the interaction term. Comment on the significance of effects and
#variation explained. Report and interpret the Type 1 and Type 3 tests of the main effects. Are the
#main effects of rank and sex significant?
  
psych4 = aov(salary ~ rank + sex, data = psy) 
psych5 = aov(salary ~ sex + rank, data = psy)
psych6 = Anova(psych4, type = 3)

summary((psych4))#RANKp = 3.34e-5 , SEXp = 0.00926
summary(psych5)#RANKp = 0.000454 , SEXp = 0.000291
psych6#RANKp = 0.0002912 , SEXp = 0.0092618


LeveneTest(aov(salary ~ rank * sex, data = psy)) #0.7241
par(mfrow=c(2,2))
plot(aov(salary ~ rank + sex, data = psy))

summary(lm(psych4))$r.squared #0.6634627

#B
# After removing the interaction term, all p-values became significant which
# indicates that the main effects of rank and sex are significant when the
# interaction is removed.
# Type 1 RANKp = 3.34e-5 , SEXp = 0.00926
# Type 1 RANKp = 0.000454 , SEXp = 0.000291
# Type 3 RANKp = 0.0002912 , SEXp = 0.0092618
# Before the only term that wasn't significant of these 6 was Type 3 SEXp
# which was 0.09671 > 0.05 but now it has become significant.
# Explained variance slightly went down to about 66.3% compared to 66.5.
  
ScheffeTest(psych4) #Assoc-Assist  0.00015 , M-F 0.0382
ScheffeTest(psych5) #Assoc-Assist  0.0018 , M-F 0.017
TukeyHSD(aov(salary ~ rank + sex, data = psy, type = 3)) 
#Assoc-Assist 3.34e-05  , M-F 0.011649
str(psych6)

#D
# Type 1 groups had Assoc-Assist  0.00015 , M-F 0.0382 and Assoc-Assist  0.0018
# , M-F 0.017 so both pairs were significant. The difference was positive on
# both pairs but rank differences were different (first 3.52, second 5.37) and 
# the sex values were only different by ~1.5 (6.8 first 5.3 second). For the 
# Type 3 group, Assoc-Assist 3.34e-05  , M-F 0.011649. The difference was 5.33
# for sex and 5.34 for rank. Difference in salary was about ~$5300 for all pairs
# except rank in the first Type 1 of ~3500 and sex in the first Type 1 ~6800.
  
 




######################################################
# Exercise 4
######################################################
carsnew <- read.csv('cars_new.csv')

cars = aov(mpg_highway ~ cylinders + origin + type, data = carsnew) 
summary(cars) # cyl - 2e-16 , origin = 0.37212, type = 0.00175

# Will drop origin because it is not sigificant and will distort our Type 3 
# because of the overlaps it has into other data pools.

cars2 = aov(mpg_highway ~ cylinders + type, data = carsnew) 
cars3 = aov(mpg_highway ~ type + cylinders, data = carsnew) 
cars4 = Anova(cars2, type = 3) 
summary(cars2) #cyl p = 2e-16, type p =0.00117
summary(cars3) #type p = 0.00201, cyl p = 2e-16
cars4 #cyl 2.2e-16 same for type
summary(lm(cars2))$r.squared #0.4572163
#A
# Dropped origin because it was not significant and would distort our Type 3 
# data because of overlaps into other data pools. Cylinders and type had
# significant effects on mpg_highway for both Type 1 groups
# Type 1 cyl p = 2e-16, type p =0.00117
# Type 1  p = 0.00201, cyl p = 2e-16
# Type 3 cyl p = 2.2e-16 type p = 2.2e-16
# This tells us that cylinders and type have main effects on highway mpg and
# about 45.7% of the variance is explained by our model.

cars5 = aov(mpg_highway ~ cylinders + type + cylinders*type, data = carsnew)
cars6 = aov(mpg_highway ~ type + cylinders + cylinders*type, data = carsnew)
cars7 = Anova(cars5, type = 3)
summary(cars5)
# Type 1cyl p = 2e-16, type p = 0.00094, interaction p = 0.00470
summary(cars6)
# Type 1 cyl p = 2e-16, type p = 0.00164, interaction p = 2e-16
cars7
# Type 3 cyl p = 2e-16, type p = 0.0006601, interaction p = 0.0046958

summary(lm(cars5))$r.squared # 0.4813821

# Cylinders, type, and interaction all have significant effects on highway mpg.
# Type 1cyl p = 2e-16, type p = 0.00094, interaction p = 0.00470
# Type 1 cyl p = 2e-16, type p = 0.00164, interaction p = 2e-16
# Type 3 cyl p = 2e-16, type p = 0.0006601, interaction p = 0.0046958
# This tells us that there are main effects and interaction effects within this
# model because we reject the null for each group. We have an explained
# variance of about 48.1% with the interaction included which gives us about 
# 2.4% more than our previous model.

carsnew$cylinders = as.factor(carsnew$cylinders)
ScheffeTest(aov(mpg_highway ~ cylinders + type + cylinders*type, data = carsnew))
# Cyl 6-4 p = 2e-16 diff = -5.72,Sports-Sedan 0.0117 diff -2.81
# Interactions:
# 6:Sedan-4:Sedan p=2e-16 diff -6.17 
# 4:Sports-4:Sedan p = 0.00036 diff -5.23
# 6:Sports-4:Sedan p = 2.7e-06 diff -6.6

# After our post-hoc test, cylinder pairs 6-4 were significant with a difference
# of -5.72 and a p-value of 2e-16. Type sports-sedan were significant with a
# p-value of 0.0117 and a difference of -2.81. There were 3 significant 
# interactions 6:Sedan-4:Sedan (p2e-16 dif -6.17), 4:Sports-4:Sedan (p = 0.00036
# diff -5.23), 6:Sports-4:Sedan (p=2.7e-6 diff -6.6). The fuel efficiency 
# across groups can be observed through the differences in values. A 4 cylinder 
# has better highway mpg compared to a 6 cylinder by about 6 mpg. A Sedan has
# better mpg by about 3 mpg. This can be further observed by the interactions of
# 6:Sedan-4:Sedan, 4:Sports-4:Sedan, and 6:Sports-4:Sedan. 



