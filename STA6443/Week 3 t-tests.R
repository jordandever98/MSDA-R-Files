library(ggplot2)
water = read.table('WATER.txt', header=TRUE)
str(water)
summary(water)

#one pop test
#comparing water data of hardness vs mortality
#H0 = mean = 1500
#HA mean != 1500
summary(water$mortal)
#mean = 1524
hist(water$mortal)
boxplot(water$mortal)
qqnorm(water$mortal); qqline(water$mortal)
sharipo.test(water$mortal)
#ttest
t.test(water$mortal, mu=1500)
?t.test



summary(water$hardness)
hist(water$hardness)
boxplot(water$hardness)
qqnorm(water$hardness); qqline(water$hardness, col = 'red')
shapiro.test(water$hardness)

install.packages(BSDA)
library(BSDA)
SIGN.test(water$hardness)




#two pop comparison
#check normality
### Mortal
shapiro.test(water$mortal [water$location=='S'])
shapiro.test(water$mortal [water$location=='N'])
#both follow normal
#check variance
var.test(mortal ~ location, water, alternative = 'two.sided')
bartlett.test(mortal~location,water)
#cannot reject null, equal var
t.test(mortal ~ location, data=water,alternative='two.sided', var.equal = TRUE)


shapiro.test(water$hardness [water$location=='S'])
#fail to reject, normal
shapiro.test(water$hardness [water$location=='N'])
#reject, not normal
wilcox.test(hardness ~ location, data=water, exact = F)
#reject, not equal





#var.test(hardness ~ location,water,alternative='two.sided')
#reject, not equal variance
#t.test(hardness ~ location, data=water, alternative='two.side', var.equal = FALSE)
#reject, unequal means





load('Research.RData')
view(research)
boxplot(salary~field, data=research)
wilcox.test(salary ~ field, data=research, exact=T)


log.salary = log(research$salary)
research = cbind(research, log.salary)
boxplot(log.salary~field, data=research)
shapiro.test(research$log.salary[research$field=='Ap'])
shapiro.test(research$log.salary[research$field=='Th'])
#fail to reject both
var.test(log.salary ~ field, data = research, alternative = 'two.sided')
#fail to reject, equal
t.test(log.salary ~ field, data = research, alternative = 'two.sided', var.equal = T)
#reject, different salary levels
