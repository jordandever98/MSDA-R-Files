library(tidyverse)
college <- read_csv("~/R-Studio/Predictive Modeling/ALL CSV FILES - 2nd Edition/College.csv")
View(college)
summary(college)
college$Private = as.factor(college$Private)
pairs(college[,1:10])

plot(college$Private, college$Outstate)

Elite=rep("No",nrow(college))
Elite[college$Top10perc >50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college, Elite)

summary(college$Elite)

plot(college$Elite, college$Outstate)

par(mfrow = c(2, 2))
hist(college$Apps)
hist(college$Grad.Rate)
hist(college$S.F.Ratio)
hist(college$Expend)


auto = read_csv("~/R-Studio/Predictive Modeling/ALL CSV FILES - 2nd Edition/Auto.csv")
auto = na.omit(auto)
summary(auto)
# everything is numeric EXCEPT horsepower and name according to the summary
auto$horsepower = as.numeric(auto$horsepower)
auto = na.omit(auto)
sapply(auto[, 1:7], range)
#mpg cylinders displacement horsepower weight acceleration year
#[1,]  9.0         3           68         46   1613          8.0   70
#[2,] 46.6         8          455        230   5140         24.8   82
sapply(auto[, 1:7], mean)
#mpg    cylinders displacement   horsepower       weight acceleration         year 
#23.445918     5.471939   194.411990   104.469388  2977.584184    15.541327    75.979592 
sapply(auto[, 1:7], sd)
#mpg    cylinders displacement   horsepower       weight acceleration         year 
#7.805007     1.705783   104.644004    38.491160   849.402560     2.758864     3.683737 

auto2 <- auto[-c(10:85), ]
sapply(auto2[, 1:7], range)
#mpg cylinders displacement horsepower weight acceleration year
#[1,] 11.0         3           68         46   1649          8.5   70
#[2,] 46.6         8          455        230   4997         24.8   82
sapply(auto2[, 1:7], mean)
#mpg    cylinders displacement   horsepower       weight acceleration         year 
#24.404430     5.373418   187.240506   100.721519  2935.971519    15.726899    77.145570 
sapply(auto2[, 1:7], sd)
#mpg    cylinders displacement   horsepower       weight acceleration         year 
#7.867283     1.654179    99.678367    35.708853   811.300208     2.693721     3.106217 

par(mfrow = c(1, 1))
plot(auto$weight, auto$acceleration)
#Slight correlation that as weight increases, acceleration decreases
plot(auto$weight, auto$mpg)
#Collelation that as weight increases, miles per gallon decreases
# Yes, weight seems to be a good predictor on mpg because there is a downward trend in mpg
# especially from 3500+ in weight, the mpg goes from ~20 to ~10.

#install.packages("ISLR2")
library(ISLR2)
boston = ISLR2::Boston
dim(boston)
?ISLR2::Boston
#506 rows, 13 columns which means 506 houses listed with 13 variables to help predict housing value

pairs(boston)
#There is a lot going on in the pairwise scatterplots but some plots that look to be correlated at first glance:
# zn & crim, indus & nox, lstat & medv, rad & tax and possibily more just hard to see with mark I eyeballs

par(mfrow = c(2, 2))
plot(boston$tax, boston$crim)
plot(boston$ptratio, boston$crim)
plot(boston$medv, boston$crim)
plot(boston$rm, boston$crim)

# It seems that more crime occurs in the higher tax range (~650 to be specific)
# More crime seems to occur when the pupil-teacher ratio is ~20.
# Most crime occurs in the lower median value range of around ~10000
# More crime around 4-6 room houses.

par(mfrow=c(1,1))

hist(boston$crim,breaks=50)
range(boston$crim)
# Most places have low crim, but the value trails all the way up to ~88 per capita crime rate by town
hist(boston$tax)
range(boston$tax)
#Most places have a tax range between ~200-400, there there is a cap from 400-600, then a big jump in
# frequency occurs at the 700 tax range
hist(boston$ptratio)
range(boston$ptratio)
# There is a very high frequency on the ~20 pupil-teacher ratio which could be influencing our prior
# look at crime vs ptratio.

dim(subset(Boston, chas == 1))
# 35 bound the Charles River

median(Boston$ptratio)
#19.05

boston[boston$medv == min(boston$medv), ]
summary(boston)
# Compared to our ranges, crime is in the 3rd quartile, indus is in 3rd quartile,
# nox is in the 3rd quartile, rm is below the first quartile, age is max, 
# dis is below 1st quartile, rad is max, tax is in the 3rd quartile, ptratio is in the 3rd quartile
# and lstat is in the 3rd quartile.

dim(subset(boston, rm > 7))
dim(subset(boston, rm > 8))
# 64 houses with 7+ rooms, and 13 with 8+ rooms.
summary((subset(boston, rm > 8)))
# Crime is very low with 75% of 8 room homes being less than 1 per capita crime rate
# and the median home value is between 21-50, with a mean of 44!
