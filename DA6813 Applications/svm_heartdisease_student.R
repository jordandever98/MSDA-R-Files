rm(list=ls())
# Install necessary packages.
install.packages('e1071')

# Load the libraries
library(e1071)

# Here, we will use a heart disease data to show how to tune and estimate support vector machine (SVM).
# Load the heart_tidy datafile (csv). For you I have uploaded the datafile to Blackboard, under Data folder. 
heart <- read.csv("~/R-Studio/DA6813 Applications/heart_tidy.csv", header = FALSE)



# To see structure of data
str(heart)

# To see first few observations
head(heart)


# check for missing values
anyNA(heart)


# Splitting data into training and testing sets
set.seed(200)
tr_ind = sample(nrow(heart), 0.7 * nrow(heart), replace = F) #Sample 70% of heart
heart_train = heart[tr_ind,]
heart_test = heart[-tr_ind,]
# I am using a 70/30 split




# Here I am declaring the final variable (dependent variable) as a factor, i.e. 0 or 1, based on 
# the presence or absence of the heart disease.
heart_train[['V14']] = factor(heart_train[['V14']])


# Let's generate a formula. I will simply use a model that considers all remaining variables as independent.
form1 = V14 ~ .


# Next we need to set gamma and cost if we use the RBF kernel. For this we can use the function 
# tune.svm from the package e1071. Caution: Depending on your system, iterating different values 
# using seq() function can take a long time depending on your system. 
# In this demo, we will use gamma ranging from 0.01 to 0.1 in the increments of 0.01. This will give us 
# 10 values of gamma in the following sequence {0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.10}.
# We will also iterate cost from 0.1 to 1 in increments of 0.1. This will give us 10 values for cost as 
# follows {0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0}.
# So, tune.svm will learn SVMs for 10x10 = 100 possible combinations of gamma and cost. Internally, 
# tune.svm also uses a 10-fold cross validation to get classification error. Thus, for 100 combinations 
# it will actually learn 10x100 = 1000 SVMs. Therefore, it takes a long time to execute this function.
# I am going to assign the output from tune.svm to an object called tuned:
tuned = tune.svm(form1, data= heart_train, gamma = seq(0.01, .1, by = 0.01),
                 cost = seq(0.1, 1, by = 0.1)) #, Scale = True will scale/standardize everything for us


# "tuned is a large list. It has a lot of output but at this point we are interested in knowing which 
# parameter values for gamma and cost are the best. Note the best is within the 100 combinations that 
# we decided to use for tuning. It doesn't actually tell us the best parameter set globally.

tuned$best.parameters

# For this example and the range of tuning parameters, we get the best value of gamma = 
# "tune$best.parameters$gamma" and the best value of cost = "tune$best.parameters$cost".

# To see the errors for different combinations you can take a look at the following output:
tuned$performances


# You can scroll and see that for the values of gamma = "tune$best.parameters$gamma" and cost = 
# "tune$best.parameters$cost" the error is the lowest at "tune$best.performance".

# Finally, using the values of the best parameters, let's run SVM. Note that since I am using RBF kernel, 
# the parameters of SVM aren't useful at all. Instead the prediction will be done using support vectors. 
# It's the observations and not the parameters associated with the variables that are used in prediction. 
# This is quite a departure from the traditional parametric methods such as linear and logistic regression.
mysvm = svm(form1, data = heart_train, gamma = tuned$best.parameters$gamma, 
            cost = tuned$best.parameters$cost)
summary(mysvm)

# The syntax for SVM from the package e1071 is as follows for the default kernel, which is radial basis 
# function (RBF): svm(formula = , data = , gamma =, cost =)

svm_pred = predict(mysvm, heart_test, type = 'response')
table(pred = svm_pred, test = heart_test$V14)

# I am assigning the output of SVM to an object called mysvm. I am also printing the summary of mysvm which 
# will give some basic information. Take a note of the number of support vectors.

# Rather than manually putting the best value of gamma and cost, I am simply using the values from the "tuned" 
# object. In case you are wondering how I found it out, go to the top right window in RStudio (Environment 
# tab) and navigate to "tuned" object. Click on the blue icon next to it to expand "tune" and you will see 
# where I got the names of the best parameters.


# Finally, using this model, let's classify our test data and get the confusion matrix.



# As an exercise you should compare this output to logistic regression and see which model performs better. 
# You can also tweak SVM by using different kernels (linear, polynomial, or Sigmoid). Note that the parameters 
# for these kernels will be different from RBF. You will find this in the documentation for e1071: 
# https://cran.r-project.org/web/packages/e1071/e1071.pdf
