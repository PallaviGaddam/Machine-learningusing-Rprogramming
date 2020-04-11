#multiple linear regression

#importing the dataset
multiplereg=read.csv("~/Machine-learningusing-Rprogramming/Regression/data/50_Startups.csv")

#encoding the categorical variable
multiplereg$State=factor(multiplereg$State,
                         levels = c('New York','California','Florida'),
                         labels = c(1,2,3))

#splitting the dataset
library(caTools)
set.seed(123)
split=sample.split(multiplereg$Profit,SplitRatio = 0.8)
multi_trainset=subset(multiplereg,split==TRUE)
multi_testset=subset(multiplereg,split==FALSE)

#fitting the model to train set
multiregressor=lm(formula = Profit~.,
                  data = multi_trainset)
summary(multiregressor)

#predicting the test results

y_pred=predict(multiregressor,newdata = multi_testset)
y_pred


#building the optimal model using backward elimination
multiregressor=lm(formula = Profit~R.D.Spend+Administration+Marketing.Spend+State,
                  data = multiplereg)
summary(multiregressor)

multiregressor=lm(formula = Profit~R.D.Spend+Administration+Marketing.Spend,
                  data = multiplereg)
summary(multiregressor)

multiregressor=lm(formula = Profit~R.D.Spend+Marketing.Spend,
                  data = multiplereg)
summary(multiregressor)
