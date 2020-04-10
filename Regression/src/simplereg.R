#simple linear regression

#importing the data set

simplereg <- read.csv("~/Machine-learningusing-Rprogramming/Regression/data/Salary_Data.csv")
#splitting the dataset
library(caTools)
set.seed(123)
split=sample.split(simplereg$Salary,SplitRatio = 2/3)
trainset=subset(simplereg,split==TRUE)
testset=subset(simplereg,split==FALSE)

#fitting the model to train set
regressor=lm(formula = Salary~YearsExperience,
             data = trainset)
summary(regressor)


#predicting the test set results
y_pred=predict(regressor,newdata = testset)
y_pred

#visualising the training set
library(ggplot2)
ggplot()+
  geom_point(aes(x=trainset$YearsExperience,y=trainset$Salary),
             colour='red')+
  geom_line(aes(x=trainset$YearsExperience,y=predict(regressor,newdata = trainset)),
            colour='blue')+
  ggtitle('Salary vs Experience(trainset)')+
  xlab('years of experience')+
  ylab('salary')

#visualising the test set
library(ggplot2)
ggplot()+
  geom_point(aes(x=testset$YearsExperience,y=testset$Salary),
             colour='red')+
  geom_line(aes(x=trainset$YearsExperience,y=predict(regressor,newdata = trainset)),
            colour='blue')+
  ggtitle('Salary vs Experience(trainset)')+
  xlab('years of experience')+
  ylab('salary')
