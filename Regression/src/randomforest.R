#Decision Tree Regression
library(caTools)

#importing the dataset

randomforest=read.csv("~/Machine-learningusing-Rprogramming/Regression/data/Position_Salaries.csv")
randomforest=randomforest[2:3]

#fitting the Decision Tree Regression
#install.packages('randomForest')
library(randomForest)
set.seed(1234)
randomregressor=randomForest(x=randomforest[1],
                             y=randomforest$Salary,
                             ntree = 500)
summary(randomregressor)

#predicting the result with Random Tree Regression
ypred_dt=predict(randomregressor,data.frame(Level=6.5))

#visualizing the Random Tree Regression results
library(ggplot2)
xgrid=seq(min(randomforest$Level),max(randomforest$Level),0.01)
ggplot()+
  geom_point(aes(x=randomforest$Level,y=randomforest$Salary),
             colour='red')+
  geom_line(aes(x=xgrid,y=predict(randomregressor,newdata = data.frame(Level=xgrid))),
            colour='blue')+
  ggtitle('Truth or Bluff(Random Tree Regression)')+
  xlab('Level')+
  ylab('salary')




