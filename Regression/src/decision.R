#Decision Tree Regression
library(caTools)

#importing the dataset

decision=read.csv("~/Machine-learningusing-Rprogramming/Regression/data/Position_Salaries.csv")
decision=decision[2:3]

#fitting the Decision Tree Regression
#install.packages('rpart')
library(rpart)
dtregressor=rpart(formula=Salary~.,
                  data = decision,
                  control = rpart.control(minsplit = 1))
summary(dtregressor)

#predicting the result with Decision Tree Regression
ypred_dt=predict(dtregressor,data.frame(Level=6.5))

#visualizing the Decision Tree Regression results
library(ggplot2)
xgrid=seq(min(decision$Level),max(decision$Level),0.1)
ggplot()+
  geom_point(aes(x=decision$Level,y=decision$Salary),
             colour='red')+
  geom_line(aes(x=xgrid,y=predict(dtregressor,newdata = data.frame(Level=xgrid))),
            colour='blue')+
  ggtitle('Truth or Bluff(Decision Tree Regression)')+
  xlab('Level')+
  ylab('salary')




