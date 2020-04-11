#SVR

library(caTools)

#importing the dataset

svr_reg=read.csv("~/Machine-learningusing-Rprogramming/Regression/data/Position_Salaries.csv")
svr_reg=svr_reg[2:3]

#fitting the SVR model
#install.packages('e1071')
library(e1071)
svrregressor=svm(formula=Salary~.,
                 data = svr_reg,
                 type='eps-regression')

#predicting the result with support vector regression 
ypred_svr=predict(svrregressor,data.frame(Level=6.5))

#visualizing the support vector regression results
library(ggplot2)
ggplot()+
  geom_point(aes(x=svr_reg$Level,y=svr_reg$Salary),
             colour='red')+
  geom_line(aes(x=svr_reg$Level,y=predict(svrregressor,newdata = svr_reg)),
            colour='blue')+
  ggtitle('Truth or Bluff(SVR)')+
  xlab('Level')+
  ylab('salary')




