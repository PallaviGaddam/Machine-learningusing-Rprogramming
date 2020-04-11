#polynomial regression
library(caTools)

#importing the dataset

polyreg=read.csv("~/Machine-learningusing-Rprogramming/Regression/data/Position_Salaries.csv")
polyreg=polyreg[2:3]

#fitting the linear regression model

linreg=lm(formula = Salary~.,
          data = polyreg)
summary(linreg)

#fitting the polynomial linear regression
polyreg$Level2=polyreg$Level^2
polyreg$Level3=polyreg$Level^3
polyreg$Level4=polyreg$Level^4

poly_reg=lm(formula = Salary~.,
            data = polyreg)
summary(poly_reg)

#visualizing the linear regression results
library(ggplot2)
ggplot()+
  geom_point(aes(x=polyreg$Level,y=polyreg$Salary),
             colour='red')+
  geom_line(aes(x=polyreg$Level,y=predict(linreg,newdata = polyreg)),
            colour='blue')+
  ggtitle('Truth or Bluff(linear regression)')+
  xlab('Level')+
  ylab('salary')

#visualizing the polyregression results
ggplot()+
  geom_point(aes(x=polyreg$Level,y=polyreg$Salary),
             colour='red')+
  geom_line(aes(x=polyreg$Level,y=predict(poly_reg,newdata = polyreg)),
            colour='blue')+
  ggtitle('Truth or Bluff(polynomial regression)')+
  xlab('Level')+
  ylab('salary')

#predicting the result with linear regression 
ypred_lin=predict(linreg,data.frame(Level=6.5))

#predicting the result with polynomial regression
ypred_polyreg=predict(poly_reg,data.frame(Level=6.5,
                                          Level2=6.5^2,
                                          Level3=6.5^3,
                                          Level4=6.5^4))
