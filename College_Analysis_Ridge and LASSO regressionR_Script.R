library(ISLR)
library(psych)
library(ggplot2)
library(car)
library(gmodels)
library(hrbrthemes)
library(caret)
library(gridExtra)
library(pROC)
library(InformationValue)
library(e1071)
library(glmnet)

# Imported the dataset from ISLR library
data("College")
write.csv(College,"College.csv")
College
head(College)
describe(College, skew = "F")
write.csv(describe(College, skew = "F"),"Descriptive Analysis . Table 1.csv")
summary(College)
describe(College)
nrow(College)
ncol(College)

#Splitting the dataset into train and test set

set.seed(123)
set_train<- sort(sample(x= nrow(College), size = nrow(College)*0.8))

train_clg<-College[set_train,]
describe(train_clg, skew = 'F')
write.csv(describe(train_clg, skew = 'F'),"Descriptive Analysis train. Table 2.csv")
test_clg<-College[-set_train,]
describe(test_clg, skew = "F")
write.csv(describe(train_clg, skew = 'F'),"Descriptive Analysis test. Table 3.csv")

train_x<- model.matrix(F.Undergrad~.,train_clg)[,-1]
test_x<- model.matrix(F.Undergrad~., test_clg)[,-1]

train_y<- train_clg$F.Undergrad
test_y<- test_clg$F.Undergrad

#Lamda min and Lambda 1se

cv.lambda<-cv.glmnet(train_x,train_y, nfolds = 10)

log(cv.lambda$lambda.min)
log(cv.lambda$lambda.1se)

plot(cv.lambda, main= "Lambda Curve\n")

# Ridge Regression

Mridge.min1 <- glmnet(train_x, train_y, alpha = 0, lambda = cv.lambda$lambda.min)
Mridge.min1

coef(Mridge.min1)

Mridge.1se <- glmnet(train_x, train_y, alpha = 0, lambda = cv.lambda$lambda.1se)
Mridge.1se

coef(Mridge.1se)

#Performance
library(Metrics)
predict1.train <- predict(Mridge.min1, newx = train_x)
train1.RSME <- rmse(train_y,predict1.train)
train1.RSME

predict1.test <- predict(Mridge.min1, newx = test_x)
test1.RSME <- rmse(test_y,predict1.test)
test1.RSME

rmse(train1.RSME,test1.RSME)

sqrt(mean((train1.RSME - test1.RSME)^2))


#Lasso Regression

Mlasso.min1 <- glmnet(train_x, train_y, alpha = 1, lambda = cv.lambda$lambda.min)
Mlasso.min1

coef(Mlasso.min1)

Mlasso.1se <- glmnet(train_x, train_y, alpha = 1, lambda = cv.lambda$lambda.1se)
Mlasso.1se

coef(Mlasso.1se)

#Performance
library(Metrics)
predict2.train <- predict(Mlasso.min1, newx = train_x)
train2.RSME <- rmse(train_y,predict2.train)
train2.RSME

predict2.test <- predict(Mlasso.min1, newx = test_x)
test2.RSME <- rmse(test_y,predict2.test)
test2.RSME

rmse(train2.RSME,test2.RSME)

sqrt(mean((train2.RSME - test2.RSME)^2))

#==================================================

fit_model <- lm(College$F.Undergrad ~ ., data=College)
summ(fit_model)

library(MASS)
step <- stepAIC(fit_model, direction = "both")
summ(step)

library(leaps)
leaps <- regsubsets(College$F.Undergrad ~ ., data=College,nbest = 3)
plot(leaps,scale="adjr2")

#===============================Finsh=====================================

