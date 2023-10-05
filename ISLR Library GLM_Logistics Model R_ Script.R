library(ISLR)
library(psych)
library(ggplot2)
library(car)
library(hrbrthemes)
library(caret)
library(gridExtra)
library(pROC)
library(InformationValue)
library(e1071)

data("College")
write.csv(College,"College.csv")
College
head(College)
describe(College, skew = "F")
summary(College)
describe(College)
nrow(College)
ncol(College)

unique(College$Private)
head(College)
College$Private<-ifelse(College$Private=="Yes",1,0)
str(College)
unique(College$Private)
#Plot of Application accepted in private college
ggplot(College, aes(x= Accept, y= F.Undergrad, fill = Private, color = Private ))+
  geom_point()+
  ggtitle("Scatter Plot:Apps. accepted in Private Univ.")+
  theme_ipsum()+
  stat_smooth()

#Hist for application accepted in private college 
ggplot(College, aes(x=Accept, fill= Private, binwidth=30)) + 
  geom_histogram(position = position_dodge())+
  scale_fill_manual(values=c("#69b3a2", "#404080"))+
  labs(fill="")+
  ggtitle("Histogram: Number of Applications in Uni.")+
  xlim(0,5000)

ggplot(College, aes(x=F.Undergrad, fill= Private, binwidth=30)) + 
  geom_histogram(position = position_dodge())+
  scale_fill_manual(values=c("#69b3a2", "#404080"))+
  labs(fill="")+
  ggtitle("Histogram: Application Accepted")+
  xlim(0,5000)



#Boxplot for application accepted in private college 
ggplot(College, aes(x= Accept, y= Outstate, fill = Private, color = Private ))+
  geom_boxplot()+
  ggtitle("Box plot Apps. accepted in Private Univ.")+
  theme_ipsum()


#Creating Test and Train Dataset
set.seed(1)
set_train<- sort(sample(x= nrow(College), size = nrow(College)*0.7))

train_clg<-College[set_train,]
describe(train_clg, skew = 'F')
test_clg<-College[-set_train,]
describe(test_clg, skew = "F")

model_logst1<- glm(Private~ . , data = train_clg, family = binomial(link = "logit"))
summary(model_logst1)

model_logst2<- glm(Private~ Accept + F.Undergrad  + Outstate, data = train_clg, family = binomial(link = "logit"))
summary(model_logs2)

coef(model_logst2)
exp(coef(model_logst2))

log_prob <- predict(model_logst2, newdata=test_clg, type="response")
test_clg$predict <- ifelse(test_clg$Private=="Yes", 1, 0)
optimal <- optimalCutoff(test_clg$predict, log_prob)[1]
confusionMatrix(test_clg$Private, log_prob)



#To check the accuracy used formula
true_positive = 64
true_negative = 155
false_positive = 8
false_negative = 7

Accuracy = (true_negative + true_positive)/(true_negative+false_positive+false_negative+true_positive)
Accuracy


Precision = true_positive/(false_positive+true_positive)
Precision

Recall = true_positive/(true_positive+false_negative)
Recall

Specificity = true_negative/(true_negative+false_positive)
Specificity

receiver_op= roc(test_clg$Private, log_prob)
plot(receiver_op, col = "Green", ylab = "Sensitivity = True Positive Rate", xlab = 'Specificity = Fale Positive Rate')

auc = auc(receiver_op)
auc
