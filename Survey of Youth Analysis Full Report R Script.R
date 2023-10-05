
library(psych)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(hrbrthemes)
library(janitor)
library(vtable)
library(jtools)

#Importing the dataset
df <- read.csv("NLYS97.csv")
df %>% 
  as.tibble()

describe(df)


df2<-df[!(df$CV_MARSTAT_==""|df$CV_MARSTAT_=="-3"|df$CV_MARSTAT_=="-4" | df$CV_MARSTAT_=="-5"),]
df2
# df2<-df[!(df$WAGE_=="-5"|df$WAGE_=="-4"|df$WAGE_=="-2"|df$WAGE_=="-1" | df$WAGE_=="0"),]
# df2
df2<-df2[!(df2$WAGE_<="3"),]
df2

#Data Cleaning

df$KEY_SEX<-as.factor(df$KEY_SEX)
df_c <-df2%>% 
  select(YEAR,KEY_SEX,KEY_RACE, CV_SAMPLE_TYPE,KEY_RACE_ETHNICITY,KEY_BDATE_Y,CV_MARSTAT_, CV_HH_POV_RATIO_,
         CV_HH_SIZE_,CVC_FIRST_MARRY_DATE_Y_XRND,CVC_MARRIAGES_TTL_XRND, CHILD_BIRTH_01_Y
         ,CV_BIO_CHILD_HH_,WAGE_,PARTNERS_UID_01_,PARTNERS_UID_02_,AGE_FIRST_CHILD,AGE_) %>% 
  filter(KEY_RACE %in% c("White","Black or African American","Asian or Pacific Islander","American Indian, Eskimo, or Aleut","Something else? (SPECIFY)")& AGE_) %>%
  na.omit()
df_c
# df_c$WAGE_<-aggregate(data=df_c, WAGE_~AGE_, FUN=mean)
# df_c$WAGE_
# 
# df_c$WAGE_ <- gsub('-4', '0', df_c$WAGE_)
# df_c$WAGE_ <- gsub('-5', '0', df_c$WAGE_)
# df_c$WAGE_ <- gsub('-2', '0', df_c$WAGE_)
# df_c$WAGE_<-aggregate(data=df_c, WAGE_~AGE_, FUN=mean)
# df_c$WAGE_


df_c %>% 
  as.tibble()

summary(df_c)
dim(df_c)
describe(df_c,skew = "FALSE")
#Table1: Descriptive Analysis
write.csv(describe(df_c),"Descriptive Table 1.csv")


#Table2- Groupby KEY_Race and Wage_ #Final# PRESENTATION
df_kr<-df_c%>%
  group_by(KEY_RACE)%>%
  summarise(mean=mean(WAGE_, na.rm = TRUE),
            median=median(WAGE_,na.rm = TRUE),
            sd=sd(WAGE_,na.rm = TRUE),
            iqr=IQR(WAGE_,na.rm = TRUE),
            min=min(WAGE_,na.rm = TRUE),
            max=max(WAGE_,na.rm = TRUE))
  
write.csv(df_kr,"KEY_Race and Wage_ Table 2.csv")

# Graph1: Bar Chart
df_c%>%
  ggplot(aes(KEY_SEX, color = "WAGE_"))+
  geom_bar(position = position_dodge(),fill="#69b3a2", width= 0.3)+
  ggtitle("SEX and WAGE")

#Table 3 Groupby SEX and  #Final#

df_y<-df_c%>%
  group_by(YEAR)%>%
  summarise(mean=mean(WAGE_, na.rm = TRUE),
            median=median(WAGE_,na.rm = TRUE),
            sd=sd(WAGE_,na.rm = TRUE),
            iqr=IQR(WAGE_,na.rm = TRUE),
            min=min(WAGE_,na.rm = TRUE),
            max=max(WAGE_,na.rm = TRUE))
write.csv(df_y,"Year vs Wage Table 3.csv")

# Graph2: Bar Chart
df_c%>%
  ggplot(aes(WAGE_, color = "YEAR"))+
  geom_bar(position = position_dodge())+
  ggtitle("YEAR and WAGE'S")+
  xlim(0,100)+
  ylim(0,125)

#Table 4 Groupby CV_MARSTAT_ and WAGE_ #Final#
df_c%>%
  group_by(CV_MARSTAT_)%>%
  summarise(mean=mean(WAGE_, na.rm = TRUE),
            median=median(WAGE_,na.rm = TRUE),
            sd=sd(WAGE_,na.rm = TRUE),
            iqr=IQR(WAGE_,na.rm = TRUE),
            min=min(WAGE_,na.rm = TRUE),
            max=max(WAGE_,na.rm = TRUE))
write.csv(df_mr,"CV_MARSTAT_ and WAGE_ Table 4.csv")

# Graph3: Bar Chart
df_c%>%
  ggplot(aes(CV_MARSTAT_, color = "WAGE_"))+
  geom_bar(position = position_dodge())+
  ggtitle("Martial Status vs Wage")+
  theme(axis.text.x = element_text(angle = 90))

#Creating new variable by transform
df_c$YEAR97_04 <- ifelse(df_c$YEAR>1997 & df_c$YEAR<2004, 1, 0)
df_c$YEAR05_13 <- ifelse(df_c$YEAR>2005 & df_c$AGE_<2013, 1, 0)


write.csv(describe(df_c),"Descriptive table with new variables Table 4.csv")

##Prepare train and test data

set_train<- sort(sample(x= nrow(df_c), size = nrow(df_c)*0.8))

# Descriptive table of train dataset
train_NLSY<-df_c[set_train,]
write.csv(describe(train_NLSY),"Descriptive table of train dataset Table 5.csv")

# Descriptive table of test dataset
test_NLSY<-df_c[-set_train,]
write.csv(describe(test_NLSY),"Descriptive table of test dataset Table 6.csv")

#applying correlation model
df_cor<-df_c[,c("WAGE_","YEAR","PARTNERS_UID_01_","PARTNERS_UID_02_","CV_HH_POV_RATIO_",
                "YEAR97_04","YEAR05_13")]
df_mtrx<-cor(df_cor)
df_mtrx
write.csv(describe(df_mtrx),"Correlation matrix Table 7.csv")

#Correctional plot

library(corrplot)
corrplot(cor(df_cor),method="circle", title = "\nCorrelation Matrix Graph")

#Linear Regression model
lm_model<- lm(formula = WAGE_~., data = train_NLSY)
summ(lm_model)
install.packages("olsrr")
library(olsrr)
#Stepwise Forward Selection Model
ols_step_forward_p(lm_model,penter = 0.05)

#Stepwise Backward Selection Model
ols_step_backward_p(lm_model,penter = 0.05)


lm_model1<- lm(formula = WAGE_~YEAR + CV_MARSTAT_+
                 YEAR97_04+YEAR05_13,data = train_NLSY)
summary(lm_model1)
summ(lm_model1)
WAGE_Pred <- predict(lm_model1, test_NLSY)
actuals_preds <- data.frame(cbind(actuals=test_NLSY$WAGE_, predicteds=WAGE_Pred))
actuals_preds

min_max_accuracy1 <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy1

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape
install.packages("DMwR")
library(DMwR2)
DMwR2::regr.eval(actuals_preds$actuals, actuals_preds$predicteds)

#===========Model2
lm_model2<- lm(formula = WAGE_~YEAR97_04,data = train_NLSY)
summ(lm_model2)
WAGE_Pred <- predict(lm_model2, test_NLSY)
actuals_preds <- data.frame(cbind(actuals=test_NLSY$WAGE_, predicteds=WAGE_Pred))
actuals_preds

min_max_accuracy2 <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy2

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape

#========Model3

lm_model3<- lm(formula = WAGE_~YEAR05_13,data = train_NLSY)
summ(lm_model3)
WAGE_Pred <- predict(lm_model2, test_NLSY)
actuals_preds <- data.frame(cbind(actuals=test_NLSY$WAGE_, predicteds=WAGE_Pred))
actuals_preds

min_max_accuracy3 <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy3

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape

#=======Model4
lm_model3<- lm(formula = WAGE_~CV_MARSTAT_,data = train_NLSY)
summ(lm_model3)
WAGE_Pred <- predict(lm_model2, test_NLSY)
actuals_preds <- data.frame(cbind(actuals=test_NLSY$WAGE_, predicteds=WAGE_Pred))
actuals_preds

min_max_accuracy3 <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy3

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape




==================================================








