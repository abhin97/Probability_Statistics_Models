
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

#Data Cleaning

df$KEY_SEX<-as.factor(df$KEY_SEX)

df_c <-df%>% 
  select(YEAR,KEY_SEX,KEY_RACE, CV_SAMPLE_TYPE,KEY_RACE_ETHNICITY,KEY_BDATE_Y,CV_MARSTAT_, CV_HH_POV_RATIO_,
         CV_HH_SIZE_,CVC_FIRST_MARRY_DATE_Y_XRND,CVC_MARRIAGES_TTL_XRND, CHILD_BIRTH_01_Y
         ,CV_BIO_CHILD_HH_,WAGE_,PARTNERS_UID_01_,PARTNERS_UID_02_,AGE_FIRST_CHILD,AGE_) %>% 
  filter(KEY_RACE %in% c("White","Black or African American","Asian or Pacific Islander","American Indian, Eskimo, or Aleut","Something else? (SPECIFY)")& AGE_) %>%
  na.omit()

df_c %>% 
  as.tibble()

summary(df_c)
dim(df_c)
describe(df_c,skew = "FALSE")
#Table1: Descriptive Analysis
write.csv(describe(df_c),"Descriptive Table 1.csv")


#Table2- Groupby KEY_Race and Wage_ #Final#
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
  ggplot(aes(KEY_RACE, color = "WAGE_"))+
  geom_bar(position = position_dodge())+
  ggtitle("RACE and WAGE")

#Table 3 Groupby Year and AGE #Final#

df_y<-df_c%>%
  group_by(YEAR)%>%
  summarise(mean=mean(AGE_, na.rm = TRUE),
            median=median(AGE_,na.rm = TRUE),
            sd=sd(AGE_,na.rm = TRUE),
            iqr=IQR(AGE_,na.rm = TRUE),
            min=min(AGE_,na.rm = TRUE),
            max=max(AGE_,na.rm = TRUE))
write.csv(df_y,"Year vs Age Table 3.csv")

# Graph2: Bar Chart
df_c%>%
  ggplot(aes(AGE_, color = "YEAR"))+
  geom_bar(position = position_dodge())+
  ggtitle("YEAR and AGE")

#Table 4 Groupby CV_MARSTAT_ and WAGE_ #Final#
df_mr<-df_c%>%
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
df_c$teen <- ifelse(df_c$AGE_>12 & df_c$AGE_<19, 1, 0)
df_c$youth <- ifelse(df_c$AGE_>19 & df_c$AGE_<25, 1, 0)
df_c$adult <- ifelse(df_c$AGE_>25 & df_c$AGE_<34, 1, 0)

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
df_cor<-df_c[,c("WAGE_","YEAR", "AGE_","PARTNERS_UID_01_","PARTNERS_UID_02_", "CV_HH_POV_RATIO_")]
df_mtrx<-cor(df_cor)
df_mtrx
write.csv(describe(df_mtrx),"Correlation matrix Table 7.csv")

#Correctional plot

library(corrplot)
corrplot(cor(df_cor),method="circle", title = "\nCorrelation Matrix Graph")

#Linear Regression model

lm_model1<- lm(formula = YEAR~AGE_+CV_SAMPLE_TYPE, data = train_NLSY)
summary(lm_model1)
summ(lm_model1)
lm_model1 %>% 
  ggplot(aes(x=YEAR, y=AGE_))+
  geom_point()
  
lm_model1 %>% 
  ggplot(aes(x=YEAR, y=AGE_))+
  geom_point()+
  geom_smooth(method="lm", col="green")+
  ggtitle(" Plot : Linear Regression Model of NLSY")

#++++++++++++++++++++++++++++++++++++++++++++++++++++To be continued in final Project Submission++++++



