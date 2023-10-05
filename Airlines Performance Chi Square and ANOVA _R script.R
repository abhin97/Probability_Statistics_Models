
#QUES 1::11.1-6 : Problem Statement : Blood Types
#Set significance level
aplha <- 0.10

#Create a vector of the values
observed <- c(12,8,24,6)

#Create a vector for the Probability
p <- c(0.20,0.28,0.36,0.16)
critical_value<-qchisq(p=0.10, df=3, lower.tail=FALSE)
result<- chisq.test(x= observed, p=p)
result

ifelse(result$statistic>critical_value, "Null hypothesis rejected", "Null hypothesis accepted")

#===============================================================================
#QUES 2::11.1-7Problem Statement 2: On Time Performance by Airlines
#Set significance level
aplha <- 0.05
#Create a vector of the values
observed <- c(125,40,10,25)
#Create a vector for the Probability
p <- c(.708,.082,.09,.12)
critical_value<-qchisq(p=1.357e-08, df=3, lower.tail=FALSE)
result<- chisq.test(x= observed, p=p)
result
critical_value<-qchisq(p=1.357e-08, df=3, lower.tail=FALSE)
ifelse(result$statistic<critical_value, "Null hypothesis rejected", "Null hypothesis accepted")


#===============================================================================
#QUES 3::Problem od Ethnicity and Movie Admissions
#2014 Vector
#Set significance level
aplha <- 0.05
#Create a vector of the values
observed <- matrix(c(724,370,335,292,174,152,107,140), nrow= 2)
observed
row.names(observed)<- c("2013", "2014")
colnames(observed)<- c("Caucasian","Hispanic","African Americal","Other")
result<- chisq.test(x= observed)
result
critical_value<-qchisq(p=5.478e-13, df=3, lower.tail=FALSE)
ifelse(result$statistic>critical_value, "Movie depends on Ethnicity", "Movie does'nt depend on ethnicity")


#===============================================================================
#QUES 4::Women in the Military
observed <- matrix(c(10791,7816,932,11819))
r1<-c(10791,62419)
r2<-c(7816,42750)
r3<-c(932,9525)
r4<-c(11819,54344)

rows=4

matrix<- matrix(c(r1,r2,r3,r4),nrow=rows,byrow=TRUE)
row.names(matrix)<-c("Army","Navy","Marine Corps","Air Force")
colnames(matrix)<-c("Officers","Enlisted")
matrix
result<-chisq.test(matrix)
result
critical_value<-qchisq(p=2.2e-16, df=3, lower.tail=FALSE)
ifelse(result$statistic>critical_value, "Relation-Rank and Branch", "No Relation-Rank and Branch")


#===============================================================================

#QUES 5::Problem 8 Condiments and cereals and desserts Sodium
alpha <- 0.05
library(psych)
#Dataset for Condiments 
df_Condiments<- data.frame('Sodium' = c (270,130,230,180,80,70,200), 'food' = rep('Condiments',7),stringsAsFactors = FALSE)
describe(df_Condiments)

#Dataset for Cereals 
df_Cerals<- data.frame('Sodium' = c (260,220,290,290,200,320,140), 'food' = rep('Cereals',7),stringsAsFactors = FALSE)
#Dataset for Desserts 
df_Desserts<- data.frame('Sodium' = c (100,180,250,250,300,360,300,160), 'food' = rep('Desserts',8),stringsAsFactors = FALSE)
#Merge all set in one table
Sodium<- rbind(df_Condiments,df_Cerals,df_Desserts)
Sodium$food<- as.factor(Sodium$food)
anova <- aov(Sodium ~ food, data = Sodium)
summary<-summary(anova)
summary
df1<-summary[[1]][1,"Df"]
df1
df2<-summary[[1]][2,"Df"]
df2
critical<- qf(p=0.05, df1, df2, lower.tail=TRUE)
F_test<-summary[[1]][[1,"F value"]]
ifelse(F_test>critical, "Difference in Mean", "Same Mean")


#===============================================================================



#QUES 6::Problem 19 Sales for leading COmpanies
alpha <- 0.01

#Dataset for Condiments 
df_Cereal1<- data.frame('Sales' = c (578,320,264,249,237), 'prod' = rep('Cereal',5),stringsAsFactors = FALSE)


#Dataset for Cereals 
df_ChocoCandy<- data.frame('Sales' = c (311,106,109,125,173), 'prod' = rep('ChocoCandy',5),stringsAsFactors = FALSE)

#Dataset for Desserts 
df_Coffee<- data.frame('Sales' = c (261,185,302,689), 'prod' = rep('Coffee',4),stringsAsFactors = FALSE)

#Merge all set in one table
Sales<- rbind(df_Cereal1,df_ChocoCandy,df_Coffee)
Sales$prod<- as.factor(Sales$prod)
anova <- aov(Sales ~ prod, data = Sales)
summary<-summary(anova)
summary
df1<-summary[[1]][1,"Df"]
df1
df2<-summary[[1]][2,"Df"]
df2
critical<- qf(p=0.01, df1, df2, lower.tail=TRUE)
F_test<-summary[[1]][[1,"F value"]]
ifelse(F_test>critical, "Difference in Mean", "Same Mean")
#performing the Turkey Test
TukeyHSD(anova)

#===============================================================================
#QUES 7::Problem 12 Sales for leading Companies
alpha <- 0.05

#Dataset for Condiments 
df_east<- data.frame('expense' = c (4946,5953,6202,7243,6113), 'country' = rep('east',5),stringsAsFactors = FALSE)


#Dataset for Cereals 
df_middle<- data.frame('expense' = c (6149,7451,6000,6479), 'country' = rep('middle',4),stringsAsFactors = FALSE)

#Dataset for Desserts 
df_west<- data.frame('expense' = c (5282,8605,6528,6911), 'country' = rep('west',4),stringsAsFactors = FALSE)

#Merge all set in one tabl
expense<- rbind(df_east,df_middle,df_west)
expense$country<- as.factor(expense$country)
anova <- aov(expense ~ country, data = expense)
summary(anova)
summary<-summary(anova)
summary
df1<-summary[[1]][1,"Df"]
df1
df2<-summary[[1]][2,"Df"]
df2
critical<- qf(p=0.05, df1, df2, lower.tail=TRUE)
F_test<-summary[[1]][[1,"F value"]]
ifelse(F_test>critical, "Difference in Mean", "Same Mean")
TukeyHSD(anova)

#===============================================================================
#QUES 8::Anova test for growth of plants
observed <-c(9.2,9.4,8.9,8.5,9.2,8.9,7.1,7.2,8.5,5.5,5.8,7.6)
PF1_light<-c(1,1,1,2,2,2,1,1,1,2,2,2)
PF2_food<-c(1,1,1,1,1,1,2,2,2,2,2,2)

df_height<-data.frame(observed,PF1_light,PF2_food)
df_height

anova<-aov(observed~PF1_light+PF2_food+PF1_light:PF2_food,data = df_height)
summary(anova)
summary<-summary(anova)
df1<-summary[[1]][1,"Df"]
df1
df2<-summary[[1]][2,"Df"]
df2
critical<- qf(p=0.05, df1, df2, lower.tail=TRUE)
F_test<-summary[[1]][[1,"F value"]]
ifelse(F_test>critical, "Difference in light and food", "No diffrence in light and food")



#===============================================================================
#                           BASEBALL DATASET
#===============================================================================
library(dplyr)
library(ggplot2)
library(psych)
df_bb<- read.csv("baseball.csv")
df_bb

describe(df_bb,skew = FALSE)
summary(df_bb)

df_bb %>% 
  ggplot(aes(x=Year))+
  geom_histogram(fill="#69b3a2", color="#e9ecef", binwidth = 0.5)+
  ggtitle("Histogram for Year")

df_bb %>% 
  ggplot(aes(x=W))+
  geom_histogram(fill="#69b3a2", color="#e9ecef", binwidth = 1)+
  ggtitle("Histogram for Wins")

df_bb %>% 
  ggplot(aes(x=Year, y=W))+
  geom_boxplot(fill="PINK")+
  ggtitle("Boxplot for Wins and Year")

# Extract decade from year 
df_bb$Decade<-df_bb$Year - (df_bb$Year %% 10)
df_bb$Decade
# Create a wins table by summing the wins by decade 
df_win<-aggregate(df_bb$W,by = list(Decade = df_bb$Decade),FUN = sum) %>% 
  as_tibble()

critical<-qchisq(p=0.05,5,lower.tail = T)

df_chitest<-chisq.test(df_win)
df_chitest

ifelse(df_chitest$statistic>critical_value_baseball,"Reject Null Hypothesis","Reject alternative hypothesis")




#===============================================================================
#                               Crop_Data
#===============================================================================
df_cd<-read.csv("crop_data.csv")
df_cd()
describe(df_cd)
unique(df_cd)

df_cd<-data.frame("yeild" = df_cd$yield,
                 "Density" = df_cd$density,
                 "Fertilizer" = df_cd$fertilizer)

anova<-aov(yeild ~ Fertilizer + Density + Fertilizer:Density,data = df_cd)
summary(anova)
summary_cd<-summary(anova)

alpha_crop<-0.05

df1<-summary_cd[[1]][1,"Df"]
df2<-summary_cd[[1]][2,"Df"]

critical<- qf(p=0.05, df1, df2, lower.tail=TRUE)

F_test<-summary_cd[[1]][[1,"F value"]]

ifelse(F_test>critical,"Null Hypothesis rejected","Null Hypothesis approved")




#==============================FINSH Assignment 2 Week 2========================
