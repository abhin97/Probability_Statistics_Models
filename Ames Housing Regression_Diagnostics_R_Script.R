#+++++++++++++++++++++++++++++
#+Regression Diagnostics
#+and Features Selection in R
#+Module 1 - ALY6015
#+Author : Abhinav Jain
#+++++++++++++++++++++++++++++


#importing the libraries
library(tidyverse)
library(hrbrthemes)
library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(caTools)
library(car)
library(BSDA)
library(vtable)
library(zoo)
library(Hmisc)
library(psych)
library(fs)
library(corrplot)
library(PerformanceAnalytics)
library(jtools)
library(car)
library(BSDA)
library(MASS)
library(leaps)
#===============
#EDA
#===============
#Import the Ames housing dataset.

df_hou <- read.csv("AmesHousing.csv")
df_hou
summary(df_hou)
st(df_hou)

 #===============

# Perform Exploratory Data Analysis and use descriptive statistics to describe the data.
describe(df_hou$SalePrice)
psych::describe(df_hou$Bsmt.Unf.SF)
describe(df_hou$Total.Bsmt.SF)
describe(df_hou$Gr.Liv.Area)
ggplot(df_hou, aes(x= SalePrice))+
  geom_histogram(fill="#69b3a2", color="#FF0000")+
  ggtitle("Sales Price of the Ames_Housing")+
  theme_ipsum()

ggplot(df_hou, aes(x= Lot.Frontage))+
  geom_histogram(fill="#FFFF00", color="#69b3a2")+
  ggtitle("Lot Frontage of the Ames_Housing")+
  theme_ipsum()

Gr.Liv.Area
ggplot(df_hou, aes(x= Gr.Liv.Area))+
  geom_histogram(fill="#FFFF00", color="#69b3a2")+
  ggtitle("Gr.Liv.Area of the Ames_Housing")+
  theme_ipsum()


ggplot(df_hou, aes(x= Gr.Liv.Area))+
  geom_boxplot(fill="#FF0000", color="#69b3a2")+
  ggtitle("Boxplot Gr.Liv.Area of the Ames_Housing")+
  theme_ipsum()

ggplot(df_hou, aes(x= SalePrice))+
  geom_boxplot(fill="#FF0000", color="#69b3a2")+
  ggtitle("Boxplot SalePrice of the Ames_Housing")+
  theme_ipsum()

unique(df_hou$Street)

unique(df_hou$MS.Zoning)

# Prepare the dataset for modeling by imputing missing values with the variable's mean value or any other value that you prefer.
#Aggregate the NA(Missing values) values of Lot.Frontage

df_hou$Lot.Frontage<- na.aggregate(df_hou$Lot.Frontage)
summary(df_hou$Lot.Frontage)

df_hou$Mas.Vnr.Area<- na.aggregate(df_hou$Mas.Vnr.Area)
summary(df_hou$Mas.Vnr.Area)

df_hou$BsmtFin.SF.1<- na.aggregate(df_hou$BsmtFin.SF.1)
summary(df_hou$BsmtFin.SF.1)

df_hou$Bsmt.Unf.SF<-na.aggregate(df_hou$Bsmt.Unf.SF)
summary(df_hou$Bsmt.Unf.SF)

df_hou$Total.Bsmt.SF<-na.aggregate(df_hou$Total.Bsmt.SF)
summary(df_hou$Total.Bsmt.SF)

df_hou$Garage.Area<-na.aggregate(df_hou$Garage.Area)
summary(df_hou$Garage.Area)

df_cor<- df_hou[,c("Lot.Frontage", "Lot.Area", "Mas.Vnr.Area", "BsmtFin.SF.1", 
                   "Bsmt.Unf.SF", "Total.Bsmt.SF","X1st.Flr.SF","X2nd.Flr.SF",
                   "Low.Qual.Fin.SF","Gr.Liv.Area","Garage.Area","Wood.Deck.SF","Open.Porch.SF","Enclosed.Porch",
                   "X3Ssn.Porch","Screen.Porch","Pool.Area","Misc.Val","SalePrice")]
summary(df_cor)

df_cor$Lot.Frontage<-as.numeric(df_cor$Lot.Frontage)
df_cor$Lot.Area<-as.numeric(df_cor$Lot.Area)
df_cor$Mas.Vnr.Area<-as.numeric(df_cor$Mas.Vnr.Area)
df_cor$BsmtFin.SF.1<-as.numeric(df_cor$BsmtFin.SF.1)
df_cor$Bsmt.Unf.SF<-as.numeric(df_cor$Bsmt.Unf.SF)
df_cor$Total.Bsmt.SF<-as.numeric(df_cor$Total.Bsmt.SF)
df_cor$X1st.Flr.SF<-as.numeric(df_cor$X1st.Flr.SF)
df_cor$X2nd.Flr.SF<-as.numeric(df_cor$X2nd.Flr.SF)
df_cor$Low.Qual.Fin.SF<-as.numeric(df_cor$Low.Qual.Fin.SF)
df_cor$Gr.Liv.Area<-as.numeric(df_cor$Gr.Liv.Area)
df_cor$Garage.Area<-as.numeric(df_cor$Garage.Area)
df_cor$Wood.Deck.SF<-as.numeric(df_cor$Wood.Deck.SF)
df_cor$Open.Porch.SF<-as.numeric(df_cor$Open.Porch.SF)
df_cor$Enclosed.Porch<-as.numeric(df_cor$Enclosed.Porch)
df_cor$X3Ssn.Porch<-as.numeric(df_cor$X3Ssn.Porch)
df_cor$Screen.Porch<-as.numeric(df_cor$Screen.Porch)
df_cor$Pool.Area<-as.numeric(df_cor$Pool.Area)
df_cor$Misc.Val<-as.numeric(df_cor$Misc.Val)
df_cor$SalePrice<-as.numeric(df_cor$SalePrice)

# Use the "cor()" function to produce a correlation matrix of the numeric values.
cor(df_cor)
round(cor(df_cor), 2)

# Produce a plot of the correlation matrix, and explain how to interpret it. (hint - check the corrplot or ggcorrplot plot libraries)
corrplot(cor(df_cor), method = "circle")

pairs(df_cor[,1:4], pch = 19)
my_cols <- c("SalePrice", "Gr.Liv.Area", "Bsmt.Unf.SF","Total.Bsmt.SF")  
pairs(my_cols, pch = 19,  cex = 0.5,
      col = my_cols[df_cor$SalePrice],
      lower.panel=NULL)
# 
plot(df_cor$SalePrice)


# Make a scatter plot for the X continuous variable with the highest correlation with SalePrice. Do the same for the X variable that has the lowest correlation with SalePrice.
#Finally, make a scatter plot between X and SalePrice with the correlation closest to 0.5. Interpret the scatter plots and describe how the patterns differ.
#HighestCorrelation
ggplot(df_cor, aes(x= SalePrice, y= Gr.Liv.Area ))+
  geom_point()+
  ggtitle("Highest correlation Gr.Liv.Area vs Sales Price")+
  theme_ipsum()+
  stat_smooth()
  
#Lowest Correlation
ggplot(df_cor, aes(x= SalePrice, y= Bsmt.Unf.SF ))+
  geom_point()+
  ggtitle("Lowest Correlation Bsmt.Unf.SF vs Sales Price")+
  theme_ipsum()+
  stat_smooth()

#Correlation with Closest to 0.5
ggplot(df_cor, aes(x= SalePrice, y= Total.Bsmt.SF ))+
  geom_point()+
  ggtitle("Closest to (0.5) Total.Bsmt.SF vs Sales Price")+
  theme_ipsum()+
  stat_smooth()


# Using at least 3 continuous variables, fit a regression model in R.
#Model 1
df_reg <- as.data.frame(df_cor)
fit <- lm(SalePrice ~ Gr.Liv.Area+ Bsmt.Unf.SF+ Total.Bsmt.SF, data = df_reg)
summ(fit)
summary(fit)$adj.r.squared
AIC(fit)
BIC(fit)
par(mfrow=c(2,2))
plot(fit)



# Report the model in equation form and interpret each coefficient of the model in the context of this problem.
SalePrice <- (-18837.76) + (85.17 * df_reg$Gr.Liv.Area) + (-23.13 * df_reg$Bsmt.Unf.SF) + (80.68 *df_reg$Total.Bsmt.SF)
summary(fit)$coefficient



# Use the "plot()" function to plot your regression model. Interpret the four graphs that are produced.
par(mfrow=c(2,2))
plot(fit)
#
crPlots(model=fit)
qqnorm(df_reg$SalePrice)
qqline(df_reg$SalePrice)
qqPlot(df_reg$Gr.Liv.Area)
sd(df_reg$SalePrice)

spreadLevelPlot(fit)
# Check your model for multicollinearity and report your findings. What steps would you take to correct multicollinearity if it exists?

vif(fit)
# Check your model for outliers and report your findings. Should these observations be removed from the model?

outlierTest(model = fit)
# Attempt to correct any issues that you have discovered in your model. Did your changes improve the model, why or why not?
hist(df_cor$SalePrice,xlab = "Sale Price",main = "Sale")

summary(powerTransform(df_cor$SalePrice))

df_cor$SalePrice_sqrt <- sqrt(df_cor$SalePrice)

hist(df_cor$SalePrice_sqrt,xlab = "SalePrice",main = "Sale Price Frequency",
     col="BLue")

fit_model2<-lm(SalePrice_sqrt~ Gr.Liv.Area+ Bsmt.Unf.SF+ Total.Bsmt.SF,data=df_cor)

summary(fit_model2)



# Use the all subsets regression method to identify the "best" model. State the preferred model in equation form.

df_hou_sub = subset(df_reg, select = c(SalePrice,Gr.Liv.Area,Bsmt.Unf.SF,Total.Bsmt.SF))

fit_sub<-lm(SalePrice ~ Gr.Liv.Area+ Bsmt.Unf.SF+ Total.Bsmt.SF,data=df_hou_sub)
summary(fit_sub)

stepAIC(fit_sub,direction="backward")
stepAIC(fit_sub,direction="forward")
stepAIC(fit_sub,direction="both")

# Compare the preferred model from step 13 with your model from step 12. How do they differ? Which model do you prefer and why?

leap<-regsubsets(SalePrice~ Gr.Liv.Area+ Bsmt.Unf.SF+ Total.Bsmt.SF,data=df_hou_sub,nbest=4)
plot(leap,scale="adjr2", main = "Regression Subset")

summary(leap)


