#Abhinav Jain 

# An athletic director suggests the median number for the paid attendance
# at 20 local football games is 3000. The data for a random sample are shown. 
# At ?? = 0.05, is there enough evidence to reject the claim?
# If you were printing the programs for the games, would you use this figure as a guide?
# 6210	3150	2700	3012	4875
# 3540	6127	2581	2642	2573
# 2792	2800	2500	3700	6030
# 5437	2758	3490	2851	2720

#Value of alpha is 0.5
alpha<-0.5

#Median number for the paid attendance
median<-3000

#For printing the programs for the games the figures as a guide

games_att<-c(6210,3150,2700,3012,4875,
          3540,6127,2581,2642,2573,
          2792,2800,2500,3700,6030,
          5437,2758,3490,2851,2720)

#
diff<- games_att-median
diff

#
posve <- length(diff[diff>0])
#
negve <- length(diff[diff<0])
critical<- qf(p=0.05, posve,negve, lower.tail=TRUE)
problem1<-binom.test(x = c(posve,negve),alternative = "two.sided", conf.level = .95)
problem1


# Exact binomial test
# 
# data:  c(posve, negve)
# number of successes = 10, number of trials = 20, p-value = 1
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
# 0.2719578 0.7280422
# sample estimates:
# probability of success 0.5
# Critical Value is 0.3357
# as the p-value is 1, no enough evidence to reject the null hypothesis

# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/binom.test.html

#===============================================================================

# Problem 2

# H0 (Owner hypothesis): median = 200
# H1 (Alternative hypothesis): median !=200
set.seed(123)
alpha<-0.5
sample<-round(runif(15,min = 150,200))
median<-200

diff<-sample - median


posve<-length(diff[diff>0])

negve<-length(diff[diff<0])
critical<- qf(p=0.05, posve,negve, lower.tail=TRUE)
problem2<-binom.test(x = c(posve,negve),alternative = "two.sided")
problem2

# Exact binomial test
# 
# data:  c(posve, negve)
# number of successes = 0, number of trials = 14, p-value = 0.0001221
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
# 0.0000000 0.2316358
# sample estimates:
# probability of success =0
# as the p value is low it indicates the enough evidence to reject 
# the null hypothesis and accept the alternative hypothesis

#===============================================================================

#Problem 3
# A random sample of men and women in prison was asked to give the length of sentence
# each received for a certain type of crime. At ?? = 0.05, test the claim that there is
# no difference in the sentence received by each gender. The data (in months) are shown here.
# 
# Males	8	12	6	14	22	27	3	2	2	
#                           2	4	6	
# Females	7	5	2	3	21	26	3	9	4	
#                         0			
# 
# 
# Males	19	15	13		
# Females	17	23	12	11	16
# Source: Based on information from the National Highway Traffic Safety Administration.

alpha<-0.05

Males<-c(8 , 12 ,6 ,14 , 22  ,27 , 32,  24,  26, 19  ,15 , 13)
Females<--c(7 ,5 ,2 ,3 ,21  ,26  ,30 ,9 ,4  ,17  ,23  ,12  ,11  ,16) 

problem3<-wilcox.test(x = Males, y = Females, alternative = "two.sided", correct = F)
problem3

# Wilcoxon rank sum exact test
# 
# data:  Males and Females
# W = 168, p-value = 2.071e-07
# alternative hypothesis: true location shift is not equal to 0

#===============================================================================
# # For the years 1970-1993 the National League (NL) and the American League (AL) (major league baseball)
# were each divided into two divisions: East and West. 
# Below are random samples of the number of games won by each league's Eastern Division.
# At ?? = 0.05, is there sufficient evidence to conclude a difference in the number of wins?
#   
# NL	89	9	8	101	90	91	9	96	108	100	9	
#         6	8				      2				      5	
# AL	108	8	9	97	100	102	9	104	95	89	8	101
#         6	1				      5			        8

alpha=0.05
NL<-c(89,96,88,101,90,91,92,96,108,100,95)	
    
AL<-c(108,86,91,97,100,102,95,104,95,89,88,101)

problem4<-wilcox.test(x=NL, y=AL, alternative = "two.sided", correct = "F")
problem4

# Wilcoxon rank sum test
# 
# data:  NL and AL
# W = 59, p-value = 0.6657
# alternative hypothesis: true location shift is not equal to 0

#===============================================================================

# Use Table K to determine whether the null hypothesis should be rejected.
# 



## try these in an R markdown document for best results
# ws = 13, n = 15, ?? = 0.01, two-tailed
CV = 16
# ws = 32, n = 28, ?? = 0.025, one-tailed
CV = 117
# ws = 65, n = 20, ?? = 0.05, one-tailed
CV = 60
# ws = 22, n = 14, ?? = 0.10, two-tailed
CV = 26



#===============================================================================
# Problem 5
# Through the Organization for Economic Cooperation and Development (OECD), 15-year-olds are tested in member countries in mathematics, reading, and science literacy. Listed are randomly selected total mathematics literacy scores (i.e., both genders) for selected countries in different parts of the world. Test, using the Kruskal-Wallis test, to see if there is a difference in means at ?? = 0.05.
# 
# Western Hemisphere	Europe	Eastern Asia
# 527	520	523
# 406	510	547
# 474	513	547
# 381	548	391
# 411	496	549
# Source: National Center for Education Statistics

WH<-data.frame(score = c(527,406,474,381,411), regien = rep("Western_Hemisphere",5))
EU<-data.frame(score = c(520,510,513,548,496), regien = rep("Europe",5))
EA<-data.frame(score = c(523,547,547,391,549), regien = rep("Eastern_Asia",5))

df = rbind(WH,EU,EA)
problem5<-kruskal.test(score ~ region , data = df)
problem5

# Kruskal-Wallis rank sum test
# 
# data:  score by regien
# Kruskal-Wallis chi-squared = 5.3078, df = 3, p-value = 0.1506

#===============================================================================

alpha6<-0.05

City<-c("1","2","3","4","5","6")
Subway<-c(845	,494	,425	,313	,108	,41)
Rail<-c(39	,291	,142	,103	,33	,38)


data_set<-data.frame(City = City, Subway = Subway, Rail = Rail)
problem6<-cor.test(data_set$Subway,data_set$Rail, method = "spearman")
problem6

# #####solution
# Spearman's rank correlation rho
# 
# data:  data_set$Subway and data_set$Rail
# S = 14, p-value = 0.2417
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# 0.6 

#===============================================================================
# Problem7
# A caramel corn company gives four different prizes, one in each box. 
# They are placed in the boxes at random. 
# Find the average number of boxes a person needs to buy to get all four prizes. (40)

set.seed(10)
t = sample(c(1,2,3,4),size = 40,replace=TRUE,p=c(0.25,0.25,0.25,0.25)) 
l= sample(c(1,2,3,4),size = 40,replace=TRUE,p=c(0.25,0.25,0.25,0.25))
m = sample(c(1,2,3,4),size = 40,replace=TRUE,p=c(0.25,0.25,0.25,0.25))
n = sample(c(1,2,3,4),size = 40,replace=TRUE,p=c(0.25,0.25,0.25,0.25))
o = sample(c(1,2,3,4),size = 40,replace=TRUE,p=c(0.25,0.25,0.25,0.25))
p = sample(c(1,2,3,4),size = 40,replace=TRUE,p=c(0.25,0.25,0.25,0.25)) 
total = sum(l == t) + sum(m == t) +sum(n == t) +sum(o == t)+sum(p == t) 
avg = total/5
cat(paste("\nThe Average is:",avg))


#===============================================================================
# Problem 8
# To win a certain lotto, 
# a person must spell the word big. 
# Sixty percent of the tickets contain the letter b, 
# 30% contain the letter i, and 
# 10% contain the letter g.
# Find the average number of tickets a person must buy to win the prize. (30)

t = sample(c("b","i","g"),size = 30,replace=TRUE,p=c(0.6,0.3,0.1))
l = sample(c("b","i","g"),size = 30,replace=TRUE)
m = sample(c("b","i","g"),size = 30,replace=TRUE)
n = sample(c("b","i","g"),size = 30,replace=TRUE)
o = sample(c("b","i","g"),size = 30,replace=TRUE)
p = sample(c("b","i","g"),size = 30,replace=TRUE)
total = sum(l == t) + sum(m == t) +sum(n == t) +sum(o == t)+sum(p == t) 
avg = total/5
cat(paste("\nThe Average is:",avg))

#=================================Finish===========================================

