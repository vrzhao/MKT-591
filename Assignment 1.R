rm(list=ls())

# Q1
# For the Cross Tab analysis to test of independence between netdome ("I don't have a clue what the internet is and what it can do for me") 
# and educ ("Level of education completed"), what is the observed count of "Moderately Disagree" and "Grad High School", 
# and the expected count of "Definitely Agree" and "Grad High School"?

lifestyle = read.csv("Assignment 1/lifestyle.csv", header = TRUE)

library(gmodels)

CrossTab1 = CrossTable(lifestyle$netdome,lifestyle$educ, chisq=TRUE, expected = TRUE, dnn = c("netdome","educ"))
CrossTab1$t[3,3]
CrossTab1$chisq$expected[3,3]


# Q2
# What is the null hypothesis for the test of independence between 'netdome' (I don't have a clue what the internet is and what it can do for me) 
# and 'educ' (Level of education completed)?



# Q3
# What is the chi-squared test statistic and degree of freedom (d.f.) for the test of independence between 'netdome' and 'educ'?

CrossTab1$chisq$statistic
CrossTab1$chisq$parameter


# Q4
# What can you say about the p-value of the test of independence between 'netdome' and 'educ'?

CrossTab1$chisq$p.value

# Q5
# Based on your test statistics and p-value, what is your conclusion for the test of independence between 'netdome' and 'educ' 
# (use 99% confidence level for your statistical decision)?

CrossTab1$chisq


# Q6
# We hear that our client (marketing manager) is interested in two levels of education completed 
# (i.e., "Grad High School or lower" vs "Att College or higher", let's name this new variable as 'new_educ') 
# instead of the current six levels. What is the observed count and the expected count of "Moderately Disagree" and "Att College or higher"?

lifestyle$new_educ = 1*(lifestyle$educ >= 4) # Att College or Higher = 1, Grad High School or lower = 0

CrossTab2 = CrossTable(lifestyle$netdome,lifestyle$new_educ, chisq=TRUE, expected = TRUE, dnn = c("netdome","new_educ"))
CrossTab2$t[3,2]
CrossTab2$chisq$expected[3,2]

# Q7
# Please conduct the lifestyle analysis with the data (with 'new_educ') from the question (6).  
# What is the chi-squared test statistic for the test of independence between 'netdome' and 'new_educ'?

CrossTab2$chisq$statistic


# Q8
# What is (pearson) correlation between the question 'politics' (I am interested in politics) and 'respondent's age'?

cor(lifestyle$politics,lifestyle$age ,method = "pearson", use = "complete.obs")


# Q9
# What is the null hypothesis for the test of (pearson) correlation between 'politics" and "respondent age"? 
# And, what is your conclusion based on your correlation analysis result?



# Q10
# In a simple bivariate regression of 'politics' (I am interested in politics, DV) on 'age' 
# (i.e., respondent age: IV), what is R-squared value and what is its interpretation?

m1 = lm(politics~age, data=lifestyle)
m1_s = summary(m1)

m1_s$r.squared

# Q11
# What is estimated prediction equation (best approximate) for the bivariate regression of 'politics' (Y) on 'age' (X)?

m1_s$coefficients[,1]

# Q12
# Based on the estimated linear regression model, what will be predicted value of the level of 'Politics', 
# if the age is known as 60 years using the estimated bivariate regression model in previous question?

sum(m1_s$coefficients[,1]*c(1,60))

# Q13
# What you CANNOT say about the estimated result of the bivariate regression model between 'gun' 
# (i.e., there should be a gun in every home) and 'age' (Thus, DV: gun, IV: age)?

m2 = lm(gun~age, data=lifestyle)
m2_s = summary(m2)

m2_s

# Q14
# If you run a multiple regression of 'liberal' ("Generally speaking, would you consider yourself to be .") 
# on 'age', 'deathpen' (I am in favor of the death penalty), and 'politics' (I'm interested in politics), 
# how would you interpret the coefficient of 'politics' based on the estimated regression results?

m3 = lm(liberal~age+deathpen+politics, data = lifestyle)
m3_s = summary(m3)

m3_s

# Q15
# What is the estimated multiple regression equation (best approximate) for predicting 'liberal'(Y) using 'age'(X1), 'deathpen' (X2) and 'politics'(X3)?

m3_s$coefficients[,1]


# Q16
# What is the predicted level of 'Liberal' tendency using the estimated multiple regression model 
# (from Question 15) when the value 'age' is set as 20, the value for 'deathpen' is set as 6 (i.e., 'definitely agree'), 
# and the value for 'politics' is set as 6 (i.e., 'definitely agree')?

sum(m3_s$coefficients[,1] * c(1,20,6,6))

# Q17
# Please run bivariate linear regression to investigate the effect of advertising ('Adv') on sales (use 'Sales' for DV and 'Adv' as IV). 
# From the estimated regression results, what can you say about the impact of 'Adv' on sales?

AD = read.csv("Assignment 1/experiment_price_ad.csv", header = TRUE)

m4 = lm(Sales~Adv, data = AD)
m4_s = summary(m4)

m4_s

# Q18
# Using a multiple linear regression model to predict sales with Advertising and store size, what can you tell for the impact of Advertising on sales?

m5 = lm(Sales~Adv+Store.Size, data = AD)
m5_s = summary(m5)

m5_s

# Q19
# Let's run multiple linear regressions to predict sales with Adv, price, and store size. 
# Analysts want to predict sales when price is 24, there is no advertising, and store size is 40. What is the predicted sales?

m6 = lm(Sales~Adv+Price+Store.Size, data = AD)
m6_s = summary(m6)

sum(m6_s$coefficients[,1] * c(1,0,24,40))

# Q20
# Let's run multiple linear regressions to predict sales with Adv, price, and store size. 
# Here, let's try to code the price variable in two different ways such as discrete dummy (factor) 
# variable or continuous (numeric) variable in the multiple regressions. Please compare the predictive power (hint: use R-squared) 
# between the two multiple regressions using two different types of coding for price (e.g., discrete [dummy] vs. continuous). 
# What CANNOT you tell about the regression results? 

unique = unique(AD["Price"])

# for (i in 1:nrow(unique)){
#   nam = paste("Price", i, sep = "")
#   assign(nam, 1*(AD$Price==unique[i,1]))
# }
  
AD$Price1=1*(AD$Price==unique[1,1])
AD$Price2=1*(AD$Price==unique[2,1])
AD$Price3=1*(AD$Price==unique[3,1])

m7 = lm(Sales~Adv+Price+Store.Size, data = AD)
m7_s = summary(m7)
m8 = lm(Sales~Adv+Price1+Price2+Price3+Store.Size, data = AD)
m8_s = summary(m8)

m7_s$r.squared
m8_s$r.squared










