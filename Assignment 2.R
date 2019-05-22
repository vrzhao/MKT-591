rm(list=ls())
# QUESTION 1
# Let’s run a multiple linear regression of ‘politics (I am interested in politics)’ on ‘age’, 
# ‘buyamer (Americans should always try to buy American products)’ and ‘gender’. Here, gender is a categorical dummy variable. 
# What is your interpretation for the effect of ‘gender’ on DV of ‘politics’?

lifestyle = read.csv("Assignment 2/imputed_lifestyle.csv", header = TRUE)

m1 = lm(politics ~ age + buyamer + gender, data = lifestyle)
m1sum = summary(m1)
m1sum

# QUESTION 2
# We want to investigate the interaction effect of gender and ‘buyamer’. 
# So, please run the multiple linear regression of ‘politics’ on ‘age’, ‘buyamer’ and ‘gender’, 
# and add one interaction term between ‘gender’ and ‘buyamer’. What is the correct interpretation about the regression results?

m2 = lm(politics ~ age + buyamer + gender + buyamer*gender, data = lifestyle)
m2sum = summary(m2)
m2sum

# QUESTION 3
# We want to investigate the interaction effect of gender and age. So, please run the multiple linear regression of ‘politics’ on ‘age’, 
# ‘buyamer’ and ‘gender’, and add one interaction term between ‘gender’ and ‘age’ to the multiple regression. What can you tell about the regression results?

m3 = lm(politics ~ age + buyamer + gender + age*gender, data = lifestyle)
m3sum = summary(m3)
m3sum


# QUESTION 4
# What is false for the ridge regression?


# QUESTION 5
# Please run the ridge regression of ‘politics’ (DV) on ‘age’, ‘buyamer’ and ‘gender’ (IVs). 
# Note, use 'MASS' package. Given lambda value is 1,000, what is the estimated coefficient value of ‘age’ variable in the ridge regression?

library(MASS)

lm.ridge(politics ~ age + buyamer + gender, data = lifestyle, lambda = 1000)

# QUESTION 6
# Please run the lasso regression (with lars R package) of ‘liberal’ (DV) on 2nd to 13th columns of the data 
# (i.e., netdome, abortion, buyamer, condoms1, deathpen, goodeduc, gun, marijuana, moreinc, politics, pollut, and sexmarr for 12 IVs). 
# If cross-validation testing indicates 0.8 of fraction of final L1 norm provides a best set of the selected IVs, what can you tell about the results.

library(lars)

IVs = as.matrix(lifestyle[,2:13])
DV = lifestyle$liberal 
res_lasso = lars(IVs, DV, type = "lasso")
coef.lars(res_lasso, s = 0.5, mode = "fraction")


# QUESTION 7
# What can you say about odds ratio?



# QUESTION 8
# In a binary logistic regression, if Xbeta𝑋𝛽 part is 0.5 what will be predicted probability?

1/(1+exp(-0.5))


# QUESTION 9
# Please run the logistic regression as instructed (i.e., use Y as dependent variable, and age, marital, housing and duration as independent variables). 
# What can you tell about ‘marital status’ for selling term deposits (use the training dataset of bank data to answer this question)?

bank_train = read.csv("Assignment 2/bank.train.csv", header = TRUE)
bank_valid = read.csv("Assignment 2/bank.valid.csv", header = TRUE)

bank_train$marital = as.factor(bank_train[,2])
bank_train$housing = as.factor(bank_train[,3])

fit.logit = glm(y ~ age + marital + housing + duration, data = bank_train, family = binomial("logit"))

summary(fit.logit)

# QUESTION 10
# From the logistic regression results, what can you say about the probability of selling term deposits when having house loan 
# (use the training dataset to answer this question)?



# QUESTION 11
# What is your interpretation for the age variable on selling the term deposits (use the training dataset of bank data to answer this question)? 
# Note: below ‘~’ means "about" or "approximately".

0.02261*1

# QUESTION 12
# What is the log odds (logit) of selling the long-term deposit to a customer with a profile of age 40, married, with housing loan, 
# and duration of last contact 100 seconds (use the training dataset to answer this question)?

sum(fit.logit$coefficients*c(1,40,1,0,0,0,1,100))

# QUESTION 13
# What is the probability of selling the term deposit to a customer with a profile of age 30, single, with housing loan, 
# and duration of last contact being 400 secs (use the training dataset to answer this question)?

1/(1+exp(-1*sum(fit.logit$coefficients*c(1,30,0,1,0,0,1,400))))

# QUESTION 14
# What is the odds ratio of selling the long-term deposit to a customer with a profile of age 30, divorced, without housing loan, 
# and duration of last contact 500 seconds (use the training dataset to answer this question)?

exp(sum(fit.logit$coefficients*c(1,30,0,0,0,0,0,500)))

# QUESTION 15
# What is the prediction hit ratios for the validation dataset by using the Logistic Regression calibrated by the training dataset 
# (use the validation dataset to answer this)?

bank_valid$marital = as.factor(bank_valid[,2])
bank_valid$housing = as.factor(bank_valid[,3])

pred.valid = predict(fit.logit, bank_valid[,-5], type = "response")
pred.valid = ifelse(pred.valid > 0.5, 1, 0)
(ctv = table(bank_valid[,5], pred.valid))
diag(prop.table(ctv,1))
sum(diag(prop.table(ctv)))

# QUESTION 16
# Which of the following is not the advantages of Bass model?

bass = read.csv("Assignment 2/Bass_Assignment 2019.csv", header = TRUE)


# QUESTION 17
# What is the estimated coefficient of innovators (p) based on historical sales data?

Sales = ts(bass$Sales, start=(2004), freq = 4)

Y = cumsum(Sales)
Y = ts(Y, start = (2004), freq = 4)
Y = c(0,Y[0:(length(Y)-1)])
Ysq = Y**2

out = lm(Sales ~ Y + Ysq)
summary(out)

a = out$coefficients[1]
b = out$coefficients[2]
c = out$coefficients[3]

mplus = (-b+sqrt(b^2-4*a*c))/(2*c)
mminus = (-b-sqrt(b^2-4*a*c))/(2*c)

m = mminus
p = a/m
q = b+p

p

# QUESTION 18
# What is the estimated coefficient of imitation based on the historical sales data?

q

# QUESTION 19
# What is Bass model prediction of sales for the year 2019 (in millions) based on historical sales data?

bass_model = function(p,q,m,t=50){
  s = double(t)
  Y = double(t+1)
  Y[1] = 0
  
  for (i in 1:t){
    s[i] = p * m + (q - p) * Y[i] - (q/m) * Y[i]^2
    Y[i+1] = Y[i] + s[i]
  }
  
  return(list(sales = s, cumsales=cumsum(s)))
}

spred = bass_model(p,q,m,t=20)

spred$sales[16]

# QUESTION 20
#As a comparison purpose, please run simple exponential smoothing model (use ‘ses’ function in fpp2 package we covered in class)
# using 0.2 for the alpha value. What is the prediction of sales for the year 2019 (in millions)?

library(fpp2)

ses.Sales = ses(Sales, alpha = 0.2, h = 50)
ses.Sales

