rm(list=ls())
set.seed(100)
# Q1
# Which of the following is true about factor analysis?

library(nFactors)
library(Hmisc)
library(GPArotation)

servqual = read.csv("Assignment 3/servqual.csv", header = TRUE)

# Q2
# Please use eigenvalue greater than 3 as factor model selection criteria for this question. 
# If you use the factor analysis to try and reduce dimensions (with Varimax rotation) for ratings on Q1 - Q22 using the selection criteria 
# (i.e., eigenvalue > 3), how many factors should be selected as the final model?

eigen(cor(servqual[,3:24]))
  
# Q3
# If you use factor analysis to try and reduce dimensions (with Varimax rotation) for ratings on Q1 - Q22, 
# what proportion (%) of the variance can be explained by the factor model using eigenvalue greater than 3 as the selection criteria ?
  
factanal(servqual[,3:24], factors = 1, rotation = "varimax")


# Q4
# In a three factor model for Q1 - Q22 (with Varimax rotation), what can the 2nd factor be representative of in terms 
# of the five potential dimensions of the survey?

factanal(servqual[,3:24], factors = 3, rotation = "varimax")


# Q5
# In a three factor model with oblique (oblimin) rotation for Q1 – Q22, what can the 3rd factor be representative of 
# in terms of the five potential dimensions of the survey?

factanal(servqual[,3:24], factors = 3, rotation = "oblimin")

# Q6
# In a three factor model with oblique (oblimin) rotation for Q1 – Q22, which two factors have the largest correlation?

factanal(servqual[,3:24], factors = 3, rotation = "oblimin")

# Q7
# Please run a three factor model with oblique (oblimin) rotation. 
# In the three factor model for Q1 – Q22, what proportion (%) of the variance can be explained by the first factor of the model?
  
factanal(servqual[,3:24], factors = 3, rotation = "oblimin")

# Q8
# What can you say about the regression of servqual (dependent variable) on the three orthogonal factor scores (with Varimax rotation) 
# obtained using a three factor model? [note, use Bartlett method for getting factor scores]

factor_res = factanal(servqual[,3:24], factors = 3, rotation = "varimax", scores = "Bartlett")
factor_scores = data.frame(factor_res$scores)
factor_scores


X1 = as.matrix(servqual[,3:24])
fx1 = as.matrix(factor_res$scores)
y1 = servqual[,1]

m1 = lm(y1~X1)
m2 = lm(y1~fx1)
summary(m1)
summary(m2)

BIC(m1)
BIC(m2)

# Q9
# What can you say about the regression of servqual (dependent variable) on the three correlated factor scores (with oblique/oblimin rotation) 
# obtained using a three factor model? [note, use Bartlett method for getting factor scores]

factor_res = factanal(servqual[,3:24], factors = 3, rotation = "oblimin", scores = "Bartlett")
factor_scores = data.frame(factor_res$scores)

X2 = as.matrix(servqual[,3:24])
fx2 = as.matrix(factor_res$scores)
y2 = servqual[,1]

m3 = lm(y2~X2)
m4 = lm(y2~fx2)
summary(m3)
summary(m4)

BIC(m3)
BIC(m4)


# Q10
# What can you say about perceptual map with Multidimensional Scaling?

SUV = read.csv("Assignment 3/LargeSUVAttributes.csv", header = TRUE)

# Q11
# Use “LargeSUVAttributes.csv” data file. Please conduct principal component analysis using prcomp in R. 
# What proportion (%) of variances can be explained by both PC1 and PC2? (note, use the 2nd column to the 14th columns of the data in ‘prcomp’ function)

SUV.sc = SUV
SUV.pc = prcomp(SUV.sc[,2:14])
summary(SUV.pc)

# Q12
# Use “LargeSUVAttributes.csv” data file. This is from consumer survey study to understand consumers’ perceptions of various attributes of large SUV brands. 
# The provided excel table is mean values of multiple SUV attributes of multiple large SUV brands. 
# First, compute distance matrix across 13 large SUV brands using 13 attributes (from 2nd column to 14 columns). 
# Note, please don’t include both ‘Ford’ and ‘GM’ to compute distance matrix. 
# Then, using commands ‘cmdscale’ in R, draw two-dimensional perceptual map. 
# In the positioning map, what is a competitor of Lexus LX 470 in consumers’ perception?

SUV.dist = dist(SUV.sc[,2:14])
SUV.mds = cmdscale(SUV.dist)
plot(SUV.mds, type = 'n')
rownames(SUV.mds) = paste("",SUV$vehrated,sep="")[1:13]
text(SUV.mds,rownames(SUV.mds), cex = 1)

# Q13
# Please see the positioning map from previous question 12. What can be incorrect statement about this perceptual mapping result?
  

# Q14
# Use “LargeSUVAttributes.csv” data file. First, please run principal component analysis (‘prcomp’ in R) 
# across 13 large SUV brands using 13 attributes (from 2nd column to 14 columns). Then, using ‘biplot’ command in R, 
# draw two-dimensional perceptual map with attribute vector lines. In the map, which SUV brand is positioned as ‘Luxurious’ one compared to other brands?

biplot(SUV.pc, cex = c(1,1))

# Q15
# If you run a linear discriminant analysis with ‘age’, ‘duration’ and ‘previous’, what is the coefficient of linear discriminant for ‘age’?

library(MASS)
library(ggplot2)
library(gmodels)
library(klaR)

bank_train = read.csv("Assignment 3/bank.train_2018.csv", header = TRUE)
bank_valid = read.csv("Assignment 3/bank.valid_2018.csv", header = TRUE)

#Class <- as.factor(bank_train$y)
#age=scale(bank_train$age)
#duration=scale(bank_train$duration)
#previous=scale(bank_train$previous)

fit.lda<- lda(y ~ age + duration + previous, data = bank_train)
fit.lda

# Q16
# When you build a linear discriminant model using the training dataset and predict using the validation dataset, 
# what is the predictive hit rate (accuracy) and the Jaccard similarity index (accuracy measure that discards true negatives)?

pred.lda = predict(fit.lda, bank_valid)
lda_t=table(pred.lda$class, bank_valid$y)
(lda_t[1,1]+lda_t[2,2])/sum(lda_t)

lda_t[2,2]/(dim(bank_valid)[1]-lda_t[1,1])


# Q17
# When you build a Naïve Bayes Classifier with ‘age’, ‘duration’ and ‘previous’ on the training dataset and predict using the validation dataset, 
# what is the predictive hit rate (accuracy) and the Jaccard similarity index?

library(e1071)
seg.nb <- naiveBayes(y ~ age + duration + previous,data=bank_train)
pred.nb <- predict(seg.nb, bank_valid) 
bayes_t=table(pred.nb,bank_valid$y)
(bayes_t[1,1]+bayes_t[2,2])/sum(bayes_t)

CrossTv=table(bank_valid$y, pred.nb)
CrossTv[2,2]/(dim(bank_valid)[1]-CrossTv[1,1])

# Q18
# If you run a CART model with ‘age’, ‘duration’, and ‘previous’ on the training dataset, 
# what are the most important variable and the second most important variable of the tree model?

library(rpart)
CART_fit.bank <- rpart(y ~ age+duration+previous,data=bank_train, method="class")
printcp(CART_fit.bank)


# Q19
# If you run a Random Forest model with ‘age’, ‘duration’, and ‘previous’ using training dataset by creating 500 trees, 
# what are the Jaccard similarity index upon prediction? Note, don’t forget run set.seed(100) before conducting this analysis.

library(randomForest)
RF_fit.bank <- randomForest(y ~ age+duration+previous,data=bank_train, ntree=500)
bank.predict <-predict(RF_fit.bank, bank_valid, predict.all=TRUE)
n=dim(bank_valid)[1]

seg.rf.class<-bank.predict$aggregate

rf_t = table(bank_valid[,4],seg.rf.class)
(rf_t[1,1]+rf_t[2,2])/sum(rf_t)

CrossTv=table(bank_valid$y,seg.rf.class)
CrossTv[2,2]/(dim(bank_valid)[1]-CrossTv[1,1]) 


# Q20
# Please see the attached figure for this question. Using the given weight values of X1 and X2, what are final out vectors (i.e., Y*)?

x1 = c(0, 0, 1, 1)
x2 = c(0, 1, 0, 1)

y1 = (x1 + x2) * 3 - 5
y2 = (x1 + x2) * (-4) + 1

y1 = ifelse(y1 > 0,1,0)
y2 = ifelse(y2 > 0,1,0)

y_star = (y1 + y2) * 2 - 1
y_star = ifelse(y_star > 0,1,0)
y_star

