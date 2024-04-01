###########################
##  Big Data Analysis    ##
##  HW3-CrossValidation  ##
##  Name: Gunter Chien   ##
###########################

###########################
##       Question        ##
###########################


### Please build a model to predict students’ IQ scores 
### with a reasonable number of IVs.

### Please use the data with 5 fold cv step-wise regression to find out 
### the most appropriate stepwise regression model to predict `iq` by 
### using age, memo, and read. The candidate models can be obtained via 
### the default “exhausted” method.


# Load in libraries
library(ISLR)
library(leaps)

# Read in the data
studIQ <- read.csv("/Users/gunterchien/Documents/GitHub/NTNU_PGCB007_BigDataAnalysis/BDA_data/stepwisereg_IQdata.csv")
colnames(studIQ) # check variable names
str(studIQ) # check variable properties
View(studIQ) # see whether the data is imported correctly
head(studIQ) # see the first six rows of data

# Function for prediction
predict.regsubsets <- function(object, newdata, id){
  
  form = as.formula(object$call[[2]])
  mat = model.matrix(form,newdata)
  coefi = coef(object,id = id)
  xvars = names(coefi)
  mat[ ,xvars]%*%coefi
}

# Conduct 5 fold cross-validation
set.seed(123456)
k <- 5
folderIndex <- sample(1:k, nrow(studIQ), replace = TRUE) # 5 numbers for grouping of participants
folderIndex

# studIQ[folderIndex == 1,] ## the data for participants belonging to the first fold
# head(studIQ[folderIndex != 1,]) ## the data for participants NOT belonging to the first fold


# Initialize an error matrix
cv.errors <- matrix(NA, k, 3)
cv.errors

# Calculate cv errors between training and testing data
for(j in 1:k){
  best.fit <- regsubsets(iq~., data = studIQ[folderIndex != j, ], nvmax = 3, method=c("exhaustive"))
  
  for(i in 1:3){
    pred <- predict.regsubsets(best.fit, studIQ[folderIndex == j, ], id=i) ## get the predicted iq in the jth folder
    cv.errors[j,i] = mean((studIQ$iq[folderIndex == j] - pred)^2)
  }
  
}

# Calculate the means of the MSE of the 3 candidates
cv.errors
colMeans(cv.errors) 
which.min(colMeans(cv.errors))

# Calculate the coefficients of the 5 fold CV model
coef(best.fit, which.min(colMeans(cv.errors)))

## StudentIQ = 80.4858 - (5.7964 × Age) + (7.4902 × Memory) + (2.6331 × Reading Ability)

## In this analysis, I used a 5-fold cross-validation method 
## to ensure the reliability of the findings, which explore 
## the relationship between IQ scores and three variables: age, 
## memory, and reading ability. The results indicate a positive 
## link between both memory and reading ability with IQ scores. 
## Specifically, for each additional point in memory score, the 
## IQ score increases by about 7.49 points. Similarly, an improvement 
## in reading ability is associated with a 2.63 point increase in IQ. 
## In contrast, age shows a negative relationship with IQ, where each 
## additional year correlates with a decrease of approximately 5.8 in 
## the IQ score. 

