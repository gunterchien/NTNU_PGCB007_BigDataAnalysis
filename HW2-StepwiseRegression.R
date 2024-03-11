#########################
## Big Data Analysis   ##
## No: 61221002L       ##
## Name: Gunter Chien  ##
#########################

### Please build a model to predict students’ IQ scores 
### with a reasonable number of IVs

# Load in libraries
library(ISLR)
library(leaps)

# Read in the data
studIQ <- read.csv("/Users/gunterchien/Documents/GitHub/NTNU_PGCB007_BigDataAnalysis/BDA_data/stepwisereg_IQdata.csv")
colnames(studIQ) # check variable names
str(studIQ) # check variable properties
View(studIQ) # see whether the data is imported correctly
head(studIQ) # see the first six rows of data

#########################
## Question (1)        ##
#########################

# Use forward selection to build a model
# with the lowest BIC

regfit.fwd <- regsubsets(iq~., data = studIQ, nvmax = 3, method = "forward")
reg.fwd.summary <- summary(regfit.fwd)

# Use the BIC criteria for forward selection
which.min(reg.fwd.summary$bic)
coef(regfit.fwd, 3, scale = "bic")

## Intercept = 70.1719
## Slope (age) = -7.1298
## Slope (memo) = 7.1585
## Slope (read) = 5.8588

## StudentIQ = 70.1719 + (−7.1298 × Age) + (7.1585 × Memory) + (5.8588 × Reading Ability)

## In the statistical analysis, I investigated the impact of memory 
## and reading ability on students' IQ scores using linear regression,
## applying forward selection based on the Bayesian Information Criterion (BIC)
## to identify the most predictive factors. The model revealed that
## both memory (with a coefficient of 7.1585) and reading ability
## (with a coefficient of 5.8588) have significant positive relationships
## with student IQ scores, while age (with a coefficient of -7.1298) shows 
## a negative influence on student IQ scores.


#########################
## Question (2)        ##
#########################

# Use backward elimination to build a model 
# with the lowest Cp

regfit.bwd <- regsubsets(iq~., data = studIQ, nvmax = 3, method = "backward")
reg.bwd.summary <- summary(regfit.bwd)

# Use the Cp criteria for backward selection
which.min(reg.bwd.summary$cp)
coef(regfit.bwd, 3, scale = "cp")

## Intercept = 70.1719
## Slope (age) = -7.1298
## Slope (memo) = 7.1585
## Slope (read) = 5.8588

## StudentIQ = 70.1719 + (−7.1298 × Age) + (7.1585 × Memory) + (5.8588 × Reading Ability)

## In the statistical analysis, I investigated the impact of memory 
## and reading ability on students' IQ scores using linear regression,
## applying backward selection based on the Mallows' Cp criterion
## to identify the most predictive factors. The model revealed that
## both memory (with a coefficient of 7.1585) and reading ability
## (with a coefficient of 5.8588) have significant positive relationships
## with student IQ scores, while age (with a coefficient of -7.1298) shows 
## a negative influence on student IQ scores.


#########################
## Question (3)        ##
#########################

# Use stepwise regression (sequential replacement) 
# to build a model with the lowest BIC

regfit.seqrep <- regsubsets(iq~., data = studIQ, nvmax = 3, method = "seqrep")
reg.seqrep.summary <- summary(regfit.seqrep)

# Use the BIC criteria for sequential replacement
which.min(reg.seqrep.summary$bic)
coef(regfit.seqrep, 3, scale = "bic")

plot(regfit.seqrep, scale = "bic")

## Intercept = 70.1719
## Slope (age) = -7.1298
## Slope (memo) = 7.1585
## Slope (read) = 5.8588

## StudentIQ = 70.1719 + (−7.1298 × Age) + (7.1585 × Memory) + (5.8588 × Reading Ability)

## In the statistical analysis, I investigated the impact of memory 
## and reading ability on students' IQ scores using linear regression,
## applying sequential replacement based on the Bayesian Information Criterion (BIC)
## to identify the most predictive factors. The model revealed that
## both memory (with a coefficient of 7.1585) and reading ability
## (with a coefficient of 5.8588) have significant positive relationships
## with student IQ scores, while age (with a coefficient of -7.1298) shows 
## a negative influence on student IQ scores.


#########################
## Question (4)        ##
#########################

# Use all possible subset regression to build a model 
# with the highest adjusted R^2

regfit.full <- regsubsets(iq~., data = studIQ, nvmax = 3, method = "exhaustive")
reg.full.summary <- summary(regfit.full)

# Use the adjusted R^2 for all possible subset regression
which.max(reg.full.summary$adjr2)
coef(regfit.full, 3, scale = "adjr2")

## Intercept = 70.1719
## Slope (age) = -7.1298
## Slope (memo) = 7.1585
## Slope (read) = 5.8588

## StudentIQ = 70.1719 + (−7.1298 × Age) + (7.1585 × Memory) + (5.8588 × Reading Ability)

## In the statistical analysis, I investigated the impact of memory 
## and reading ability on students' IQ scores using linear regression,
## applying all possible subset regression based on the adjusted R^2 
## to identify the most predictive factors. The model revealed that 
## both memory (with a coefficient of 7.1585) and reading ability (with 
## a coefficient of 5.8588) have significant positive relationships 
## with student IQ scores, while age (with a coefficient of -7.1298) 
## shows a negative influence on student IQ scores.

