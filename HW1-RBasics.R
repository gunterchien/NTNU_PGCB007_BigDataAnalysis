#########################
## Big Data Analysis   ##
## No: 61221002L       ##
## Name: Gunter Chien  ##
#########################


#########################
## Question (1)        ##
#########################

# Read in the data
dat1 <- read.csv("/Users/gunterchien/Desktop/碩一下學期/大數據資料分析/scripts/BDA_data/UTRGV1_10.csv")
colnames(dat1) # check variable names
str(dat1) # check variable properties
View(dat1) # see whether the data is imported correctly
head(dat1) # see the first six rows of data

dat2 <- read.csv("/Users/gunterchien/Desktop/碩一下學期/大數據資料分析/scripts/BDA_data/UTRGV11_19.csv")
colnames(dat2) # check variable names
str(dat2) # check variable properties
View(dat2) # see whether the data is imported correctly
head(dat2) # see the first six rows of data

# Combine the two data by row
dat3 <- rbind(dat1, dat2)
dat3

## RQ1: midterm vs. final grade

t.test(dat3[, "midExamGrade"], dat3[,"finalExamGrade"], paired = TRUE, var.equal = TRUE)
# t(18) = -1.634, p = .120

## RQ2: class A final vs. class B final

t.test(dat3[, "finalExamGrade"][dat3[, "Class"] == "A"], 
       dat3[, "finalExamGrade"][dat3[, "Class"] == "B"], paired = FALSE, var.equal = TRUE)
# t(17) = -0.677, p = .508


#########################
## Question (2)        ##
#########################

# Read in the data
dat4 <- read.csv("/Users/gunterchien/Desktop/碩一下學期/大數據資料分析/scripts/BDA_data/studendAcaRecord.csv")
colnames(dat4) # check variable names
str(dat4) # check variable properties
View(dat4) # see whether the data is imported correctly
head(dat4) # see the first six rows of data

dat5 <- read.csv("/Users/gunterchien/Desktop/碩一下學期/大數據資料分析/scripts/BDA_data/studentQOL.csv")
colnames(dat5) # check variable names
str(dat5) # check variable properties
View(dat5) # see whether the data is imported correctly
head(dat5) # see the first six rows of data

# Combine the two data by column
# dat6 <- cbind(dat4, dat5)
# dim(dat6)

dat6 <- merge(dat4, dat5, by = "studID")
dim(dat6)

## RQ: correlation between midterm and quality of life (QOL)

cor(dat6[, "midExamGrade"], dat6[, "QOL"]) 
# r = .199

cor.test(dat6[, "midExamGrade"], dat6[, "QOL"]) 
# r = .199


#########################
## Question (3)        ##
#########################

## RQ1: simple regression (DV = 'finalExamGrade', IV = 'studyHR')

reg_final_studyHR <- lm(finalExamGrade ~ studyHR, data = dat6)
summary(reg_final_studyHR)
# Intercept = -140.785 **, slope (studyHR) = 17.722 *** 

# The p-value for 'studyHR' is extremely low (7.07e-05), 
# far below .05. This means there is very strong evidence 
# against the null hypothesis (which would suggest no 
# relationship between 'studyHR' and 'finalExamGrade'). 
# Therefore, we can conclude that 'studyHR' is indeed a 
# significant predictor of 'finalExamGrade'.


## RQ2: one-way ANOVA (DV = 'midExamGrade', IV = 'Class')

aov_midterm_class <- aov(midExamGrade ~ Class, data = dat6)
summary(aov_midterm_class)
# F(2) = .12, p = .887

# The p-value of .887 is much higher than .05. This indicates that 
# there is no statistically significant difference in midExamGrade 
# among the different classes. In other words, 'Class' is not a 
# significant predictor of 'midExamGrade' in the dataset dat6. The 
# class to which an individual belongs does not significantly affect 
# their mid-exam grades.

