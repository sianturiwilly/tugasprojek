# Heart Attack Prediction

## Author:  1. Hendi Kurniawan,
##          2. Prajudi Wiliam Chirdeardo
##
## Date:    4 July 2021
## 
## Brief Description:
## Predict heart attack using variables based on 
## diagnosed symptom factor.

rm(list = ls())
## Outline:
##  1. Data Extraction
##  2. Exploratory Data Analysis
##  3. Data Preperation
##  4. Modeling
##  5. Evaluating
##  6. Recomendation

# 1.  Data Extraction
# 1.1 Read data
heart_df <- read.csv("data/heart.csv")

# 1.2 Structure of Data Frame
str (heart_df)

# 1.3 Data Dimension
d <- dim(heart_df)
m <- d[1] # m : number of rows
n <- d[2] # n : number of columns

# Statistical Summary
summary(heart_df)

# Noted!
# age - age in years
# sex - sex (1 = male; 0 = female)
# cp - chest pain type (1 = typical angina; 2 = atypical angina; 3 = non-anginal pain; 0 = asymptomatic)
# trestbps - resting blood pressure (in mm Hg on admission to the hospital)
# chol - serum cholestoral in mg/dl
# fbs - fasting blood sugar > 120 mg/dl (1 = true; 0 = false)
# restecg - resting electrocardiographic results (1 = normal; 2 = having ST-T wave abnormality; 0 = hypertrophy)
# thalach - maximum heart rate achieved
# exang - exercise induced angina (1 = yes; 0 = no)
# oldpeak -  
# slope - the slope of the peak exercise ST segment (2 = upsloping; 1 = flat; 0 = downsloping)
# caa - number of major vessels (0-3) colored by flourosopy
# thal - 2 = normal; 1 = fixed defect; 3 = reversable defect
# output - the predicted attribute - diagnosis of heart disease (Value 0 = Less Chance ; Value 1 = More Chance)

# 2. Exploratory Data Analysis
library (ggplot2)

# Change Variable Sex, cp, fbs, restecg, exng, slp, caa, thall and output to factor

heart_df$sex <-factor(heart_df$sex,
                         levels = c(1,0),
                         labels = c("Male","Female"))

heart_df$cp <-factor(heart_df$cp,
                      levels = c(1,2,3,0),
                      labels = c("Typical","atypical",
                                 "non anginal", "asymptomatic"))

heart_df$fbs <-factor(heart_df$fbs,
                      levels = c(1,0),
                      labels = c(">120 mg/dl","< 120 mg/dl"))

heart_df$restecg <-factor(heart_df$restecg,
                      levels = c(0,1,2),
                      labels = c("hypertrophy","ST-T Wave Abnormality", "Normal"))

heart_df$exng <-factor(heart_df$exng,
                          levels = c(0,1),
                          labels = c("Yes","No"))

heart_df$slp <-factor(heart_df$slp,
                       levels = c(0,1,2),
                       labels = c("downsloping","flat", "upsloping"))

heart_df$caa <-factor(heart_df$caa,
                      levels = c(0,1,2,3),
                      labels = c("0","1","2","3"))

heart_df$thall <-factor(heart_df$thall,
                      levels = c(1,2,3),
                      labels = c("fixed defect","normal","reversable defect"))

heart_df$output <-factor(heart_df$output,
                         levels = c(1,0),
                         labels = c("More","Less"))

#2.1 Univariate Analysis
ggplot(heart_df, aes(x=age, y=..count..)) + 
  geom_histogram(binwidth = 2, 
                 fill = "grey",
                 color = "BLUE") + 
  geom_density(color = "Red") + 
  ggtitle("Age") + 
  theme(plot.title = element_text(hjust = 0.5))

plottingbar = function(x_column, label){
  ggplot(heart_df, aes(x = x_column)) +
    geom_bar()+
    labs(x = NULL)  + 
    ggtitle(paste (label))+
    theme(plot.title = element_text(hjust = 0.5))
}

plottingbar(heart_df$sex, "Gender")
plottingbar(heart_df$cp, "Chest Pain")
plottingbar(heart_df$fbs, "Fasting Blood Sugar")
plottingbar(heart_df$restecg, "Resting Electocardiograph")
plottingbar(heart_df$exng, "excercise induced angina")
plottingbar(heart_df$slp, "The Slope of peak Excercise ST Seg.")
plottingbar(heart_df$caa, "number of mayor vassel")
plottingbar(heart_df$thall, "Thallium Stress Test")
plottingbar(heart_df$output, "Output")

plottingbp = function(y_row, label, unit){
  ggplot(heart_df, aes(y = y_row))+
    geom_boxplot()+
    labs (y = paste(unit))+
    ggtitle (paste(label))+
    theme(plot.title = element_text(hjust = 0.5))
}

plottingbp(heart_df$age, "Age", "Age")
plottingbp(heart_df$trtbps, "Resting Blood Pressure", "mmHg")
plottingbp(heart_df$chol, "Serum Cholestoral", "mg/dl")
plottingbp(heart_df$thalachh, "Maximum heart rate", "count")
plottingbp(heart_df$oldpeak, "ST Depression induced by exercise relative to rest", "count")

#2.1 Bivariate Analysis
#Factor Variable
plotting1 = function(x_column, label){
  ggplot(heart_df, aes(x = x_column, fill=output))+
    geom_bar(alpha = 0.5)+
    labs(title = paste(label, "vs Output"), 
         x = label)+
    theme (plot.title = element_text(hjust = 0.5))
}

plotting1(heart_df$sex, "Gender")
plotting1(heart_df$cp, "Chest Pain")
plotting1(heart_df$fbs, "Fasting Blood Sugar")
plotting1(heart_df$restecg, "Resting Electocardiograph")
plotting1(heart_df$exng, "excercise induced angina")
plotting1(heart_df$slp, "The Slope of peak Excercise ST Seg.")
plotting1(heart_df$caa, "number of mayor vassel")
plotting1(heart_df$thall, "Thallium Stress Test")

#Integer Variable
plotting2 = function(x_column, label){
  ggplot(heart_df, aes(x = x_column, fill=output))+
    geom_density(alpha=0.2)+
    labs(title = paste(label, "vs Output"), 
         x = label)+
    theme (plot.title = element_text(hjust = 0.5))
}

plotting2(heart_df$age, "Age")
plotting2(heart_df$trtbps, "Resting Blood Press")
plotting2(heart_df$chol, "Cholestoral")
plotting2(heart_df$thalachh, "Maximum heart rate")
plotting2(heart_df$oldpeak, "ST Depression induced by exercise relative to rest")

#2.2 Multivariate Analysis
plottingpoint = function(x_axis,label_x, y_axis,label_y, shape, color){
  ggplot(data = heart_df, aes (x = x_axis, 
                               y= y_axis,
                               shape = shape,
                               color = color))+
    geom_point()+
    labs(x = label_x,
         y = label_y)
}

plottingpoint(heart_df$age, "Age", heart_df$trtbps, "Resting Blood Pressure", heart_df$sex, heart_df$output)
plottingpoint(heart_df$age, "Age", heart_df$chol, "Serum Cholestoral", heart_df$sex, heart_df$output)
plottingpoint(heart_df$age, "Age", heart_df$thalachh, "Maximum heart rate", heart_df$sex, heart_df$output)
plottingpoint(heart_df$age, "Age", heart_df$oldpeak, "ST Depression induced by exercise relative to rest", heart_df$sex, heart_df$output)


#change all factor variable to integer
heart_df$sex <-as.integer(heart_df$sex)
heart_df$cp <-as.integer(heart_df$cp)
heart_df$fbs <-as.integer(heart_df$fbs)
heart_df$restecg <-as.integer(heart_df$restecg)
heart_df$exng <-as.integer(heart_df$exng)
heart_df$slp <-as.integer(heart_df$slp)
heart_df$caa <-as.integer(heart_df$caa)
heart_df$thall <-as.integer(heart_df$thall)

#Pearson Correlation Coefficient
r <- cor(heart_df[1:13])
r <- round(r,3)

#install.packages("psych")
library(psych)
corPlot(heart_df[1:13], cex = 1.2)

# 3. Data Preperation

# 3.1 Data Cleaning
# 3.1.1 Get Outliers and remove Value in variable trtbps
out_trtbps <- boxplot.stats(heart_df$trtbps)$out
out_idx1 <- which(heart_df$trtbps%in%c(out_trtbps))
heart_df_dummy <- heart_df[-out_idx1,]

# 3.1.2 Get Outliers and remove Value in variable chol
out_chol <- boxplot.stats(heart_df_dummy$chol)$out
out_idx2 <- which(heart_df_dummy$chol%in%c(out_chol))
heart_df_dummy2 <- heart_df_dummy[-out_idx2,]

# 3.1.3 Get Outliers and remove Value in variable thalachh
out_thalachh <- boxplot.stats(heart_df_dummy2$thalachh)$out
out_idx3 <- which(heart_df_dummy2$thalachh%in%c(out_thalachh))
heart_df_dummy3 <- heart_df_dummy2[-out_idx3,]

# 3.1.4 Get Outliers and remove Value in variable oldpeak
out_oldpeak <- boxplot.stats(heart_df_dummy3$oldpeak)$out
out_idx4 <- which(heart_df_dummy3$oldpeak%in%c(out_oldpeak))
heart_df_new <- heart_df_dummy3[-out_idx4,]

# 3.1.5 New Bloxplot without outliers
ggplot(heart_df_dummy, aes(y = trtbps))+
  geom_boxplot()+
  labs (y = "mmHg")+
  ggtitle ("Resting Blood Pressure New")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(heart_df_dummy2, aes(y = chol))+
  geom_boxplot()+
  labs (y = "mg/dl")+
  ggtitle ("Serum Cholestoral New")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(heart_df_dummy3, aes(y = thalachh))+
  geom_boxplot()+
  labs (y = "count")+
  ggtitle ("Maximum heart rate")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(heart_df_new, aes(y = oldpeak))+
  geom_boxplot()+
  labs (y = "count")+
  ggtitle ("ST Depression induced by exercise relative to rest")+
  theme(plot.title = element_text(hjust = 0.5))

#3.2 remove rows with missing value
heart_df_new <- na.omit(heart_df_new)

# 3.3 New Data Dimension
d <- dim(heart_df_new)
m <- d[1] # m : number of rows
n <- d[2] # n : number of columns

## 3.4 Training and Testing Division
## train test = 70:30
set.seed(2021)

train_idx <- sample(m, 0.7 * m)
train_idx[1:3]

train_data <- heart_df_new[ train_idx , ]
test_data <- heart_df_new[ -train_idx , ]

# 4. Modeling
## 4.1 Logistic Regression
logit <- glm(formula = output ~.  ,
             data = train_data,
             family = binomial)
summary(logit)

## 4.2 Decision Tree
library(party)
dt <- ctree(formula = output~.,
            data = train_data)
plot(dt)

## 4.3. Random Forest
library(randomForest)
set.seed(2021)
rf <- randomForest(formula = output ~.,
                   data =train_data)
rf

## 4.4. Support Vector Machine (SVM)
library(e1071)
model.svm <- svm(formula = output ~.,
                 data = train_data)
model.svm


# 5. Evaluating
## Actual Value
actual <- test_data$output

#1. predicted Output - Logistic Regression
pred.prob <- predict(logit, test_data, type="response")
pred.prob > 0.5
pred.logit <- factor(pred.prob > 0.5,
                     levels = c(TRUE,FALSE),
                     labels = c("More","Less"))

#2. predicted Output - Decision Tree
pred.dt <- predict(dt, test_data)

#3. prediction Output - Random Forest
pred.rf <- predict(rf, test_data)

#4. prediction Output - SVM
pred.svm <- predict(model.svm, test_data)

# Confusion Matrix
table.logic <- table(actual, pred.logit,
                     dnn = c("actual","predicted"))
table.logic
table.dt <- table(actual, pred.dt,
                  dnn = c("actual","predicted"))
table.dt
table.rf <- table(actual, pred.rf,
                  dnn = c("actual","predicted"))
table.rf
table.svm <- table(actual, pred.svm,
                   dnn = c("actual","predicted"))
table.svm


## performance evaluation
performance <- function(prediction, actual, method){
  #1 Create Confusion Matrix (cm)
  cm <- table(actual, prediction,
              dnn = c("actual","predicted"))
  
  #2. extract TP, TN, FP, FN
  TP <- cm[1,1]
  TN <- cm[2,2]
  FP <- cm[2,1]
  FN <- cm[1,2]
  
  #3. compute accuracy, precision, recall, f1score
  accuracy <- (TP+TN) / (TP+TN+FP+FN)
  precision <- TP/(FP+TP)
  recall <- TP/(FN+TP)
  f1score <- (2*precision*recall)/(precision+recall)
  
  #4. prepare output function
  result <-paste("***Method: ", method,
                 "\n Accuracy = ", round(accuracy,3),
                 "\n Precision = ", round(precision,3),
                 "\n Recall = ", round(recall,3),
                 "\n F1 Score = ", round(f1score,3))
  cat (result)
}

performance(pred.logit, actual, "Logistic Regression")
performance(pred.dt, actual, "decision tree")
performance(pred.rf, actual, "random forest")
performance(pred.svm, actual, "Support vactor Machine")

# 6. Recomendation
## 1. Support Vector Machine algorithm the best among all the tested algorithms with precision 81,6%
## 2. However, the accuracy rate is only about 61.9% for the four algorithms used
## 3. The result can be improved by better data preparation ot using other algorithms
