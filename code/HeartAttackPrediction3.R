# Heart Attack Prediction

## Author: 
## Date: 
## Brief Description:
## Predict heart attack using regression algorithm.

rm(list = ls())
# Outline:
## 1. Data Extraction
## 2. Exploratory Data Analysis
## 3. Data Preparation
## 4. Modeling
## 5. Evaluation
## 6. Recommendation

## 1. Data Extraction

# read data
Heart_df <- read.csv("data/heart.csv")

# structure of data frame
str(Heart_df)

# data dimension
d <- dim(Heart_df)
m <- d[1] # m : number of rows
n <- d[2] # n : number of columns

# statistical summary
summary(Heart_df)

#Note 
# age - age in years
# sex - sex (1 = male; 0 = female)
# cp - chest pain type (1 = typical angina; 2 = atypical angina; 3 = non-anginal pain; 0 = asymptomatic)
# trestbps - resting blood pressure (in mm Hg on admission to the hospital)
# chol - serum cholestoral in mg/dl
# fbs - fasting blood sugar > 120 mg/dl (1 = true; 0 = false)
# restecg - resting electrocardiographic results (1 = normal; 2 = having ST-T wave abnormality; 0 = hypertrophy)
# thalach - maximum heart rate achieved
# exang - exercise induced angina (1 = yes; 0 = no)
# oldpeak - ST depression induced by exercise relative to rest
# slope - the slope of the peak exercise ST segment (2 = upsloping; 1 = flat; 0 = downsloping)
# caa - number of major vessels (0-3) colored by flourosopy
# thal - 2 = normal; 1 = fixed defect; 3 = reversable defect
# output - the predicted attribute - diagnosis of heart disease (angiographic disease status) (Value 0 = < diameter narrowing; Value 1 = > 50% diameter narrowing)

## 2. Exploratory Data Analysis
library(ggplot2)

# 2.1. Univariate Analysis
ggplot(Heart_df, aes(x=age, y=..count..)) + 
  geom_histogram(binwidth = 2, 
                 fill = "grey",
                 color = "BLUE") + 
  geom_density(color = "Red") + 
  ggtitle("Age") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Heart_df, aes(x =output, 
                     fill =as.character(Heart_df$output)))+
  geom_histogram(binwidth =0.5) + 
  labs(fill = 'Output') + 
  ggtitle ("Count of Output")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Heart_df, aes(x =sex, 
                     fill =as.character(Heart_df$sex)))+
  geom_histogram(binwidth =0.5) + 
  labs(fill = 'Gender') + 
  ggtitle ("Count of Gender")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Heart_df, aes(x =cp, 
                     fill =as.character(Heart_df$cp)))+
  geom_histogram(binwidth =0.5) + 
  labs(fill = 'Chest Pain') + 
  ggtitle ("Count of Chest Pain")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Heart_df, aes(x =fbs, 
                     fill =as.character(Heart_df$fbs)))+
  geom_histogram(binwidth =0.5) + 
  labs(fill = 'fasting blood sugar') + 
  ggtitle ("Count of fasting blood sugar")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Heart_df, aes(x =restecg, 
                     fill =as.character(Heart_df$restecg)))+
  geom_histogram(binwidth =0.5) + 
  labs(fill = 'resting electrocardiographic') + 
  ggtitle ("Count of resting electrocardiographic results")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Heart_df, aes(x =exng, 
                     fill =as.character(Heart_df$exng)))+
  geom_histogram(binwidth =0.5) + 
  labs(fill = 'exercise induced angina') + 
  ggtitle ("Count of exercise induced angina")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Heart_df, aes(x =slp, 
                     fill =as.character(Heart_df$slp)))+
  geom_histogram(binwidth =0.5) + 
  labs(fill = 'the slope of the peak exercise ST segment') + 
  ggtitle ("Count of the slope of the peak exercise ST segment")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Heart_df, aes(x =caa, 
                     fill =as.character(Heart_df$caa)))+
  geom_histogram(binwidth =0.5) + 
  labs(fill = 'number of major vessels') + 
  ggtitle ("number of major vessels")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Heart_df, aes(x =thall, 
                     fill =as.character(Heart_df$thall)))+
  geom_histogram(binwidth =0.5) + 
  labs(fill = 'thall') + 
  ggtitle ("Thallium Stress test")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Heart_df, aes(y = age))+
  geom_boxplot()

ggplot(data = Heart_df, aes(y = trtbps))+
  geom_boxplot()+
  ggtitle("Boxplot of resting blood pressure")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(y = chol))+
  geom_boxplot()+
  ggtitle("Boxplot of serum cholestoral")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(y = thalachh))+
  geom_boxplot()+
  ggtitle("Boxplot of maximum heart rate achieved")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(y = oldpeak))+
  geom_boxplot()+
  ggtitle("Boxplot of ST depression induced by exercise relative to rest")+
  theme(plot.title = element_text(hjust =0.5))

# 2.2. Bivariate Analysis
## Output: 1 = more chance; 0 = Low chance
Heart_df$output <-factor(Heart_df$output,
                         levels = c(1,0),
                         labels = c("M","L"))

ggplot(data = Heart_df, aes(x =age, fill=output))+
  geom_density(alpha =0.2)+
  ggtitle("Distribusion of age according to Output Variable")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(x =sex, fill=output))+
  geom_density(alpha =0.2)+
  ggtitle("Distribusion of Sex according to Output Variable")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(x =cp, fill=output))+
  geom_density(alpha =0.2)+
  ggtitle("Distribusion of cp according to Output Variable")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(x =trtbps, fill=output))+
  geom_density(alpha =0.2)+
  ggtitle("Distribusion of trtbps according to Output Variable")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(x =chol, fill=output))+
  geom_density(alpha =0.2)+
  ggtitle("Distribusion of chol according to Output Variable")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(x =fbs, fill=output))+
  geom_density(alpha =0.2)+
  ggtitle("Distribusion of fbs according to Output Variable")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(x =restecg, fill=output))+
  geom_density(alpha =0.2)+
  ggtitle("Distribusion of restecg according to Output Variable")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(x =thalachh, fill=output))+
  geom_density(alpha =0.2)+
  ggtitle("Distribusion of thalachh according to Output Variable")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(x =exng, fill=output))+
  geom_density(alpha =0.2)+
  ggtitle("Distribusion of exng according to Output Variable")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(x =oldpeak, fill=output))+
  geom_density(alpha =0.2)+
  ggtitle("Distribusion of oldpeak according to Output Variable")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(x =slp, fill=output))+
  geom_density(alpha =0.2)+
  ggtitle("Distribusion of slp according to Output Variable")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(x =caa, fill=output))+
  geom_density(alpha =0.2)+
  ggtitle("Distribusion of caa according to Output Variable")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(x =thall, fill=output))+
  geom_density(alpha =0.2)+
  ggtitle("Distribusion of Thall according to Output Variable")+
  theme(plot.title = element_text(hjust =0.5))

# 2.3. Multivariate Analysis
ggplot(data = Heart_df, aes (x = age,
                             y = trtbps,
                             shape = output,
                             color = output))+
  geom_point()

ggplot(data = Heart_df, aes (x = age,
                             y = chol,
                             shape = output,
                             color = output))+
  geom_point()

ggplot(data = Heart_df, aes (x = age,
                             y = thalachh,
                             shape = output,
                             color = output))+
  geom_point()

ggplot(data = Heart_df, aes (x = age,
                             y = oldpeak,
                             shape = output,
                             color = output))+
  geom_point()

## install.packages("corrgram")
#Pearson's Correlation Coefficient
r <- cor(Heart_df[ , 1:14])
r <- round(r, 3)

library(corrgram)
corrgram(Heart_df, order = TRUE,
         upper.panel = panel.cor,
         main="Heart Attack Variable")

# 3. Data preparation
# 3.1 Feature Extraction/ Selection

# 3.2 Data Cleaning

# 3.2.2 Get Outliers Values
out_trtbps <- boxplot.stats(Heart_df$trtbps)$out
out_chol <- boxplot.stats(Heart_df$chol)$out
out_thalachh <- boxplot.stats(Heart_df$thalachh)$out
out_oldpeak <- boxplot.stats(Heart_df$oldpeak)$out

# 3.2.3 Get indices of Outliers value
out_idx1 <- which(Heart_df$trtbps%in%c(out_trtbps))
out_idx2 <- which(Heart_df$chol%in%c(out_chol))
out_idx3 <- which(Heart_df$thalachh%in%c(out_thalachh))
out_idx4 <- which(Heart_df$oldpeak%in%c(out_oldpeak))

# 3.2.4 Remove Those Rows
Heart_df_1 <- Heart_df[-out_idx1, ]
Heart_df_2 <- Heart_df_1[-out_idx2, ]
Heart_df_3 <- Heart_df_2[-out_idx3, ]
Heart_df <- Heart_df_3[-out_idx4, ]


# New Bloxpot without outliers
ggplot(data = Heart_df, aes(y = trtbps))+
  geom_boxplot()+
  ggtitle("Boxplot of resting blood pressure")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(y = chol))+
  geom_boxplot()+
  ggtitle("Boxplot of serum cholestoral")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(y = thalachh))+
  geom_boxplot()+
  ggtitle("Boxplot of maximum heart rate achieved")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = Heart_df, aes(y = oldpeak))+
  geom_boxplot()+
  ggtitle("Boxplot of ST depression induced by exercise relative to rest")+
  theme(plot.title = element_text(hjust =0.5))

#Implement PCA (note: do  not use target var)
pca.out <- prcomp(Heart_df[ ,1:13], scale = TRUE)

##set k [ k <<n ]
k <- 10
newfeatures <-pca.out$x[,1:k]
newfeatures_df <- data.frame(newfeatures)

newheart_df <- cbind(Heart_df$output, newfeatures_df)
colnames(newheart_df)[1]<-"output"

# data dimension
d <- dim(newheart_df)
m <- d[1] # m : number of rows
n <- d[2] # n : number of columns

# 3.3 Training and Testing Division
##train test = 70:30
set.seed(2021)

train_idx <- sample(m, 0.7 * m)
train_idx[1:3]

train_data <- newheart_df[ train_idx , ]
test_data <- newheart_df[ -train_idx , ]

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

## 5. Evaluation

## Actual Value
actual <- test_data$output

#1. predicted Output - Logistic Regression
pred.prob <- predict(logit, test_data, type="response")
pred.prob > 0.5
pred.logit <- factor(pred.prob > 0.5,
                     levels = c(FALSE,TRUE),
                     labels = c("L","M"))

#2. predicted Output - Decision Tree
pred.dt <- predict(dt, test_data)

#3. prediction Output - Random Forest
pred.rf <- predict(rf, test_data)

#4. prediction Output - SVM
pred.svm <- predict(model.svm, test_data)

## performance evaluation
performance <- function(prediction, actual, method){
  #1 Create Confusion Matrix (cm)
  cm <- table(actual, prediction,
              dnn = c("actual","predicted"))
  
  #2. extract TP, TN, FP, FN
  TP <- cm[2,2]
  TN <- cm[1,1]
  FP <- cm[1,2]
  FN <- cm[2,1]
  
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

##6. recomendation
