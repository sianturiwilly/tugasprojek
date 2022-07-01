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

# structure of dataframe
str(Heart_df)

# data dimension
d <- dim(Heart_df)
m <- d[1] # m : number of rows
n <- d[2] # n : number of columns

# statistical summary
summary(Heart_df)

## 2. Exploratory Data Analysis
library(ggplot2)

#@ Change sex from int to factor
Heart_df$sex <- factor(Heart_df$sex)
Heart_df$output <- factor(Heart_df$output)

# 2.1. Univariate Analysis
ggplot(data = Heart_df, aes(x = age)) + 
  geom_bar()+
  labs(title ="Univariate variable age")

ggplot(data = Heart_df, aes(x = sex)) + 
  geom_bar()+
  labs(title = "Univariate variable Sex")

# 2.2. Bivariate Analysis
ggplot(data = Heart_df, aes(x = age, fill = sex))+
  geom_density(alpha = 0.2)

ggplot(data = Heart_df, aes(x = cp, fill = sex))+
  geom_density(alpha = 0.2)

ggplot(data = Heart_df, aes(x = trtbps, fill = output))+
  geom_density(alpha = 0.2)

ggplot(data = Heart_df, aes(x = chol, fill = output))+
  geom_density(alpha = 0.2)

ggplot(data = Heart_df, aes(x = fbs, fill = output))+
  geom_density(alpha =0.2)

ggplot(data = Heart_df, aes(x = restecg, fill = output))+
  geom_density(alpha =0.2)

ggplot(data = Heart_df, aes(x = thalachh, fill = output))+
  geom_density(alpha =0.2)

ggplot(data = Heart_df, aes(x = exng, fill = output))+
  geom_density(alpha =0.2)

ggplot(data = Heart_df, aes(x = oldpeak, fill = output))+
  geom_density(alpha =0.2)

ggplot(data = Heart_df, aes(x = slp, fill = output))+
  geom_density(alpha =0.2)

ggplot(data = Heart_df, aes(x = caa, fill = output))+
  geom_density(alpha =0.2)

ggplot(data = Heart_df, aes(x = thall, fill = output))+
  geom_density(alpha =0.2)

# 2.3. Multivariate Analysis


