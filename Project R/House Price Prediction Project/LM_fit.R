## Install library
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(MLmetrics)
library(GGally)
library(glmnet)
## Load file and prepare
house_price_india <- read_excel("Downloads/house-price-india-QueryResult.xlsx") %>% as_tibble()
summary(house_price_india)

## check null
anyNA(house_price_india)
colSums(is.na(house_price_india))

## detect outliers
boxplot(house_price_india)

quartiles <- quantile(house_price_india$price,probs = c(0.25,0.75))
iqr <- IQR(house_price_india$price)

upper <- quartiles[2] + 1.5*iqr

## remove outliers
data_no_outliers <- filter(house_price_india,house_price_india$price < upper )
boxplot(data_no_outliers$price)

## split data
train_test_split <- function(data, trainRatio=0.8) {
  set.seed(42)
  n <- nrow(data)
  id <- sample(1:n, size=trainRatio*n)
  train_data <- data[id, ]
  test_data <- data[-id, ]
  
  list(train=train_data, test=test_data) 
}

set.seed(42)
splitData <- train_test_split(data_no_outliers, 0.8)
train_data <- splitData$train
test_data <- splitData$test

#train model
lm_model <- train(price ~ .,
                  data = train_data,
                  method = "lm",
                  preProcess = c("center", "scale"))
train_p <- predict(lm_model)
test_p <- predict(lm_model,newdata = test_data)

train_mae <- mean(abs(train_p-train_data$price))
test_mae <- mean(abs(test_p - test_data$price))

