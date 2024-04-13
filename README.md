---
title: "Healthcare status prediction: A Supervised machine learning approach"
author: "Adekunle Joseph Damilare"
date: "`r Sys.Date()`"
output:
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r library}
library(tidyverse)
```

## Accessing the dataset

```{r train}
url1 <- "https://raw.githubusercontent.com/josephdamilare01/Data-Mining-Data-Mining-the-Healthcare-Dataset/main/disease_train(5).csv"
```
```{r head}
Train <- read.csv(url1)
head(Train)
```

# Data exploration
``` {r strrrr}
str(Train)
```
## Checking for missing values using dlookr package
```{r a}
library(dlookr)
plot_na_pareto(Train)
```





## Checking for outliers
```{r d}
diagnose_outlier(Train)
plot_outlier(Train)

```

### Cleaning the data and handling data issues
## Handling missing variables and outliers
```{r e}
Train_corrected <- Train %>% mutate(test_X3 = as.numeric( imputate_na(Train, xvar =test_X3, method = "mean")) %>% as.data.frame(), 
                                    test_X2 = as.numeric( imputate_na(Train, xvar =test_X2, method = "mean")) %>% as.data.frame(),
                                    test_X1 = as.numeric( imputate_na(Train, xvar =test_X1, method = "mean")) %>% as.data.frame(),
                                    test_X4 = as.numeric( imputate_na(Train, xvar =test_X4, method = "mean")) %>% as.data.frame(),
                                    test_X5 = as.numeric( imputate_na(Train, xvar =test_X5, method = "mean")) %>% as.data.frame() )
library(visdat)

vis_miss(Train_corrected)
```


## Checking if there is duplicate
```{r f}
length(unique(Train_corrected$id))
length(Train_corrected$id)
```
## We remove test_X6 due to the percent of missing values present in it (96%)
```{r g}
Train_corrected <-Train_corrected %>% select(-c(test_X6, id))
```
## Checking the percentage of empty rows to the decide if to remove it or not
```{r h}
n <- Train_corrected %>% filter(gender == "" )
a <-length(n$sick)
s <- length(Train_corrected$sick)
(a/s)*100 
```
## Filtering out the rows with empty space
``` {r i}
Train_corrected <- Train_corrected %>% filter(!gender =="")
```
### Data exploration
## Descriptive Analysis
```{r j}
library(summarytools)
K1 <- Train_corrected %>% select(starts_with("test_X")) 

summary(K1)
```
## Correlation Analysis
```{r k}
library(corrplot)
l <- cor(K1)
corrplot(l, "pie")
```


## Visualization
```{r l}
ggplot(Train_corrected, aes(x = gender, fill = gender)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100, 2), "%")), vjust = -0.5) +
  facet_wrap(~ target, scale = "free") +
  scale_fill_manual(values = c("red", "blue")) 

ggplot(Train_corrected, aes(x=sick, fill = sick))+geom_bar() + geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100, 2), "%")), vjust = -0.5) +
  facet_wrap(~ target, scale = "free") +
  scale_fill_manual(values = c("red", "blue"))


ggplot(Train_corrected, aes(x=pregnant, fill= pregnant))+geom_bar() + geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100, 2), "%")), vjust = -0.5) +
  facet_wrap(~ target, scale = "free") +
  scale_fill_manual(values = c("red", "blue"))
ggplot(Train_corrected, aes(x= suspect, fill=suspect))+geom_bar() + geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100, 2), "%")), vjust = -0.5) +
  facet_wrap(~ target, scale = "free") +
  scale_fill_manual(values = c("red", "blue"))
ggplot(Train_corrected, aes(x=treatment_type1, fill=treatment_type1))+geom_bar() + geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100, 2), "%")), vjust = -0.5) +
  facet_wrap(~ target, scale = "free") +
  scale_fill_manual(values = c("red", "blue"))
ggplot(Train_corrected, aes(x=surgery, fill= surgery))+geom_bar() + geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100, 2), "%")), vjust = -0.5) +
  facet_wrap(~ target, scale = "free") +
  scale_fill_manual(values = c("red", "blue"))
ggplot(Train_corrected, aes(x=mood_stabiliser, fill= mood_stabiliser))+geom_bar() + geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100, 2), "%")), vjust = -0.5) +
  facet_wrap(~ target, scale = "free") +
  scale_fill_manual(values = c("red", "blue"))
ggplot(Train_corrected, aes(x=mental_health, fill=mental_health))+geom_bar() + geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100, 2), "%")), vjust = -0.5) +
  facet_wrap(~ target, scale = "free") +
  scale_fill_manual(values = c("red", "blue"))
ggplot(Train_corrected, aes(x=medication_B, fill=medication_B))+geom_bar() + geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100, 2), "%")), vjust = -0.5) +
  facet_wrap(~ target, scale = "free") +
  scale_fill_manual(values = c("red", "blue"))
ggplot(Train_corrected, aes(x=disorder, fill=disorder))+geom_bar() + geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100, 2), "%")), vjust = -0.5) +
  facet_wrap(~ target, scale = "free") +
  scale_fill_manual(values = c("red", "blue"))
ggplot(Train_corrected, aes(x=tumor, fill=tumor))+geom_bar() + geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100, 2), "%")), vjust = -0.5) +
  facet_wrap(~ target, scale = "free") +
  scale_fill_manual(values = c("red", "blue"))
ggplot(Train_corrected, aes(x=enlargement, fill=enlargement))+geom_bar() + geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100, 2), "%")), vjust = -0.5) +
  facet_wrap(~ target, scale = "free") +
  scale_fill_manual(values = c("red", "blue"))
ggplot(Train_corrected, aes(x=concern_type2, fill= concern_type2))+geom_bar() + geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100, 2), "%")), vjust = -0.5) +
  facet_wrap(~ target, scale = "free") +
  scale_fill_manual(values = c("red", "blue"))
ggplot(Train_corrected, aes(x=concern_type1, fill=concern_type1))+geom_bar() + geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100, 2), "%")), vjust = -0.5) +
  facet_wrap(~ target, scale = "free") +
  scale_fill_manual(values = c("red", "blue"))

```


### Data preprocessing - Scaling method
``` {r z}

Model_data <- scale(K1)
```
## Adding the Target variable to model data
``` {r m}
target <- Train_corrected %>% select(target)
Model_data <- cbind(Model_data, target)
```

## Splitting the dataset into 25% test and 75% train set
``` {r n}
library(caTools)
sample <- sample.split(Model_data$target, SplitRatio = 0.75)
Model_data_Train <- subset(Model_data, sample==T)
Model_data_Test <- subset(Model_data, sample==F)
```
### Model buiding and training with parameter tuning - Cart Decision tree
```{r y}
library(caret)
ctrl <- trainControl(method = "cv", number = 10)
tunegrid <- expand.grid(.cp = seq(0.01, 0.5, by = 0.01))
model <- train(target ~ ., data = Model_data, method = "rpart", trControl = ctrl, tuneGrid = tunegrid)
```
## Selecting the best decison tree based on the parameter
```{r o}
best_model <- model$finalModel
```
## Visualizing the tree
``` {r p}
library(rpart.plot)
rpart.plot(best_model)
```




### Evaluating the performance of the decision tree
```{r q}
pp <- predict(best_model, newdata = Model_data_Test, type = "class")
con_pp <- confusionMatrix(as.factor(Model_data_Test$target), pp)
con_pp
```
### Building a support vector machine model
```{r r}
library(e1071)
sv <- svm(as.factor(target) ~., data = Model_data_Train, cost = 10, kernel = "sigmoid", scale = T)
summary(sv)
```
## Tuning the svm model
```{r s}
tune <- tune(svm, as.factor(target)~., data = Model_data_Train, kernel="sigmoid", ranges = list(cost = c(0.1, 1,10,20,30,40,50)))
```
```{r t}
summary(tune)
```
## We then retrained the model with cost value of 0.1
```{r v}
sv_r <- svm(as.factor(target) ~., data = Model_data_Train, cost = 1, kernel = "sigmoid", scale = T)
summary(sv_r)
```
## Model evaluation: Retrained vs trained
```{r w}
sv_pred <- predict(sv, newdata = Model_data_Test, type ="class")
acc <- confusionMatrix(as.factor(Model_data_Test$target), sv_pred)
acc
```

```{r x}
sv_rp <- predict(sv_r, newdata = Model_data_Test, type ="class")
acc_r <- confusionMatrix(as.factor(Model_data_Test$target), sv_rp)
acc_r
```
## Conclusion
In summary, the Decision Tree model exhibited superior performance in accurately predicting medical diagnoses with three categories (high risk, low risk, and moderate) compared to the Support Vector Machine model, achieving higher overall accuracy, precision, and agreement with actual diagnoses.
