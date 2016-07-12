# Machinelearning_PROJECT
---
title: "Meachine learning"
output: html_document
---

#Reading and Data exploration 


```{r}
library(caret)
library(randomForest)
library(ggthemes)
library(gridExtra)
library(ggplot2)
library(grid)
train <- read.csv('D:/R/lession_8/pml-training.csv')
test <- read.csv('D:/R/lession_8/pml-testing.csv')

```

##Keeping the required variables for analysis by dropping variables with no data.

```{r}
train = read.csv("pml-training.csv",header=TRUE)
train_used = train[,c(8:11,37:49,60:68,84:86,102,113:124,140,151:160)]

```

###The raw dataset contained 19622 rows of data, with 160 variables. Many variables contained largely missing data (usually with only one row of data), so these were removed from the dataset. In addition, variables not concerning the movement sensors were also removed. This resulted in a dataset of 53 variables.

##Data partation into training and test set in the ration of 60 40 respectively

```{r}
train_part = createDataPartition(train_used$classe, p = 0.6, list = FALSE)
training = train_used[train_part, ]
testing = train_used[-train_part, ]

```


##Building model by using randomforsets with 500 trees

```{r}

set.seed(1111)
random_forest=randomForest(classe~.,data=training,ntree=500,importance=TRUE)
random_forest
```


##Plotting graph for number of trees vs error rate.

```{r}
plot(random_forest,main="Random Forest: Error Rate vs Number of Trees")
```


##Sorting and plotting mean decrease in accuracy and mean decrease in gini scores  to know highltimportance variables.

```{r}

imp=importance(random_forest)
impL=imp[,c(6,7)]
imp.ma=as.matrix(impL)
imp.df=data.frame(imp.ma)

write.csv(imp.df, "imp.df.csv", row.names=TRUE)
imp.df.csv=read.csv("imp.df.csv",header=TRUE)

colnames(imp.df.csv)=c("Variable","MeanDecreaseAccuracy","MeanDecreaseGini")
imp.sort =  imp.df.csv[order(-imp.df.csv$MeanDecreaseAccuracy),] 

imp.sort = transform(imp.df.csv, 
                     Variable = reorder(Variable, MeanDecreaseAccuracy))

VIP=ggplot(data=imp.sort, aes(x=Variable, y=MeanDecreaseAccuracy)) + 
  ylab("Mean Decrease Accuracy")+xlab("")+
  geom_bar(stat="identity",fill="skyblue",alpha=.8,width=.75)+ 
  coord_flip()+theme_few() 

imp.sort.Gini <- transform(imp.df.csv, 
                           Variable = reorder(Variable, MeanDecreaseGini))

VIP.Gini=ggplot(data=imp.sort.Gini, aes(x=Variable, y=MeanDecreaseGini)) + 
  ylab("Mean Decrease Gini")+xlab("")+
  geom_bar(stat="identity",fill="skyblue",alpha=.8,width=.75)+ 
  coord_flip()+theme_few() 

VarImpPlot=arrangeGrob(VIP, VIP.Gini,ncol=2)
grid.draw(VarImpPlot)

```

##Predicting from the model that built on test set of data.

```{r}
test_predictions = predict(random_forest, newdata=testing)
confusionMatrix(test_predictions,testing$classe)
```


###The model was applied to the testing dataset and generated predictions for the class of weightlifting type. Above is the code that was used and the confusion matrix for the testing dataset. The accuracy is very high, at over 99%. The model accurately predicted all of the 20 test subjects.
