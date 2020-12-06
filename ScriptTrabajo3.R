library(readxl)
FoodChoices <- read_excel("C:/Users/Javier Villanueva/Desktop/FoodChoices.xlsx")
View(FoodChoices)

# CLEANING THE DATA
df <- na.omit(FoodChoices)

#SETTING SEED
set.seed(1234)

# DIVIDING THE DATA SET IN TRAIN AND TEST SETS
library(caTools)
df$Gender=factor(df$Gender)
split <- sample.split(df$Gender, SplitRatio = 0.6)
train <- subset(df, split==TRUE)
test <- subset(df, split==FALSE)

# TO MEASURE ACCURACY
accuracy <- function(x){sum(diag(x))/sum(x)}

#LINEAR DISCRIMINANT ANALYSIS
library(MASS)
m.lda <- lda(Gender~., data = train)
p.lda <- predict(m.lda,test)
(t.lda <- ftable(Predicted=p.lda$class, Correct=test$Gender))
accuracy(t.lda)

# QUADRATIC DISCRIMINANT ANALYSIS
m.qda <- qda(Gender~., data = train)
p.qda <- predict(m.qda,test)
(t.qda <- ftable(Predicted=p.qda$class, Correct=test$Gender))
accuracy(t.qda)

# MULTINOMIAL LOGISTIC REGRESSION
library(nnet)
m.mlr <- multinom(Gender~., data = train)
(t.mlr<- ftable(Predicted=predict(m.mlr,test), Correct=test$Gender))
accuracy(t.mlr)

#TREES, RPART ALGORITHM
library(rpart)
m.rp <- rpart(Gender~., data = train)
(t.rp <- ftable(Predicted=predict(m.rp, test, type = "class"), Correct=test$Gender))
accuracy(t.rp)

#SUPPORT VECTOR MACHINES
library(kernlab)
m.svm <- ksvm(Gender~., data=train, scale=FALSE)
(t.svm <- ftable(Predicted=predict(m.svm,test), Correct=test$Gender))
accuracy(t.svm)

#NAIVE BAYES
library(e1071)

m.nb <- naiveBayes(Gender ~ ., data = train, subset)
p.nb <- predict(m.nb, test, threshold = 0.05)
(t.nb <- ftable(Predicted=p.nb, Correct=test$Gender))
accuracy(t.nb)

#CROSS VALIDATION 10
library(caret)
library(klaR)
x = df[,-1]
y = df$Gender
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
predict(model$finalModel,x)
table(predict(model$finalModel,x)$class,y)
t.cv = table(predict(model$finalModel,x)$class,y)
accuracy(t.cv)

#KMEANS ALGORITHM
Results <- kmeans(df[,2:5], 2, nstart=20)
(t.km <- ftable(Predicted=Results$cluster, Correct=df$Gender))
1-accuracy(t.km)

#COMPARING ACCURACIES
A.lda <- accuracy(t.lda)
A.mlr <- accuracy(t.mlr)
A.nb <- accuracy(t.nb)
A.cv <- accuracy(t.cv)
A.svm <- accuracy(t.svm)
A.qda <- accuracy(t.qda)
A.rp <- accuracy(t.rp)
A.km <- (1- accuracy(t.km))

library(plotly)
plot_ly(
  +     x = c("Multinomial", "Naive Bayes", "Linear Discriminant", "Cross Validation", "Support Vector Machines", "Quadratic Discriminant", "Rpart", "Kmeans"),
  +     y = c(A.mlr, A.nb, A.lda, A.cv, A.svm, A.qda, A.rp, A.km),
  +     name = "ACCURACIES",
  +     type = "bar")