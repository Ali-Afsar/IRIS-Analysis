
# Install Package Caret

install.packages("caret")

install.packages("care", dependencies = c("Depends", "Suggests"))

install.packages("ellipse")

library(ellipse)
library(caret)

# Attaching dataset to the R environment

data("iris")

# Writing Data to our Local Machine

write.csv(iris,"iris.csv", row.names = FALSE)

# Define the filename

filename <- "iris.csv"

# Load the dataset from local directory

dataset <- read.csv(filename)

# Set the column names for dataset (We do not need this step, because there is already a proper column Name in my iris Dataset)

colnames(dataset) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")

# Create a Training and Validation Dataset

validation_index <- createDataPartition(dataset$Species, p = 0.80, list = FALSE)

## Select 80% of the data for training

traindataset <- dataset[validation_index,]

## Select 20% of the data for Validation

validationdataset <- dataset[-validation_index,]

# Summarize the data set
# 1. Dimension of the Dataset
# 2. Types of Attribute
# 3. Peek at the data itself
# 4. Levels of the class attribute
# 5. Breakdown of the instances in each class
# 6. Statical Summary of all attributes

# 1. Dimesion of the dataset

dim(traindataset)

# Types of Attribute

sapply(traindataset,class)

# 3. Peek at the data

head(traindataset)

# 4. Levels of the class

levels(traindataset$Species)

# 5. Class Distribution

PercentageOfData <- prop.table(table(traindataset$Species)) *100

CountData <- table(traindataset$Species)

cbind(CountData, PercentageOfData)

# 6. Statical Summary

summary(traindataset)

# Visualisation of Dataset
# 1. Univariate plot to better understand each attribute
# 2. Multivariate plot to better understand the relationship between attributes

# 1. Univariate
# Split input(x) and Output(y)

x <- traindataset[,1:4]
y <- traindataset[,5]

# Box Plot for each attribute on one Plot

 par(mfrow = c(1,4))
 for (i in 1:4) {
   boxplot(x[,i], main = names(traindataset)[i])
 }
 
# Bar Plot of Class breakdown
 
 plot(y)
 
# Multivariate (Drwaing scatter plot matrix)
 
featurePlot(x = x, y = y, plot = "ellipse")


# Box and Whisker Plot of each attribute

featurePlot(x = x, y =y, plot = "box")

# Density plots for each attributes by class value

scales <- list(x = list(relation = "free"), y = list(relation = "free"))
featurePlot(x = x, y = y, plot = "density", scales = scales)


# Evaluate some Algorithm
# 1. Set-up the Test Harness to use 10 fold cross validation
# 2. Build 5 different models to predict species from flower measurement
# 3. Select the best Model

# Run Algorithms using 10 fold cross validation

control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

# Build Model
# 1. Linear Discriminant Analysis (LDA)
# 2. Classification and Regression Tree (CART)
# 3. k- Nearest Neighbors (KNN)
# 4. Support Vector Machines (SVM) with a linear kernel
# 5. Random Forest (RF)

# 1. Linear Algorithm

install.packages("e1071")
set.seed(7)
fit.lda <- train(Species ~., data = traindataset, method = "lda", metric = metric, trControl = control)

# 2. Non Linear Algorithm (CART)
set.seed(7)
fit.cart <- train(Species~., data = traindataset, method = "rpart", metric = metric, trControl = control)

# 3. KNN
set.seed(7)
fit.knn <- train(Species~., data = traindataset, method = "knn", metric = metric, trControl = control)

# 4. SVM
set.seed(7)
fit.svm <- train(Species~., data = traindataset, method = "svmRadial", metric = metric, trControl = control)

# 5. Random Forest
set.seed(7)
fit.rf <- train(Species~., data = traindataset, method = "rf", metric = metric, trControl = control)


# Select Best Model based on accuracy
# Summarize accuracy of model

result <- resamples(list(lda = fit.lda, cart = fit.cart, knn = fit.knn, svm = fit.svm, rf = fit.rf))

summary(result)

# Compare Accuracy of Model

dotplot(result) # Now we can see the accuracy is high with lda, so we will choose lda model

# Summarizing the Best Model (LDA)

print(fit.lda)

fit.lda$results

# Making prediction to know how my model fits with my validation data

prediction <- predict(fit.lda, validationdataset)

confusionMatrix(prediction, validationdataset$Species)