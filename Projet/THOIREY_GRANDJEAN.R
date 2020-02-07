### Projet Iris statistic

install.packages('caret')
library(caret)
install.packages('e1071')
install.packages('kernlab')


data("iris")
### 1. Data exploration
## 1.1


setosa <- iris$Sepal.Length[iris$Species == "setosa"]
versicolor <- iris$Sepal.Length[iris$Species == "versicolor"]
virginica <- iris$Sepal.Length[iris$Species == "virginica"]
sl = iris[["Sepal.Length"]] #Numeric
sw = iris[["Sepal.Width"]]  #Numeric
pl = iris[["Petal.Length"]] #Numeric
pw = iris[["Petal.Width"]]  #Numeric
sp = iris[["Species"]]      #Qualitative

## 1.2

summary(iris)
#Sepal.Length    Sepal.Width     Petal.Length    Petal.Width          Species  
#Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100   setosa    :50  
#1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300   versicolor:50  
#Median :5.800   Median :3.000   Median :4.350   Median :1.300   virginica :50  
#Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199                  
#3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800                  
#Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500                  

## 1.3 and 1.4
# Frequency for each categorical variables
table(sp)
#setosa versicolor  virginica 
#50         50         50


### 2. Graphic data reprensentation

## 2.5
# pie, barplot, dotchart
pie(summary(sp))
barplot(summary(sp))
dotchart(summary(sp))
# Data are equaly distributed

## 2.6
# Histogram for each numeric variable
hist(sl)
hist(sw)
hist(pl)
hist(pw)

## 2.7
# Histogram for each numeric variable per species
qplot(x = Sepal.Width,
      data = iris,
      binwidth = 0.2,
      fill = Species,
      xlab = "Sepal Width (cm)")

qplot(x = Sepal.Length,
      data = iris,
      binwidth = 0.2,
      fill = Species,
      xlab = "Sepal Length (cm)")

qplot(x = Petal.Width,
      data = iris,
      binwidth = 0.2,
      fill = Species,
      xlab = "Petal Width (cm)")

qplot(x = Petal.Length,
      data = iris,
      binwidth = 0.2,
      fill = Species,
      xlab = "Petal Length(cm)")

ggpairs(iris)

## 2.8
# Plot and Sunflowerplot 
plot(iris$Petal.Length, iris$Petal.Width)
sunflowerplot(iris$Petal.Length, iris$Petal.Width)
# With the plot we can see that some of observations have been plotted on top of each other.
# Sunflowerplot indicates this number via the petal of the sunflower 

## 2.9
# Scatter plot
pairs(iris[1:5], main = "Edgar Anderson's Iris Data", pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])
# Petal.Length and Petal.Width are the most useful features to identify various flower types.
# While Setosa can be easily identified (linearly separable, red points), virginica and Versicolor have 
# some overlap (almost linearly separable).

### 3. Regression Analysis
## 3.10
# Khi_Square
chisqmatrix <- function(x) {
   names = colnames(x);  num = length(names)
   m = matrix(nrow=num,ncol=num,dimnames=list(names,names))
   for (i in 1:(num-1)) {
      for (j in (i+1):num) {
         m[i,j] = chisq.test(x[,i],x[,j],)$p.value
      }
   }
   return (m)
}
chisq_iris = chisqmatrix(iris)
chisq_iris

# As we can see from the result, the p-value is smaller than the threshold value of 5% for each pairs of category
# except for the Sepal Length and the the Sepal Width.
# This enable us to safely reject the null hypothesis and accept the alternalte hypothesis.


## 3.11

## Classification Tree

#building the classification tree
#install if necessary
install.packages("tree")
library(tree)
tree1 <- tree(Species ~ Petal.Length + Petal.Width, data = iris)
summary(tree1)
#Classification tree:
#   tree(formula = Species ~ Petal.Length + Petal.Width, data = iris)
#Number of terminal nodes:  5 
#Residual mean deviance:  0.157 = 22.77 / 145 
#Misclassification error rate: 0.02667 = 4 / 150 
plot(tree1)
text(tree1)


tree1 <- tree(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data = iris)
summary(tree1)

#Classification tree:
#tree(formula = Species ~ Sepal.Width + Sepal.Length + Petal.Length + 
#           Petal.Width, data = iris)
#Variables actually used in tree construction:
#Number of terminal nodes:  6 
#Residual mean deviance:  0.1253 = 18.05 / 144 
#Misclassification error rate: 0.02667 = 4 / 150
plot(tree1)
text(tree1)

##########

## Linear Discriminant Analysis


# Split data into train set and test set with a ratio of 80/20
data_split <- createDataPartition(iris$Species, p = 0.8, list = FALSE)

test <- iris[-data_split,] # 20%
train_ <- iris[data_split,] # 80%

# List of types for attributes
sapply(train_, class)


# List of y class levels
levels(train_$Species)

# train set repartition
percentage <- prop.table(table(train_$Species)) * 100
cbind(freq=table(train_$Species), percentage=percentage)

summary(train_)

# Split dataset into x and y, y being class labels
x <- train_[,1:4] 
y <- train_[,5]

# density plots by class value for each attribute
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# ML Algorithm Evaluation
# Algorithms will be assessed using 10-fold crossvalidation, setup here
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# build models

# Linear Discriminant Analysis
set.seed(7)
fit.lda <- train(Species~., data=train_, method="lda", metric=metric, trControl=control)

# Best Model Summary
print(fit.lda)

# Evaluate LDA model on test data
predictions <- predict(fit.lda, test)
confusionMatrix(predictions, test$Species)