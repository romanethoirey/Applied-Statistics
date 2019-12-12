### Projet Iris statistic

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
# Sepal Length
summary(sl)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.300   5.100   5.800   5.843   6.400   7.900 

# Sepal Width
summary(sw)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.000   2.800   3.000   3.057   3.300   4.400 

# Petal Length
summary(pl)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   1.600   4.350   3.758   5.100   6.900 

# Petal Width
summary(pw)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.100   0.300   1.300   1.199   1.800   2.500

## 1.3 and 1.4
# Frequency for each categorical variables
summary(sp) / length(sp)
#    setosa versicolor  virginica 
# 0.3333333  0.3333333  0.3333333


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
#install.packages("ggplot2")
library(ggplot2); library(GGally)

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
#https://svaditya.github.io/oldblog/chi_square_and_t_tests_on_iris_data.html
#https://tobiasrausch.com/courses/ml2019/dataExploration.html
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



#https://warwick.ac.uk/fac/sci/moac/people/students/peter_cock/r/iris_lm/
# Linear Model
lm(Petal.Length ~ Petal.Width, data=iris)
ggplot(data=iris, aes(x=Sepal.Length, y=Petal.Width)) + geom_point() + geom_smooth(method="lm")


iris$Petal.Width.Bin = cut(iris$Petal.Width, breaks=4, labels=c("tiny", "small", "large", "xlarge"))
iris$Petal.Width.Bin = factor(iris$Petal.Width.Bin)
ggplot(data=iris, aes(x=Species, y=Petal.Width.Bin)) + geom_jitter(aes(color=Petal.Width.Bin), width=0.2, height=0.2)

chisq.test(x=iris$Species, y=iris$Petal.Width.Bin)










