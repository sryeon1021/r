head(iris)
tail(iris)
summary(iris)
summary(iris$Sepal.Length)
str(iris)
View(iris)
dim(iris)
nrow(iris)
ncol(iris)
length(iris)
ls()
split(iris,iris$Species)
subset(iris, Sepal.Length>=7)
substr(iris$Species,1,3)
 iris[order(iris$Sepal.Length),]
 aggregate(Petal.Length ~ Species, iris, mean)