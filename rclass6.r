plot(iris$Petal.Length)
plot(iris$Petal.Length, iris$Petal.Width)
plot(iris$Petal.Length, iris$Petal.Width, main = "iris data", xlab = "Petal Length", ylab = "Petal Width", col = iris$Species)
 pairs(~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length, data = iris, col = iris$Species)
hist(iris$Sepal.Width)
x <- aggregate(Petal.Length ~ Species, iris, mean)
barplot(x$Petal.Length, names = x$Species)
x <- aggregate(Petal.Length ~ Species, iris, sum)
pie(x$Petal.Length, labels = x$Species)
x <- tapply(iris$Petal.Length, iris$Petal.Width, mean)
x
plot(x, type = 'o') ''
boxplot(iris$Sepal.Width)