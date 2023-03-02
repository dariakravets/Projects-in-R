library(datasets)
data("mtcars")
head(mtcars)

plot(mtcars$mpg, mtcars$am, pch = 19, col = "#0665a5")

t.test(mtcars$mpg~mtcars$am)

mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$am <- factor(mtcars$am,labels=c('Automatic','Manual'))

mult <- lm(mpg~., mtcars)
st <- step(mult, direction = "both")
summary(st)

plot(st)
