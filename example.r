head(iris)
set.seed(1234)
sel <- sample(1:150,120)

train <- iris[sel, ]
test  <- iris[-sel, ]

m1 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + 
                          Petal.Width + factor(Species) , train)
m2 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + 
          Petal.Width , train)
m3 = lm(Sepal.Length ~ Sepal.Width + Petal.Width , train)

predict(model, test) # 예측값

sum((test$Sepal.Length - predict(m1, test))^2)
sum((test$Sepal.Length - predict(m2, test))^2)
sum((test$Sepal.Length - predict(m3, test))^2)

library(e1071)
m4 = svm(Sepal.Length ~ Sepal.Width + Petal.Length + 
          Petal.Width + factor(Species) , train)
sum((test$Sepal.Length - predict(m4, test))^2)

library(randomForest)
m5 = randomForest(Sepal.Length ~ Sepal.Width + Petal.Length + 
           Petal.Width + Species , train, importance=T)
sum((test$Sepal.Length - predict(m5, test))^2)
varImpPlot(m5)

library(neuralnet)
m6 = neuralnet(Sepal.Length ~ Sepal.Width + Petal.Length + 
                    Petal.Width, train, 
               hidden = c(5, 3))
sum((test$Sepal.Length - predict(m6, test))^2)
plot(m6)


