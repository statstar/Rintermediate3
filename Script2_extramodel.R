aov(hwy~drv, mpg) %>% summary
lm(hwy~drv, mpg) %>% summary

library(mgcv)
library(randomForest)
model1 <- lm(hwy~cty, mpg)
model2 <- gam(hwy~s(cty), data=mpg)
model3 <- randomForest(hwy~cty, data=mpg)

(predict(model1, mpg) - mpg$hwy)^2 %>% sum
(predict(model2, mpg) - mpg$hwy)^2 %>% sum
(predict(model3, mpg) - mpg$hwy)^2 %>% sum

plot(mpg$hwy, predict(model1, mpg), type="p",
     col="blue")
points(mpg$hwy, predict(model2, mpg), col="red", pch=16)
abline(0,1)
