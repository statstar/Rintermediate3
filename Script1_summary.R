# 데이터 분석
# Import -> tidy -> Model -> 
  
# 데이터탐색 

vote <- c(rep("A",187), rep("B",59), rep("C",13))
table(vote)
table(vote)/sum(table(vote))
binom.test(x=590, n=2590, p=0.25)


library(ggplot2)
library(dplyr)
head(mpg) %>% data.frame

mpg$manufacturer %>% table
mpg$manufacturer %>% table %>% barplot

mpg %>% 
  count(manufacturer, sort=T)

summary(mpg$displ)
hist(mpg$displ)
boxplot(mpg$displ)

# 시각적으로 정규성 확인
qqnorm(mpg$displ)
qqline(mpg$displ)

shapiro.test(mpg$displ)
3.936e-08
3.936* 10^-8

## 일표본 t검정
mean(mpg$displ)

# 귀무가설 : 모평균 3.7
t.test(mpg$displ, mu=3.7)

head(mpg) %>% data.frame
# 이변량 분석
## 범주형 * 범주형
## 실린더의 수와 자동차의 구동방식간에 연관이 있을까?
table(mpg$cyl)
table(mpg$drv)
mpg %>% 
  mutate(cyl=ifelse(cyl==5, 4, cyl)) -> mpg

table(mpg$cyl)
table(mpg$cyl, mpg$drv)

table(mpg$cyl, mpg$drv) %>% 
  barplot(beside=T, legend=rownames(.))

table(mpg$cyl, mpg$drv) %>% 
  chisq.test()

## 범주형 * 연속형
table(mpg$drv)
summary(mpg$cty)

### 1. 시각적 확인
boxplot(cty ~ class, mpg) #앞은 연속형 ~ 뒤는 범주형

### 2. 요약값
mpg %>% 
  group_by(class) %>% 
  summarise(n=n(), mean=mean(cty), sd=sd(cty)) %>% 
  arrange(-mean)

### 3. 등분산성 검정
bartlett.test(cty ~ drv, mpg)

### 4. 통계적가설검정
result <- aov(cty ~ drv, mpg)

TukeyHSD(result)

## 연속형 * 연속형
names(mpg)
### 1. 시각적
plot(hwy~cty, mpg)
abline(lm(hwy~cty, mpg), col="blue")
### 2. 상관성
cor(mpg$hwy, mpg$cty)

### 3. 회귀분석
out <- lm(hwy~cty, mpg)
summary(out)

# 정규성
qqnorm(out$residuals)
qqline(out$residuals)
shapiro.test(out$residuals)

library(car)
boxCox(out, lambda = seq(1, 2, 0.01))

# 독립성
plot(out$fitted.values, out$residuals) # x 예측값, y 잔차
durbinWatsonTest(out)

# 등분산성
spreadLevelPlot(out)
ncvTest(out)

## 다중회귀모형
names(mpg)

out <- lm(hwy~cty+fl+class+drv+factor(cyl)+displ, data=mpg)
summary(out)

influencePlot(out)



lm(hwy~cyl, data=mpg)
lm(hwy~factor(cyl), data=mpg)

## vif(다중공선성)
vif(out)

## 모형선택
library(leaps)
out2 <- regsubsets(hwy~cty+fl+class+drv+factor(cyl)+displ, data=mpg)
plot(out2, scale="adjr")
plot(out2, scale="bic")
plot(out2, scale="Cp")


states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
head(states)
fit=lm(Murder~Population+Illiteracy+Income+Frost,data=states)
influencePlot(fit)

which(rownames(states) %in% c("Nevada", "Rhode Island", 
                        "Alaska"))

fit2=lm(Murder~Population+Illiteracy+Income+Frost,
       data=states[c(-2,-28,-39),])

summary(fit)
summary(fit2)






