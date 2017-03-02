x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y~x)
summary(fit)

data(mtcars)
y <- mtcars$mpg
x <- mtcars$wt
fit <- lm(y ~ x)
newdata <- data.frame(x=mean(x))
p1 <- predict(fit, newdata, interval="confidence")
print(p1)


newdata2 = data.frame(x=3)
p2 <- predict(fit, newdata2, interval = "prediction")
print(p2)

fit2 <- lm(y ~ I(x/2), data = mtcars)
coef <- summary(fit2)$coefficients
coef[2,1] + c(-1,1)*qt(0.975, df = fit2$df.residual)*coef[2,2]

cf1 <- summary(fit)$coefficients
cf1

fit3 <- lm(y ~ I(x+5), data = mtcars)
cf3 <- summary(fit3)$coefficients
cf3

fit5 <- lm (y ~1)
fit6 <- lm (y ~ x)
anova(fit5)
anova(fit6)
278/1126