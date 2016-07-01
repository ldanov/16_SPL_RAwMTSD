reg1 <- lm(Gini.Coefficient.now ~ GDP.now + Inflation.now + Unemployment.now, inputTest)
summary(reg1)
str(summary(reg1))
plot(reg1$residuals)

reg2 <- lm(Gini.Coefficient.fDif ~ GDP.fDif + Inflation.fDif + Unemployment.fDif, inputTest)
summary(reg2)
str(summary(reg2))
plot(reg2$residuals)

kpss.test(reg1$residuals, null = "L")
kpss.test(reg2$residuals, null = "L")

reg3 <- glm(Gini.Coefficient.now ~ GDP.now + Inflation.now + Unemployment.now, inputTest, family = binomial(link = "logit"))
summary(reg3)
str(summary(reg3))
plot(reg3$residuals)