reg.now = lm(Gini.Coefficient.now ~ GDP.now + Inflation.now + Unemployment.now, input)
summary(reg.now)
str(summary(reg.now))
plot(reg.now$residuals)

reg.fd = lm(Gini.Coefficient.fd ~ GDP.fd + Inflation.fd + Unemployment.fd, input)
summary(reg.fd)
str(summary(reg.fd))
plot(reg.fd$residuals)

kpss.test(reg.now$residuals, null = "L")
kpss.test(reg.fd$residuals, null = "L")

reg.now.logit = glm(Gini.Coefficient.now ~ GDP.now + Inflation.now + Unemployment.now, input, family = binomial(link = "logit"))
summary(reg.now.logit)
str(summary(reg.now.logit))
plot(reg.now.logit$residuals)
