# Lab3 - Shreya Sabharwal
load('BostonData.Rdat')
plot(boston)
# 1. NO.concentration and distance.to.work have a negative correlation.
mod1 <- lm(home.value ~ NO.concentration , data = boston)
summary(mod1)
mod2 <- lm(home.value ~ distance.to.work , data = boston)
summary(mod2)
mod3 <- lm(home.value ~ student.teacher.ratio , data = boston)
summary(mod3)
# 2. 0.2564- student teacher ratio best explains the data
mod.full <- lm(home.value ~ NO.concentration+student.teacher.ratio+distance.to.work, data = boston)
summary(mod.full)
# Adjusted R of this model is better than that of single regression, it fits the data better.

predict(mod.full, newdata=data.frame("distance.to.work"=3, "NO.concentration"=0.35, "student.teacher.ratio"=10), interval="prediction")
plot(fitted(mod.full), resid(mod.full))
abline(h=0, lty=2)
