# Weighted regression analysis
# data set gestation, 1513 births are represented

library(GLMsData)
data("gestation")
summary(gestation)

plot( Weight ~ Age, data=gestation, las=1, pch=ifelse( Births<50, 1, 19),
      xlab="Gestational age (weeks)", ylab="Mean birthweight (kg)",
      xlim=c(20, 45), ylim=c(0, 4))

#There 3 babies born at 30 weeks; 
#this information should be weighted accordingly.
wlm=lm(Weight ~ Age, data=gestation,weights = Births)
summary(wlm)
RSSw=sum(gestation$Births*residuals(wlm)^2)


olm=lm(Weight ~ Age, data=gestation)
oRSS=sum(residuals(olm)^2)
summary(olm)

plot(Weight ~ Age, data=gestation, pch=ifelse( Births<30, 1, 19),
      las=1, xlim=c(20, 45), ylim=c(0, 4),
      xlab="Gestational age (weeks)", ylab="Mean birthweight (in kg)" )

abline(coef(olm), lty=2, lwd=2)
abline(coef(wlm), lty=1, lwd=2)
legend("topleft", lwd=c(2, 2), bty="n",
         lty=c(2, 1, NA, NA), pch=c(NA, NA, 1, 19), # NA shows nothing
         legend=c("Ordinary regression", "Weighted regression",
                  "Based on 30 or fewer obs.","Based on more than 30 obs."))

# We can observe that the weighted line is closer to the observations that are made of obs. 
# having more than 20 births.

fitv=predict(wlm,se.fit = T)
ci.lo=fitv$fit - qt(p = 0.975,df = wlm$df.residual)*fitv$se.fit
ci.up=fitv$fit + qt(p = 0.975,df = wlm$df.residual)*fitv$se.fit
lines(gestation$Age,ci.lo,col="red")
lines(gestation$Age,ci.up,col="red")


par(mfrow=c(2,2))
plot(olm)
plot(wlm)



