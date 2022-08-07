library(GLMsData)
library(tidyverse)
library(ggfortify)
data("danishlc")
# The number of incidents of lung cancer
# from 1968 to 1971 in 4 Danish cities,
# data is recorded by age group

# using the number of cases is not relevant
# the population sizes by age group are
# different
# Let's then consider a rate of lung cancer,
# such the number of cases / pop

# Data visualization
AgeO=ordered(danishlc$Age,levels=c("40-54","55-59","60-64","65-69","70-74",">74"))
plot(log(Cases/Pop) ~ AgeO ,data = danishlc)
attach(danishlc)
interaction.plot(x.factor = AgeO, trace.factor = City,response = Cases/Pop,
                 fun = mean)
# Poisson model
m.pois=glm(Cases ~ Age+City + offset(log(Pop)),
           data=danishlc,
           family = poisson(link = "log") )



anova(m.pois, test = "Chisq")
summary(m.pois)

# Keep only Age
m.pois2= update(m.pois, .~ offset(log(Pop))  + relevel(Age,"40-54"))
summary(m.pois2)
pchisq(28.307,18,lower.tail = F)

autoplot(m.pois2)
# we can consider Age as quantitative since the
# categories are not equally spaced

danishlc$Age1 = rep(c(40,55,60,65,70,75),4)

m.pois3 = update(m.pois, .~ offset(log(Pop))  + Age1)
summary(m.pois3)

pchisq(48.968,22,lower.tail = F)
par(mfrow=c(2,2))
plot(m.pois3)


# Quadratic Age
m.pois4 = update(m.pois, .~ offset(log(Pop))  + poly(Age1,2,raw = T))
summary(m.pois4)
pchisq(32.50,21,lower.tail = F)
par(mfrow=c(2,2))
plot(m.pois4)


# Interpretation of model pois 2
#(Intercept)    Age40-54    Age55-59    Age60-64    Age65-69    Age70-74 
#0.01163227  0.24456340  0.72184932  1.09789513  1.40776870  1.55106436

# Randomized Quantile residuals - optional
library(statmod)
par(mfrow=c(2,2))
scatter.smooth(qres.pois(m.pois) ~ (fitted(m.pois)),
               ylab="Standardized residuals", xlab="Fitted values",
               main="Factor Age*City model", las=1 )
scatter.smooth(qres.pois(m.pois3) ~ (fitted(m.pois3)),
                  ylab="Standardized residuals", xlab="Fitted values",
                  main="Numeric age model", las=1 )
scatter.smooth(qres.pois(m.pois2) ~ (fitted(m.pois2)),
               ylab="Standardized residuals", xlab="Fitted values",
               main="Factor age model", las=1 )
scatter.smooth(qres.pois(m.pois4) ~ (fitted(m.pois4)),
               ylab="Standardized residuals", xlab="Fitted values",
               main="Quadratic age model", las=1 )



anova(m.pois3,m.pois4,test="Chisq")

AIC(m.pois,m.pois2,m.pois3,m.pois4)


###################Overdispersion##############
# DATA
data(pock)

# Ploting
plot(Count ~ log2(Dilution), data=pock, las=1,
     xlab="Log2 of dilution",
     ylab = "Pockmark count")
boxplot(Count ~ log2(Dilution), data= pock)

pock %>% group_by(Dilution) %>% summarise(mc=mean(Count), vc=var(Count))

#The variance increases with the mean
plot((v) ~ (m))
coef(lm(log(v) ~ log(m)))


############ Poisson
model.pois=glm(Count ~ log2(Dilution),
           data=pock,
           family = poisson(link = "log") )

summary(model.pois)


autoplot(model.pois)
# Deviance GOF
pchisq(290.44,46,lower.tail = F)

#Pearson GOF
X2=sum(resid(model.pois,type="pearson")^2)
pchisq(X2,46,lower.tail = F)

#Test overdispersion
library(AER)
dispersiontest(model.pois,trafo = 2,alternative = "g")
# trafo =1 : quasi-poisson
# trafo = 2 : NB
# "g": overdispersion


################## Negative binomial
library(MASS)
nb = glm.nb(Count ~ log2(Dilution) , data=pock)
summary(nb)
# Theta is parameter K.
# Variance = mu + m^2/k
par(mfrow=c(2,2))
plot(nb)

pchisq(50.857,46,lower.tail = F)


#quasi-Poisson
qpois = glm(Count ~ log2(Dilution),
            data=pock,
            family = quasipoisson(link = "log"))


summary(qpois)

autoplot(qpois)


# Interpretation
# The expected count of pockmarks decreases by a factor
# of about exp(-0.70)~0.5
# for every 2 fold increase in dilution
#original 1 2 4 8 16
#log2()   0 1 2 3 4

