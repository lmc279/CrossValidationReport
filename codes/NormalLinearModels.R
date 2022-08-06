# Example lungcap (Lung Capacity) data
#install.packages("GLMsData")
library(GLMsData) # Load the GLMsData, you need to install it first
library(help="GLMsData")
library(tidyverse)
library(ggfortify)

## What is this?
data(lungcap)
??lungcap
# This study explored the relationship between lung capacity (FEV) 
# and smoking status, 
# age, height, and gender

head(lungcap) # few first data points in the data
str(lungcap) # gives the structure of the data frame
summary(lungcap) # provide a summary


# quantitative (numeric) variables 
# qualitative/categorical (Factors) variables
lungcap$Smoke= factor(lungcap$Smoke, 
                      levels = c(0,1), 
                      labels = c("NonSmoker","Smoker"))

summary(lungcap)

#################### DATA VIZ #############################
par(mfrow=c(2,2))
#par(mar=c(1,1,1,1))
#dev.off()
plot(FEV ~ Age , 
      data = lungcap, 
      main="FEV vs. Age",
      xlab="Age (in years)", 
      ylab="FEV (in liters)",
      las=1,
      xlim=c(0,20),
      ylim=c(0,6))

plot(FEV ~ Ht, 
      data=lungcap, 
      main="FEV vs height",
      xlab="Height (in inches)",
      ylab="FEV (in L)",
      las=1, ylim=c(0, 6) )


plot( FEV ~ Gender, 
      data=lungcap,
      main="FEV vs gender", 
      ylab="FEV (in L)",
      las=1, 
      ylim=c(0, 6))

plot( FEV ~ Smoke, 
      data=lungcap, 
      main="FEV vs Smoking status",
      ylab="FEV (in L)", 
      xlab="Smoking status",las=1, ylim=c(0, 6))


# Examine the relationship among 3 variables
plot( FEV ~ Age,
      data=subset(lungcap, Smoke=="Smoker"), 
      main="FEV vs age\n for smokers", 
      ylab="FEV (in L)", xlab="Age (in years)",
      ylim=c(0, 6), xlim=c(0, 20), las=1)
plot( FEV ~ Age,
      data=subset(lungcap, Smoke=="NonSmoker"),
      main="FEV vs age\nfor non-smokers",
      ylab="FEV (in L)", xlab="Age (in years)",
      ylim=c(0, 6), xlim=c(0, 20), las=1)
plot( FEV ~ Ht, data=subset(lungcap, Smoke=="Smoker"),
        main="FEV vs height\nfor smokers",
        ylab="FEV (in L)", xlab="Height (in inches)",
        xlim=c(45, 75), ylim=c(0, 6), las=1)
plot( FEV ~ Ht, data=subset(lungcap, Smoke=="NonSmoker"),
        main="FEV vs height\nfor non-smokers",
        ylab="FEV (in L)", xlab="Height (in inches)",
        xlim=c(45, 75), ylim=c(0, 6), las=1)


# Plot with ggplot
lungcap %>% 
  ggplot(aes(x=Age,y=FEV)) + 
  geom_point()+ 
  geom_smooth() +
  facet_grid(Smoke~Gender)

par(mfrow=c(1,1))
AgeAdjust <- lungcap$Age + ifelse(lungcap$Smoke=="Smoker", 0, 0.5)
plot( FEV ~ AgeAdjust, data=lungcap,
      pch = ifelse(Smoke=="Smoker", 3, 20),
      xlab="Age (in years)", ylab="FEV (in L)", main="FEV vs age", las=1)


plot( FEV ~ Age, data=lungcap,
      col = ifelse(Smoke=="Smoker", 2, 1),pch=20,
      xlab="Age (in years)", ylab="FEV (in L)", main="FEV vs age", las=1)
legend("topleft", pch=20,col=c(2,1), legend=c("Smokers","NonSmokers"))

# Interactions plots
par(mfrow=c(1,1))
interaction.plot(x.factor = lungcap$Smoke,
                 trace.factor = lungcap$Gender,
                 response = lungcap$FEV)
with(data = lungcap,interaction.plot(Smoke,Gender,FEV,xlab="Smoking status", ylab="FEV (in L)",
                                     main="Mean FEV, by gender\n and smoking status", las=1))
with(data = lungcap,interaction.plot(Smoke,Gender,Age,xlab="Smoking status", ylab="Age (in years)",
                                     main="Mean Age, by gender\n and smoking status", las=1))


### Coding of Factors
contrasts(lungcap$Gender)
# Since these codes are arbitrarily assigned, 
#you can change your reference level using:
contrasts(relevel(lungcap$Gender, "M") )


##################### Fit Normal Regression ##################
model1=lm(FEV ~ Age + Ht + Gender + Smoke, data = lungcap)
# FEV = -4.45 +0.065Age + 0.1 Ht + 0.157Male - 0.087Smoker
summary(model1)
# FEMALE - NonSmoker
# FEV = -4.45 +0.065Age + 0.1 Ht
# MALE - NonSmoker
# FEV = -4.293 +0.065Age + 0.1 Ht
# FEMALE - Smoker
# FEV = -4.537 +0.065Age + 0.1 Ht 
# MALE - Smoker
# FEV = -4.223 +0.065Age + 0.1 Ht


# Confidence Intervals Betas
confint(model1,level = 0.9)

# Confidence Interval mu predicted value
new.d=data.frame(Age=20,Ht=66,Gender="F",Smoke="Smoker")

fitv=predict.lm(model1,newdata = new.d,se.fit = TRUE)
ci.lo=fitv$fit - qt(p = 0.975,df = model1$df.residual)*fitv$se.fit
ci.up=fitv$fit + qt(p = 0.975,df = model1$df.residual)*fitv$se.fit

(cbind(Lo=ci.lo, Estimate=fitv$fit , Up=ci.up))


## line 
ggplot(lungcap,aes(x = Age,y=FEV))+
  geom_point()+
  facet_grid(Gender~Smoke,scales = "free")+
  geom_smooth(method = "lm" )


# sequential analysis
anova(model1)

# Comparing Nested Models
modelA=lm(FEV ~ Age + Ht, data = lungcap)
modelB=lm(FEV ~ Age + Ht + Gender, data = lungcap)


RSSA=sum(resid(modelA)^2)
RSSB=sum(resid(modelB)^2)
SS=RSSA - RSSB
DF=df.residual(modelA) - df.residual(modelB)
Fs=(SS/DF) / (RSSB/df.residual(modelB))
pvalue = pf(Fs,df1=DF,df2=df.residual(modelB),lower.tail = F)


# another way
anova(modelB,modelA)

# Comparing non-nested models
c(aicA=extractAIC(modelA), aicb=extractAIC(modelB))
c(bicA=extractAIC(modelA, k=log(nobs(modelA))), 
  bicB= extractAIC(modelB, k=log(nobs(modelA))))

# model selection with Stepwise
min.model=lm((FEV) ~ Age, data=lungcap)
max.model=lm((FEV) ~ (Age + Ht + Gender + Smoke)^3, data=lungcap)

summary(max.model)

# Stepwise algorithms for model selection
# Forward
# Backward
# Both
for.m=step(min.model, 
       direction = "forward",
       scope = list(upper =  max.model,lower = min.model),
       trace = TRUE)

back.m=step(max.model, 
           direction = "backward",
           scope = list(upper =  max.model,lower = min.model),
           trace = T)


summary(for.m)
summary(back.m)


##################### DIAGNOSTICS ########################
# Residuals Plots
autoplot(model1) + theme_bw()

lm.inf=influence.measures(model1)
lm.inf$is.inf
lm.inf$is.inf[apply(lm.inf$is.inf, 1, any),]
barplot(rowSums(lm.inf$is.inf))




# Transformation
library(car)
BCT = powerTransform(model1) # Box-Cox transformation
BCT$roundlam
model2= update(model1, log(FEV) ~ .)
summary(model2)
autoplot(model2)

model3= update(model2, log(FEV) ~ Age+Smoke+Gender)
summary(model3)



################ Predictive Modeling with Regression ################

mapeback=numeric()
mapefor=numeric()


for (k in (1:100)) {
  
 indtrain = sample((1: nrow(lungcap)),0.8*nrow(lungcap))
 train.data = lungcap[indtrain,]
 test.data = lungcap[-indtrain,]

 
 min.model=lm(FEV ~ Age + Gender + Smoke, data=train.data)
 max.model=lm((FEV) ~ (Age + Ht + Gender + Smoke)^3, data=train.data)
 
 for.m=step(min.model, 
            direction = "forward",
            scope = list(upper =  max.model,lower = min.model),
            trace = F)
 
 back.m=step(max.model, 
             direction = "backward",
             scope = list(upper =  max.model,lower = min.model),
             trace = F)

 prd.for.m = predict(for.m,newdata = test.data)
 prd.back.m = predict(back.m,newdata = test.data)

library(Metrics)

mapefor[k] = mape(actual = test.data$FEV,predicted = prd.for.m)*100
mapeback[k] = mape(actual = test.data$FEV,predicted = prd.back.m)*100

}

boxplot(tibble(Back=mapeback,For=mapefor))
