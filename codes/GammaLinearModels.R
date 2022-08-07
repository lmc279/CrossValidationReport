library(GLMsData)
library(ggfortify)
data("lime")
??lime


 summary(lime)
 hist(lime$Foliage)
# A model for the foliage is sought.

##################### DATA VISUSALIZATION###############
plot((Foliage) ~ (DBH), data=lime, 
      pch=ifelse(Origin=="Natural",1,ifelse(Origin=="Coppice",19,8)),
       las=1, xlab="DBH in cm", ylab="Foliage in kg")
legend("topleft", pch=c(1,19,8), legend = c("Natural","Coppice","Planted"))

lime %>% ggplot(aes(y=log(Foliage),x=(DBH))) + geom_point(aes(color=Origin))  +
  geom_smooth(method = "loess",aes(color=Origin),se = F) 
  
facet_wrap(~Origin)


# Plot vs Age
plot((Foliage)~ Age, data=lime,  
     pch=ifelse(Origin=="Natural",1,ifelse(Origin=="Coppice",19,8))
     , las=1, xlab="Age in years", ylab="Foliage in kg")
legend("topleft", pch=c(1,19,8), legend = c("Natural","Coppice","Planted"))

# Plot vs Origin
plot(Foliage~ Origin, data=lime, las=1)



library(car)
powerTransform(Foliage~ DBH,data = lime)

# Plot vs DBH transformed
plot(Foliage~ log(DBH), data=lime, 
     pch=ifelse(Origin=="Natural",1,ifelse(Origin=="Coppice",19,8)),
     las=1, xlab="DBH in cm", ylab="Foliage in kg")
legend("topleft", pch=c(1,19,8), legend = c("Natural","Coppice","Planted"))


# Plot vs Age
plot((Foliage)~ Age, data=lime,  
     pch=ifelse(Origin=="Natural",1,ifelse(Origin=="Coppice",19,8))
     , las=1, xlab="Age in years", ylab="Foliage in kg")
legend("topleft", pch=c(1,19,8), legend = c("Natural","Coppice","Planted"))

# Plot vs Origin
plot(Foliage~ Origin, data=lime, las=1)

############# GAMMA Model ##############
lime.log = glm(Foliage ~ log(DBH)*Origin , family=Gamma(link = "log"),
               data=lime)

anova(lime.log,test = "F") # F test since phi is unknown
summary(lime.log)

######## DIAGNOSTICS ##############
autoplot(lime.log)

model.inf = influence.measures(lime.log)
model.inf$is.inf
model.inf$is.inf[apply(model.inf$is.inf, 1, any),]
barplot(rowSums(model.inf$is.inf))


powerTransform(lime.log)

plot(log(Foliage)~ log(DBH), data=lime,  
     pch=ifelse(Origin=="Natural",1,ifelse(Origin=="Coppice",19,8))
     , las=1, xlab="log(DBH in cm)", ylab="log(Foliage in kg)")
legend("topleft", pch=c(1,19,8), legend = c("Natural","Coppice","Planted"))

cf=coef(lime.log)

abline(cf[1],cf[2],col="green")
abline(cf[1]+cf[3],cf[2]+cf[5],col="blue")
abline(cf[1]+cf[4],cf[2]+cf[6],col="red")


## Origin = Coppice
# log Foliage = -4.6 +  1.8log(DBH)  
# ## Origin = Natural
# log Foliage = -5 +  1.86log(DBH)  
# ## Origin = Planted
# log Foliage = -6.67 +  2.6 log(DBH)  


# predictions
mapeback=numeric()
mapefor=numeric()
mapelime=numeric()

for (k in (1:100)) {
  
  indtrain = sample((1: nrow(lime)),0.8*nrow(lime))
  train.data = lime[indtrain,]
  test.data = lime[-indtrain,]
  
  
  min.model=glm(Foliage ~ 1, data=train.data,family = Gamma(link="log"))
  max.model=glm(Foliage ~ (DBH + Origin + Age)^3, data=train.data,family = Gamma(link="log"))
  
  for.m=step(min.model, 
             direction = "forward",
             scope = list(upper =  max.model,lower = min.model),
             trace = F)
  
  back.m=step(max.model, 
              direction = "backward",
              scope = list(upper =  max.model,lower = min.model),
              trace = F)
  
  lime.log = glm(Foliage ~ log(DBH)*Origin , family=Gamma(link = "log"),
                 data=train.data)
  
  prd.for.m = predict(for.m,newdata = test.data,type = "response")
  prd.back.m = predict(back.m,newdata = test.data,type = "response")
  prd.lime.m = predict(lime.log,newdata = test.data,type = "response")
  
  
  library(Metrics)
  
  mapefor[k] = mse(actual = test.data$Foliage,predicted = prd.for.m)*100
  mapeback[k] = mse(actual = test.data$Foliage,predicted = prd.back.m)*100
  mapelime[k] = mse(actual = test.data$Foliage,predicted = prd.lime.m)*100
}

mapes= data.frame(Backward=mapeback,Forward=mapefor,LimeModel=mapelime)
summary(mapes)
boxplot(mapes)

