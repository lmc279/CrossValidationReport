library(GLMsData)
library(ggfortify)


############## Binomial Model #####################
data("turbines")
str(turbines)
?turbines

# The proportion of turbine wheels out of a total of
#m turbines developing fissures (narrow cracks)
plot(Fissures/Turbines ~ Hours, data=turbines,
     xlab="Hours of use",
     ylab="Proportion of turbines with fissures")


bin.logit= glm(Fissures/Turbines ~ Hours,
            data=turbines,
            family=binomial(link = "logit"),
            weights = Turbines)


summary(bin.logit)
# interpretation in odds
# Increase Hours by 100h will lead to increase the odds of developing fissures
# by a factor of exp(100*0.0009992)=1.105.


#using link function probit
bin.probit = update( bin.logit, family=binomial(link="probit") )
#using link function c-log-log
bin.cll = update( bin.logit, family=binomial(link="cloglog") )

# compare the different coefficient
cfs=rbind( Logistic=coef(bin.logit), Probit=coef(bin.probit), CLL=coef(bin.cll))
cfs
cbind(cfs, Res.deviance=c(deviance(bin.logit), deviance(bin.probit), deviance(bin.cll)) )

# plot the models and compare
nH = seq(0,5000,length=10)

# log(odds) = b0 + b1H
pr.logit=predict(bin.logit,newdata = data.frame(Hours=nH),type="response")
pr.probit=predict(bin.probit,newdata = data.frame(Hours=nH),type="response")
pr.cll=predict(bin.cll,newdata = data.frame(Hours=nH),type="response")


lines(pr.logit ~ nH,lwd=2)
lines(pr.probit ~ nH,col="red",lty=2,lwd=2)
lines(pr.cll ~ nH,col="blue",lty=4,lwd=2)
legend("topleft",lwd=2,lty = c(1,2,4),col = c("black","red","blue"),legend = c("Logit","Probit","c-log-log"))

# Interpretation odds
# increasing Hours by one increases the odds of a
#turbine developing fissures by a factor of
# exp(0.000992)=1.000992.
# Ex.: increase Hours by 500 then the odds increase by a factor of
# exp(500*0.000992)=1.64


autoplot(bin.logit)



# Goodness of fit test - Pearson
pear.res =sum((resid(bin.logit,type="pearson"))^2)
pchisq(pear.res,9,lower.tail = FALSE)
# Deviance test
pchisq(bin.logit$deviance,9,lower.tail = F)

########################## Example 2: germ data with factors
data("germ")
?germ

str(germ)
par(mfrow=c(1,1))
plot(Germ/Total ~ Extract, data=germ)
plot(Germ/Total ~ Seeds, data=germ)

with(germ,interaction.plot(x.factor = Extract,trace.factor = Seeds,response = Germ/Total))

germ.bin= glm(Germ/Total ~ Seeds * Extract  ,
              data=germ,
              weights = Total,
              family = binomial)
summary(germ.bin)

exp(coef(germ.bin))
#interpretation without interaction:
# The odds of seed germination when using cucumber extracts is 2.9 times
# the odds of seed germination when using bean extracts.

#OR=Odds_c/Odds_b = 2.9

# The odds of seed germination when using OA75 seeds is 1.31 times
# the odds of seed germination when using OA73 seeds.


################ Overdispersion ###############

germ.binI= glm(Germ/Total ~ Seeds+Extract, da=germ,
               weights = Total,
              family = binomial)
summary(germ.binI)

autoplot(germ.binI)


# Goodness-of-fit test
# residual deviance
pchisq(33.278,df = 17,lower.tail = FALSE)
# may be there is overdispersion

################# QUASI-Binomial #############33
germ.binII= glm(Germ/Total ~ Seeds+Extract, 
                data=germ, 
                weights = Total,
               family = quasibinomial)
summary(germ.binII)
autoplot(germ.binII)


# compare the coefficients

summary(germ.binII)$coefficients/
summary(germ.binI)$coefficients

################# Binary logistic #############

################# Ordinal regression #############

################# Multinomial regression #############

