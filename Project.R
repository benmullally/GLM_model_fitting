setwd("~/Documents/MSc Data Science/MATH401/Project")
load("Project2022.Rdata")
library(ggplot2)
library(lmtest)
Project2022 = Project2022[-c(107),]
colnames(Project2022)
ggplot(Project2022) +
  geom_point(aes(x=x1,y=y))

ggplot(Project2022) +
  geom_point(aes(x=x2,y=y))

ggplot(Project2022) +
  geom_point(aes(x=x3,y=y))

ggplot(Project2022) +
  geom_point(aes(x=x4,y=y))

ggplot(Project2022) +
  geom_point(aes(x=time,y=y))



dim(Project2022)

#poisson model, with offset for time
logTime = log(Project2022$time)
#forward fit
M0 = glm(y~1+offset(logTime),family="poisson",data=Project2022)
M1.1 = glm(y~x1+offset(logTime), family="poisson", data=Project2022)
M1.2 = glm(y~x2+offset(logTime), family="poisson", data=Project2022)
M1.3 = glm(y~x3+offset(logTime), family="poisson", data=Project2022)
M1.4 = glm(y~x4+offset(logTime), family="poisson", data=Project2022)
c(lrtest(M0,M1.1)$P[2],lrtest(M0,M1.2)$P[2],lrtest(M0,M1.3)$P[2],lrtest(M0,M1.4)$P[2])

M1.31 = glm(y~x3+x1+offset(logTime), family="poisson", data=Project2022)
M1.32 = glm(y~x3+x2+offset(logTime), family="poisson", data=Project2022)
M1.34 = glm(y~x3+x4+offset(logTime), family="poisson", data=Project2022)
c(lrtest(M1.3,M1.31)$P[2],lrtest(M1.3,M1.32)$P[2],lrtest(M1.3,M1.34)$P[2])

M1.341 = glm(y~x3+x4+x1+offset(logTime), family="poisson", data=Project2022)
M1.342 = glm(y~x3+x4+x2+offset(logTime), family="poisson", data=Project2022)
c(lrtest(M1.34,M1.341)$P[2],lrtest(M1.34,M1.342)$P[2])

M1.3412 = glm(y~x3+x4+x1+x2+offset(logTime), family="poisson", data=Project2022)
lrtest(M1.341,M1.3412)$P[2]

#M1.341
#best model so far

#look for interactions
M1.341.31 = glm(y~x3+x4+x1+x3*x1+offset(logTime), family="poisson", data=Project2022)
M1.341.34 = glm(y~x3+x4+x1+x3*x4+offset(logTime), family="poisson", data=Project2022)
M1.341.32 = glm(y~x3+x4+x1+x3*x2+offset(logTime), family="poisson", data=Project2022)
M1.341.14 = glm(y~x3+x4+x1+x1*x4+offset(logTime), family="poisson", data=Project2022)
M1.341.12 = glm(y~x3+x4+x1+x1*x2+offset(logTime), family="poisson", data=Project2022)
M1.341.42 = glm(y~x3+x4+x1+x4*x2+offset(logTime), family="poisson", data=Project2022)

c(lrtest(M1.341,M1.341.31)$P[2], lrtest(M1.341,M1.341.34)$P[2], lrtest(M1.341,M1.341.32)$P[2], 
  lrtest(M1.341,M1.341.14)$P[2], lrtest(M1.341,M1.341.12)$P[2], lrtest(M1.341,M1.341.42)$P[2])

M1.341.14.12 = glm(y~x3+x4+x1+x1*x4+x1*x2+offset(logTime), family="poisson", data=Project2022)
M1.341.14.13 = glm(y~x3+x4+x1+x1*x4+x1*x3+offset(logTime), family="poisson", data=Project2022)
M1.341.14.23 = glm(y~x3+x4+x1+x1*x4+x2*x3+offset(logTime), family="poisson", data=Project2022)
M1.341.14.24 = glm(y~x3+x4+x1+x1*x4+x2*x4+offset(logTime), family="poisson", data=Project2022)
M1.341.14.34 = glm(y~x3+x4+x1+x1*x4+x3*x4+offset(logTime), family="poisson", data=Project2022)

c(lrtest(M1.341.14,M1.341.14.12)$P[2], lrtest(M1.341.14,M1.341.14.13)$P[2], lrtest(M1.341.14,M1.341.14.23)$P[2],
  lrtest(M1.341.14,M1.341.14.24)$P[2], lrtest(M1.341.14,M1.341.14.34)$P[2])


#So final model is M1.341.14 with log(time) offset
BIC(M1.341.14) 
AIC(M1.341.14)
#deviance
print(c(M1.341.14$deviance, M1.341.14$df.resid, qchisq(.95,df=M1.341.14$df.resid)))


summary(M1.341.14)$coef
X = model.matrix(M1.341.14)
HAT = X %*% solve(t(X)%*%X) %*% t(X)
leverage = diag(HAT)
plot(leverage,ylab="leverage",xlab="observation number")
which.max(leverage)
mean(leverage)
#investigate leverage
bigLev = leverage-mean(leverage)*3
bigLev = bigLev[(bigLev > 0)==TRUE]
bigLev
plot(leverage,Project2022$x4)

Project2022 = Project2022[-c(4,40,74,115,119,134),]
logTime = log(Project2022$time)
M1.341.14 = glm(y~x3+x4+x1+x1*x4+offset(logTime), family="poisson", data=Project2022)
summary(M1.341.14)$coef
#No major changes in the covariates wrt to sd so decide to keep those rows in despite high leverage.

#check residuals
resids=residuals(M1.341.14,type="pearson")
yFitted=fitted(M1.341.14) ## Fitted values on the scale of the original data
par(mfrow=c(1,2))
cols=(as.numeric(Project2022$x2)-1)*2+1 ## black and green for 1 and 2
plot(Project2022$x1,resids,pch=4,xlab="x1",ylab="residuals",col=cols)
plot(Project2022$x3,resids,pch=4,xlab="x3",ylab="residuals",col=cols)
plot(Project2022$x4,resids,pch=4,xlab="x4",ylab="residuals",col=cols)
plot(yFitted,resids,pch=4,xlab="Fitted",ylab="residuals",col=cols)

resids[(resids >3)]
#remove 53,127 and 165?
Project2022res = Project2022[-c(53,127,165),]
logTime = log(Project2022res$time)
M1.341.14.res = glm(y~x3+x4+x1+x1*x4+offset(logTime), family="poisson", data=Project2022res)
summary(M1.341.14.res)$coef
#Again no major changes in the covariates wrt to sd so decide to keep those rows in despite high residuals
#Comment on  large spread but The lack of a pattern suggests a good fit.

load("Project2022.Rdata")
Project2022 = Project2022[-c(107),]
logTime = log(Project2022$time)
library(MASS)
NB.341 = glm.nb(y~x3+x4+x1+offset(logTime), data=Project2022)
AIC(NB.341)
BIC(NB.341)
AIC(M1.341.14) #still better
BIC(M1.341.14)

NB.341.14 = glm.nb(y~x3+x4+x1+x1*x4+offset(logTime), data=Project2022)
AIC(NB.341.14)
BIC(NB.341.14) #NB.341.14 is better! slighty...

print(c(NB.341.14$deviance, NB.341.14$df.resid, qchisq(.95,df=NB.341.14$df.resid)))
#best!

summary(NB.341.14)$coef
X = model.matrix(NB.341.14)
HAT = X %*% solve(t(X)%*%X) %*% t(X)
leverage = diag(HAT)
plot(leverage,ylab="Leverage",xlab="Observation number")
which.max(leverage)
mean(leverage)
#investigate leverage
bigLev = leverage-mean(leverage)*3
bigLev = bigLev[(bigLev > 0)==TRUE]
plot(leverage,Project2022$x1)

Project2022t = Project2022[-c(4,40,74,115,119,134),]
logTimet = log(Project2022t$time)
M1.341.14t = glm(y~x3+x4+x1+x1*x4+offset(logTimet), family="poisson", data=Project2022t)
summary(M1.341.14t)$coef
#No major changes in the covariates wrt to sd so decide to keep those rows in despite high leverage.
#check residuals
resids=residuals(NB.341.14,type="pearson")
yFitted=fitted(NB.341.14) ## Fitted values on the scale of the original data
par(mfrow=c(1,2))
cols=(as.numeric(Project2022$x2)-1)*2+1 ## black and green for 1 and 2
plot(Project2022$x1,resids,pch=4,xlab="x1",ylab="residuals",col=cols)
plot(Project2022$x3,resids,pch=4,xlab="x3",ylab="residuals",col=cols)
plot(Project2022$x4,resids,pch=4,xlab="x4",ylab="residuals",col=cols)
plot(yFitted,resids,pch=4,xlab="Fitted",ylab="residuals",col=cols)

#Shows how good fit is. variation of residuals clear but decent fit.
plot(yFitted,Project2022$y,pch=4,xlab="Fitted",ylab="y")
abline(lm(Project2022$y~yFitted),col='green')

#Backwards fitting with nb
NB.3412 = glm.nb(y~x3+x4+x1+x2+offset(logTime), data=Project2022)

NB.341 = glm.nb(y~x3+x4+x1+offset(logTime), data=Project2022)
NB.342 = glm.nb(y~x3+x4+x2+offset(logTime), data=Project2022)
NB.312 = glm.nb(y~x3+x1+x2+offset(logTime), data=Project2022)
NB.412 = glm.nb(y~x4+x1+x2+offset(logTime), data=Project2022)
c(lrtest(NB.3412,NB.341)$P[2],lrtest(NB.3412,NB.342)$P[2],lrtest(NB.3412,NB.312)$P[2],lrtest(NB.3412,NB.412)$P[2])
#Remove x2 as highest p-value

NB.34 = glm.nb(y~x3+x4+offset(logTime), data=Project2022)
NB.31 = glm.nb(y~x3+x1+offset(logTime), data=Project2022)
NB.41 = glm.nb(y~x4+x1+offset(logTime), data=Project2022)
c(lrtest(NB.341,NB.34)$P[2],lrtest(NB.341,NB.31)$P[2],lrtest(NB.341,NB.41)$P[2])
#All significant so NB.341 best

#Forward fitting to check for interactions on nb
NB.341.31 = glm.nb(y~x3+x4+x1+x3*x1+offset(logTime), data=Project2022)
NB.341.34 = glm.nb(y~x3+x4+x1+x3*x4+offset(logTime), data=Project2022)
NB.341.32 = glm.nb(y~x3+x4+x1+x3*x2+offset(logTime), data=Project2022)
NB.341.14 = glm.nb(y~x3+x4+x1+x1*x4+offset(logTime), data=Project2022)
NB.341.12 = glm.nb(y~x3+x4+x1+x1*x2+offset(logTime), data=Project2022)
NB.341.42 = glm.nb(y~x3+x4+x1+x4*x2+offset(logTime), data=Project2022)
c(lrtest(NB.341,NB.341.31)$P[2], lrtest(NB.341,NB.341.34)$P[2], lrtest(NB.341,NB.341.32)$P[2], 
  lrtest(NB.341,NB.341.14)$P[2], lrtest(NB.341,NB.341.12)$P[2], lrtest(NB.341,NB.341.42)$P[2])
#NB.341.14 TAKEN

NB.341.14.12 = glm.nb(y~x3+x4+x1+x1*x4+x1*x2+offset(logTime), data=Project2022)
NB.341.14.13 = glm.nb(y~x3+x4+x1+x1*x4+x1*x3+offset(logTime), data=Project2022)
NB.341.14.23 = glm.nb(y~x3+x4+x1+x1*x4+x2*x3+offset(logTime), data=Project2022)
NB.341.14.24 = glm.nb(y~x3+x4+x1+x1*x4+x2*x4+offset(logTime), data=Project2022)
NB.341.14.34 = glm.nb(y~x3+x4+x1+x1*x4+x3*x4+offset(logTime), data=Project2022)

c(lrtest(NB.341.14,NB.341.14.12)$P[2], lrtest(NB.341.14,NB.341.14.13)$P[2], lrtest(NB.341.14,NB.341.14.23)$P[2],
  lrtest(NB.341.14,NB.341.14.24)$P[2], lrtest(NB.341.14,NB.341.14.34)$P[2])
#NO MORE INTERACTIONS
AIC(NB.341.14.34)
BIC(NB.341.14.34 )
print(c(NB.341.14$deviance, NB.341.14$df.resid, qchisq(.95,df=NB.341.14$df.resid)))
#best

# over 1 time unit so log(time) = 0 so has no affect on predictions
#predictions 1
s = summary(NB.341.14)
predicted = sum(c(1,0.5,0,0,10,0,0)*NB.341.14$coef)
a1=c(1,0.5,0,0,10,0,0)
V = vcov(NB.341.14)
Vpred = (t(a1) %*% V %*% a1)[1,1]
CIU=predicted+qnorm(0.975)*sqrt(Vpred) 
CIL=predicted-qnorm(0.975)*sqrt(Vpred) 
print(exp(predicted))
print(exp(c(CIL,CIU)))

#predictions 2
s = summary(NB.341.14)
predicted = sum(c(1,0.5,0,1,10,0,10)*NB.341.14$coef)
a1=c(1,0.5,0,1,10,0,10)
V = vcov(NB.341.14)
Vpred = (t(a1) %*% V %*% a1)[1,1]
CIU=predicted+qnorm(0.975)*sqrt(Vpred) 
CIL=predicted-qnorm(0.975)*sqrt(Vpred) 
print(exp(predicted))
print(exp(c(CIL,CIU)))

#\begin{wrapfigure}{L}{3in}
#  \includegraphics{Project_files/figure-latex/myplot-1.pdf}
#\end{wrapfigure}