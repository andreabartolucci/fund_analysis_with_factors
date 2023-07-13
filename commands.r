#install.packages("aod")
#install.packages("aod")
#install.packages("corrplot")
##install.packages("data.table")
#install.packages("ggplot2")
#install.packages("here")
#install.packages("lmtest")
#install.packages("olsrr")
#install.packages("stargazer")
#install.packages("tidyverse")
#install.packages("psych")
#install.packages("vtable")
#install.packages("jtools")
library(jtools)
library(olsrr)
library(stargazer)
library(tidyverse)
library(here)
library(corrplot)
library(ggplot2)
library(aod)
library(data.table)
library(lmtest)
library(vtable)
theme_set(theme_bw())

#data loading
ret <- read.csv("./data/data.csv")
ret <- ret[1:length(ret)]
ret <- ret[1:2095,]
rownames(ret) <- ret$Date

#line plot
par(mfrow = c(3, 3))
plot(ret$ARKK_ret, main = "ARKK", type='l')
plot(ret$Mkt.RF, main = "Mkt", type='l')
plot(ret$SMB, main = "SMB", type='l')
plot(ret$HML, main = "HML", type='l')
plot(ret$CMA, main = "CMA", type='l')
plot(ret$RMW, main = "RMW", type='l')
plot(ret$Mom, main = "mom", type='l')

#histogram
hist(ret$ARKK_ret, breaks = 50, main = "ARKK returns distribution", xlab = "")
lines(density(ret$ARKK_ret), lwd = 2, col = "red")

#scatter plots
par(mfrow=c(3,2))
plot(ret$ARKK_ret,ret$Mkt.RF, main="ARKK vs Mkt")
plot(ret$ARKK_ret,ret$SMB, main="ARKK vs SMB")
plot(ret$ARKK_ret,ret$HML, main="ARKK vs HML")
plot(ret$ARKK_ret,ret$CMA, main="ARKK vs CMA")
plot(ret$ARKK_ret,ret$RMW, main="ARKK vs RMW")
plot(ret$ARKK_ret,ret$Mom, main="ARKK vs Mom")

#sumary statistics
st(data.frame(ret[c(2,4:length(ret))]))
cor(data.frame(ret[c(2,4:length(ret))]))

###############################
### LINEAR REGRESSION MODEL ###
###############################

#MODEL 1: regression FF5+Mom
ff5_aug = lm(ARKK_ret ~ Mkt.RF + SMB + HML + RMW + CMA + Mom, data=ret)
stargazer(ff5_aug, type="text", hac=TRUE, out="out.txt")

#R2 significance test
k=7
n=2095
p_value_HO_R2_diff0=1-pbeta(summary(ff5_aug)$r.squared, shape1 = (k-1)/2, shape2 = (n-k)/2)
p_value_HO_R2_diff0

#model diagnostic
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(ff5_aug)

#multicollinearity
ols_vif_tol(ff5_aug)

#omitted variables
resettest(ff5_aug)

#autocorrelation test
acf(ff5_aug$residuals, type="correlation")
lmtest::dwtest(ff5_aug)
lmtest::bgtest(ff5_aug, order = 14)

#heteroschedasticty test
lmtest::bptest(ff5_aug)

###################################
### NON-LINEAR REGRESSION MODEL ###
###################################

#variable creation
#polynomial 2nd grade
ret$Mkt.RF2<-ret$Mkt.RF^2
ret$SMB2<-ret$SMB^2
ret$HML2<-ret$HML^2
ret$CMA2<-ret$CMA^2
ret$RMW2<-ret$RMW^2
ret$Mom2<-ret$Mom^2

#polynomial 3rd grade
ret$Mkt.RF3<-ret$Mkt.RF^3
ret$SMB3<-ret$SMB^3
ret$HML3<-ret$HML^3
ret$CMA3<-ret$CMA^3
ret$RMW3<-ret$RMW^3
ret$Mom3<-ret$Mom^3

#MODEL 2: regression poly 2
ff5_aug_poly_2 = lm(ARKK_ret ~ Mkt.RF + Mkt.RF2 + SMB + SMB2 + HML + HML2 + RMW + RMW2 + CMA + CMA2 + Mom + Mom2, data=ret)
stargazer(ff5_aug_poly_2, type="text", hac=TRUE)

#MODEL 3:regression poly 2 and 3
ff5_aug_poly_2_3 = lm(ARKK_ret ~ Mkt.RF + Mkt.RF2 + Mkt.RF3 + SMB + SMB2 + SMB3 + HML + HML2  + HML3 + RMW + RMW2 + RMW3 + CMA + CMA2 + CMA3 + Mom + Mom2 + Mom3, data=ret)
stargazer(ff5_aug_poly_2_3, type="text", hac=TRUE)

stargazer(ff5_aug_poly_2, ff5_aug_poly_2_3, type="text", hac=TRUE, out="out.txt")

#R2 significance test
k=19
n=2095
p_value_HO_R2_diff0=1-pbeta(summary(ff5_aug_poly_2_3)$r.squared, shape1 = (k-1)/2, shape2 = (n-k)/2)
p_value_HO_R2_diff0

#model diagnostic
par(mfrow=c(2,2)) 
plot(ff5_aug_poly_2_3)

#multicollinearity
ols_vif_tol(ff5_aug_poly_2_3)

#autocorrelation test
acf(ff5_aug_poly_2_3$residuals, type="correlation")
lmtest::dwtest(ff5_aug_poly_2_3)
lmtest::bgtest(ff5_aug_poly_2_3, order = 14)

#heteroschedasticty test
lmtest::bptest(ff5_aug_poly_2_3)

##########################
### HYPOTHESIS TESTING ###
##########################

#dummy creation
ret$covid <- ifelse(ret$Date >=20200201, 1, 0)
ret$Intercept <- 1

#MODEL 4:test whether betas and alpha have changed during and after Covid 19
ff5_aug_poly_covid = lm(ARKK_ret ~ 0  + Intercept  + Mkt.RF  + Mkt.RF*covid  + Mkt.RF2  + Mkt.RF2*covid  + Mkt.RF3  + Mkt.RF3*covid  + SMB  + SMB*covid  + SMB2  + SMB2*covid  + SMB3  + SMB3*covid  + HML  + HML*covid  + HML2   + HML2*covid   + HML3  + HML3*covid  + RMW  + RMW*covid  + RMW2  + RMW2*covid  + RMW3  + RMW3*covid  + CMA  + CMA*covid  + CMA2  + CMA2*covid  + CMA3  + CMA3*covid  + Mom  + Mom*covid  + Mom2  + Mom2*covid  + Mom3  + Mom3*covid, data=ret)
#summary(ff5_aug_poly_covid)
stargazer(ff5_aug_poly_covid, type="text", out="out.txt")

#wald test
wald.test(Sigma = vcov(ff5_aug_poly_covid), b = coef(ff5_aug_poly_covid), Terms = c(3,21:38))

####################
### INTERACTIONS ###
####################

#MODEL 5: test whether the effect of the factors is mediated by the Market excess returns (interactions)
ff5_aug_poly_interaction = lm(ARKK_ret ~ 0 + Intercept + Mkt.RF + Mkt.RF2 + Mkt.RF3 + SMB + SMB2 + SMB3 + HML + HML2  + HML3 + RMW + RMW2 + RMW3 + CMA + CMA2 + CMA3 + Mom + Mom2 + Mom3 + SMB*Mkt.RF + SMB2*Mkt.RF + SMB3*Mkt.RF + HML*Mkt.RF + HML2*Mkt.RF  + HML3*Mkt.RF + RMW*Mkt.RF + RMW2*Mkt.RF + RMW3*Mkt.RF + CMA*Mkt.RF + CMA2*Mkt.RF + CMA3*Mkt.RF + Mom*Mkt.RF + Mom2*Mkt.RF + Mom3*Mkt.RF, data=ret)
#summary(ff5_aug_poly_interaction)
stargazer(ff5_aug_poly_interaction, type="text", out="out.txt")

#wald test
wald.test(Sigma = vcov(ff5_aug_poly_covid), b = coef(ff5_aug_poly_covid), Terms = 20:34)
















