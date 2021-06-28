rm(list = ls())
install.packages("readxl")
install.packages("latticeExtra")
library("stats")
library("MASS")
library("lmtest")
library("aTSA")
library("stargazer")
library("RFGLS")
library("sjPlot")
library("sjmisc")
library("sjlabelled")
library("ggpubr")
library("LatticeExtra")

GDP <- datascriptie$`GDP (million)`
L <- datascriptie$`Emply x ratio (million)`
K <- datascriptie$`Capital (million)`

GDP <- ts(GDP, start = 1951, frequency = 4)
L <- ts(L, start = 1951, frequency = 4)
K <- ts(K, start = 1951, frequency = 4)

logGDP <- log(GDP)
logL <- log(L)
logK <- log(K)

regols <- lm(logGDP ~ logK + logL ) 
lagK <- lm(logK ~ lag(logK) + lag(logL))
lagL <- lm(logL ~ lag(logK) + lag(logL))
mean(lagK$residuals) 
var(lagK$residuals)

GDP_in_billions_USD <- GDP/1000

pgdp <- ggplot(datascriptie, aes(x=observation_date, y=GDP_in_billions_USD)) +
  geom_line() + 
  xlab("")
pgdp

Kapital_in_billions_USD <- K/1000

pk <- ggplot(datascriptie, aes(x=observation_date, y=Kapital_in_billions_USD)) +
  geom_line() + 
  xlab("")
pk

Labour_in_billions_USD <- L/1000

pl <- ggplot(datascriptie, aes(x=observation_date, y=Labour_in_billions_USD)) +
  geom_line() + 
  xlab("")
pl

log_GDP_in_millions_USD <- logGDP

logpgdp <- ggplot(datascriptie, aes(x=observation_date, y=log_GDP_in_millions_USD)) +
  geom_line() + 
  xlab("")
logpgdp

log_kapital_in_millions_USD <- logK

logpk <- ggplot(datascriptie, aes(x=observation_date, y=log_kapital_in_millions_USD)) +
  geom_line() + 
  xlab("")
logpk

log_labour_in_millions_USD <- logL

logpl <- ggplot(datascriptie, aes(x=observation_date, y=log_labour_in_millions_USD)) +
  geom_line() + 
  xlab("")
logpl

acf(residuals(reg1))
acf(residuals(reg1), type="partial")

library("nlme")
reggls <- gls(logGDP ~ logK + logL,
               data=datascriptie, correlation=corARMA(p=1), method="ML")
summary(reggls)

c_ols <- regols$coefficients[1]
beta1_ols <- regols$coefficients[2]
beta2_ols <- regols$coefficients[3]
beta_ols <- c(c_ols, beta1_ols, beta2_ols)
e_ols <- regols$residuals
var_ols <- t(e_ols)%*%e_ols/(277-3)

c_fgls <- reggls$coefficients[1]
beta1_fgls <- reggls$coefficients[2]
beta2_fgls <- reggls$coefficients[3]
beta_fgls <- c(c_fgls, beta1_fgls, beta2_fgls)
e_fgls <- reggls$residuals
var_fgls <- t(e_fgls)%*%e_fgls/(277-3)

difflogGDP <- diff(logGDP)
difflogK <- diff(logK)
difflogL <- diff(logL)

regfd_ols <- lm(difflogGDP ~ difflogK + difflogL)
regfd_gls <- gls(difflogGDP ~ difflogK + difflogL,
                          data=datascriptie, correlation=corARMA(p=1), method="ML")

cfd_ols <- regfd_ols$coefficients[1]
betafd1_ols <- regfd_ols$coefficients[2]
betafd2_ols <- regfd_ols$coefficients[3]
betafd_ols <- c(cfd_ols, betafd1_ols, betafd2_ols)
efd_ols <- regfd_ols$residuals
varfd_ols <- t(efd_ols)%*%efd_ols/(276-3)

cfd_gls <- regfd_gls$coefficients[1]
betafd1_gls <- regfd_gls$coefficients[2]
betafd2_gls <- regfd_gls$coefficients[3]
betafd_gls <- c(cfd_gls, betafd1_gls, betafd2_gls)
efd_gls <- regfd_gls$residuals
varfd_gls <- t(efd_gls)%*%efd_gls/(276-3)


epsi <- function(xxx){
  0.998286*xxx + rnorm(n = 1, mean = 0, sd = 0.003790755)
}

epsil1 <- replicate(1000, (epsi(e_ols[277])))
epsil5 <-replicate(1000, epsi(epsi(epsi(epsi(epsi(e_ols[277]))))))
epsil25 <-replicate(1000, epsi(epsi(epsi(epsi(epsi(epsi(epsi(epsi(epsi(epsi(epsi(
  epsi(epsi(epsi(epsi(epsi(epsi(epsi(epsi(epsi(epsi(
    epsi(epsi(epsi(epsi(epsi(e_ols[277])))))))))))))))))))))))))))

KL <- cbind(logK, logL)
KL_var <- VAR(KL, type = "const", lag.max = 1, ic = "AIC")

KL_var

PI_est <- cbind(c(1.0356413, 0.01763721), c(-0.1964899, 0.90429467))
CON_est <- c(1.8518855, 0.89378831)

KL_old <- cbind(KL[277,])

KLfunc <- function(xx){
  CON_est + PI_est%*%xx + c(rnorm(n = 1, mean = 0, sd = 0.1), 
                          rnorm(n = 1, mean = 0, sd = 0.1))
}

rep_KLnew0 <- replicate(1000, c(KLfunc(KL_old)))
repKLnew0 <- matrix(rep_KLnew0, ncol = 1000, nrow = 2)

rep_KLnew5 <- replicate(1000, KLfunc(KLfunc(KLfunc(KLfunc(KLfunc(KLfunc(KL_old)))))))
repKLnew5 <- matrix(rep_KLnew5, ncol = 1000, nrow = 2)

repKLnew25 <- replicate(1000,  KLfunc(KLfunc(KLfunc(KLfunc(
  KLfunc(KLfunc(KLfunc(KLfunc(KLfunc(KLfunc(KLfunc(KLfunc(
    KLfunc(KLfunc(KLfunc(KLfunc(KLfunc(KLfunc(KLfunc(KLfunc(
      KLfunc(KLfunc(KLfunc(KLfunc(KLfunc(KLfunc(KL_old)))))))))))))))))))))))))))
repKLnew25 <- matrix(repKLnew25, ncol = 1000, nrow = 2)

y_T0 <- matrix(nrow = 1000, ncol = 1)
for (i in 1:dim(y_T0)[1]){
  y_T0[i] = c_ols + repKLnew0[1,i]*beta1_ols + repKLnew0[2,i]*beta2_ols + epsil1[i]
}
ytzero <- c(y_T0)

y_T5 <- matrix(nrow = 1000, ncol = 1)
for (i in 1:dim(y_T5)[1]){
  y_T5[i] = c_ols + repKLnew5[1,i]*beta1_ols + repKLnew5[2,i]*beta2_ols + epsil5[i]
}
ytfive <- c(y_T5)

y_T25 <- matrix(nrow = 1000, ncol = 1)
for (i in 1:dim(y_T25)[1]){
  y_T25[i] = c_ols + repKLnew25[1,i]*beta1_ols + repKLnew25[2,i]*beta2_ols + epsil25[i]
}
yttwentyfive <- c(y_T25)

y_diff5 <- (y_T5 - y_T0)
y_growth5 <- c(y_diff5/y_T0)

y_diff25 <- (y_T25 - y_T0)
y_growth25 <- c(y_diff25/y_T0)

DELX_ols <- cbind(difflogK, difflogL)
DELXXinv_ols <- solve(t(DELX_ols)%*%DELX_ols)

vcovfd_ols <- vcov(regfd_ols)
vcovfd_fgls <- vcov(regfd_gls)

xtplus25 <- c(mean(repKLnew25[1,]), mean(repKLnew25[,1]))
xtplus5 <- c(mean(repKLnew5[1,]), mean(repKLnew5[,1]))
xtplus0 <- c(mean(repKLnew0[1,]), mean(repKLnew0[,1]))
del_xtplus5 <- c(0, xtplus5 - xtplus0)
del_xtplus25 <- c(0, xtplus25 - xtplus0) 

a_t5_ols <- t(del_xtplus5)%*%vcovfd_ols%*%del_xtplus5
a_t25_ols <- t(del_xtplus25)%*%vcovfd_ols%*%del_xtplus25

a_t5_fgls <- t(del_xtplus5)%*%vcovfd_fgls%*%del_xtplus5
a_t25_fgls <- t(del_xtplus25)%*%vcovfd_fgls%*%del_xtplus25

hyper5_ols <- genhypergeo(NULL, (278-2)/2, (278-2)/2*0.5*(5*varfd_ols-a_t5_ols), tol = 1e-06, maxiter = 2000)
G5_ols <- 100*(exp(del_xtplus5%*%betafd_ols)*hyper5_ols - 1)

hyper25_ols <- genhypergeo(NULL, (278-2)/2, (278-2)/2*0.5*(25*varfd_ols-a_t25_ols), tol = 1e-06, maxiter = 2000)
G25_ols <- 100*(exp(del_xtplus25%*%betafd_ols)*hyper25_ols - 1)

hyper5_fgls <- genhypergeo(NULL, (278-2)/2, (278-2)/2*0.5*(5*varfd_gls-a_t5_fgls), tol = 1e-06, maxiter = 2000)
G5_fgls <- 100*(exp(del_xtplus5%*%betafd_gls)*hyper5_fgls - 1)

hyper25_fgls <- genhypergeo(NULL, (278-2)/2, (278-2)/2*0.5*(25*varfd_gls-a_t25_fgls), tol = 1e-06, maxiter = 2000)
G25_fgls <- 100*(exp(del_xtplus25%*%betafd_gls)*hyper25_fgls - 1)

G5_cons <- 100*(exp(del_xtplus5%*%beta_ols + 2*var_ols)  - 1)

G25_cons <- 100*(exp(del_xtplus25%*%beta_ols + 2*var_ols)  - 1)

vecghat5 <- rep(G5_ols/100, 1000)
vecgtilde5 <- rep(G5_fgls/100, 1000)
vecgbar5 <- rep(G5_cons/100, 1000)

vecghat25 <- rep(G25_ols/100, 1000)
vecgtilde25 <- rep(G25_fgls/100, 1000)
vecgbar25 <- rep(G25_cons/100, 1000)

difference_5_hat <- c(y_growth5 - vecghat5)^2
q95hat5 <- quantile(difference_5_hat, 0.95)
plot(density(difference_5_hat))
mean(difference_5_hat)
var(difference_5_hat)

difference_5_tilde <- c(y_growth5 - vecgtilde5)^2
q95tilde5 <- quantile(difference_5_tilde, 0.95)
plot(density(difference_5_tilde))
mean(difference_5_tilde)
var(difference_5_tilde)

difference_5_bar <- c(y_growth5 - vecgbar5)^2
q95bar5 <- quantile(difference_5_bar, 0.95)
plot(density(difference_5_bar))
mean(difference_5_bar)
var(difference_5_bar)

difference_25_hat <- c(y_growth25 - vecghat25)^2
q95hat25 <- quantile(difference_25_hat, 0.95)
plot(density(difference_25_hat))
mean(difference_25_hat)
var(difference_25_hat)

difference_25_tilde <- c(y_growth25 - vecgtilde25)^2
q95tilde25 <- quantile(difference_25_tilde, 0.95)
plot(density(difference_25_tilde))
mean(difference_25_tilde)
var(difference_25_tilde)

difference_25_bar <- c(y_growth25 - vecgbar25)^2
q95bar25 <- quantile(difference_25_bar, 0.95)
plot(density(difference_25_bar))
mean(difference_25_bar)
var(difference_25_bar)

q95hat5
q95tilde5
q95bar5
q95hat25
q95tilde25
q95bar25
