#### Emotion Sensor Time Series Analysis  ####

library(reshape2)
library(dplyr)
library(tseries)
library(MASS)
library(signal)
library(forecast)
library(rmgarch)
library(fGarch)
library(fracdiff)
library(astsa)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(multitaper)
library(dplR)

### Loading Data ###

setwd("~/Documents/Berkeley/Stat248")

sensors = c("EMG","BVP","GSR","RES")
emotions = c("NoEmotion","Anger","Hate","Grief","PLove","RLove","Joy","Reverence")
cols = numeric(32); k=1
for(i in sensors){
  for(j in emotions){
    cols[k] = paste0(i,"_",j)
    k = k + 1
  }
} 

for(i in 1:20){
  name = paste0("day",i)
  file = paste0("./Data/day",i,".csv")
  assign(name, read.csv(file, col.names=cols))
}



#### Exploratory Analysis ####

for(i in 1:20){
  d = paste0("day",i)
  plot_d = paste0("plot_",d)
  assign(plot_d, cbind(stack(get(d)[1:8]), stack(get(d)[9:16]), 
                       stack(get(d)[17:24]), stack(get(d)[25:32])))
}
colnames(plot_day1) = colnames(plot_day2) = colnames(plot_day3) = colnames(plot_day4) = 
  colnames(plot_day5) = colnames(plot_day6) = colnames(plot_day7) = colnames(plot_day8) =
  colnames(plot_day9) = colnames(plot_day10) = colnames(plot_day11) = colnames(plot_day12) =
  colnames(plot_day13) = colnames(plot_day14) = colnames(plot_day15) = colnames(plot_day16) =
  colnames(plot_day17) = colnames(plot_day18) = colnames(plot_day19) = colnames(plot_day20) =
  c("EMG","EMG_Cat","BVP","BVP_Cat","GSR","GSR_Cat","RES","RES_Cat")

plot_total = bind_rows(plot_day1,plot_day2)
for(i in 3:20){
  plot_add = paste0("plot_day",i)
  plot_total = bind_rows(plot_total,get(plot_add))
}

# Log transform of EMG 
plot_total$log_EMG = log(plot_total$EMG)
plot_total$log_EMG[is.na(plot_total$log_EMG)]=-4
plot_total$log_EMG[plot_total$log_EMG==-Inf]=-3
plot(1:(nrow(plot_total[,1])),plot_total$log_EMG,type="l")

# Plots for each sensor over entire 20 day span 
plot(1:(nrow(plot_total[,1])),plot_total$EMG,type="l")
plot(1:(nrow(plot_total[,1])),plot_total$BVP,type="l")
plot(1:(nrow(plot_total[,1])),plot_total$GSR,type="l")
plot(1:(nrow(plot_total[,1])),plot_total$RES,type="l")

# FIGURE FOR PAPER: Raw Data
time = seq(from=1,to=21,length.out=320000+1)
plot_total$time = time[1:(length(time)-1)]
p1 = ggplot(data = plot_total) + geom_line(aes(x = time, y = EMG)) +
        theme(axis.title.x = element_blank()) + labs(list(y = "EMG"))
p2 = ggplot(data = plot_total) + geom_line(aes(x = time, y = BVP)) + 
        theme(axis.title.x = element_blank()) + labs(list(y = "BVP"))
p3 = ggplot(data = plot_total) + geom_line(aes(x = time, y = GSR)) + 
        theme(axis.title.x = element_blank()) + labs(list(y = "GSR"))
p4 = ggplot(data = plot_total) + geom_line(aes(x = time, y = RES)) + 
        labs(list(x = "Time (Days)", y = "RES"))
grid.arrange(p1, p2, p3, p4, nrow=4, top="Raw Sensor Data")

# Plots for each sensor on first and last days
plot(1:16000,plot_day1$EMG,type="l")
plot(1:16000,plot_day20$EMG,type="l")
plot(1:16000,plot_day1$BVP,type="l")
plot(1:16000,plot_day20$BVP,type="l")
plot(1:16000,plot_day1$GSR,type="l")
plot(1:16000,plot_day20$GSR,type="l")
plot(1:16000,plot_day1$RES,type="l")
plot(1:16000,plot_day20$RES,type="l")

# FIGURE FOR PAPER: Emotion examples on Day 2
par(mfrow=c(4,3))
plot(1:2000,day2$EMG_Anger,type="l",xlab="",ylab="EMG",main="Anger")
plot(1:2000,day2$EMG_Grief,type="l",xlab="",ylab="EMG",main="Grief")
plot(1:2000,day2$EMG_Joy,type="l",xlab="",ylab="EMG",main="Joy")
plot(1:2000,day2$BVP_Anger,type="l",xlab="",ylab="BVP")
plot(1:2000,day2$BVP_Grief,type="l",xlab="",ylab="BVP")
plot(1:2000,day2$BVP_Joy,type="l",xlab="",ylab="BVP")
plot(1:2000,day2$GSR_Anger,type="l",xlab="",ylab="GSR")
plot(1:2000,day2$GSR_Grief,type="l",xlab="",ylab="GSR")
plot(1:2000,day2$GSR_Joy,type="l",xlab="",ylab="GSR")
plot(1:2000,day2$RES_Anger,type="l",xlab="",ylab="RES")
plot(1:2000,day2$RES_Grief,type="l",xlab="",ylab="RES")
plot(1:2000,day2$RES_Joy,type="l",xlab="",ylab="RES")

# Boxplots for each sensor 
summary(plot_total$EMG)
summary(plot_total$log_EMG)
summary(plot_total$BVP)
summary(plot_total$GSR)
summary(plot_total$RES)
boxplot(plot_total$log_EMG,plot_total$BVP,plot_total$GSR,
        plot_total$RES,names=c("log(EMG)","BVP","GSR","RES"))

# FIGURE FOR PAPER: Histograms and kernal density estimation for each sensor  
par(mfrow=c(2,2))
hist(plot_total$log_EMG,freq=FALSE,main="log(EMG)",xlab="")
lines(density(plot_total$log_EMG))
abline(v=median(plot_total$log_EMG),col="red")
abline(v=mean(plot_total$log_EMG),col="blue")
hist(plot_total$BVP,freq=FALSE,main="BVP",xlab="",ylim=c(0,.1))
lines(density(plot_total$BVP))
abline(v=median(plot_total$BVP),col="red")
abline(v=mean(plot_total$BVP),col="blue")
legend("topright",col=c("red","blue"),lty=c(1,1),legend=c("Median","Mean"))
hist(plot_total$GSR,freq=FALSE,main="GSR",xlab="",ylim=c(0,.2))
lines(density(plot_total$GSR))
abline(v=median(plot_total$GSR),col="red")
abline(v=mean(plot_total$GSR),col="blue")
hist(plot_total$RES,freq=FALSE,main="RES",xlab="",ylim=c(0,.25))
lines(density(plot_total$RES))
abline(v=median(plot_total$RES),col="red")
abline(v=mean(plot_total$RES),col="blue")

# FIGURE FOR PAPER: Seasonal decomposition of each sensor
EMG=ts(plot_total$log_EMG,frequency=16000)
fitEMG = stl(EMG, s.window="periodic", robust=TRUE)
plot(fitEMG,main="Decomposition of log(EMG)")
BVP=ts(plot_total$BVP,frequency=16000)
fitBVP = stl(BVP, s.window="periodic", robust=TRUE)
plot(fitBVP,main="Decomposition of BVP")
GSR=ts(plot_total$GSR,frequency=16000)
fitGSR = stl(GSR, s.window="periodic", robust=TRUE)
plot(fitGSR,main="Decomposition of GSR")
RES=ts(plot_total$RES,frequency=16000)
fitRES = stl(RES, s.window="periodic", robust=TRUE)
plot(fitRES,main="Decomposition of RES")



#### ARIMA Modeling ####

# log_EMG 
# auto.arima: ARIMA(2,1,2)  AIC=93033.43* AICc=93033.43 BIC=93086.55
# auto.arima w/o diff: ARIMA(2,0,0) AIC=150859.3 AICc=150859.3 BIC=150901.8
# ARIMA(1,0,0): aic = 189434.4  bic = 189466.3
# ARIMA(1,0,1): aic = 97815.08  bic = 97857.57
# ARIMA(2,0,0): aic = 150859.3  bic = 150901.8
# ARIMA(2,0,1): aic = 96042.27!!!  bic = 96095.39!!!
adf.test(plot_total$log_EMG[(1:304000)]) #stationary series!!!
kpss.test(plot_total$log_EMG[(1:304000)]) #non-stationary series
par(mfrow = c(3, 1))
plot(1:length(plot_total$log_EMG), plot_total$log_EMG, type = "l")
acf(plot_total$log_EMG)
pacf(plot_total$log_EMG)
par(mfrow = c(2, 1))
acf(diff(plot_total$log_EMG))
pacf(diff(plot_total$log_EMG))
EMG_Sensor = plot_total$log_EMG
acf2(EMG_Sensor,50)
Differenced_EMG_Sensor = diff(plot_total$log_EMG)
acf2(Differenced_EMG_Sensor,50)

#fitEMG = auto.arima(plot_total$log_EMG[(1:304000)],max.p=2,max.q=2,start.p=0,start.q=0,ic="bic")
fitEMG = arima(plot_total$log_EMG[(1:304000)],order=c(2,0,1))
BIC(fitEMG)
predEMG = predict(fitEMG, n.ahead=16000)
predEMG2 = predict(fitEMG)
MSE_EMG = mean((plot_total$log_EMG[(304001:320000)]-predEMG$pred)^2)
plot(1:length(plot_total$log_EMG), plot_total$log_EMG, type="l", xlim=c(300000,320000))
lines(predEMG$pred,col="red")

res = residuals(fitEMG)
par(mfrow = c(2, 1)) 
acf(res)
pacf(res)
qqnorm(res, main="Normal Q-Q Plot: log(EMG)")
qqline(res)

# BVP
# auto.arima: ARIMA(2,0,2)  AIC=1729399!!! AICc=1729399 BIC=1729463!!!
# ARIMA(1,0,0): aic = 2056048  bic = 2056080
# ARIMA(0,0,1): aic = 1938766  bic = 1938798
# ARIMA(1,0,1): aic = 1827322  bic = 1827365
# ARIMA(2,0,0): aic = 1874164  bic = 1874207
# ARIMA(2,0,1): aic = 1772439  bic = 1772493
adf.test(plot_total$BVP[(1:304000)]) #stationary series!!!
kpss.test(plot_total$BVP[(1:304000)]) #stationary series
par(mfrow = c(3, 1))
plot(1:length(plot_total$BVP), plot_total$BVP, type = "l")
acf(plot_total$BVP)
pacf(plot_total$BVP)
par(mfrow = c(2, 1))
acf(diff(plot_total$BVP))
pacf(diff(plot_total$BVP))
BVP_Sensor = plot_total$BVP
acf2(BVP_Sensor,50)
Differenced_BVP_Sensor = diff(plot_total$BVP)
acf2(Differenced_BVP_Sensor,50)

#fitBVP = auto.arima(plot_total$BVP[(1:304000)],max.p=2,max.q=2,start.p=0,start.q=0,ic="bic")
fitBVP = arima(plot_total$BVP[(1:304000)],order=c(2,0,2))
BIC(fitBVP)
predBVP = predict(fitBVP, n.ahead=16000)
predBVP2 = predict(fitBVP)
MSE_BVP = mean((plot_total$BVP[(304001:320000)]-predBVP$pred)^2)
plot(1:length(plot_total$BVP), plot_total$BVP, type="l", xlim=c(300000,320000))
lines(predBVP$pred,col="red")

res = residuals(fitBVP)
par(mfrow = c(2, 1)) 
acf(res)
pacf(res)
qqnorm(res, main="Normal Q-Q Plot: BVP")
qqline(res)

# GSR
# auto.arima: ARIMA(2,1,2)  AIC=-893642.8* AICc=-893642.8 BIC=-893589.7
# auto.arima w/o diff: ARIMA(0,0,2) AIC=598972.7 AICc=598972.7 BIC=599015.2
# ARIMA(1,0,0): aic = -891920.8  bic = -891888.9
# ARIMA(0,1,0): aic = -891883.6  bic = -891873
# ARIMA(1,0,1): aic = -892122.8  bic = -892080.3
# ARIMA(2,0,0): aic = -892102.2  bic = -892059.7
# ARIMA(2,0,2): aic = -893434.3!!!  bic = -893370.6!!!
adf.test(plot_total$GSR[(1:304000)]) #stationary series!!!
kpss.test(plot_total$GSR[(1:304000)]) #non-stationary series
par(mfrow = c(3, 1))
plot(1:length(plot_total$GSR), plot_total$GSR, type = "l")
acf(plot_total$GSR)
pacf(plot_total$GSR)
par(mfrow = c(2, 1))
acf(diff(plot_total$GSR))
pacf(diff(plot_total$GSR))
GSR_Sensor = plot_total$GSR
acf2(GSR_Sensor,50)
Differenced_GSR_Sensor = diff(plot_total$GSR)
acf2(Differenced_GSR_Sensor,50)

#fitGSR = auto.arima(plot_total$GSR[(1:304000)],max.p=2,max.q=2,start.p=0,start.q=0,ic="bic")
fitGSR = arima(plot_total$GSR[(1:304000)],order=c(2,0,2))
BIC(fitGSR)
predGSR = predict(fitGSR, n.ahead=16000)
predGSR2 = predict(fitGSR)
MSE_GSR = mean((plot_total$GSR[(304001:320000)]-predGSR$pred)^2)
plot(1:length(plot_total$GSR), plot_total$GSR, type="l", xlim=c(300000,320000))
lines(predGSR$pred,col="red")

res = residuals(fitGSR)
par(mfrow = c(2, 1)) 
acf(res)
pacf(res)
qqnorm(res, main="Normal Q-Q Plot: GSR")
qqline(res)

# RES
# auto.arima: ARIMA(1,1,1)  AIC=-750610.5!!! AICc=-750610.5 BIC=-750578.6
# auto.arima w/o diff: ARIMA(0,0,0) AIC=2039891 AICc=2039891 BIC=2039912
# ARIMA(1,0,0): aic = -621370.3  bic = -621338.4
# ARIMA(0,1,0): aic = -621370.2  bic = -621359.5
# ARIMA(1,1,0): aic = -716647.3  bic = -716626.1
# ARIMA(1,1,1): aic = -750610.5  bic = -750578.6!!!
# ARIMA(2,1,1): aic = -750610.1  bic = -750567.6
# ARIMA(2,1,2): aic = -750606.7  bic = -750553.5
adf.test(plot_total$RES[(1:304000)]) #stationary series
kpss.test(plot_total$RES[(1:304000)]) #non-stationary series!!!
par(mfrow = c(3, 1))
plot(1:length(plot_total$RES), plot_total$RES, type = "l")
acf(plot_total$RES)
pacf(plot_total$RES)
par(mfrow = c(2, 1))
acf(diff(plot_total$RES))
pacf(diff(plot_total$RES))
RES_Sensor = plot_total$RES
acf2(RES_Sensor,50)
Differenced_RES_Sensor = diff(plot_total$RES)
acf2(Differenced_RES_Sensor,50)

#fitRES = auto.arima(plot_total$RES[(1:304000)],max.p=2,max.q=2,start.p=0,start.q=0,ic="bic")
fitRES = arima(plot_total$RES[(1:304000)],order=c(1,1,1))
BIC(fitRES)
predRES = predict(fitRES, n.ahead=16000)
predRES2 = predict(fitRES)
MSE_RES = mean((plot_total$RES[(304001:320000)]-predRES$pred)^2)
plot(1:length(plot_total$RES), plot_total$RES, type="l", xlim=c(300000,320000))
lines(predRES$pred,col="red")

res = residuals(fitRES)
par(mfrow = c(2, 1)) 
acf(res)
pacf(res)
qqnorm(res, main="Normal Q-Q Plot: RES")
qqline(res)

# Plot ARIMA
arima_plot = function(sensor, pred1, pred2, mse, ylim=-1, ylimMSE=5, title1="ARIMA()"){
  time = seq(from=1,to=21,length.out=320000+1)
  plot_total$time = time[1:(length(time)-1)]
  X = data.frame(time=plot_total$time, y = sensor, pred = rep(0,320000))
  X$pred[1:304000] = pred2
  X$pred[304001:320000] = pred1
  label1=paste0("MSE = ", round(mse,4))
  return(ggplot(data = subset(X, time>=15 & y >-2.5)) + 
    geom_line(data=subset(X, time>=15 & y >ylim & time<20), aes(x=time, y=y)) + 
    geom_line(data=subset(X, time>20), aes(x=time, y=pred), color="red", size=.8) +
    geom_line(data=subset(X, time>=20 & y >ylim & time<21), aes(x=time, y=y), alpha=0.3) +
    geom_line(data=subset(X, time>=15 & time<=20), aes(x=time, y=pred), color="blue", size=.8) +
    geom_vline(aes(xintercept=20),lty=2) + 
    labs(list(title = title1, x = "Time (Days)", y = "Sensor Reading")) +
    annotate(geom="text", x=15.7, y=ylimMSE, label=label1, color="red"))
}
p1=arima_plot(plot_total$log_EMG, predEMG$pred, predEMG2$pred, mse=MSE_EMG, 
              ylimMSE=5, title1="ARIMA(2,0,1): log(EMG) Sensor")
p2=arima_plot(plot_total$BVP, predBVP$pred, predBVP2$pred, mse=MSE_BVP, 
              ylimMSE=100,title1="ARIMA(2,0,2): BVP Sensor")
p3=arima_plot(plot_total$GSR, predGSR$pred, predGSR2$pred, mse=MSE_GSR, 
              ylimMSE=12,title1="ARIMA(2,0,2): GSR Sensor")
p4=arima_plot(plot_total$RES, predRES$pred, predRES2$pred, mse=MSE_RES, 
              ylimMSE=65,title1="ARIMA(1,1,1): RES Sensor")

# FIGURE FOR PAPER: ARIMA Forecast 
grid.arrange(p1, p2, p3, p4, nrow=2, top="ARIMA Forecasting")




#### GARCH Modeling ####

# log_EMG
par(mfrow=c(2,1))
# GARCH(1,0)*
summary(fit <- garchFit(~garch(1,0),data=plot_total$log_EMG))
u1 = fit@sigma.t
plot(window(plot_total$log_EMG, start=306000, end=308000), type="l")
lines(window(plot_total$log_EMG-2*u1, start=306000, end=308000), lty=2, col=4)
lines(window(plot_total$log_EMG+2*u1, start=306000, end=308000), lty=2, col=4)
# GARCH(1,1)
summary(fit <- garchFit(~garch(1,1),data=plot_total$log_EMG))
u2 = fit@sigma.t
plot(window(plot_total$log_EMG, start=306000, end=308000), type="l")
lines(window(plot_total$log_EMG-2*u2, start=306000, end=308000), lty=2, col=4)
lines(window(plot_total$log_EMG+2*u2, start=306000, end=308000), lty=2, col=4)

# BVP
par(mfrow=c(2,1))
# GARCH(1,0)
summary(fit <- garchFit(~garch(1,0),data=plot_total$BVP))
u3 = fit@sigma.t
plot(window(plot_total$BVP, start=306000, end=308000), type="l")
lines(window(plot_total$BVP-2*u3, start=306000, end=308000), lty=2, col=4)
lines(window(plot_total$BVP+2*u3, start=306000, end=308000), lty=2, col=4)
# GARCH(1,1)*
summary(fit <- garchFit(~garch(1,1),data=plot_total$BVP))
u4 = fit@sigma.t
plot(window(plot_total$BVP, start=306000, end=308000), type="l")
lines(window(plot_total$BVP-2*u4, start=306000, end=308000), lty=2, col=4)
lines(window(plot_total$BVP+2*u4, start=306000, end=308000), lty=2, col=4)

# GSR
par(mfrow=c(2,1))
# GARCH(1,0)
summary(fit <- garchFit(~garch(1,0),data=plot_total$GSR))
u5 = fit@sigma.t
plot(window(plot_total$GSR, start=306000, end=308000), type="l")
lines(window(plot_total$GSR-2*u5, start=306000, end=308000), lty=2, col=4)
lines(window(plot_total$GSR+2*u5, start=306000, end=308000), lty=2, col=4)
# GARCH(1,1)*
summary(fit <- garchFit(~garch(1,1),data=plot_total$GSR))
u6 = fit@sigma.t
plot(window(plot_total$GSR, start=306000, end=308000), type="l")
lines(window(plot_total$GSR-2*u6, start=306000, end=308000), lty=2, col=4)
lines(window(plot_total$GSR+2*u6, start=306000, end=308000), lty=2, col=4)

# RES
par(mfrow=c(2,1))
# GARCH(1,0)*
summary(fit <- garchFit(~garch(1,0),data=plot_total$RES))
u7 = fit@sigma.t
plot(window(plot_total$RES, start=306000, end=308000), type="l")
lines(window(plot_total$RES-2*u7, start=306000, end=308000), lty=2, col=4)
lines(window(plot_total$RES+2*u7, start=306000, end=308000), lty=2, col=4)
# GARCH(1,1)
summary(fit <- garchFit(~garch(1,1),data=plot_total$RES))
u8 = fit@sigma.t
plot(window(plot_total$RES, start=306000, end=308000), type="l")
lines(window(plot_total$RES-2*u8, start=306000, end=308000), lty=2, col=4)
lines(window(plot_total$RES+2*u8, start=306000, end=308000), lty=2, col=4)

# Plot GARCH 
garch_plot = function(sensor, plot_total, ylim=-1, title1="GARCH(1,1)"){
  time = seq(from=1,to=21,length.out=320000+1)
  plot_total$time = time[1:(length(time)-1)]
  summary(fit <- garchFit(~garch(1,1),data=sensor))
  X = data.frame(time=plot_total$time, y = sensor, upper = sensor+2*fit@sigma.t,
                 lower = sensor-2*fit@sigma.t)
  return(ggplot(data = subset(X, time>=2 & y >-2.5 & time<3)) + 
    geom_line(data=subset(X, time>=2 & y>ylim & time<3),aes(x=time,y=y)) + 
    geom_line(data=subset(X, time>=2 & y>ylim & time<3),aes(x=time,y=lower),color="blue",linetype=2) +
    geom_line(data=subset(X, time>=2 & y>ylim & time<3),aes(x=time,y=upper),color="blue",linetype=2) +
    labs(list(title = title1, x = "Time (Days)", y = "Sensor Reading")))
}
p1=garch_plot(plot_total$log_EMG, plot_total, title1="GARCH(1,1): log(EMG) Sensor")
p2=garch_plot(plot_total$BVP, plot_total, title1="GARCH(1,1): BVP Sensor")
p3=garch_plot(plot_total$GSR, plot_total, title1="GARCH(1,1): GSR Sensor")
p4=garch_plot(plot_total$RES, plot_total, title1="GARCH(1,1): RES Sensor")

# FIGURE FOR PAPER: ARIMA Forecast 
grid.arrange(p1, p2, p3, p4, nrow=2, top="GARCH Modeling")



#### Harmonic Regression ####
harmonic = function(sensor, plot_total, ylim=-1, ylimMSE, title1){
  time = seq(from=1,to=21,length.out=320000+1)
  plot_total$time = time[1:(length(time)-1)]
  X = data.frame(time=plot_total$time,
                 y = sensor,
                 sin(2*pi*8*plot_total$time), cos(2*pi*8*plot_total$time), # per emotion
                 sin(2*pi*2*plot_total$time), cos(2*pi*2*plot_total$time), # half a day
                 sin(2*pi*1*plot_total$time), cos(2*pi*1*plot_total$time), # one day 
                 sin(2*pi*(1/2)*plot_total$time), cos(2*pi*(1/2)*plot_total$time), # 2 days
                 sin(2*pi*(1/3.5)*plot_total$time), cos(2*pi*(1/3.5)*plot_total$time), # 3.5 days 
                 sin(2*pi*(1/7)*plot_total$time), cos(2*pi*(1/7)*plot_total$time), # 7 days
                 sin(2*pi*(1/10)*plot_total$time), cos(2*pi*(1/10)*plot_total$time), # 10 days
                 sin(2*pi*(1/20)*plot_total$time), cos(2*pi*(1/20)*plot_total$time) # 20 days
  )
  # ggplot(data=subset(X, time>1)) + geom_line(aes(x=time, y=X[X$time>19,3]))
  mod = lm(y ~ . - time, data = X[(1:304000),])
  print(summary(mod))
  print(paste0("AIC: ", AIC(mod)))
  resid = residuals(mod)
  X$pred = rep(0,320000); X$se = rep(0,320000)
  pred1 = predict(mod, X[304001:320000,],interval="confidence")
  X$pred[1:304000] = predict(mod)
  X$pred[304001:320000] = pred1[,"fit"]
  # X$lwr[304001:320000] = pred[,"lwr"]
  # X$upr[304001:320000] = pred[,"upr"]
  
  MSE = mean((X$y[(304001:320000)]-X$pred[(304001:320000)])^2)
  label1=paste0("MSE = ", round(MSE,4))
  return(ggplot(data=subset(X, time>=15 & y >-2.5)) + 
    geom_line(data=subset(X, time>=15 & y >ylim & time<20),aes(x=time,y=y)) + 
    geom_line(data=subset(X, time>20), aes(x=time, y=pred),color="red",size=.8) +
    geom_line(data=subset(X, time>=20 & y >ylim & time<21),aes(x=time,y=y),alpha=0.3) +
    geom_line(data=subset(X, time>=15 & time<=20),aes(x=time, y=pred),color="blue",size=.8) +
    geom_vline(aes(xintercept=20),lty=2) + 
      labs(list(title = title1, x = "Time (Days)", y = "Sensor Reading")) +
    annotate(geom="text", x=15.7, y=ylimMSE, label=label1, color="red"))
    # geom_ribbon(data = subset(X, time>20), aes(x=time, ymin=seMin, ymax=seMax), alpha=0.3)
}
p1=harmonic(plot_total$log_EMG,plot_total,ylimMSE=5,title1="log(EMG) Sensor")
p2=harmonic(plot_total$BVP,plot_total,ylimMSE=100,title1="BVP Sensor")
p3=harmonic(plot_total$GSR,plot_total,ylimMSE=12,title1="GSR Sensor")
p4=harmonic(plot_total$RES,plot_total,ylimMSE=65,title1="RES Sensor")

# FIGURE FOR PAPER: Harmonic Regression Forecast 
grid.arrange(p1, p2, p3, p4,nrow=2, top="Harmonic Regression")



#### Spectral Analysis ####

# FREQUENCY STUFF
periodogram = function(sensor){
  par(mfrow=c(2,1))
  raw = spec.pgram(sensor, taper=0)
  plot(raw)
  plot(raw, log="no")
  return(raw)
}
rawEMG = periodogram(plot_total$EMG)
rawBVP = periodogram(plot_total$BVP)
rawGSR = periodogram(plot_total$GSR)
rawRES = periodogram(plot_total$RES)

spec.df = data.frame(freqEMG=rawEMG$freq, specEMG=rawEMG$spec,
                      freqBVP=rawBVP$freq, specBVP=rawBVP$spec,
                      freqGSR=rawGSR$freq, specGSR=rawGSR$spec,
                      freqRES=rawRES$freq, specRES=rawRES$spec)
time.period = rev(c(1/8, 1/4, 1/2, 1, 2, 4, 8, 10, 20, 40, 80, 365, 730)) 
time.labels = rev(c("1/8","1/4","1/2","1","2","4","8","10","20","40","80","365","730"))
time.freqs = 1/time.period * 1/8
spec.df$periodEMG = 1/spec.df$freqEMG; spec.df$periodBVP = 1/spec.df$freqBVP
spec.df$periodGSR = 1/spec.df$freqGSR; spec.df$periodRES = 1/spec.df$freqRES

plot1 = function(spec.df, freq, spec, time.freqs, time.labels){
  ggplot(data=subset(spec.df)) + geom_line(aes(x=freq, y=spec)) + 
    scale_x_continuous("Period (days)", breaks=time.freqs, labels=time.labels) + 
    scale_y_continuous()
}
plot2 = function(spec.df, freq, spec, time.freqs, time.labels){
  ggplot(data=subset(spec.df)) + geom_line(aes(x=freq, y=spec)) + 
    scale_x_continuous("Period (days)", breaks=time.freqs, labels=time.labels) + 
    scale_y_log10() 
}
plot3 = function(spec.df, freq, spec, time.freqs, time.labels){
  ggplot(data=subset(spec.df)) + geom_line(aes(x=freq, y=spec)) + 
    scale_x_log10("Period (days)", breaks=time.freqs, labels=time.labels) + 
    scale_y_log10() 
}
plot1(spec.df, spec.df$freqEMG, spec.df$specEMG, time.freqs, time.labels)
plot1(spec.df, spec.df$freqBVP, spec.df$specBVP, time.freqs, time.labels)
plot1(spec.df, spec.df$freqGSR, spec.df$specGSR, time.freqs, time.labels)
plot1(spec.df, spec.df$freqRES, spec.df$specRES, time.freqs, time.labels)

emg2=plot2(spec.df, spec.df$freqEMG, spec.df$specEMG, time.freqs, time.labels)
bvp2=plot2(spec.df, spec.df$freqBVP, spec.df$specBVP, time.freqs, time.labels)
gsr2=plot2(spec.df, spec.df$freqGSR, spec.df$specGSR, time.freqs, time.labels)
res2=plot2(spec.df, spec.df$freqRES, spec.df$specRES, time.freqs, time.labels)

emg1=plot3(spec.df, spec.df$freqEMG, spec.df$specEMG, time.freqs, time.labels)
bvp1=plot3(spec.df, spec.df$freqBVP, spec.df$specBVP, time.freqs, time.labels)
gsr1=plot3(spec.df, spec.df$freqGSR, spec.df$specGSR, time.freqs, time.labels)
res1=plot3(spec.df, spec.df$freqRES, spec.df$specRES, time.freqs, time.labels)

# FIGURE FOR PAPER: Periodograms
grid.arrange(emg1, emg2, nrow=1, top="Logged Periodogram: EMG Sensor")
grid.arrange(bvp1, bvp2, nrow=1, top="Logged Periodogram: BVP Sensor")
grid.arrange(gsr1, gsr2, nrow=1, top="Logged Periodogram: GSR Sensor")
grid.arrange(res1, res2, nrow=1, top="Logged Periodogram: RES Sensor")



# KERNAL 
choose_kernal = function(sensor, title1){
  k = kernel("daniell", c(9, 9, 9))
  smooth.spec <- spec.pgram(sensor, kernel=k, taper = 0)
  # Note how the confidence interval got much narrower
  spec.df = data.frame(freq=smooth.spec$freq, `c(9,9,9)`=smooth.spec$spec)
  names(spec.df) = c("freq","c(9,9,9)")
  # Add other smooths
  k <- kernel("daniell", c(9, 9))
  spec.df[, "c(9,9)"] = spec.pgram(sensor, kernel=k, taper=0, plot=FALSE)$spec
  k <- kernel("daniell", c(9))
  spec.df[, "c(9)"] = spec.pgram(sensor, kernel=k, taper=0, plot=FALSE)$spec
  # melt from wide format into long format
  spec.df = melt(spec.df, id.vars="freq", value.name="spec", variable.name="kernel")
  plot1 = ggplot(data = subset(spec.df)) + geom_path(aes(x = freq, y = spec, 
              color = kernel)) + theme(axis.title.x = element_blank()) + 
              scale_x_continuous(breaks = time.freqs, labels = time.labels) + 
              scale_y_log10()
  plot2 = ggplot(data = subset(spec.df)) + geom_path(aes(x = freq, y = spec, 
              color = kernel)) + scale_x_log10("Period (days)", 
              breaks = time.freqs, labels = time.labels) + scale_y_log10()
  grid.arrange(plot1, plot2, top=title1)
}
choose_kernal(plot_total$EMG, title1="Kernel Smoothing: EMG Sensor")
choose_kernal(plot_total$BVP, title1="Kernel Smoothing: BVP Sensor")
choose_kernal(plot_total$GSR, title1="Kernel Smoothing: GSR Sensor")
choose_kernal(plot_total$RES, title1="Kernel Smoothing: RES Sensor")

# TAPERING
tapering = function(sensor, title1){
  k = kernel("daniell", c(9, 9))
  smooth.spec = spec.pgram(sensor, kernel = k, taper = 0, plot = FALSE)
  spec.df = data.frame(freq = smooth.spec$freq, `0%` = smooth.spec$spec)
  names(spec.df) <- c("freq", "0%")
  # Add other tapers
  spec.df[, "10%"] = spec.pgram(sensor, kernel = k, taper = 0.1, plot = FALSE)$spec
  spec.df[, "30%"] = spec.pgram(sensor, kernel = k, taper = 0.3, plot = FALSE)$spec
  spec.df = melt(spec.df, id.vars = "freq", value.name = "spec", variable.name="taper")
  plot1 = ggplot(data = subset(spec.df)) + geom_path(aes(x = freq, y = spec, 
              color = taper)) + scale_x_continuous(breaks = time.freqs, 
              labels = time.labels) + scale_y_log10() + theme(axis.title.x=element_blank()) 
  plot2 <- ggplot(data = subset(spec.df)) + geom_path(aes(x = freq, y = spec, 
            color = taper)) + scale_x_log10("Period (years)", breaks = time.freqs, 
            labels = time.labels) + scale_y_log10()
  grid.arrange(plot1, plot2, top=title1)
}
tapering(plot_total$EMG, title1="Tapering: EMG Sensor")
tapering(plot_total$BVP, title1="Tapering: BVP Sensor")
tapering(plot_total$GSR, title1="Tapering: GSR Sensor")
tapering(plot_total$RES, title1="Tapering: RES Sensor")

# MULTI-TAPERING
spec.mtm(plot_total$EMG, nw = 16, k = 2 * 16 - 1, jackknife = TRUE, dtUnits = "day")
spec.mtm(plot_total$BVP, nw = 16, k = 2 * 16 - 1, jackknife = TRUE, dtUnits = "day")
spec.mtm(plot_total$GSR, nw = 16, k = 2 * 16 - 1, jackknife = TRUE, dtUnits = "day")
spec.mtm(plot_total$RES, nw = 16, k = 2 * 16 - 1, jackknife = TRUE, dtUnits = "day")

# TIME FREQUENCY ESTIMATION
wavelet = function(sensor){
  time = seq(from=1,to=21,length.out=320000+1)
  time = time[1:(length(time)-1)]
  time = time[seq(from=1,to=320000,by=10)]
  sensor = sensor[seq(from=1,to=320000,by=10)]
  wave.out = morlet(y1 = sensor, x1 = time, p2 = 8, dj = 0.1, siglvl = 0.95)
  wave.out$period = wave.out$period/8
  levs = quantile(wave.out$Power, c(0, 0.25, 0.5, 0.75, 0.95, 1))
  wavelet.plot(wave.out, wavelet.levels = levs, crn.ylim = c(22.5, 30))
}
wavelet(plot_total$EMG)
wavelet(plot_total$BVP)
wavelet(plot_total$GSR)
wavelet(plot_total$RES)


