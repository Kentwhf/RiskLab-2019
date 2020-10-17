### Libraries
library(lubridate)
library(ggplot2)
library(timeSeries)
library(dplyr)
library(tidyr)
library(reshape2)
library(directlabels)
library(data.table)
library(PerformanceAnalytics)
library(randomForest)


riskLabreturn = read.csv("riskLabreturn.csv", header = TRUE)
head(riskLabreturn)

riskLabreturn$Date = as.Date(riskLabreturn$Date, format = "%m/%d/%Y")
riskLabreturn$AGT = as.numeric(as.character(riskLabreturn$AGT)) * 0.00145726
riskLabreturn$AD = as.numeric(as.character(riskLabreturn$AD)) * 0.002544504
riskLabreturn$WEED = as.numeric(as.character(riskLabreturn$WEED)) * 0.071239906
riskLabreturn$CSH.UN = as.numeric(as.character(riskLabreturn$CSH.UN)) * 0.010707942
riskLabreturn$CSU = as.numeric(as.character(riskLabreturn$CSU)) * 0.08086692
riskLabreturn$DOL = as.numeric(as.character(riskLabreturn$DOL)) * 0.039386311
riskLabreturn$H = as.numeric(as.character(riskLabreturn$H)) * 0.041379272
riskLabreturn$KXS = as.numeric(as.character(riskLabreturn$KXS)) * 0.006722021
riskLabreturn$NPI = as.numeric(as.character(riskLabreturn$NPI)) * 0.015403223
riskLabreturn$MFI = as.numeric(as.character(riskLabreturn$MFI)) * 0.011518638
riskLabreturn$PKI = as.numeric(as.character(riskLabreturn$PKI)) * 0.018814902
riskLabreturn$RY = as.numeric(as.character(riskLabreturn$RY)) * 0.501888347
riskLabreturn$SU = as.numeric(as.character(riskLabreturn$SU)) * 0.176529042
riskLabreturn$SPB = as.numeric(as.character(riskLabreturn$SPB)) * 0.006688242
riskLabreturn$TCS = as.numeric(as.character(riskLabreturn$TCS)) * 0.000602752
riskLabreturn$TVK = as.numeric(as.character(riskLabreturn$TVK)) * 0.000720675
riskLabreturn$X = as.numeric(as.character(riskLabreturn$X)) * 0.011721312
riskLabreturn$ZYME = as.numeric(as.character(riskLabreturn$ZYME)) * 0.00180873
riskLabreturn$Market = as.numeric(as.character(riskLabreturn$Market)) 
riskLabreturn$RiskFree = as.numeric(as.character(riskLabreturn$RiskFree))/100





riskLabreturn$HighVol <- rowMeans(riskLabreturn[2:(1 + nrow(riskLabreturn)),c(2:4,6,9,12,16:17,19)],na.rm = TRUE)/(0.00145726 + 0.002544504 + 0.071239906 + 0.08086692 + 0.006722021 + 0.018814902 + 0.000602752 + 0.000720675 + 0.00180873)
riskLabreturn$LowVol <- rowMeans(riskLabreturn[2:(1 + nrow(riskLabreturn)), c(5,7:8,10:11, 13:15,18)], na.rm = TRUE)/(0.010707942 + 0.039386311 + 0.041379272 + 0.015403223 + 0.011518638 + 0.501888347 + 0.176529042 + 0.006688242 + 0.011721312)
riskLabreturn$HighDiv <- rowMeans(riskLabreturn[2:(1 + nrow(riskLabreturn)), c(3,5,8,10,12,13,15,17,18)], na.rm = TRUE)/(0.002544504 + 0.010707942 + 0.041379272 + 0.015403223 + 0.018814902 + 0.050188347 + 0.006688242 + 0.000720675 + 0.011721312)
riskLabreturn$LowDiv <- rowMeans(riskLabreturn[2:(1 + nrow(riskLabreturn)), c(2,4,6:7,9,11,14,16,19)], na.rm = TRUE)/(1 -(0.002544504 + 0.010707942 + 0.041379272 + 0.015403223 + 0.018814902 + 0.050188347 + 0.006688242 + 0.000720675 + 0.011721312))

riskLabreturn$Tech <- rowMeans(riskLabreturn[2:(1+nrow(riskLabreturn)), c(6,9,16)], na.rm = TRUE)/(0.08086692 + 0.006722021 + 0.000602752)
riskLabreturn$Energy <- rowMeans(riskLabreturn[2:(1+nrow(riskLabreturn)), c(12,14,17)], na.rm = TRUE)/(0.176529042 + 0.018814902 + 0.000720675)
riskLabreturn$Healthcare <- rowMeans(riskLabreturn[2:(1+nrow(riskLabreturn)), c(4,5,19)], na.rm = TRUE)/(0.071239906 + 0.010707942 + 0.00180873)
riskLabreturn$Condefense <- rowMeans(riskLabreturn[2:(1+nrow(riskLabreturn)), c(2,7,11)], na.rm = TRUE)/(0.00145726 + 0.039386311 + 0.011518638)
riskLabreturn$Financials <- rowMeans(riskLabreturn[2:(1+nrow(riskLabreturn)), c(3,13,18)], na.rm = TRUE)/(0.501888347 + 0.011721312 + 0.002544504)
riskLabreturn$Utilities <- rowMeans(riskLabreturn[2:(1+nrow(riskLabreturn)), c(8,10,15)], na.rm = TRUE)/(0.015403323 + 0.041379272 + 0.006688242)
riskLabreturn$Small <- rowMeans(riskLabreturn[2:(1+nrow(riskLabreturn)), c(19,17,16,15,3,2)], na.rm = TRUE)/(0.00180873 + 0.000720675 + 0.000602752 + 0.006688242 + 0.002544504 + 0.00145726)
riskLabreturn$Medium <- rowMeans(riskLabreturn[2:(1+nrow(riskLabreturn)), c(18,12,11,10,9,5)], na.rm = TRUE)/(0.011721312 + 0.018814902 + 0.011518638 + 0.015403223 + 0.006722021 + 0.010707942)
riskLabreturn$Large <- rowMeans(riskLabreturn[2:(1+nrow(riskLabreturn)), c(14,13,8,7,6,4)], na.rm = TRUE)/(0.176529042 + 0.501888347 + 0.041379272 + 0.039386311 + 0.08086692 + 0.071239906)
riskLabreturn$VolDif <- riskLabreturn$LowVol - riskLabreturn$HighVol
riskLabreturn$DivDif <- riskLabreturn$HighDiv - riskLabreturn$LowDiv
riskLabreturn$Excess <- riskLabreturn$Market - riskLabreturn$RiskFree


riskLabreturn$AGT =  riskLabreturn$AGT/ 0.00145726
riskLabreturn$AD =  riskLabreturn$AD/ 0.002544504
riskLabreturn$WEED =  riskLabreturn$WEED/ 0.071239906
riskLabreturn$CSH.UN =  riskLabreturn$CSH.UN/ 0.010707942
riskLabreturn$CSU =  riskLabreturn$CSU/ 0.08086692
riskLabreturn$DOL =  riskLabreturn$DOL/ 0.039386311
riskLabreturn$H =  riskLabreturn$H/ 0.041379272
riskLabreturn$KXS =  riskLabreturn$KXS/ 0.006722021
riskLabreturn$NPI =  riskLabreturn$NPI/ 0.015403223
riskLabreturn$MFI =  riskLabreturn$MFI/ 0.011518638
riskLabreturn$PKI =  riskLabreturn$PKI/ 0.018814902
riskLabreturn$RY =  riskLabreturn$RY/ 0.501888347
riskLabreturn$SU =  riskLabreturn$SU/ 0.176529042
riskLabreturn$SPB =  riskLabreturn$SPB/ 0.006688242
riskLabreturn$TCS =  riskLabreturn$TCS/ 0.000602752
riskLabreturn$TVK =  riskLabreturn$TVK/ 0.000720675
riskLabreturn$X =  riskLabreturn$X/ 0.011721312
riskLabreturn$ZYME =  riskLabreturn$ZYME/ 0.00180873

#Alternative
model.RY <- glm(riskLabreturn$RY ~ riskLabreturn$VolDif * riskLabreturn$DivDif * riskLabreturn$Excess + offset(riskLabreturn$RiskFree), family = gaussian(), method = "glm.fit", data = riskLabreturn)
summary(model.RY)
plot(model.RY)



#priceModel = function(x, y, z){coef(model.AGT)[1]}
#ggplot(riskLabreturn, aes(x = model.AGT, y = riskLabreturn$AGT)) + geom_point() + geom_smooth(method = "lm")

index <- sample(996,498)
AGTTrain <- riskLabreturn$AGT[index]
AGTTest <- data.frame(AGTTest = riskLabreturn$AGT[-index])
model.AGT <- lm(AGTTrain ~ riskLabreturn$VolDif[index] + riskLabreturn$DivDif[index] + riskLabreturn$Excess[index] + offset(riskLabreturn$RiskFree[index]), data = riskLabreturn)
summary(model.AGT)
prediction <- predict(model.AGT, AGTTest, type = "terms")



index <- sample(996,498)
RYTrain <- riskLabreturn$RY[index]
RYTest <- data.frame(RYTest = riskLabreturn$RY[-index])
model.RY <- lm(RYTrain ~ riskLabreturn$VolDif[index] + riskLabreturn$DivDif[index] + riskLabreturn$Excess[index] + offset(riskLabreturn$RiskFree[index]), data = riskLabreturn)
summary(model.RY)
prediction <- predict(model.RY, RYTest, type = "terms")



index <- sample(60:960,450)
WEEDTrain <- riskLabreturn$WEED[index]
WEEDTest <- data.frame(WEEDTest = riskLabreturn$WEED[-index])
model.WEED <- lm(WEEDTrain ~ riskLabreturn$VolDif[index] + riskLabreturn$DivDif[index] + riskLabreturn$Excess[index] + offset(riskLabreturn$RiskFree[index]), data = riskLabreturn)
summary(model.WEED)
prediction <- predict(model.WEED, WEEDTest[1:450,2], type = "terms")



index <- sample(996,498)
CSH.UNTrain <- riskLabreturn$CSH.UN[index]
CSH.UNTest <- data.frame(CSH.UNTest = riskLabreturn$CSH.UN[-index])
model.CSH.UN <- lm(CSH.UNTrain ~ riskLabreturn$VolDif[index] + riskLabreturn$DivDif[index] + riskLabreturn$Excess[index] + offset(riskLabreturn$RiskFree[index]), data = riskLabreturn)
summary(model.CSH.UN)
prediction <- predict(model.CSH.UN, CSH.UNTest, type = "terms")


index <- sample(996,498)
CSUTrain <- riskLabreturn$CSU[index]
CSUTest <- data.frame(CSUTest = riskLabreturn$CSU[-index])
model.CSU <- lm(CSUTrain ~ riskLabreturn$VolDif[index] + riskLabreturn$DivDif[index] + riskLabreturn$Excess[index] + offset(riskLabreturn$RiskFree[index]), data = riskLabreturn)
summary(model.CSU)
prediction <- predict(model.CSU, CSUTest, type = "terms")


index <- sample(996,498)
SPBTrain <- riskLabreturn$SPB[index]
SPBTest <- data.frame(SPBTest = riskLabreturn$SPB[-index])
model.SPB <- lm(SPBTrain ~ riskLabreturn$VolDif[index] + riskLabreturn$DivDif[index] + riskLabreturn$Excess[index] + offset(riskLabreturn$RiskFree[index]), data = riskLabreturn)
summary(model.SPB)
prediction <- predict(model.SPB, SPBTest, type = "terms")


index <- sample(996,498)
XTrain <- riskLabreturn$X[index]
XTest <- data.frame(XTest = riskLabreturn$X[-index])
model.X <- lm(XTrain ~ riskLabreturn$VolDif[index] + riskLabreturn$DivDif[index] + riskLabreturn$Excess[index] + offset(riskLabreturn$RiskFree[index]), data = riskLabreturn)
summary(model.X)
prediction <- predict(model.X, XTest, type = "terms")


index <- sample(996,498)
ADTrain <- riskLabreturn$AD[index]
ADTest <- data.frame(ADTest = riskLabreturn$AD[-index])
model.AD <- lm(ADTrain ~ riskLabreturn$VolDif[index] + riskLabreturn$DivDif[index] + riskLabreturn$Excess[index] + offset(riskLabreturn$RiskFree[index]), data = riskLabreturn)
summary(model.AD)
prediction <- predict(model.AD, ADTest, type = "terms")



index <- sample(996,498)
MFITrain <- riskLabreturn$MFI[index]
MFITest <- data.frame(MFITest = riskLabreturn$MFI[-index])
model.MFI <- lm(MFITrain ~ riskLabreturn$VolDif[index] + riskLabreturn$DivDif[index] + riskLabreturn$Excess[index] + offset(riskLabreturn$RiskFree[index]), data = riskLabreturn)
summary(model.MFI)
prediction <- predict(model.MFI, MFITest, type = "terms")



model.PKI <- lm(riskLabreturn$PKI ~ riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)
summary(model.PKI)

model.TVK <- lm(riskLabreturn$TVK ~ riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)
summary(model.TVK)


model.TCS <- lm(riskLabreturn$TCS ~ riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)
summary(model.TCS)

model.SU <- lm(riskLabreturn$SU ~ riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)
summary(model.SU)

model.DOL <- lm(riskLabreturn$DOL ~ riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)
summary(model.DOL)

model.NPI <- lm(riskLabreturn$NPI ~ riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)
summary(model.NPI)

model.Small <- lm(riskLabreturn$Small ~ riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)
summary(model.Small)

model.Medium <- lm(riskLabreturn$Medium ~ riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)
summary(model.Medium)

model.Large <- lm(riskLabreturn$Large ~riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)
summary(model.Large)
plot(model.Large)


model.Tech <- lm(riskLabreturn$Tech ~ riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)
summary(model.Tech)


model.Energy <- lm(riskLabreturn$Energy ~ riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)


summary(model.Energy)

model.Healthcare <- lm(riskLabreturn$Healthcare ~ riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)
summary(model.Healthcare)

model.Condefense <- lm(riskLabreturn$Condefense ~ riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)
summary(model.Condefense)

model.Financials <- lm(riskLabreturn$Financials ~ riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)
summary(model.Financials)

model.Utilities <- lm(riskLabreturn$Utilities ~ riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)
summary(model.Utilities)




length(na.omit(riskLabreturn$H))
model.H <- lm(riskLabreturn$H ~ riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)
summary(model.H)

model.KXS <- lm(riskLabreturn$KXS ~ riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)
summary(model.KXS)

model.ZYME <- lm(riskLabreturn$ZYME ~ riskLabreturn$VolDif + riskLabreturn$DivDif + riskLabreturn$Excess + offset(riskLabreturn$RiskFree), data = riskLabreturn)
summary(model.ZYME)
