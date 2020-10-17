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



###Read File
stockdf <- read.csv("riskLab.csv")
head(stockdf)
tail(stockdf)

stockdf <- na.omit(stockdf)



stockdf$Adj.Close = as.numeric(as.character(stockdf$Adj.Close))
stockdf$Volume = as.numeric(as.character(stockdf$Volume))
stockdf$High = as.numeric(as.character(stockdf$High))
stockdf$Low = as.numeric(as.character(stockdf$Low))
stockdf$Open = as.numeric(as.character(stockdf$Open))


stockdf$ret <- c(-diff(stockdf$Adj.Close)/stockdf$Adj.Close[-1] * 100, NA)
stockdf$intraVol <- (stockdf$High - stockdf$Low)/stockdf$Open * 100


stockdf$Open <- NULL
stockdf$High <- NULL
stockdf$Adj.Close <- NULL

unique(stockdf$Company)

stockdf$Ticker[stockdf$Company == "Alaris Royalty Corp."] <- "AD"
stockdf$Ticker[stockdf$Company == "TECSYS Inc."] <- "TCS"
stockdf$Ticker[stockdf$Company == "Kinaxis"] <- "KXS"
stockdf$Ticker[stockdf$Company == "Constellation Software"] <- "CSU"
stockdf$Ticker[stockdf$Company == "TerraVest Industries Inc"] <- "TVK"
stockdf$Ticker[stockdf$Company == "Parkland Fuel Corporation"] <- "PKI"
stockdf$Ticker[stockdf$Company == "Suncor Energy Inc."] <- "SU"
stockdf$Ticker[stockdf$Company == "Zymeworks"] <- "ZYME"
stockdf$Ticker[stockdf$Company == "Chartwell Retirement Residences"] <- "CSH.UN"
stockdf$Ticker[stockdf$Company == "AGT Food and Ingredients"] <- "AGT"
stockdf$Ticker[stockdf$Company == "Maple Leaf Foods"] <- "MFI"
stockdf$Ticker[stockdf$Company == "Dollarama"] <- "DOL"
stockdf$Ticker[stockdf$Company == "TMX Group Limited"] <- "X"
stockdf$Ticker[stockdf$Company == "Royal Bank of Canada"] <- "RY"
stockdf$Ticker[stockdf$Company == "superior plus corp"] <- "SPB"
stockdf$Ticker[stockdf$Company == "Northland Power"] <- "NPI"
stockdf$Ticker[stockdf$Company == "Hydro One Limited"] <- "H"
stockdf$Ticker[stockdf$Company == "Canopy Growth Corporation"] <- "WEED"


unique(stockdf$Ticker)
stockdf$Company <- NULL

head(stockdf)


stockdf$MarketCap[stockdf$Ticker == "TCS"| stockdf$Ticker == "TVK"|stockdf$Ticker == "ZYME"|stockdf$Ticker == "AGT" | stockdf$Ticker == "AD" | stockdf$Ticker == "SPB"] <- "Small"
stockdf$MarketCap[stockdf$Ticker == "KXS"| stockdf$Ticker == "PKI"|stockdf$Ticker == "CSH.UN"|stockdf$Ticker == "MFI" | stockdf$Ticker == "X" | stockdf$Ticker == "NPI"] <- "Medium"
stockdf$MarketCap[stockdf$Ticker == "CSU"| stockdf$Ticker == "SU"|stockdf$Ticker == "WEED"|stockdf$Ticker == "DOL" | stockdf$Ticker == "RY" | stockdf$Ticker == "H"] <- "Large"

stockdf$Sector[stockdf$Ticker == "TCS"| stockdf$Ticker == "KXS"|stockdf$Ticker == "CSU"] <- "Technology"
stockdf$Sector[stockdf$Ticker == "TVK"| stockdf$Ticker == "PKI"|stockdf$Ticker == "SU"] <- "Energy"
stockdf$Sector[stockdf$Ticker == "ZYME"| stockdf$Ticker == "CSH.UN"|stockdf$Ticker == "WEED"] <- "Healthcare"
stockdf$Sector[stockdf$Ticker == "AGT"| stockdf$Ticker == "MFI"|stockdf$Ticker == "DOL"] <- "Consumer Defensive"
stockdf$Sector[stockdf$Ticker == "AD"| stockdf$Ticker == "X"|stockdf$Ticker == "RY"] <- "Financial Services"
stockdf$Sector[stockdf$Ticker == "SPB"| stockdf$Ticker == "NPI"|stockdf$Ticker == "H"] <- "Utilities"

#stockdf$VolSize[stockdf$ret < 10] <- "Below - 10%"
#stockdf$VolSize[stockdf$ret >= -10 & stockdf$ret < -5] <- "-10% to -5%"
#stockdf$VolSize[stockdf$ret >= -5 & stockdf$ret < -2] <- "-5% to -2%"
#stockdf$VolSize[stockdf$ret >= -2 & stockdf$ret < 0] <- "-2% to 0%"
#stockdf$VolSize[stockdf$ret >= 0 & stockdf$ret < 2] <- "0% to 2%"
#stockdf$VolSize[stockdf$ret >= 2 & stockdf$ret < 5] <- "2% to 5%"
#stockdf$VolSize[stockdf$ret >= 5 & stockdf$ret < 10] <- "5% to 10%"
#stockdf$VolSize[stockdf$ret >= 10] <- "Above 10%"


table(stockdf$MarketCap)

ggplot(stockdf, aes(x = stockdf$ret)) + geom_histogram(breaks = seq(-10, 10, by = 1), binwidth = 0.5, aes(y = ..density..)) + facet_wrap(~MarketCap)
ggplot(stockdf, aes(x = stockdf$ret)) + geom_histogram(breaks = seq(-10, 10, by = 1), binwidth = 0.5, aes(y = ..density..)) + facet_wrap(~Sector)
ggplot(stockdf, aes(x = stockdf$ret)) + geom_histogram(breaks = seq(-10, 10, by = 1), binwidth = 0.5, aes(y = ..density..)) + facet_wrap(~Ticker)



TCS <- na.omit(stockdf$ret[stockdf$Ticker == "TCS" & stockdf$ret <= 20 & stockdf$ret >= -20])
KXS <- na.omit(stockdf$ret[stockdf$Ticker == "KXS" & stockdf$ret <= 20 & stockdf$ret >= -20])
CSU <- na.omit(stockdf$ret[stockdf$Ticker == "CSU" & stockdf$ret <= 20 & stockdf$ret >= -20])
TVK <- na.omit( stockdf$ret[stockdf$Ticker == "TVK" & stockdf$ret <= 20 & stockdf$ret >= -20])
PKI <- na.omit( stockdf$ret[stockdf$Ticker == "PKI" & stockdf$ret <= 20 & stockdf$ret >= -20])
SU <- na.omit( stockdf$ret[stockdf$Ticker == "SU" & stockdf$ret <= 20 & stockdf$ret >= -20])
ZYME <- na.omit( stockdf$ret[stockdf$Ticker == "ZYME" & stockdf$ret <= 20 & stockdf$ret >= -20])
CSH.UN <- na.omit( stockdf$ret[stockdf$Ticker == "CSH.UN" & stockdf$ret <= 20 & stockdf$ret >= -20])
WEED <- na.omit( stockdf$ret[stockdf$Ticker == "WEED" & stockdf$ret <= 20 & stockdf$ret >= -20])
AGT <- na.omit( stockdf$ret[stockdf$Ticker == "AGT" & stockdf$ret <= 20 & stockdf$ret >= -20])
MFI <- na.omit( stockdf$ret[stockdf$Ticker == "MFI" & stockdf$ret <= 20 & stockdf$ret >= -20])
DOL <- na.omit( stockdf$ret[stockdf$Ticker == "DOL" & stockdf$ret <= 20 & stockdf$ret >= -20])
AD <- na.omit( stockdf$ret[stockdf$Ticker == "AD" & stockdf$ret <= 20 & stockdf$ret >= -20])
X <- na.omit( stockdf$ret[stockdf$Ticker == "X" & stockdf$ret <= 20 & stockdf$ret >= -20])
RY <- na.omit( stockdf$ret[stockdf$Ticker == "RY" & stockdf$ret <= 20 & stockdf$ret >= -20])
SPB <- na.omit( stockdf$ret[stockdf$Ticker == "SPB" & stockdf$ret <= 20 & stockdf$ret >= -20])
NPI <- na.omit( stockdf$ret[stockdf$Ticker == "NPI" & stockdf$ret <= 20 & stockdf$ret >= -20])
H <- na.omit(stockdf$ret[stockdf$Ticker == "H" & stockdf$ret <= 20 & stockdf$ret >= -20])
L <- na.omit( stockdf$ret[stockdf$MarketCap == "Large" & stockdf$ret <= 20 & stockdf$ret >= -20])
M <- na.omit( stockdf$ret[stockdf$MarketCap == "Medium" & stockdf$ret <= 20 & stockdf$ret >= -20])
S <- na.omit( stockdf$ret[stockdf$MarketCap == "Small" & stockdf$ret <= 20 & stockdf$ret >= -20])
Tec <- na.omit( stockdf$ret[stockdf$Sector == "Technology" & stockdf$ret <= 20 & stockdf$ret >= -20])
Uti <- na.omit( stockdf$ret[stockdf$Sector == "Utilities" & stockdf$ret <= 20 & stockdf$ret >= -20])
Hea <- na.omit( stockdf$ret[stockdf$Sector == "Healthcare" & stockdf$ret <= 20 & stockdf$ret >= -20])
Ene <- na.omit( stockdf$ret[stockdf$Sector == "Energy" & stockdf$ret <= 20 & stockdf$ret >= -20])
Fin <- na.omit( stockdf$ret[stockdf$Sector == "Financial Services" & stockdf$ret <= 20 & stockdf$ret >= -20])
Con <- na.omit( stockdf$ret[stockdf$Sector == "Consumer Defensive" & stockdf$ret <= 20 & stockdf$ret >= -20])


sum_table <- data.frame(volatility = c(sd(TCS), sd(KXS), sd(CSU), sd(TVK), sd(PKI), sd(SU), sd(ZYME), sd(CSH.UN), sd(WEED), sd(AGT), 
                                       sd(MFI), sd(DOL), sd(AD), sd(X), sd(RY), sd(SPB), sd(NPI), sd(H), sd(L), sd(M), sd(S), sd(Tec), sd(Uti), sd(Hea), sd(Ene), sd(Fin), sd(Con)),
                        VaRFive = 100 * c(VaR(TCS/100, method = "historical"),VaR(KXS/100, method = "historical"),VaR(CSU/100, method = "historical"),VaR(TVK/100, method = "historical"),
                                VaR(PKI/100, method = "historical"),VaR(SU/100, method = "historical"),VaR(ZYME/100, method = "historical"),VaR(CSH.UN/100, method = "historical"),
                                VaR(WEED/100, method = "historical"),VaR(AGT/100, method = "historical"),VaR(MFI/100, method = "historical"),VaR(DOL/100, method = "historical"),
                                VaR(AD/100, method = "historical"),VaR(X/100, method = "historical"),VaR(RY/100, method = "historical"),VaR(SPB/100, method = "historical"),
                                VaR(NPI/100, method = "historical"),VaR(H/100, method = "historical"),VaR(L/100, method = "historical"),VaR(M/100, method = "historical"),
                                VaR(S/100, method = "historical"),VaR(Tec/100, method = "historical"),VaR(Uti/100, method = "historical"),VaR(Hea/100, method = "historical"),
                                VaR(Ene/100, method = "historical"),VaR(Fin/100, method = "historical"), VaR(Con/100, method = "historical")),
                        VarOne =  100 * c(VaR(TCS/100, p = 0.99, method = "historical"),VaR(KXS/100, p = 0.99, method = "historical"),VaR(CSU/100, p = 0.99, method = "historical"),VaR(TVK/100, p = 0.99, method = "historical"),
                                                  VaR(PKI/100, p = 0.99, method = "historical"),VaR(SU/100, p = 0.99, method = "historical"),VaR(ZYME/100, p = 0.99, method = "historical"),VaR(CSH.UN/100, p = 0.99, method = "historical"),
                                                  VaR(WEED/100, p = 0.99, method = "historical"),VaR(AGT/100, p = 0.99, method = "historical"),VaR(MFI/100, p = 0.99, method = "historical"),VaR(DOL/100, p = 0.99, method = "historical"),
                                                  VaR(AD/100, p = 0.99, method = "historical"),VaR(X/100, p = 0.99, method = "historical"),VaR(RY/100, p = 0.99, method = "historical"),VaR(SPB/100, p = 0.99, method = "historical"),
                                                  VaR(NPI/100, p = 0.99, method = "historical"),VaR(H/100, p = 0.99, method = "historical"),VaR(L/100, p = 0.99, method = "historical"),VaR(M/100, p = 0.99, method = "historical"),
                                                  VaR(S/100, p = 0.99, method = "historical"),VaR(Tec/100, p = 0.99, method = "historical"),VaR(Uti/100, p = 0.99, method = "historical"),VaR(Hea/100, p = 0.99, method = "historical"),
                                                  VaR(Ene/100, p = 0.99, method = "historical"),VaR(Fin/100, p = 0.99, method = "historical"), VaR(Con/100, p = 0.99, method = "historical")),
                        Ticker = c("TCS", "KXS", "CSU", "TVK", "PKI", "SU", "ZYME", "CSH.UN", "WEED", "AGT", "MFI", "DOL", "AD", "X", "RY", "SPB", "NPI", "H", "L", "M", "S", "Tec", "Uti", "Hea", "Ene", "Fin", "Con"))

sum_table

ggplot(sum_table, aes(x = ticker, y = volatility)) + geom_col()


retRY <- stockdf$ret[stockdf$Ticker == "WEED" & stockdf$ret <= 20 & stockdf$ret >= -20]
VaR(retRY/100, p = 0.95, method = "gaussian")

