# Crptocurrency Time Series Analysis

install.packages('PairTrading')
install.packages('devtools')
install.packages('xts')


library(magrittr)
library(dplyr)
library(zoo)
library(xts)
library(PairTrading)
library(devtools)
library(ggplot2)
library(astsa)
library(tseries)
library(forecast)
library(fUnitRoots)
library(urca)
library(MTS)
library(dplyr)
library(corrplot)
library(caret)

# Download the Bitcoin dataset
bitcoin <- read.csv('bitcoin_price.csv')
ethereum <- read.csv('ethereum_price.csv')
litecoin <- read.csv('litecoin_price.csv')
ripple <- read.csv('ripple_price.csv')
monero <- read.csv('monero_price.csv')
dash <- read.csv('dash_price.csv')

View(bitcoin)
View(ethereum)
View(litecoin)
View(dash)
summary(bitcoin)
summary(ethereum)
str(bitcoin)

# Converting Date column from factor to date
bitcoin$Date <- as.Date(bitcoin$Date, format='%b %d, %Y')
ethereum$Date <- as.Date(ethereum$Date, format = '%b %d, %Y')
litecoin$Date <- as.Date(litecoin$Date, format = '%b %d, %Y')
ripple$Date <- as.Date(ripple$Date, format = '%b %d, %Y')
monero$Date <- as.Date(monero$Date, format = '%b %d, %Y')
dash$Date <- as.Date(dash$Date, format = '%d-%b-%y')


# Analyzing both the datasets for the same interval (August 7, 2017 onwards)
# Ethereum starts from the above date
# Creating subset for bitcoin for that interval

bitcoin_new <- bitcoin[bitcoin$Date > "2015-08-06",] 
View(bitcoin_new)
litecoin_new <- litecoin[litecoin$Date > "2015-08-06",]
ripple_new <- ripple[ripple$Date > "2015-08-06",]
monero_new <- monero[monero$Date > "2015-08-06",]
dash_new <- dash[dash$Date > "2015-08-06",]

# Now both the cryptocurrencies are in the same interval and have 929 entries

# Sorting both the datasets
library(sqldf)
bitcoin_new_ordered <- sqldf("Select * from bitcoin_new order by Date asc")
ethereum_ordered <- sqldf("Select * from ethereum order by Date asc")
litecoin_ordered <- sqldf("Select * from litecoin_new order by Date asc")
ripple_ordered <- sqldf("Select * from ripple_new order by Date asc")
monero_ordered <- sqldf("Select * from monero_new order by Date asc")
dash_ordered <- sqldf("Select * from dash_new order by Date asc")

# Merging both the datasets
cryptocurrency <- bitcoin_new_ordered %>% 
  left_join(ethereum_ordered,by=c('Date'='Date')) %>%
  left_join(litecoin_ordered,by=c('Date'='Date')) %>%
  left_join(ripple_ordered,by=c('Date'='Date')) %>%
  left_join(monero_ordered,by=c('Date'='Date')) %>%
  left_join(dash_ordered,by=c('Date'='Date'))
View(cryptocurrency)

# Chanign column names
cols <- c(sapply(colnames(bitcoin_new_ordered), function(x) paste0(x,"_bitcoin")),
          sapply(colnames(ethereum_ordered)[-1], function(x) paste0(x,"_ethreum")),
          sapply(colnames(litecoin_ordered)[-1], function(x) paste0(x,"_litecoin")),
          sapply(colnames(ripple_ordered)[-1], function(x) paste0(x,"_ripple")),
          sapply(colnames(monero_ordered)[-1], function(x) paste0(x,"_monero")),
          sapply(colnames(dash_ordered)[-1], function(x) paste0(x,"_dash")))

colnames(cryptocurrency) <- cols  

# Plotting the time series plot for both the series
library(ggplot2)

bitcoin_graph <- ggplot(data = bitcoin_new_ordered, x = Date) +
  geom_line(aes(x = Date, y = Close, col = 'red')) 

ggplot(data = cryptocurrency, aes(x = Date)) +
  geom_line(aes(y = Close_ethreum), color = 'red') +
  geom_line(aes(y = Close_litecoin), color = 'green') +
  geom_line(aes(y = Close_ripple), color = 'black') +
  geom_line(aes(y = Close_monero), color = 'cyan') +
  geom_line(aes(y = Close_dash), color = 'blue')  +ggtitle('Close Price')

cryptocurrency$

# Plotting the scatter plot to see if there is any correlation between bitcoin close price and ethereum price
plot(bitcoin_new_ordered$Close,ethereum_ordered$Close)
cor(bitcoin_new_ordered$Close,ethereum_ordered$Close)



# Johansen Test to check cointegration
install.packages('urca')
library(urca)

jotest <- ca.jo(cryptocurrency[,c(5,11,17,23,29,35)],type='trace',K=2,ecdet='none',spec='longrun')
summary(jotest)
# co-int series
co_series <- cryptocurrency$Close.bitcoin + 4.33*cryptocurrency$Close.ethreum + 
  23.70*cryptocurrency$Close.litecoin - 1995.25*cryptocurrency$Close.ripple - 
  19.94*cryptocurrency$Close.monero - 14.46*cryptocurrency$Close.dash

adf.test(co_series, k =1)

ccm(cryptocurrency[,c(5,11,17,23,29,35)],lags = 15)


# Creating individual models for all the cryptocurrencies close price
# Bitcoin
install.packages('forecast')
library(forecast)
install.packages('zoo')
library(zoo)

ggplot(cryptocurrency) + geom_line(aes(x=Date.bitcoin, y=Close.bitcoin),col='red')

acf2(cryptocurrency$Close.bitcoin)
# Here, we see a spike at lag 1 PACF. Hence, taking difference
acf2(diff(cryptocurrency$Close.bitcoin))

sarima(cryptocurrency$Close.bitcoin,1,1,0,1,1,0,10)
sarima.for(ts(cryptocurrency$Close.bitcoin),5,2,11,n.ahead=12)
forecast(cryptocurrency$Close.bitcoin, method="arima", h=24, level=95)


# Ethereum


ggplot(cryptocurrency) + geom_line(aes(x=Date.bitcoin, y=Close.ethreum),col='red')

acf2(cryptocurrency$Close.ethreum)
acf2(diff(cryptocurrency$Close.ethreum))

sarima(ts(cryptocurrency$Close.ethreum),1,1,0)


lag1_bitcoin <- diff(cryptocurrency$Close.bitcoin)

model <- lm(cryptocurrency$Close.bitcoin ~ cryptocurrency$Close.litecoin+cryptocurrency$Close.ethreum+
              cryptocurrency$Close.ripple+cryptocurrency$Close.monero+cryptocurrency$Close.dash)
summary(model)

model2 <- lm(cryptocurrency$Close.bitcoin~cryptocurrency$Close.ethreum)
summary(model2)

library(corrplot)
M <- cor(cryptocurrency[,c(5,11,17,23,29,35)])
corrplot(M,method='pie')

acf2(model$residuals)
write.csv(M, 'correlation.csv')


myPCA <- prcomp(cryptocurrency[,c(5,11,17,23,29,35)], scale. = T, center = T)
myPCA$rotation
myPCA$
screeplot(myPCA)

a <-princomp(cryptocurrency[,c(5,11,17,23,29,35)])
summary(a)


a$loadings



#--------------------------------------------------------
# Descriptive Statistics

# Average price of bitcoin over the years.

ggplot(data = cryptocurrency, x = Date) 
  geom_line(aes(x = Date, y = Close_bitcoin, col = 'red')) + ggtitle('Bitcoin Time Series')

summary(bitcoin_new_ordered)

bitcoin_avg_closeprice_2015 <- cryptocurrency %>% filter(Date_bitcoin < '2016-01-01') %>% summarise(mean(Close_bitcoin))
bitcoin_avg_closeprice_2016 <- cryptocurrency %>% filter(Date_bitcoin > '2015-12-31' & Date_bitcoin < '2017-01-01') %>% summarise(mean(Close_bitcoin))
bitcoin_avg_closeprice_2017 <- cryptocurrency %>% filter(Date_bitcoin > '2016-12-31' & Date_bitcoin < '2018-01-01') %>% summarise(mean(Close_bitcoin))
bitcoin_avg_closeprice_2018 <- cryptocurrency %>% filter(Date_bitcoin > '2017-12-31' & Date_bitcoin < '2019-01-01') %>% summarise(mean(Close_bitcoin))

Date <- c('2015','2016','2017','2018')
Avg_price <- rbind(bitcoin_avg_closeprice_2015,bitcoin_avg_closeprice_2016,bitcoin_avg_closeprice_2017,bitcoin_avg_closeprice_2018)
bitcoin_avg_closeprice_hist <- data.frame(Date,Avg_price)

plot(bitcoin_avg_closeprice_hist$Date,bitcoin_avg_closeprice_hist$mean.Close_bitcoin.)


# Average price of ethereum over the years.

ggplot(data = cryptocurrency, x = Date) +
  geom_line(aes(x = Date, y = Close_ethreum, col = 'blue')) + ggtitle('Ethereum Time Series')

summary(bitcoin_new_ordered)

ethereum_avg_closeprice_2015 <- cryptocurrency %>% filter(Date < '2016-01-01') %>% summarise(mean(Close_ethreum))
ethereum_avg_closeprice_2016 <- cryptocurrency %>% filter(Date > '2015-12-31' & Date < '2017-01-01') %>% summarise(mean(Close_ethreum))
ethereum_avg_closeprice_2017 <- cryptocurrency %>% filter(Date > '2016-12-31' & Date < '2018-01-01') %>% summarise(mean(Close_ethreum))
ethereum_avg_closeprice_2018 <- cryptocurrency %>% filter(Date > '2017-12-31' & Date < '2019-01-01') %>% summarise(mean(Close_ethreum))

Date <- c('2015','2016','2017','2018')
Avg_price <- rbind(ethereum_avg_closeprice_2015,ethereum_avg_closeprice_2016,ethereum_avg_closeprice_2017,ethereum_avg_closeprice_2018)
ethereum_avg_closeprice_hist <- data.frame(Date,Avg_price)

plot(ethereum_avg_closeprice_hist$Date,ethereum_avg_closeprice_hist$mean.Close_ethreum.)


# Average price of Lite Coin over the years.

ggplot(data = cryptocurrency, x = Date) +
  geom_line(aes(x = Date, y = Close_litecoin, col = 'blue')) + ggtitle('Lite coin Time Series')


litecoin_avg_closeprice_2015 <- cryptocurrency %>% filter(Date < '2016-01-01') %>% summarise(mean(Close_litecoin))
litecoin_avg_closeprice_2016 <- cryptocurrency %>% filter(Date > '2015-12-31' & Date < '2017-01-01') %>% summarise(mean(Close_litecoin))
litecoin_avg_closeprice_2017 <- cryptocurrency %>% filter(Date > '2016-12-31' & Date < '2018-01-01') %>% summarise(mean(Close_litecoin))
litecoin_avg_closeprice_2018 <- cryptocurrency %>% filter(Date > '2017-12-31' & Date < '2019-01-01') %>% summarise(mean(Close_litecoin))

Date <- c('2015','2016','2017','2018')
Avg_price <- rbind(litecoin_avg_closeprice_2015,litecoin_avg_closeprice_2016,litecoin_avg_closeprice_2017,litecoin_avg_closeprice_2018)
litecoin_avg_closeprice_hist <- data.frame(Date,Avg_price)

plot(litecoin_avg_closeprice_hist$Date,litecoin_avg_closeprice_hist$mean.Close_litecoin.)

# Average price of Ripple over the years.

ggplot(data = cryptocurrency, x = Date) +
  geom_line(aes(x = Date, y = Close_ripple, col = 'blue')) + ggtitle('Ripple Time Series')


ripple_avg_closeprice_2015 <- cryptocurrency %>% filter(Date < '2016-01-01') %>% summarise(mean(Close_ripple))
ripple_avg_closeprice_2016 <- cryptocurrency %>% filter(Date > '2015-12-31' & Date < '2017-01-01') %>% summarise(mean(Close_ripple))
ripple_avg_closeprice_2017 <- cryptocurrency %>% filter(Date > '2016-12-31' & Date < '2018-01-01') %>% summarise(mean(Close_ripple))
ripple_avg_closeprice_2018 <- cryptocurrency %>% filter(Date > '2017-12-31' & Date < '2019-01-01') %>% summarise(mean(Close_ripple))

Date <- c('2015','2016','2017','2018')
Avg_price <- rbind(ripple_avg_closeprice_2015,ripple_avg_closeprice_2016,ripple_avg_closeprice_2017,ripple_avg_closeprice_2018)
ripple_avg_closeprice_hist <- data.frame(Date,Avg_price)

plot(ripple_avg_closeprice_hist$Date,ripple_avg_closeprice_hist$mean.Close_ripple.)


# Average price of Monero over the years.

ggplot(data = cryptocurrency, x = Date) +
  geom_line(aes(x = Date, y = Close_monero, col = 'blue')) + ggtitle('Monero Time Series')


monero_avg_closeprice_2015 <- cryptocurrency %>% filter(Date < '2016-01-01') %>% summarise(mean(Close_monero))
monero_avg_closeprice_2016 <- cryptocurrency %>% filter(Date > '2015-12-31' & Date < '2017-01-01') %>% summarise(mean(Close_monero))
monero_avg_closeprice_2017 <- cryptocurrency %>% filter(Date > '2016-12-31' & Date < '2018-01-01') %>% summarise(mean(Close_monero))
monero_avg_closeprice_2018 <- cryptocurrency %>% filter(Date > '2017-12-31' & Date < '2019-01-01') %>% summarise(mean(Close_monero))

Date <- c('2015','2016','2017','2018')
Avg_price <- rbind(monero_avg_closeprice_2015,monero_avg_closeprice_2016,monero_avg_closeprice_2017,monero_avg_closeprice_2018)
monero_avg_closeprice_hist <- data.frame(Date,Avg_price)

plot(monero_avg_closeprice_hist$Date,monero_avg_closeprice_hist$mean.Close_monero)



# Average price of Dash over the years.

ggplot(data = cryptocurrency, x = Date) +
  geom_line(aes(x = Date, y = Close_dash, col = 'blue')) + ggtitle('Dash Time Series')


dash_avg_closeprice_2015 <- cryptocurrency %>% filter(Date < '2016-01-01') %>% summarise(mean(Close_dash))
dash_avg_closeprice_2016 <- cryptocurrency %>% filter(Date > '2015-12-31' & Date < '2017-01-01') %>% summarise(mean(Close_dash))
dash_avg_closeprice_2017 <- cryptocurrency %>% filter(Date > '2016-12-31' & Date < '2018-01-01') %>% summarise(mean(Close_dash))
dash_avg_closeprice_2018 <- cryptocurrency %>% filter(Date > '2017-12-31' & Date < '2019-01-01') %>% summarise(mean(Close_dash))

Date <- c('2015','2016','2017','2018')
Avg_price <- rbind(dash_avg_closeprice_2015,dash_avg_closeprice_2016,dash_avg_closeprice_2017,dash_avg_closeprice_2018)
dash_avg_closeprice_hist <- data.frame(Date,Avg_price)

plot(dash_avg_closeprice_hist$Date,dash_avg_closeprice_hist$mean.Close_dash)

final_avg_cry_list <- rbind(bitcoin_avg_closeprice_hist,ethereum_avg_closeprice_hist,litecoin_avg_closeprice_hist,ripple_avg_closeprice_hist,monero_avg_closeprice_hist,dash_avg_closeprice_hist)

colnames(bitcoin_avg_closeprice_hist) <- c('Date','Avg_Close_Price')
colnames(ethereum_avg_closeprice_hist) <- c('Date','Avg_Close_Price')
colnames(litecoin_avg_closeprice_hist) <- c('Date','Avg_Close_Price')
colnames(ripple_avg_closeprice_hist) <- c('Date','Avg_Close_Price')
colnames(monero_avg_closeprice_hist) <- c('Date','Avg_Close_Price')
colnames(dash_avg_closeprice_hist) <- c('Date','Avg_Close_Price')

write.csv(bitcoin_avg_closeprice_hist,'Bitcoin_Avg_Price.csv')
write.csv(ethereum_avg_closeprice_hist,'Ethereum_Avg_Price.csv')
write.csv(litecoin_avg_closeprice_hist,'LiteCoin_Avg_Price.csv')
write.csv(ripple_avg_closeprice_hist,'Ripple_Avg_Price.csv')
write.csv(monero_avg_closeprice_hist,'Monero_Avg_Price.csv')
write.csv(dash_avg_closeprice_hist,'Dash_Avg_Price.csv')



# -----------------------------------------------------------------------------------------
# Regression and Forecasting

#Forecasting for Bitcoin

train <- bitcoin_2015[-c(910:929),]
test <- bitcoin_2015[c(910:929),]

row.names(holtdf)<- 1 : nrow(holtdf)

holtt <-  holt(train$Close, type = "additive", damped = F, h=20) #holt forecast values
holtf <- forecast(holtt, h = 20)
holtdf <- as.data.frame(holtf)
plot(holtf, ylim = c(0,20000)) 

View(test)

holtfdf <- cbind(test, holtdf[,1])
accuracy(holtdf[,1], test[,5])
ggplot() + geom_line(data = holtfdf, aes(Date, holtfdf[,5]), color = "blue") + geom_line(data = holtfdf, aes(Date, holtfdf[,8]), color = "Dark Red")

######################################
cor <- cor(cryptocurrency[,c(5,11,17,23,29,35)])
View(cor)
corrplot(cor, method = "pie")
a <- cor(cryptocurrency[,c(5,11,17,23,29,35)], method = "kendall")
corrplot(a, method = "pie")

#Regression
#connection between volatility and volume
#find volatility and 

#Finding Volatility
library(TTR)
volatility_bitcoin <- volatility(cryptocurrency$Close.bitcoin, n = 10, calc = "close", N = 260, mean0 = FALSE)
cor <- cor(volatility_bitcoin,cryptocurrency$Volume.bitcoin)

volatility_bitcoin[is.na(volatility_bitcoin)] <- 0
corrplot(cor, method = "pie")

View(volatility_bitcoin)

str(cryptocurrency)
cryptocurrency$Volume.bitcoin <- as.numeric(cryptocurrency$Volume.bitcoin)


plot(volatility_bitcoin,cryptocurrency$Volume.bitcoin)

library(tseries)
data <- get.hist.quote('VOD.L')
price <- cryptocurrency$Close.bitcoin
ret <- log(lag(price),1) - log(price)
vol <- sd(ret) * sqrt(250) * 100
vol
dev.off()

#############################################################################
#Regression

acf2(diff(cryptocurrency$Close.bitcoin))

bit_m <- lm(cryptocurrency$Close.bitcoin ~ lag(cryptocurrency$Close.bitcoin,1) + lag(cryptocurrency$Close.bitcoin,5) + lag(cryptocurrency$Close.bitcoin,10) + lag(cryptocurrency$Close.bitcoin,19) + lag(cryptocurrency$Close.bitcoin,20) + cryptocurrency$Volume.bitcoin, data = cryptocurrency)
summary(bit_m)

m <- lm(cryptocurrency$Close.bitcoin ~ cryptocurrency$Volume.bitcoin, data = cryptocurrency)
summary(m)

Date <- seq(as.Date("2018-02-21"), by = "day", length.out = 10)
price <- c(0,0,0,0,0,0,0,0,0,0)
test_new <- data.frame(Date,price)

train <- cryptocurrency[-c(910:929),]
test <- cryptocurrency[c(910:929),]


#19 and 20 seem correlated

acf2(diff(cryptocurrency$Close.dash))

dash_m <- lm(cryptocurrency$Close.dash ~ lag(cryptocurrency$Close.dash,1) + lag(cryptocurrency$Close.dash,3) + lag(cryptocurrency$Close.dash,6) + lag(cryptocurrency$Close.dash,10) + lag(cryptocurrency$Close.dash,16) + lag(cryptocurrency$Close.dash,20), data = cryptocurrency)
summary(dash_m)

dash_m1 <- lm(cryptocurrency$Close.dash ~ lag(cryptocurrency$Close.dash,1) + lag(cryptocurrency$Close.dash,3) + lag(cryptocurrency$Close.dash,10), data = cryptocurrency)
summary(dash_m1)

acf2(diff(cryptocurrency$Close.ethereum))

eth_m <- lm(cryptocurrency$Close.ethereum ~ lag(cryptocurrency$Close.ethereum,6) + lag(cryptocurrency$Close.ethereum,12) + lag(cryptocurrency$Close.ethereum,19) + lag(cryptocurrency$Close.ethereum,20), data = cryptocurrency)
summary(eth_m)

acf2(diff(cryptocurrency$Close.litecoin))

lite_m <- lm(cryptocurrency$Close.litecoin ~ lag(cryptocurrency$Close.litecoin,6) + lag(cryptocurrency$Close.litecoin,10) + lag(cryptocurrency$Close.litecoin,13) + lag(cryptocurrency$Close.litecoin,16), data = cryptocurrency)
summary(lite_m)

acf2(diff(cryptocurrency$Close.ripple))

rip_m <- lm(cryptocurrency$Close.ripple ~ lag(cryptocurrency$Close.ripple,1) + lag(cryptocurrency$Close.ripple,10) + lag(cryptocurrency$Close.ripple,15) + lag(cryptocurrency$Close.ripple,18), data = cryptocurrency)
summary(rip_m)

acf2(diff(cryptocurrency$Close.monero))

mon_m <- lm(cryptocurrency$Close.monero ~ lag(cryptocurrency$Close.monero,1) + lag(cryptocurrency$Close.monero,6) + lag(cryptocurrency$Close.monero,10) + lag(cryptocurrency$Close.monero,17) + lag(cryptocurrency$Close.monero,20), data = cryptocurrency)
summary(mon_m)

#################################################################################

close <- cryptocurrency[,c(1,5,11,17,23,29,35)]
corr <- cor(close, use = "pairwise.complete")
corrplot(corr, order="hclust", diag = FALSE, tl.col = "black", tl.cex = 0.7,
         title = "Correlation matrix (ordered by hierarchical clustering)",
         mar = c(0,1,2,0))

#MarketCap Analysis
str(cryptocurrency)
backup <- cryptocurrency
cryptocurrency$Market.Cap.bitcoin  <- gsub(",","",cryptocurrency$Market.Cap.bitcoin)
cryptocurrency$Market.Cap.ethereum  <- gsub(",","",cryptocurrency$Market.Cap.ethereum)
cryptocurrency$Market.Cap.litecoin  <- gsub(",","",cryptocurrency$Market.Cap.litecoin)
cryptocurrency$Market.Cap.ripple  <- gsub(",","",cryptocurrency$Market.Cap.ripple)
cryptocurrency$Market.Cap.dash  <- gsub(",","",cryptocurrency$Market.Cap.dash)
cryptocurrency$Market.Cap.monero  <- gsub(",","",cryptocurrency$Market.Cap.monero)

Currency <- c("bitcoin","ethereum","litecoin","ripple","monero","dash")
cryptocurrency$Market.Cap.bitcoin <- as.numeric(cryptocurrency$Market.Cap.bitcoin)
cryptocurrency$Market.Cap.ethereum <- as.numeric(cryptocurrency$Market.Cap.ethereum)
cryptocurrency$Market.Cap.litecoin <- as.numeric(cryptocurrency$Market.Cap.litecoin)
cryptocurrency$Market.Cap.ripple <- as.numeric(cryptocurrency$Market.Cap.ripple)
cryptocurrency$Market.Cap.dash <- as.numeric(cryptocurrency$Market.Cap.dash)
cryptocurrency$Market.Cap.monero <- as.numeric(cryptocurrency$Market.Cap.monero)

eth <- na.omit(cryptocurrency$Market.Cap.ethereum)

meanMarketCap <- c(mean(cryptocurrency$Market.Cap.bitcoin),mean(eth),
                   mean(cryptocurrency$Market.Cap.litecoin),mean(cryptocurrency$Market.Cap.ripple),
                   mean(cryptocurrency$Market.Cap.monero),mean(cryptocurrency$Market.Cap.dash))
meanCap <- data.frame(Currency,meanMarketCap)
meanCap <- meanCap[order(meanCap$meanMarketCap),]
write.csv(meanCap,"MeanCap.csv")

barplot(meanCap[,2], names.arg = meanCap[,1],las=2 , cex.names=0.9, col = "gold",
        main="Average Market Capital of the Cryptocurrencies")


library(xts)
rownames(close) <- close$Date
close.xts <- as.xts(close)
View(close.xts)

price10 <- as.xts(close.xts$Close.bitcoin)
plot(price10, main="Price")

cryptocurrency$Date.bitcoin


#n <- length(close$Close.bitcoin);
#ret_bit <- log(close$Close.bitcoin[-1]/close$Close.bitcoin[-n])
#ret_eth <- log(close$Close.ethereum[-1]/close$Close.ethereum[-n])
#ret_lite <- log(close$Close.litecoin[-1]/close$Close.litecoin[-n])
#ret_rip <- log(close$Close.ripple[-1]/close$Close.ripple[-n])
#ret_mon <- log(close$Close.monero[-1]/close$Close.monero[-n])
#ret_dash <- log(close$Close.dash[-1]/close$Close.dash[-n])

#vol_bit <- volatility(ret_bit,n=10,calc = "close",N= 260)
#vol_eth <- volatility(ret_eth,n=10,calc = "close",N= 260)
#vol_lite <- volatility(ret_lite,n=10,calc = "close",N= 260)
#vol_rip <- volatility(ret_rip,n=10,calc = "close",N= 260)
#vol_mon <- volatility(ret_mon,n=10,calc = "close",N= 260)
#vol_dash <- volatility(ret_dash,n=10,calc = "close",N= 260)

ret_bit <- log(close$Close.bitcoin)/lag(log(close$Close.bitcoin))
ret_eth <- log(close$Close.ethereum)/lag(log(close$Close.ethereum))
ret_lite <- log(close$Close.litecoin)/lag(log(close$Close.litecoin))
ret_rip <- log(close$Close.ripple)/lag(log(close$Close.ripple))
ret_mon <- log(close$Close.monero)/lag(log(close$Close.monero))
ret_dash <- log(close$Close.dash)/lag(log(close$Close.dash))

vol_bit <- ret_bit*ret_bit
vol_eth <- ret_eth*ret_eth
vol_lite <- ret_lite*ret_lite
vol_rip <- ret_rip*ret_rip
vol_mon <- ret_mon*ret_mon
vol_dash <- ret_dash*ret_dash

date_cry <- cryptocurrency$Date.bitcoin[1:929]
CurrencyVolatility <- data.frame(date_cry,vol_bit,vol_eth,vol_lite,vol_rip,vol_mon,vol_dash)
View(CurrencyVolatility)


bitVol_plot <- ggplot(data = CurrencyVolatility, x = date_cry) + geom_line(aes(x = date_cry, y = vol_bit, col = "red"))

ggplot(data = CurrencyVolatility, x = date_cry) + geom_line(aes(x = date_cry, y = vol_bit, col = "red")) + ggtitle("Bitcoin Volatility")
ggplot(data = CurrencyVolatility, x = date_cry) + geom_line(aes(x = date_cry, y = vol_dash, col = "red")) + ggtitle("Dash Volatility")
ggplot(data = CurrencyVolatility, x = date_cry) + geom_line(aes(x = date_cry, y = vol_eth, col = "red")) + ggtitle("Ethereum Volatility")
ggplot(data = CurrencyVolatility, x = date_cry) + geom_line(aes(x = date_cry, y = vol_lite, col = "red")) + ggtitle("Litecoin Volatility")
ggplot(data = CurrencyVolatility, x = date_cry) + geom_line(aes(x = date_cry, y = vol_mon, col = "red")) + ggtitle("Monero Volatility")
ggplot(data = CurrencyVolatility, x = date_cry) + geom_line(aes(x = date_cry, y = vol_rip, col = "red")) + ggtitle("Ripple Volatility")


bitcoin_plot + geom_line(aes(x = date_cry, y = vol_eth, col = "Blue"), data = CurrencyVolatility) +
  geom_line(aes(x = date_cry, y = vol_lite, col = "Dark Green"), data = CurrencyVolatility) +
  geom_line(aes(x = date_cry, y = vol_rip, col = "Black"), data = CurrencyVolatility) +
  geom_line(aes(x = date_cry, y = vol_mon, col = "Cyan"), data = CurrencyVolatility) +
  geom_line(aes(x = date_cry, y = vol_dash, col = "Green"), data = CurrencyVolatility)


##########################################################################################################
cryptocurrency$Volume.bitcoin  <- gsub(",","",cryptocurrency$Volume.bitcoin)
cryptocurrency$Volume.ethereum  <- gsub(",","",cryptocurrency$Volume.ethereum)
cryptocurrency$Volume.litecoin  <- gsub(",","",cryptocurrency$Volume.litecoin)
cryptocurrency$Volume.ripple  <- gsub(",","",cryptocurrency$Volume.ripple)
cryptocurrency$Volume.dash  <- gsub(",","",cryptocurrency$Volume.dash)
cryptocurrency$Volume.monero  <- gsub(",","",cryptocurrency$Volume.monero)

cryptocurrency$Volume.bitcoin <- as.numeric(cryptocurrency$Volume.bitcoin)
cryptocurrency$Volume.ethereum <- as.numeric(cryptocurrency$Volume.ethereum)
cryptocurrency$Volume.litecoin <- as.numeric(cryptocurrency$Volume.litecoin)
cryptocurrency$Volume.ripple <- as.numeric(cryptocurrency$Volume.ripple)
cryptocurrency$Volume.dash <- as.numeric(cryptocurrency$Volume.dash)
cryptocurrency$Volume.monero <- as.numeric(cryptocurrency$Volume.monero)

CurrencyVolume <- cryptocurrency[,c(6,12,18,24,30,36)]

str(CurrencyVolatility)

cor <- cor(CurrencyVolatility[,2:7],CurrencyVolume)
View(cor)
corrplot(cor, method = "pie")

bitcoin_df <- data.frame(CurrencyVolatility$date_cry, cryptocurrency[2:929,5],CurrencyVolatility$vol_bit,CurrencyVolume$Volume.bitcoin)
View(bitcoin_df)
colnames(bitcoin_df) <- c("Date","Close","Volatility","Volume")

#Training and Testing data
train <- bitcoin_df[700:920,]
test <- bitcoin_df[921:929,]

#Regression Model
m <- lm(Close ~ lag(Volatility) + log(Volume), train)
summary(m)

predict <- predict(m,test)
compTable <- data.frame(test$Date,test$Close,predict)
View(compTable)

compTable <- na.omit(compTable)

compTable$error <- abs(compTable$test.Close - compTable$predict)/compTable$test.Close
mean(compTable$error)
View(test)

a <- data.frame(compTable$test.Date,compTable$predict) 
b <- data.frame(train$Date,train$Close)
colnames(a) <- c("Date","Close")
colnames(b) <- c("Date","Close")

comp_bit <- compTable

forecast_series <- rbind(b,a)
View(comp_bit)

bit_fore <- ggplot(data = forecast_series, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))
bit <- ggplot(data = train, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))

install.packages("Metrics")
library(Metrics)

forecast::accuracy(comp_bit$predict,comp_bit$test.Close)

#MAPE = 7.98

write.csv(forecast_series,"forecastValues.csv")

##################
#Ethereum
View(cryptocurrency)
eth_df <- data.frame(CurrencyVolatility$date_cry, cryptocurrency[1:929,11],CurrencyVolatility$vol_eth,CurrencyVolume$Volume.ethereum)
View(eth_df)
colnames(eth_df) <- c("Date","Close","Volatility","Volume")

#Training and Testing data
train <- eth_df[700:920,]
test <- eth_df[921:929,]

#Regression Model
m <- lm(Close ~ lag(Volatility) + log(Volume), train)
summary(m)

predict <- predict(m,test)
compTable <- data.frame(test$Date,test$Close,predict)
View(compTable)

compTable <- na.omit(compTable)

compTable$error <- abs(compTable$test.Close - compTable$predict)/compTable$test.Close
mean(compTable$error)
View(train)

comp_eth <- compTable

a <- data.frame(compTable$test.Date,compTable$predict) 
b <- data.frame(train$Date,train$Close)
colnames(a) <- c("Date","Close")
colnames(b) <- c("Date","Close")

forecast_series <- rbind(b,a)
View(forecast_series)

ggplot(data = forecast_series, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))
ggplot(data = train, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))

forecast::accuracy(comp_eth$predict,comp_eth$test.Close)

#MAPE = 26.17

##################
#Litecoin
View(cryptocurrency)
lite_df <- data.frame(CurrencyVolatility$date_cry, cryptocurrency[1:929,17],CurrencyVolatility$vol_lite,CurrencyVolume$Volume.litecoin)
View(lite_df)
colnames(lite_df) <- c("Date","Close","Volatility","Volume")

#Training and Testing data
train <- lite_df[700:920,]
test <- lite_df[921:929,]

#Regression Model
m <- lm(Close ~ lag(Volatility) + log(Volume), train)
summary(m)

predict <- predict(m,test)
compTable <- data.frame(test$Date,test$Close,predict)
View(compTable)

compTable <- na.omit(compTable)

compTable$error <- abs(compTable$test.Close - compTable$predict)/compTable$test.Close
mean(compTable$error)
View(train)

comp_lite <- compTable

a <- data.frame(compTable$test.Date,compTable$predict) 
b <- data.frame(train$Date,train$Close)
colnames(a) <- c("Date","Close")
colnames(b) <- c("Date","Close")

forecast_series <- rbind(b,a)
View(forecast_series)

ggplot(data = forecast_series, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))
ggplot(data = train, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))

forecast::accuracy(comp_lite$predict,comp_lite$test.Close)
#MAPE = 17.18

##################################
##################
#Dash
View(cryptocurrency)
dash_df <- data.frame(CurrencyVolatility$date_cry, cryptocurrency[1:929,35],CurrencyVolatility$vol_dash,CurrencyVolume$Volume.dash)
View(dash_df)
colnames(dash_df) <- c("Date","Close","Volatility","Volume")

#Training and Testing data
train <- dash_df[700:920,]
test <- dash_df[921:929,]

#Regression Model
m <- lm(Close ~ lag(Volatility) + log(Volume), train)
summary(m)

predict <- predict(m,test)
compTable <- data.frame(test$Date,test$Close,predict)
View(compTable)

compTable <- na.omit(compTable)

compTable$error <- abs(compTable$test.Close - compTable$predict)/compTable$test.Close
mean(compTable$error)
View(train)

comp_dash <- compTable

a <- data.frame(compTable$test.Date,compTable$predict) 
b <- data.frame(train$Date,train$Close)
colnames(a) <- c("Date","Close")
colnames(b) <- c("Date","Close")

forecast_series <- rbind(b,a)
View(forecast_series)

ggplot(data = forecast_series, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))
ggplot(data = train, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))

forecast::accuracy(comp_dash$predict,comp_dash$test.Close)
#MAPE = 15.20

###################################################################3
#Ripple
View(cryptocurrency)
rip_df <- data.frame(CurrencyVolatility$date_cry, cryptocurrency[1:929,23],CurrencyVolatility$vol_rip,CurrencyVolume$Volume.ripple)
View(rip_df)
colnames(rip_df) <- c("Date","Close","Volatility","Volume")

#Training and Testing data
train <- rip_df[700:920,]
test <- rip_df[921:929,]

#Regression Model
m <- lm(Close ~ log(Volume) + lag(Volatility), train)
summary(m)

predict <- predict(m,test)
compTable <- data.frame(test$Date,test$Close,predict)
View(compTable)

compTable <- compTable[-c(1,2),]

compTable$error <- abs(compTable$test.Close - compTable$predict)/compTable$test.Close
mean(compTable$error)
View(train)

comp_rip <- compTable

a <- data.frame(compTable$test.Date,compTable$predict) 
b <- data.frame(train$Date,train$Close)
colnames(a) <- c("Date","Close")
colnames(b) <- c("Date","Close")

forecast_series <- rbind(b,a)

ggplot(data = forecast_series, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))
ggplot(data = train, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))

forecast::accuracy(compTable$predict,compTable$test.Close)
#MAPE = 15.84

#########################################
#Monero
View(cryptocurrency)
mon_df <- data.frame(CurrencyVolatility$date_cry, cryptocurrency[1:929,29],CurrencyVolatility$vol_mon,CurrencyVolume$Volume.monero)
View(mon_df)
colnames(mon_df) <- c("Date","Close","Volatility","Volume")

#Training and Testing data
train <- mon_df[700:920,]
test <- mon_df[921:929,]

#Regression Model
m <- lm(Close ~ lag(Volatility) + log(Volume), train)
summary(m)

predict <- predict(m,test)
compTable <- data.frame(test$Date,test$Close,predict)
View(compTable)

compTable <- na.omit(compTable)

compTable$error <- abs(compTable$test.Close - compTable$predict)/compTable$test.Close
mean(compTable$error)
View(train)

comp_mon <- compTable

a <- data.frame(compTable$test.Date,compTable$predict) 
b <- data.frame(train$Date,train$Close)
colnames(a) <- c("Date","Close")
colnames(b) <- c("Date","Close")

forecast_series <- rbind(b,a)
View(forecast_series)

ggplot(data = forecast_series, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))
ggplot(data = train, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))

forecast::accuracy(comp_mon$predict,comp_mon$test.Close)
#MAPE = 34.21


########################################################3
## ARIMAX

acf2(diff(log(train$Close)))

vars_matrix <- cbind(lag(train$Volatility),log(train$Volume))
View(vars_matrix)

library(lmtest)
arimax_model <- auto.arima(train$Close, xreg = vars_matrix)
coeftest(arimax_model)

arimax_model_2 <- auto.arima(train$Close, xreg = vars_matrix[,1])
coeftest(arimax_model_2)

forecast(arimax_model_2,xreg =vars_matrix_test[,1], h=8) -> arimax_forecast

vars_matrix_test <- cbind(lag(test$Volatility),log(test$Volume))

test <- na.omit(test)
test$Close