# Time-Series-Analysis-of-Cryptocurrencies-using-R

## Problem Statement:
Blockchain and cryptocurrencies have recently created a new technology boom in the market. Bitcoin is the first decentralized cryptocurrency created in 2009. Time Series analysis of these cryptocurrencies is a new emerging area nowadays. It can help people better understand about investing in cryptocurrencies and the nature of volatility of these currencies in the coming years. Our analysis gives a deep understanding about forecasting the prices of cryptocurrencies using time series analysis. We compare several alternative univariate and multivariate models for forecasting the price of six of the most capitalized series: Bitcoin, Ethereum, Lite coin, Ripple, Dash and Monero. We compare univariate autoregressive models to univariate linear regression models based on a large set of crypto-predictors. The predictors include financial assets such as stock prices, volume and volatility. We also perform co-integration between different cryptocurrencies to see how does the price of one cryptocurrency affect others. This can help an individual to understand about trading pairs and invest accordingly.

## Data Source and Dataset Description:
### Cryptocurrency:
The data used in this study are the cryptocurrencies prices along with volume and market cap of each currency. The data is present from 7th August 2015 to 28th February 2018 and on a daily basis. The dataset is available on Kaggle. The dataset contains information regarding 6 major cryptocurrencies.
https://www.kaggle.com/sudalairajkumar/cryptocurrencypricehistory/data

### Bitcoin:
Most popular and prominent cryptocurrency based on the decentralization and cryptography. The decentralization means that the Bitcoin network is controlled and owned by all of its users, who must adhere to the same set of rules.

### Ethereum:
Ethereum also provides a cryptocurrency token called Ether which can be transferred between accounts and used to compensate participant nodes for computations performed.

### Lite Coin:
It is often considered as Bitcoin’s leading rival. It has one main feature which distinguishes it from Bitcoin: it is significantly faster regarding transactions, and it is particularly attractive in time–critical situations.

### Ripple:
Developed by the banking industry in 2012, is a Blockchain network which incorporates a payment system and a currency system known as XRP.

### Monero:
Monero uses a public ledger to record transactions while new units are created through a process called mining. Monero aims to improve on existing cryptocurrency design by obscuring sender, recipient and amount of every transaction made as well as making the mining process more egalitarian.

### Dash:
Dash aims to be the most user friendly and scalable payments system in the world.

## Desriptive Statistics and Co-Integration:
1) Average Price of all cryptocurrencies over the last 6 years
2) Correlation between all the six cryptocurrencies
3) Average Market Capital of cryptocurrencies
4) Time Series Plots
5) Volatility

## Forecasting:
1) Stationarity Rules
2) ACF/PACF Plots
3) Regression Model
4) ARIMA Model

## Scope for future work:
Cryptocurrency is a vast topic to explore and we have the following ideas that we can work on in future to further improve our analysis.

•	Exploring the market factors effecting volatilities of the currency price, could make an interesting arena. Any significant factors that effect the volatility could be further used to model and forecast the price and market trends

•	Another aspect could be to further understand how best we can utilize the co-integration series in combination with the forecast models to provide the best stock trading techniques

•	As cryptocurrency is a very volatile currency, even minor changes can influence its prices. It would be worth investigating and collecting data about hacking, banning of ICO by China or negative news because these factors can have a significant impact on the trading price of the cryptocurrency. Incorporating these factors into the ARIMA or VAR model will help us capture most of the variances in the trading price

•	Another, approach to forecast cryptocurrency prices is to explore how a few trendy ML approaches would perform. We can try to implement Random Forest or non-linear regression considering lagged values along with other features such as volume and volatility to predict the future values of cryptocurrency





