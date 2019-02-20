#### Loading Packages ####
library(readr)
library(tidyverse)
library(lubridate)

###Importing Data
GOOG <- read_csv("C:/Users/ia767/Downloads/GOOG.csv") %>% select(Date, Close)
AAPL <- read_csv("C:/Users/ia767/Downloads/AAPL.csv") %>% select(Date, Close)
SP500 <- read_csv("C:/Users/ia767/Downloads/^GSPC (1).csv") %>% select(Date, Close)
names(GOOG) <- c("Date", "Google")
names(AAPL) <- c("Date", "Apple")
names(SP500) <- c("Date", "SP500")

stocks <- left_join(GOOG, AAPL, by = "Date")

###Computing Returns
apple_return <- c(diff(log(stocks$Apple))) #daily return %
Google_return <- c(diff(log(stocks$Google))) #daily return %
sp500_return <- c(diff(log(SP500$SP500))) #daily return %

###Calculating Annual Returns
time_past <- as.numeric(difftime(stocks$Date[nrow(stocks)], stocks$Date[1], units = c("days")))
#retrieves total numbers of days between first and last date

apple_cum_return <- cumprod(1 + apple_return)[length(apple_return)] #final cumulative return
apple_return_annual <- ( (1+apple_cum_return)^(365/time_past) ) - 1 #20.54%

Google_cum_return <- cumprod(1 + Google_return)[length(Google_return)] #final cumulative return
Google_return_annual <- ( (1+Google_cum_return)^(365/time_past) ) - 1 #17.23%  

###Calculating Annual Volatilities
apple_vol <- sqrt(256)*sd(apple_return, na.rm = TRUE)
Google_vol <- sqrt(256)*sd(Google_return, na.rm = TRUE)

###Calculating Correlation
apple_Google_cor <- cor(apple_return, Google_return)

###Plots
plot(stocks$Date, stocks$Apple, col = 'blue', type = "l", 
     xlab = "", ylab = "% Returns", main = "Apple Price")
plot(stocks$Date, stocks$Google, type = "l",
     xlab = "", ylab = "% Returns", main = "Google Price")

###Make a portfolio of the two assets, and choose weights that maximize the
#portfolio Sharpe ratio. What is the optimal allocation ? 
w <- seq(0, 1, 0.01) #apple weights vector
portfolio_return <- rep(0, length(w)) #empty portolio return vector
portfolio_vol <- rep(0, length(w)) #empty portolio volatility vector
sharpe_ratio <- rep(0, length(w)) #empty sharpe ratio vector

for (i in c(1:length(w))) {
  portfolio_return[i] <- (w[i] * (1+apple_return_annual)) + ((1-w[i]) * (1 + Google_return_annual))
  portfolio_vol[i] <- sqrt( ( (w[i]^2)*(apple_vol^2) +   ( (1 - w[i])^2 )* (Google_vol^2 )   +   2*w[i]*(1-w[i])*apple_vol*Google_vol*apple_Google_cor ) )
  sharpe_ratio[i] <- (portfolio_return[i] - 0.025)/ portfolio_vol[i]
}

which.max(sharpe_ratio) #the "optimal" portfolio is the portfolio with weight 0 in apple

####Estimating Excess Returns ####
rf <- 2.5/100
apple_reg_term <- apple_return - rf
Google_reg_term <- Google_return - rf
sp500_return <- Google_return - rf

reg <- lm(apple_reg_term  ~ sp500_return)

alpha <- reg$residuals
beta <- unname(reg$coefficients["sp500_reg_term"])


### A plot
after2018 <- which(stocks$Date >= "2018-01-01")

plot(stocks$Date[after2018], c(NA, apple_reg_term)[after2018], type = "l", 
     xlab = NA, ylab = "Excess Returns", main = "Apple's Realized & Predicted Excess Returns") + 
  lines(stocks$Date[after2018], c(NA, predict(reg))[after2018], col = "red")
legend("bottomleft", inset=.05,  legend=c("Realized", "Predicted"),
       col=c("black", "red"), lty=1, cex=0.8)
