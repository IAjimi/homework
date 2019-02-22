#### Loading Packages ####
library(readr)
library(tidyverse)
library(lubridate)

###Importing Data
HW3_Data <- read_excel("C:/Users/ia767/Downloads/HW3-Data.xlsx")
names(HW3_Data) <- c("Date", "Apple", "SP500")

####QUESTION 3####
###Computing Returns
apple_return <- c(diff(log(HW3_Data$Apple))) #monthly return %
sp500_return <- c(diff(log(HW3_Data$SP500))) 

###Calculating Annual Returns
time_past <- as.numeric(difftime(HW3_Data$Date[62], HW3_Data$Date[1], units = c("days")))
#retrive total numbers of days between first and last date
time_past <- time_past/30.5 #roughly calculating n# months

apple_cum_return <- cumprod(1 + apple_return)[length(apple_return)] #final cumulative return
apple_return_annual <- ( (1+apple_cum_return)^(12/time_past) ) - 1 #26.75%

sp500_cum_return <- cumprod(1 + sp500_return)[length(sp500_return)] #final cumulative return
sp500_return_annual <- ( (1+sp500_cum_return)^(12/time_past) ) - 1 #20.87%  

###Calculating Annual Volatilities
apple_vol <- sqrt(12)*sd(apple_return, na.rm = TRUE) #sqrt(12) since monthly figures
sp500_vol <- sqrt(12)*sd(sp500_return, na.rm = TRUE)

###Calculating Correlation
apple_SP500_cor <- cor(apple_return, sp500_return)

###A Plot
plot(HW3_Data$Date, c(NA, apple_return), col = 'blue', type = "l", 
     xlab = "", ylab = "% Returns", main = "Apple v. SP500 Returns Over Time")+ 
  lines(HW3_Data$Date, c(NA, sp500_return))
legend("bottomleft", inset=.05,  legend=c("SP500", "Apple"),
       col=c("black", "blue"), lty=1, cex=0.8)

###d) Make a portfolio of the two assets, and choose weights that maximize the
#portfolio Sharpe ratio, defined as 
#(expected portfolio return - risk-free return) / portfolio volatility. 
#Use a risk-free return of 0.25% per month. What is the optimal allocation in Apple stock? 

w <- seq(0, 1, 0.01) #apple weights vector
portfolio_return <- rep(0, length(w)) #empty portolio return vector
portfolio_vol <- rep(0, length(w)) #empty portolio volatility vector
sharpe_ratio <- rep(0, length(w)) #empty sharpe ratio vector

rf <- (1.0025)^12 - 1 #risk-free return of 0.25% per month, annualized

for (i in c(1:length(w))) {
  portfolio_return[i] <- (w[i] * (1+apple_return_annual)) + ((1-w[i]) * (1 + sp500_return_annual))
  portfolio_vol[i] <- sqrt( ( (w[i]^2)*(apple_vol^2) +   ( (1 - w[i])^2 )* (sp500_vol^2 )   +   2*w[i]*(1-w[i])*apple_vol*sp500_vol*apple_SP500_cor ) )
  sharpe_ratio[i] <- ( portfolio_return[i] - (1 + rf) )/ portfolio_vol[i]
}

which.max(sharpe_ratio) #the "optimal" portfolio
w[which.max(sharpe_ratio)] #the weight of apple at this optimal portfolio, 7%
sharpe_ratio[which.max(sharpe_ratio)] 
portfolio_return[which.max(sharpe_ratio)]
portfolio_vol[which.max(sharpe_ratio)]

sharpe_ratio[i] <- ( portfolio_return[i] - (1 + rf) )/ portfolio_vol[i]

####QUESTION 4####
rf2 <- 0.25/100 
apple_reg_term <- apple_return - rf2 #y
sp500_reg_term <- sp500_return - rf2 #x

reg <- lm(apple_reg_term  ~ sp500_reg_term) #running regression

alpha <- reg$residuals #storing residuals
mean(alpha)

beta <- unname(reg$coefficients["sp500_reg_term"]) #getting beta coeff

time_period <- which(HW3_Data$Date >= "2018-11-01" & HW3_Data$Date <= "2019-01-01") 
#finding row number of time period
cumsum(alpha[time_period])[2] #taking final cumsum

### A plot
plot(HW3_Data$Date, HW3_Data$Apple, type = "l", 
     xlab = "", ylab = "Stock Price", main = "Apple's Stock Price")

## Another Plot
plot(HW3_Data$Date, c(NA, apple_reg_term), type = "l", 
     xlab = NA, ylab = "Excess Returns", main = "Apple's Realized & Predicted Excess Returns") + 
  lines(HW3_Data$Date, c(NA, predict(reg)), col = "red")
legend("bottomleft", inset=.05,  legend=c("Realized", "Predicted"),
       col=c("black", "red"), lty=1, cex=0.8)

###QUESTION 5####
w <- seq(0, 2, 0.001) #apple weights vector
portfolio_return <- rep(0, length(w)) #empty portolio return vector
portfolio_vol <- rep(0, length(w)) #empty portolio volatility vector
sharpe_ratio <- rep(0, length(w)) #empty sharpe ratio vector

for (i in c(1:length(w))) {
  portfolio_return[i] <- (w[i] * (1.05)) + ((1-w[i]) * (1.018))
  portfolio_vol[i] <- sqrt( ( (w[i]^2)*(0.2^2) +   ( (1 - w[i])^2 )* (0.12^2 )   +   2*w[i]*(1-w[i])*0.2*0.12*0.2 ) )
  sharpe_ratio[i] <- ( portfolio_return[i] - (1.001) )/ portfolio_vol[i]
}

#Min Vol Portfolio
which.min(portfolio_vol)
w[which.min(portfolio_vol)] 
sharpe_ratio[which.min(portfolio_vol)] 
portfolio_return[which.min(portfolio_vol)]
portfolio_vol[which.min(portfolio_vol)]

#Max Sharpe Ratio Portfolio
which.max(sharpe_ratio) 
w[which.max(sharpe_ratio)] 
sharpe_ratio[which.max(sharpe_ratio)] 
portfolio_return[which.max(sharpe_ratio)]
portfolio_vol[which.max(sharpe_ratio)]

#Levered Portfolio
which(w == 1.5)
w[which(w == 1.5)] 
sharpe_ratio[which(w == 1.5)] 
portfolio_return[which(w == 1.5)]
portfolio_vol[which(w == 1.5)]
