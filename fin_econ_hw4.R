##Question 2####
returns_per <- c(0.55, -0.322, -0.333, 0.385, 0.721, -0.497, 0.452, -0.18) #r
returns <- 1 + returns_per #1 + r
mean(returns) #arithmetic mean
mean(log(returns)) #mean log return
mean(returns_per) - ((sd(returns_per)^2)/2)

portfolio_value <- rep(0, length(returns)+1) #creates portfolio vector for this simulation
portfolio_value[1] <- 100 #sets initial value of portfolio

for(i in c(1:length(returns))){
  portfolio_value[i+1] <- portfolio_value[i]*(returns[i]) #calculates portfolio value for every t
}

portfolio_value #prints out portfolio values

## Portfolio Time Path Math ####
set.seed(3)
tries <- 500000 #number of tries
final_value <- rep(0, tries) #stores final portfolio value
r <- 0.7/100 #monthly return
vol <- 5/100 #monthly volatility
time_periods <- 12*5 #number time periods
initial_value <- 100 #initial portfolio value

for (i in c(1:tries)) { #full simulation, repeats portfolio simulation 500,000+ times
  monthly_returns <- rnorm(time_periods, r, vol) #generates random returns
  
  portfolio_value <- rep(0, time_periods) #creates portfolio vector for this simulation
  portfolio_value[1] <- initial_value #sets initial value of portfolio
  
  for (j in c(2:time_periods)) { #portfolio simulation
    portfolio_value[j] <- portfolio_value[j-1]*(1 + monthly_returns[j])
  }
  
  final_value[i] <- portfolio_value[time_periods] #stores final value
}

sort(final_value)[0.05*tries] #value at risk, orders vector by portfolio value and picks value at 5%

mean(final_value) #expected value
unname(quantile(final_value, 0.5)) #median
length(final_value[final_value > mean(final_value) ]) / length(final_value) #prob being above mean
length(final_value[final_value > unname(quantile(final_value, 0.5)) ]) / length(final_value) #prob being above median

length(final_value[final_value < initial_value]) / length(final_value) #prob of being below initial value
length(final_value[final_value > 300]) / length(final_value) #prob of being above 300

#Question 5#### 
#a)  Assuming  geometric  Brownian  motion  for  stocks,  
#what  is  the  probability  of  stocks  outperforming the bond return?
r <- 6.5/100
vol <- 14/100
time_periods <- 20 #number time periods
initial_value <- 100 #initial portfolio value

#Lognormal wealth distribution
log_mean <- log(initial_value)+(r - (vol^2)/2 )*time_periods #computes log mean
log_vol <- vol*sqrt(time_periods) #computes log 
log_returns <- rnorm(tries, log_mean, log_vol) #generates 500,000 random returns based on log_mean, log_vol
new_returns<- exp(log_returns) #gets regular value

mean(new_returns)
length(new_returns[new_returns > 102.9]) / length(new_returns) #probability of being above a 2.9% return
