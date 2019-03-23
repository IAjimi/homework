### FIN ECON PROJECT: LOOPED ##
#a few notes
##1: good thing, you don't need to download anything before running the code
##all you need to get the volatilites is the STOCK SYMBOL (from yahoo finance)
##plus end and start dates, type '2018-12-12'
##2: this is not run in r's global environment, so need to access elements using e$
##this allows us to loop over everything in e without messing up the rest of the things in our r env
##3: you need to reset e everytime you want to change something in code
#that's e <- new.env()

###other wise all looped, you just need to input things (ticket/index name + start & end dates) 
#in the USING THE FUNCTION section

#just run everything !!

## Loading Packages ####
#if you don't have it, use install.packages(package_name)
library(tidyverse)
library(lubridate)
library(quantmod)

## Setting Up Function and Enviroment ####
#partly from https://stackoverflow.com/questions/22136174/getsymbols-not-returning-data-as-expected

e <- new.env() #setting up a new environment

vol_cal <- function(stock_name, start, end) {
  
  ##Getting Data from Yahoo Finance
  s <- getSymbols(stock_name, src="yahoo",  from = start, to = end, env = e) #quantmode gets stock data from yahoo
  df <- do.call(merge, eapply(e, Cl)[s]) #applies loop over environment
  #only keeps closing prices of the named stocks, merges df so all stocks are in one file
  
  df <- data.frame(df) #convert result of query into dataframe
  
  for (i in c(1:ncol(df))){
    assign(paste(s[i], "Return", sep = "_"), c(diff(log(df[, i]))) , envir = e ) #calc return for every stock
    assign(paste(s[i], "vol", sep = "_"),  sqrt(256)*sd(c(diff(log(df[, i])))) , envir = e ) #calc vol
  }
}

### USING THE FUNCTION ####
#just run something like the code below
#switch tickers above as needed
vol_cal(c("^DJI", "^GSPC", "^RUT", "^NDX"),  as.Date('2000-01-01'),  as.Date('2018-12-31')) #runs function

### Summary stats
## subsetting vectors
return_vectors <- ls(e)[str_detect(ls(e), "Return")]
vol_vectors <- ls(e)[str_detect(ls(e), "vol")] 
#takes names of things in environment e, only if have "return" or "vol" in them

## summary stats: mean return, annualized vol
lapply(mget(return_vectors, e), mean, na.rm = TRUE)#loops over the return vectors, returns mean 
mget(vol_vectors,  envir= e) #returns vol for everything

## correlation matrix
big_corr_matrix_data <- data.frame(mget(return_vectors,  envir= e))
big_corr_matrix <- cor(big_corr_matrix_data) #gets correlation of df
big_corr_matrix <- as.data.frame(big_corr_matrix) 
names(big_corr_matrix) <- substr(return_vectors, 1, 3) #adds col names
row.names(big_corr_matrix) <- names(big_corr_matrix) #adds row names
round(big_corr_matrix, 2) #returns matrix!

### SUMMARY ####
eapply(e, mean)[return_vectors] #loops over e, returns mean return for everything
mget(vol_vectors,  envir= e) #returns annualized vol for everything
round(big_corr_matrix, 2) #returns matrix!

