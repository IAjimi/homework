### FIN ECON PROJECT: LOOPED ##
#a few notes
##1: all you need to get the volatilites is the STOCK SYMBOL (from yahoo finance)
##plus end and start dates, format '2018-12-12' (yyyy-mm-dd)
##2: this is not run in r's global environment but a new env called 'e'
##this allows us to loop over everything without messing up the rest of the things in the global env
##3: you need to reset e everytime you want to change stock_name or date
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
  
  ##volatilities
  vol_vectors <- ls(e)[str_detect(ls(e), "vol")] #subsets environment for volatilies
  assign("volatilities", mget(vol_vectors,  envir= e), envir = globalenv()) #returns vol for everything
  
  ##correlation matrix
  return_vectors <- ls(e)[str_detect(ls(e), "Return")] #subsets enviroment for return vectors
  big_corr_matrix_data <- data.frame(mget(return_vectors,  envir= e)) #greates a df made up of the return vectors
  big_corr_matrix <- cor(big_corr_matrix_data) #gets correlation of df
  big_corr_matrix <- as.data.frame(big_corr_matrix) 
  names(big_corr_matrix) <- substr(return_vectors, 1, 3) #adds col names
  row.names(big_corr_matrix) <- names(big_corr_matrix) #adds row names
  assign("corr_matrix", round(big_corr_matrix, 2), envir = globalenv()) #returns matrix!
}

### INPUT ####
#just run something like the code below
#switch tickers above as needed
e <- new.env() 
vol_cal(c("^DJI", "^GSPC", "^RUT", "^NDX", "CPTNX"),  as.Date('2000-01-01'),  as.Date('2018-12-31')) 
#runs function

### OUTPUT ####
volatilities
corr_matrix
