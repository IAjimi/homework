### PREDICTION: LOGIT V. RANDOM GUESSES ####
### Loading Libraries ####
library(sandwich)
library(lmtest)
library(tidyverse)
library(haven)

### Loading Dataset ####
youth <- read_dta("~/youth.dta")

### Pre-Regression ####
reduced_df <- select(youth, ageyrs, male, black, hisp, other, partners, drank, 
                     pot, coke, glue, heroin, steroids, speed) 
#smaller df for plotting, summary stats, & prediction

### Probit ####
myprobit <- glm(consider ~ ageyrs+ male+ black+ hisp+ other+ partners+ drank+ pot+ coke+ 
                  glue+ heroin+ steroids+ speed, 
                family = binomial(link = "probit"), data = youth)
coeftest(myprobit, vcov = vcovHC(myprobit, type="HC1")) #using robust s.e.

#predictions
yhat_probit <- predict(myprobit, newdata = reduced_df, type = "response")

#classification error
probit_predicts <- rep(0, length(yhat_probit))
probit_predicts[yhat_probit > 0.5] <- 1
1 - sum(diag(table(probit_predicts, youth$consider))) / length(youth$consider)
#probit classification error: 17.5%

## Basic Guessing Model ####
set.seed(4)

tries <- c(1:100) #number of tries
accuracy <- rep(0, max(tries)) #result vector

for (i in tries) {
  dart <- sample(c(0,1), length(youth$consider), 
                 prob = c(1 - mean(youth$consider), mean(youth$consider)),
                 replace = TRUE)
  #predicting randomly that an individual considered suicide based on mean of population
  
  accuracy[i] <- sum(diag(table(dart, youth$consider))) / length(youth$consider)
  #diag of the dart, youth$consider table has 0,0 and 1,1 i.e. all accurate predictions
}

1 - mean(accuracy) #mean error: 29.24%