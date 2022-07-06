##############################
## Created by Carlo at Hult ##
##############################
####### Jun 28th 2022 ########
##### Hult Business School ###

joined_prices <- read.csv("~/Downloads/joined_prices.csv")
## What is wrong with this data? Masasing

joined_prices$dates_fixed <- as.Date(joined_prices$Date)
joined_prices$X <- NULL

#using the YAHOO API to pull in pricing data for WFC; SPY and AGG
#install.packages("quantmod")
library(quantmod)

#STEP 1: pulling in pricing data:
stock1 <- getSymbols("WFC", auto.assign=FALSE)
getSymbols("WFC")

#pulling in S&P500 via SPY
stock2 <- getSymbols("SPY", auto.assign=FALSE)
getSymbols("SPY")

#pulling in US Bonds via AGG
getSymbols("AGG")
fixed_income <- getSymbols("AGG", auto.assign=FALSE)
#used an underscore in fixe_income because we cannot have names

#STEP2: combining all data frames together:
joined_prices <- merge.xts(stock1, stock2, fixed_income)

#STEP3: pulling in only Adjusted prices: 
joined_prices_only <- joined_prices[,c(6,12,18)]

#STEP4: convert the prices into returns (compare the prices, it is much harder to compare function of prices)
#version A1: Expert version(optional)
joined_returns_loop <- as.data.frame(joined_prices_only)
joined_returns_loop$log_ret <- c()
joied_returns_loop$log_ret[1] <- NA

for (i in 2:nrow(joined_returns_loop)){
joined_returns_loop$log_ret[i] <- log(joined_returns_loop$WFC.Adjusted[i]/joined_returns_loop$WFC.Adjusted[i-1])
}

#UDF to add time window (delta T)
# x= price  t= time but before we used i
#formula log(x[i]) represents the price at a certain time
window_returns <- function(x, t){
  compounded <- rep(NA, each=(t-1))
  for(i in t:length(x)){
  compounded[i] <- log(x[i]/ x[i-t+1])  
  }
  return(compounded)
} #closing the window_returns UDF

#FATTO DAL PROF
window_returns <- function(x, t){ #t in months, x is your variable
  compounded <- rep(NA, each=(t-1)) #creating an empty vector for returns
  for(i in t:length(x)){ # we use 2 because we'll be starting from the 2nd day 
    compounded[i] <- log(x[i]/ x[i-t+1])#the i-t+1 defines our time window !!! IMPORTANT
  } #closing the i-loop
  return(compounded)
}#closing window_returns function


#calling the UDF
window_returns(x=joined_returns_loop$WFC.Adjusted, t=25)
window_returns(x=joined_returns_loop$SPY.Adjusted, t=25)
window_returns(x=joined_returns_loop$AGG.Adjusted, t=25)

#STEP 4: version B - required but is not the best option

stock1_returns <- dailyReturn(getSymbols("WFC", auto.assign=FALSE))
stock1_returns <- monthlyReturn(getSymbols("WFC", auto.assign=FALSE))

stock2_returns <- monthlyReturn(getSymbols("SPY", auto.assign=FALSE))
fixed_income_returns <- monthlyReturn(getSymbols("AGG", auto.assign=FALSE))

#STEP 5: combining returns into 1 data frame

#adding a benchmark: Russel 1000: VONE

benchmark_returns <- monthlyReturn(getSymbols("VONE", auto.assign=FALSE))

joined_monthlyreturns <- merge.xts(stock1_returns, stock2_returns, fixed_income_returns, benchmark_returns) 

#STEP 6: calculating portfolio returns
WFC_alloc <- 0.3
SPY_alloc <- 0.25
AGG_alloc <- 0.45

alloc_vect <- c(0.3, 0.25, 0.45)
#creating a new variable to host portfolio returns
joined_monthlyreturns$portfolio <- joined_monthlyreturns$monthly.returns * WFC_alloc +
                                    joined_monthlyreturns$monthly.returns.1 * SPY_alloc +
                                    joined_monthlyreturns$monthly.returns.2 * AGG_alloc

### moving to investment risk
# calculating sigma to show total risk for assets and portfolio

time_index <- nrow(joined_monthlyreturns)

joined_monthlyreturns <- as.data.frame(joined_monthlyreturns)

WFC_sigma <- sd(joined_monthlyreturns$monthly.returns[(time_index-11):time_index]) *sqrt(12)
                
SPY_sigma <- sd(joined_monthlyreturns$monthly.returns.1[(time_index-11):time_index]) *sqrt(12)

AGG_sigma <- sd(joined_monthlyreturns$monthly.returns.2[(time_index-11):time_index]) *sqrt(12)                                       

Portfolio_sigma <- sd(joined_monthlyreturns$portfolio[(time_index-11):time_index]) *sqrt(12)       

#calculating Tracking error on active returns:
#we use montly returns and if we want a yearly Tracking error we do sqrt(12)

WFC_te <- sd(joined_monthlyreturns$monthly.returns[(time_index-11):time_index]-
               joined_monthlyreturns$monthly.returns.3[(time_index-11):time_index]) *sqrt(12)
                      
SPY_te <- sd(joined_monthlyreturns$monthly.returns.1[(time_index-11):time_index]-
            joined_monthlyreturns$monthly.returns.3[(time_index-11):time_index]) *sqrt(12)

AGG_te <- sd(joined_monthlyreturns$monthly.returns.2[(time_index-11):time_index]-
            joined_monthlyreturns$monthly.returns.3[(time_index-11):time_index]) *sqrt(12)

portfolio_te <- sd(joined_monthlyreturns$portfolio[(time_index-11):time_index]-
               joined_monthlyreturns$monthly.returns.3[(time_index-11):time_index]) *sqrt(12)

### implementing Sharpe ratio

riskfree <- 0.0000000000000001

WFC_sharpe <- (mean(joined_monthlyreturns$monthly.returns[(time_index-11):time_index])-riskfree)/WFC_sigma

SPY_sharpe <- (mean(joined_monthlyreturns$monthly.returns.1[(time_index-11):time_index])-riskfree)/SPY_sigma

AGG_sharpe <- (mean(joined_monthlyreturns$monthly.returns.2[(time_index-11):time_index])-riskfree)/AGG_sigma

Portfolio_sharpe <- (mean(joined_monthlyreturns$portfolio[(time_index-11):time_index])-riskfree)/Portfolio_sigma       


WFC_sharpe_2y <- (mean(joined_monthlyreturns$monthly.returns[(time_index-23):time_index])-riskfree)/WFC_sigma

SPY_sharpe_2y <- (mean(joined_monthlyreturns$monthly.returns.1[(time_index-23):time_index])-riskfree)/SPY_sigma

AGG_sharpe_2y <- (mean(joined_monthlyreturns$monthly.returns.2[(time_index-23):time_index])-riskfree)/AGG_sigma

Portfolio_sharpe_2y <- (mean(joined_monthlyreturns$portfolio[(time_index-23):time_index])-riskfree)/Portfolio_sigma 


WFC_sharpe_3y <- (mean(joined_monthlyreturns$monthly.returns[(time_index-35):time_index])-riskfree)/WFC_sigma

SPY_sharpe_3y <- (mean(joined_monthlyreturns$monthly.returns.1[(time_index-35):time_index])-riskfree)/SPY_sigma

AGG_sharpe_3y <- (mean(joined_monthlyreturns$monthly.returns.2[(time_index-35):time_index])-riskfree)/AGG_sigma

Portfolio_sharpe_3y <- (mean(joined_monthlyreturns$portfolio[(time_index-35):time_index])-riskfree)/Portfolio_sigma 



WFC_Sharpe_all<-c(WFC_sharpe, WFC_sharpe_2y, WFC_sharpe_3y)

SPY_Sharpe_all<-c(SPY_sharpe, SPY_sharpe_2y, SPY_sharpe_3y)

AGG_Sharpe_all<-c(AGG_sharpe, AGG_sharpe_2y, AGG_sharpe_3y)

Portfolio_Sharpe_all<-c(Portfolio_sharpe, Portfolio_sharpe_2y, Portfolio_sharpe_3y)


