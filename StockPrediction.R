## Trail By Ramesh

library(quantmod)

install.packages("lubridate")
install.packages("stringi")
library("lubridate")

startDate = as.Date("2017-01-01")
# The beginning of the date range we want to look at
endDate = as.Date("2017-06-03") 
# The end of the date range we want to look at
getSymbols("AAPL", src = "yahoo", from = startDate, to = endDate) 

## Google doesn't work for Indian Stocks
getSymbols("532898", src = "google", from = startDate, to = endDate) 

getSymbols("POWERGRID.NS", src = "yahoo", from = startDate, to = Sys.Date()) 


library(quantmod)
tickers = spl('POWERGRID.NS')
data <- new.env()
#getSymbols("POWERGRID.NS", src = 'yahoo', from = '2015-01-01', env = data, auto.assign = T)
getSymbols("POWERGRID.NS", src = "yahoo", from = '2015-01-01', to = Sys.Date())
getSymbols("532898", src = "google")
dim(POWERGRID.NS)
class(POWERGRID.NS)
colnames(POWERGRID.NS)
head(POWERGRID.NS$POWERGRID.NS.Close)

#Saving to Workspace
saveSymbols(Symbols = "POWERGRID.NS",
            file.path="E:/Ramesh/DataScience/R_Work",env = .GlobalEnv)

#Loading from Workspace
load("E:/Ramesh/DataScience/R_Work/POWERGRID.NS.RData")

#Chart
candleChart(POWERGRID.NS)
barChart(POWERGRID.NS)
chart_Series(POWERGRID.NS)
chart_Series(POWERGRID.NS,type = "candlesticks")
chart_Series(POWERGRID.NS,type = "line")
chart_Series(POWERGRID.NS$POWERGRID.NS.Close)
type = c("auto", "candlesticks", "matchsticks", "bars","line")

library(forecast)
y=auto.arima(POWERGRID.NS$POWERGRID.NS.Close)
plot(forecast(y,h=30))
summary(y)


## Error/Warning is there for nnetar
Close <- as.data.frame(value=coredata(POWERGRID.NS$POWERGRID.NS.Close))
colnames(Close) <- 'Price'
fit <- nnetar(Close$Price)
plot(forecast(fit,h=60))
points(1:length(Close$Price),fitted(fit),type="l",col="green")

