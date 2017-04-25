#Load Quandl package and create data frames for each fund in the portfolio
#Large Growth	Vanguard Growth Index I 	(VIGIX)
#Large Blend	Schwab S&P 500 Index 	(SWPPX)
#Large Blend	American Funds Fundamental Invs R6 	(RFNGX)
#Mid-Cap Value	Vanguard Selected Value Inv 	(VASVX)
#Small Growth	Vanguard Small Cap Growth Index I 	(VSGIX)
#Small Blend	Vanguard Small Cap Index I 	(VSCIX)
#Real Estate	Cohen & Steers Instl Realty Shares 	(CSRIX)
#Foreign Large Blend	Schwab International Index 	(SWISX)
#Foreign Large Growth	American Funds Europacific Growth R6 	(RERGX)
#Diversified Emerging Mkts	DFA Emerging Markets I 	(DFEMX)
#Diversified Emerging Mkts	DFA Emerging Markets Small Cap I 	(DEMSX)
#Multisector Bond	Loomis Sayles Bond Instl 	(LSBDX)
#Intermediate-Term Bond	Vanguard Total Bond Market Index I 	(VBTIX)
#Inflation-Protected Bond	Vanguard Inflation-Protected Secs Inv 	(VIPSX)

library(Quandl)
library(ggplot2)
library(quantmod)
library(lubridate)
library(dplyr)
library(data.table)
library(forecast)
library(ts)

#Set filter variable to include only a subset of the data frames

filter_start_date <- as.Date('2013-01-01', format = "%Y-%m-%d")
filter_end_date <- as.Date('2016-12-31', format = "%Y-%m-%d")
actual_start_date <- as.Date('2017-01-01', format = "%Y-%m-%d")
actual_end_date <- as.Date('2017-03-31', format = "%Y-%m-%d")

vigix_df <- subset(data.frame(Quandl("YAHOO/FUND_VIGIX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= filter_start_date & Date <= filter_end_date)
vigix_df <- vigix_df[,c("Date","Adjusted.Close")]
vigix_df$Ticker <- "VIGIX"
vigix_df$Index <- Delt(vigix_df$Adjusted.Close)[,1]
vigix_df = within(vigix_df, {
  Indicator = ifelse(Index < 0, 0, 1)
})

swppx_df <- subset(data.frame(Quandl("YAHOO/FUND_SWPPX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= filter_start_date & Date <= filter_end_date)
swppx_df <- swppx_df[,c("Date","Adjusted.Close")]
swppx_df$Ticker <- "SWPPX"
swppx_df$Index <- Delt(swppx_df$Adjusted.Close)[,1]
swppx_df = within(swppx_df, {
  Indicator = ifelse(Index < 0, 0, 1)
})

rfngx_df <- subset(data.frame(Quandl("YAHOO/FUND_RFNGX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= filter_start_date & Date <= filter_end_date)
rfngx_df <- rfngx_df[,c("Date","Adjusted.Close")]
rfngx_df$Ticker <- "RFNGX"
rfngx_df$Index <- Delt(rfngx_df$Adjusted.Close)[,1]
rfngx_df = within(rfngx_df, {
  Indicator = ifelse(Index < 0, 0, 1)
})

vasvx_df <- subset(data.frame(Quandl("YAHOO/FUND_VASVX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= filter_start_date & Date <= filter_end_date)
vasvx_df <- vasvx_df[,c("Date","Adjusted.Close")]
vasvx_df$Ticker <- "VASVX"
vasvx_df$Index <- Delt(vasvx_df$Adjusted.Close)[,1]
vasvx_df = within(vasvx_df, {
  Indicator = ifelse(Index < 0, 0, 1)
})

vsgix_df <- subset(data.frame(Quandl("YAHOO/FUND_VSGIX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= filter_start_date & Date <= filter_end_date)
vsgix_df <- vsgix_df[,c("Date","Adjusted.Close")]
vsgix_df$Ticker <- "VSGIX"
vsgix_df$Index <- Delt(vsgix_df$Adjusted.Close)[,]
vsgix_df = within(vsgix_df, {
  Indicator = ifelse(Index < 0, 0, 1)
})

vscix_df <- subset(data.frame(Quandl("YAHOO/FUND_VSCIX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= filter_start_date & Date <= filter_end_date)
vscix_df <- vscix_df[,c("Date","Adjusted.Close")]
vscix_df$Ticker <- "VSCIX"
vscix_df$Index <- Delt(vscix_df$Adjusted.Close)[,1]
vscix_df = within(vscix_df, {
  Indicator = ifelse(Index < 0, 0, 1)
})

csrix_df <- subset(data.frame(Quandl("YAHOO/FUND_CSRIX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= filter_start_date & Date <= filter_end_date)
csrix_df <- csrix_df[,c("Date","Adjusted.Close")]
csrix_df$Ticker <- "CSRIX"
csrix_df$Index <- Delt(csrix_df$Adjusted.Close)[,1]
csrix_df = within(csrix_df, {
  Indicator = ifelse(Index < 0, 0, 1)
})

swisx_df <- subset(data.frame(Quandl("YAHOO/FUND_SWISX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= filter_start_date & Date <= filter_end_date)
swisx_df <- swisx_df[,c("Date","Adjusted.Close")]
swisx_df$Ticker <- "SWISX"
swisx_df$Index <- Delt(swisx_df$Adjusted.Close)[,1]
swisx_df = within(swisx_df, {
  Indicator = ifelse(Index < 0, 0, 1)
})

rergx_df <- subset(data.frame(Quandl("YAHOO/FUND_RERGX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= filter_start_date & Date <= filter_end_date)
rergx_df <- rergx_df[,c("Date","Adjusted.Close")]
rergx_df$Ticker <- "RERGX"
rergx_df$Index <- Delt(rergx_df$Adjusted.Close)[,1]
rergx_df = within(rergx_df, {
  Indicator = ifelse(Index < 0, 0, 1)
})

dfemx_df <- subset(data.frame(Quandl("YAHOO/FUND_DFEMX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= filter_start_date & Date <= filter_end_date)
dfemx_df <- dfemx_df[,c("Date","Adjusted.Close")]
dfemx_df$Ticker <- "DFEMX"
dfemx_df$Index <- Delt(dfemx_df$Adjusted.Close)[,1]
dfemx_df = within(dfemx_df, {
  Indicator = ifelse(Index < 0, 0, 1)
})

demsx_df <- subset(data.frame(Quandl("YAHOO/FUND_DEMSX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= filter_start_date & Date <= filter_end_date)
demsx_df <- demsx_df[,c("Date","Adjusted.Close")]
demsx_df$Ticker <- "DEMSX"
demsx_df$Index <- Delt(demsx_df$Adjusted.Close)[,1]
demsx_df = within(demsx_df, {
  Indicator = ifelse(Index < 0, 0, 1)
})

lsbdx_df <- subset(data.frame(Quandl("YAHOO/FUND_LSBDX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= filter_start_date & Date <= filter_end_date)
lsbdx_df <- lsbdx_df[,c("Date","Adjusted.Close")]
lsbdx_df$Ticker <- "LSBDX"
lsbdx_df$Index <- Delt(lsbdx_df$Adjusted.Close)[,1]
lsbdx_df = within(lsbdx_df, {
  Indicator = ifelse(Index < 0, 0, 1)
})


vbtix_df <- subset(data.frame(Quandl("YAHOO/FUND_VBTIX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= filter_start_date & Date <= filter_end_date)
vbtix_df <- vbtix_df[,c("Date","Adjusted.Close")]
vbtix_df$Ticker <- "VBTIX"
vbtix_df$Index <- Delt(vbtix_df$Adjusted.Close)[,1]
vbtix_df = within(vbtix_df, {
  Indicator = ifelse(Index < 0, 0, 1)
})

vipsx_df <- subset(data.frame(Quandl("YAHOO/FUND_VIPSX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= filter_start_date & Date <= filter_end_date)
vipsx_df <- vipsx_df[,c("Date","Adjusted.Close")]
vipsx_df$Ticker <- "VIPSX"
vipsx_df$Index <- Delt(vipsx_df$Adjusted.Close)[,1]
vipsx_df = within(vipsx_df, {
  Indicator = ifelse(Index < 0, 0, 1)
})

# Create data frames for analysis
all_funds_df <- rbind(vigix_df, swppx_df, rfngx_df, vasvx_df, vsgix_df, vscix_df, csrix_df, swisx_df, rergx_df, 
                      dfemx_df, demsx_df, lsbdx_df, vbtix_df, vipsx_df)
all_funds_df <- na.omit(all_funds_df)

forecast_df <- cbind(vigix_df, swppx_df, rfngx_df, vasvx_df, vsgix_df, vscix_df, csrix_df, swisx_df, rergx_df, 
                     dfemx_df, demsx_df, lsbdx_df, vbtix_df, vipsx_df)
#Add date dependant variables week, month, quarter, 

all_funds_df$Year <- year(all_funds_df$Date)
all_funds_df$Month <- month(all_funds_df$Date)
all_funds_df$Quarter <- quarter(all_funds_df$Date)

max_pos_runs <- function(x) 
  { 
  r <- rle(x);
  max(r$lengths[r$values==1])
}
max_neg_runs <- function(x) 
{ 
  r <- rle(x);
  max(r$lengths[r$values==0])
}

summary_funds_df <- all_funds_df %>% group_by(Ticker, Year, Quarter, Month) %>% summarise(No_Growth_Days = sum(Indicator), No_Positive_Runs = max_pos_runs(Indicator), No_No_Growth_Days = (length(Indicator) - sum(Indicator)), No_Negative_Runs = max_neg_runs(Indicator), Month_Begin_Price = Adjusted.Close[which(Date == min(Date))], Month_End_Price = Adjusted.Close[which(Date == max(Date))], Range =  (Adjusted.Close[which(Date == max(Date))] - Adjusted.Close[which(Date == min(Date))]))
summary_funds_df$Growth_Ratio <- summary_funds_df$No_Positive_Runs/summary_funds_df$No_Growth_Days
summary_funds_df$No_Growth_Ratio <- summary_funds_df$No_Negative_Runs/summary_funds_df$No_No_Growth_Days

# Plot to show high-level movement
ggplot(all_funds_df, aes(x = Date, y = Adjusted.Close, group = Ticker, color = Ticker)) + geom_line()
ggplot(all_funds_df, aes(x = Date, y = Index, group = Ticker, color = Ticker)) + geom_line()

#ggplot(subset(all_funds_df, Ticker == "CSRIX"), aes(x = Date, y = Index, color = Ticker)) + geom_line()
#ggplot(subset(all_funds_df, Ticker == "DEMSX"), aes(x = Date, y = Index, color = Ticker)) + geom_line() 
#ggplot(subset(all_funds_df, Ticker == "DFEMX"), aes(x = Date, y = Index, color = Ticker)) + geom_line() 
#ggplot(subset(all_funds_df, Ticker == "LSBDX"), aes(x = Date, y = Index, color = Ticker)) + geom_line() 
#ggplot(subset(all_funds_df, Ticker == "RERGX"), aes(x = Date, y = Index, color = Ticker)) + geom_line() 
#ggplot(subset(all_funds_df, Ticker == "RFNGX"), aes(x = Date, y = Index, color = Ticker)) + geom_line() 
#ggplot(subset(all_funds_df, Ticker == "SWISX"), aes(x = Date, y = Index, color = Ticker)) + geom_line() 
#ggplot(subset(all_funds_df, Ticker == "SWPPX"), aes(x = Date, y = Index, color = Ticker)) + geom_line() 
#ggplot(subset(all_funds_df, Ticker == "VASVX"), aes(x = Date, y = Index, color = Ticker)) + geom_line() 
#ggplot(subset(all_funds_df, Ticker == "VBTIX"), aes(x = Date, y = Index, color = Ticker)) + geom_line() 
#ggplot(subset(all_funds_df, Ticker == "VIGIX"), aes(x = Date, y = Index, color = Ticker)) + geom_line() 
#ggplot(subset(all_funds_df, Ticker == "VIPSX"), aes(x = Date, y = Index, color = Ticker)) + geom_line() 
#ggplot(subset(all_funds_df, Ticker == "VSCIX"), aes(x = Date, y = Index, color = Ticker)) + geom_line() 
#ggplot(subset(all_funds_df, Ticker == "VSGIX"), aes(x = Date, y = Index, color = Ticker)) + geom_line() 

test_reg <- lm(Month_End_Price ~ No_Growth_Days + No_No_Growth_Days + No_Positive_Runs + No_Negative_Runs + as.factor(Ticker) + as.factor(Month), data=summary_funds_df, subset = Year == 2013| Year == 2014 |Year ==2015 | Year ==2016)
summary_funds_df[['predictions']] <- predict(test_reg, summary_funds_df)
summary_funds_df[['residuals']] <- test_reg$residuals

par(mfrow = c(2, 2))
plot(test_reg)

#ARIMA modelling VIGIX
par(mfrow=c(1,2))
Acf(vigix_df[,2],main="")
Pacf(vigix_df[,2],main="")
vigix_fit <- auto.arima(vigix_df[,2],seasonal=FALSE)

#ARIMA modelling SWPPX
par(mfrow=c(1,2))
Acf(swppx_df[,2],main="")
Pacf(swppx_df[,2],main="")
swppx_fit <- auto.arima(swppx_df[,2],seasonal = FALSE)

#ARIMA modelling RFNGX
par(mfrow=c(1,2))
Acf(rfngx_df[,2],main="")
Pacf(rfngx_df[,2],main="")
rfngx_fit <- auto.arima(rfngx_df[,2],seasonal = FALSE)

#ARIMA modelling VASVX
par(mfrow=c(1,2))
Acf(vasvx_df[,2],main="")
Pacf(vasvx_df[,2],main="")
vasvx_fit <- auto.arima(vasvx_df[,2],seasonal = FALSE)

#ARIMA modelling VSGIX
par(mfrow=c(1,2))
Acf(vsgix_df[,2],main="")
Pacf(vsgix_df[,2],main="")
vsgix_fit <- auto.arima(vsgix_df[,2],seasonal = FALSE)

#ARIMA modelling VSCIX
par(mfrow=c(1,2))
Acf(vscix_df[,2],main="")
Pacf(vscix_df[,2],main="")
vscix_fit <- auto.arima(vscix_df[,2],seasonal = FALSE)

#ARIMA modelling CSRIX
par(mfrow=c(1,2))
Acf(csrix_df[,2],main="")
Pacf(csrix_df[,2],main="")
csrix_fit <- auto.arima(csrix_df[,2],seasonal = FALSE)

#ARIMA modelling SWISX
par(mfrow=c(1,2))
Acf(swisx_df[,2],main="")
Pacf(swisx_df[,2],main="")
swisx_fit <- auto.arima(swisx_df[,2],seasonal = FALSE)

#ARIMA modelling RERGX
par(mfrow=c(1,2))
Acf(rergx_df[,2],main="")
Pacf(rergx_df[,2],main="")
rergx_fit <- auto.arima(rergx_df[,2],seasonal = FALSE)

#ARIMA modelling DFEMX
par(mfrow=c(1,2))
Acf(dfemx_df[,2],main="")
Pacf(dfemx_df[,2],main="")
dfemx_fit <- auto.arima(dfemx_df[,2],seasonal = FALSE)

#ARIMA modelling DEMSX
par(mfrow=c(1,2))
Acf(demsx_df[,2],main="")
Pacf(demsx_df[,2],main="")
demsx_fit <- auto.arima(demsx_df[,2],seasonal = FALSE)

#ARIMA modelling LSBDX
par(mfrow=c(1,2))
Acf(lsbdx_df[,2],main="")
Pacf(lsbdx_df[,2],main="")
lsbdx_fit <- auto.arima(lsbdx_df[,2],seasonal = FALSE)

#ARIMA modelling VBTIX
par(mfrow=c(1,2))
Acf(vbtix_df[,2],main="")
Pacf(vbtix_df[,2],main="")
vbtix_fit <- auto.arima(vbtix_df[,2],seasonal = FALSE)

#ARIMA modelling VIPSX
par(mfrow=c(1,2))
Acf(vipsx_df[,2],main="")
Pacf(vipsx_df[,2],main="")
vipsx_fit <- auto.arima(vipsx_df[,2],seasonal = FALSE)

#Residual Diagnostics
par(mfrow=c(4,3))
Acf(residuals(vigix_fit))
hist(vigix_fit$residuals, nclass="FD", main="Histogram of residuals")
plot(vigix_fit$residuals, main="Residuals", ylab="", xlab="Data point")
Acf(residuals(swppx_fit))
hist(swppx_fit$residuals, nclass="FD", main="Histogram of residuals")
plot(swppx_fit$residuals, main="Residuals", ylab="", xlab="Data point")
Acf(residuals(rfngx_fit))
hist(rfngx_fit$residuals, nclass="FD", main="Histogram of residuals")
plot(rfngx_fit$residuals, main="Residuals", ylab="", xlab="Data point")
Acf(residuals(vasvx_fit))
hist(vasvx_fit$residuals, nclass="FD", main="Histogram of residuals")
plot(vasvx_fit$residuals, main="Residuals", ylab="", xlab="Data point")

par(mfrow=c(4,3))
Acf(residuals(vscix_fit))
hist(vscix_fit$residuals, nclass="FD", main="Histogram of residuals")
plot(vscix_fit$residuals, main="Residuals", ylab="", xlab="Data point")
Acf(residuals(csrix_fit))
hist(csrix_fit$residuals, nclass="FD", main="Histogram of residuals")
plot(csrix_fit$residuals, main="Residuals", ylab="", xlab="Data point")
Acf(residuals(swisx_fit))
hist(swisx_fit$residuals, nclass="FD", main="Histogram of residuals")
plot(swisx_fit$residuals, main="Residuals", ylab="", xlab="Data point")
Acf(residuals(rergx_fit))
hist(rergx_fit$residuals, nclass="FD", main="Histogram of residuals")
plot(rergx_fit$residuals, main="Residuals", ylab="", xlab="Data point")

par(mfrow=c(4,3))
Acf(residuals(dfemx_fit))
hist(dfemx_fit$residuals, nclass="FD", main="Histogram of residuals")
plot(dfemx_fit$residuals, main="Residuals", ylab="", xlab="Data point")
Acf(residuals(demsx_fit))
hist(demsx_fit$residuals, nclass="FD", main="Histogram of residuals")
plot(demsx_fit$residuals, main="Residuals", ylab="", xlab="Data point")
Acf(residuals(lsbdx_fit))
hist(lsbdx_fit$residuals, nclass="FD", main="Histogram of residuals")
plot(lsbdx_fit$residuals, main="Residuals", ylab="", xlab="Data point")
Acf(residuals(vbtix_fit))
hist(vbtix_fit$residuals, nclass="FD", main="Histogram of residuals")
plot(vbtix_fit$residuals, main="Residuals", ylab="", xlab="Data point")

par(mfrow=c(1,3))
Acf(residuals(vipsx_fit))
hist(vipsx_fit$residuals, nclass="FD", main="Histogram of residuals")
plot(vipsx_fit$residuals, main="Residuals", ylab="", xlab="Data point")

# Create a data frame with forecast values
vigix_forecast_df <- data.frame(forecast(vigix_fit, h = 90))
swppx_forecast_df <- data.frame(forecast(swppx_fit, h = 90))
rfngx_forecast_df <- data.frame(forecast(rfngx_fit, h = 90))
vasvx_forecast_df <- data.frame(forecast(vasvx_fit, h = 90))
vscix_forecast_df <- data.frame(forecast(vscix_fit, h = 90))
csrix_forecast_df <- data.frame(forecast(csrix_fit, h = 90))
swisx_forecast_df <- data.frame(forecast(swisx_fit, h = 90))
rergx_forecast_df <- data.frame(forecast(rergx_fit, h = 90))
dfemx_forecast_df <- data.frame(forecast(dfemx_fit, h = 90))
demsx_forecast_df <- data.frame(forecast(demsx_fit, h = 90))
lsbdx_forecast_df <- data.frame(forecast(lsbdx_fit, h = 90))
vbtix_forecast_df <- data.frame(forecast(vbtix_fit, h = 90))
vipsx_forecast_df <- data.frame(forecast(vipsx_fit, h = 90))

forecast_df <- cbind(vigix = vigix_forecast_df$Point.Forecast, swppx = swppx_forecast_df$Point.Forecast, 
                     rfngx = rfngx_forecast_df$Point.Forecast, vasvx = vasvx_forecast_df$Point.Forecast, 
                     vscix = vscix_forecast_df$Point.Forecast, csrix = csrix_forecast_df$Point.Forecast, 
                     swisx = swisx_forecast_df$Point.Forecast, rergx = rergx_forecast_df$Point.Forecast, 
                     dfemx = dfemx_forecast_df$Point.Forecast, demsx = demsx_forecast_df$Point.Forecast, 
                     lsbdx = lsbdx_forecast_df$Point.Forecast, vbtix = vbtix_forecast_df$Point.Forecast, 
                     vipsx = vipsx_forecast_df$Point.Forecast)
forecast_summary <- rbind(head(forecast_df, n = 1),tail(forecast_df, n = 1))
forecast_range <- rbind(vigix = (forecast_summary[2,1] - forecast_summary[1,1]), 
                        swppx = (forecast_summary[2,2] - forecast_summary[1,2]),
                        rfngx = (forecast_summary[2,3] - forecast_summary[1,3]),
                        vasvx = (forecast_summary[2,4] - forecast_summary[1,4]),
                        vscix = (forecast_summary[2,5] - forecast_summary[1,5]),
                        csrix = (forecast_summary[2,6] - forecast_summary[1,6]),
                        swisx = (forecast_summary[2,7] - forecast_summary[1,7]),
                        rergx = (forecast_summary[2,8] - forecast_summary[1,8]),
                        dfemx = (forecast_summary[2,9] - forecast_summary[1,9]),
                        demsx = (forecast_summary[2,10] - forecast_summary[1,10]),
                        lsbdx = (forecast_summary[2,11] - forecast_summary[1,11]),
                        vbtix = (forecast_summary[2,12] - forecast_summary[1,12]),
                        vipsx = (forecast_summary[2,13] - forecast_summary[1,13]))

# Creating testing data frame
vigix_testing_df <- subset(data.frame(Quandl("YAHOO/FUND_VIGIX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= actual_start_date & Date <= actual_end_date)
swppx_testing_df <- subset(data.frame(Quandl("YAHOO/FUND_SWPPX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= actual_start_date & Date <= actual_end_date)
rfngx_testing_df <- subset(data.frame(Quandl("YAHOO/FUND_RFNGX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= actual_start_date & Date <= actual_end_date)
vasvx_testing_df <- subset(data.frame(Quandl("YAHOO/FUND_VASVX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= actual_start_date & Date <= actual_end_date)
vsgix_testing_df <- subset(data.frame(Quandl("YAHOO/FUND_VSGIX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= actual_start_date & Date <= actual_end_date)
vscix_testing_df <- subset(data.frame(Quandl("YAHOO/FUND_VSCIX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= actual_start_date & Date <= actual_end_date)
csrix_testing_df <- subset(data.frame(Quandl("YAHOO/FUND_CSRIX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= actual_start_date & Date <= actual_end_date)
swisx_testing_df <- subset(data.frame(Quandl("YAHOO/FUND_SWISX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= actual_start_date & Date <= actual_end_date)
rergx_testing_df <- subset(data.frame(Quandl("YAHOO/FUND_RERGX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= actual_start_date & Date <= actual_end_date)
dfemx_testing_df <- subset(data.frame(Quandl("YAHOO/FUND_DFEMX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= actual_start_date & Date <= actual_end_date)
demsx_testing_df <- subset(data.frame(Quandl("YAHOO/FUND_DEMSX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= actual_start_date & Date <= actual_end_date)
lsbdx_testing_df <- subset(data.frame(Quandl("YAHOO/FUND_LSBDX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= actual_start_date & Date <= actual_end_date)
vbtix_testing_df <- subset(data.frame(Quandl("YAHOO/FUND_VBTIX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= actual_start_date & Date <= actual_end_date)
vipsx_testing_df <- subset(data.frame(Quandl("YAHOO/FUND_VIPSX", api_key="5fNF2ytDvc8vEGTqi2Tg")), subset = Date >= actual_start_date & Date <= actual_end_date)

actual_df <- cbind(vigix = vigix_testing_df$Adjusted.Close, swppx = swppx_testing_df$Adjusted.Close, 
                   rfngx = rfngx_testing_df$Adjusted.Close, vasvx = vasvx_testing_df$Adjusted.Close, 
                   vscix = vscix_testing_df$Adjusted.Close, csrix = csrix_testing_df$Adjusted.Close, 
                   swisx = swisx_testing_df$Adjusted.Close, rergx = rergx_testing_df$Adjusted.Close, 
                   dfemx = dfemx_testing_df$Adjusted.Close, demsx = demsx_testing_df$Adjusted.Close, 
                   lsbdx = lsbdx_testing_df$Adjusted.Close, vbtix = vbtix_testing_df$Adjusted.Close, 
                   vipsx = vipsx_testing_df$Adjusted.Close)

actual_summary <- rbind(head(actual_df, n = 1), tail(actual_df, n = 1))

actual_range <- rbind(vigix = (actual_summary[2,1] - actual_summary[1,1]), 
                      swppx = (actual_summary[2,2] - actual_summary[1,2]),
                      rfngx = (actual_summary[2,3] - actual_summary[1,3]),
                      vasvx = (actual_summary[2,4] - actual_summary[1,4]),
                      vscix = (actual_summary[2,5] - actual_summary[1,5]),
                      csrix = (actual_summary[2,6] - actual_summary[1,6]),
                      swisx = (actual_summary[2,7] - actual_summary[1,7]),
                      rergx = (actual_summary[2,8] - actual_summary[1,8]),
                      dfemx = (actual_summary[2,9] - actual_summary[1,9]),
                      demsx = (actual_summary[2,10] - actual_summary[1,10]),
                      lsbdx = (actual_summary[2,11] - actual_summary[1,11]),
                      vbtix = (actual_summary[2,12] - actual_summary[1,12]),
                      vipsx = (actual_summary[2,13] - actual_summary[1,13]))


