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

#Set filter variable to include only a subset of the data frames

filter_start_date <- as.Date('2013-01-01', format = "%Y-%m-%d")
filter_end_date <- as.Date('2016-12-31', format = "%Y-%m-%d")

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

test_reg <- lm(Month_End_Price ~ No_Growth_Days + No_No_Growth_Days + No_Positive_Runs + No_Negative_Runs + as.factor(Ticker) + as.factor(Month), data=summary_funds_df, subset = Year == 2013| Year == 2014 |Year ==2015 | Year ==2016)
summary_funds_df[['predictions']] <- predict(test_reg, summary_funds_df)
summary_funds_df[['residuals']] <- test_reg$residuals

par(mfrow = c(2, 2))
plot(test_reg)

#ARIMA modelling
vigix_fit <- auto.arima(vigix_df[,4],seasonal = FALSE)
plot(forecast(vigix_fit,h=10),include=80)
par(mfrow=c(1,2))
Acf(vigix_df[,2],main="")
Pacf(vigix_df[,2],main="")



