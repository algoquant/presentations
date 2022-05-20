library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(xts_check_TZ=FALSE)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# load package "HighFreq"
library(HighFreq)
symbol <- "SPY"  # define symbol
# load OHLC data
returns <- calc_rets(xts_data=to.daily(SPY))
nrows <- nrow(returns)  # number of observations
mean_rets <- mean(returns[, 1])  # calculate mean
sd_rets <- sd(returns[, 1])  # calculate standard deviation
# calculate skew and kurtosis
(sum(((returns[, 1] - mean_rets)/sd_rets)^3)) nrows
(sum(((returns[, 1] - mean_rets)/sd_rets)^4)) nrows
library(PerformanceAnalytics)
chart.Histogram(returns[, 1], main="", 
  xlim=c(-6e-5, 6e-5), 
  methods = c("add.density", "add.normal"))
# add title
title(main=paste(symbol, "density"), line=-1)
# install package "HighFreq" from github
install.packages("devtools")
library(devtools)
install_github(repo="algoquant/HighFreq")
# load package "HighFreq"
library(HighFreq)
# set data directories
data_dir <- "/Users/jerzy/Develop/data/hfreq/src/"
output_dir <- "/Users/jerzy/Develop/data/hfreq/scrub/"
# define symbol
symbol <- "SPY"
# load a single day of TAQ data
symbol <- load(
  file.path(data_dir,
      paste0(symbol, "/2014.05.02.",
             symbol, ".RData")))
# scrub, aggregate single day of TAQ data to OHLC
ohlc_data <- scrub_agg(taq_data=get(symbol))
# aggregate TAQ data for symbol, save to file
save_scrub_agg(symbol,
         data_dir=data_dir,
         output_dir=output_dir,
         period="minutes")
# load package "HighFreq"
library(HighFreq)
# define symbol
symbol <- "SPY"
# load OHLC data
output_dir <- "/Users/jerzy/Develop/data/hfreq/scrub/"
symbol <- load(
  file.path(output_dir,
      paste0(symbol, ".RData")))
interval <-
  "2013-11-11 09:30:00/2013-11-11 10:30:00"
chart_Series(SPY[interval],
      name=symbol)
par(mfrow=c(2,1))  # set plot panels
library(quantmod)
library(TTR)
interval <- "2013-11-11/2013-11-15"
var_iance <- volatility(OHLC=SPY,
               calc="yang.zhang", n=20)
chart_Series(var_iance[interval],
      name=paste(symbol, "vol w/ ON spikes"))
var_iance <- volatility(OHLC=SPY,
               calc="rogers.satchell", n=20)
chart_Series(var_iance[interval],
      name=paste(symbol, "vol w/o ON spikes"))
par(mfrow=c(2,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
# daily variance and volume
daily_var <- apply.daily(x=SPY, FUN=agg_ohlc,
                  agg_fun="vol_ohlc")
colnames(daily_var) <- paste0(symbol, ".var")
daily_volume <- apply.daily(x=Vo(SPY), FUN=sum)
chart_Series(daily_var,
       name=paste(symbol, "variance"))
chart_Series(daily_volume,
       name=paste(symbol, "volume"))
par(mfrow=c(3,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
# daily volume
daily_volu <- apply.daily(x=SPY, 
        FUN=function(datav) sum(Vo(datav)))
colnames(daily_volu) <- paste0(symbol, ".volume")
interval <- "2010"
chart_Series(sqrt(daily_var[interval]), 
       name=paste(symbol, "std dev"))
chart_Series(daily_volu[interval],
       name=paste(symbol, "volume"))
chart_Series(
  sqrt(daily_var[interval]/daily_volu[interval]),
  name=paste(symbol, "illiquidity"))
library(HighFreq)  # load package "HighFreq"
# daily Hurst exponents
daily_hurst <- apply.daily(x=SPY,
                     FUN=agg_ohlc,
                     agg_fun="hurst_ohlc")
colnames(daily_hurst) <-
  paste(colname(get(symbol)), ".Hurst")
chart_Series(roll_sum(daily_hurst, 10)[-(1:10)]/10,
       name=paste(symbol, "Hurst"))
abline(h=0.5, col="blue", lwd=2)
par(mfrow=c(2,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
chart_Series(roll_sum(daily_var, 10)[-(1:10)]/10,
       name=paste(symbol, "variance"))
chart_Series(roll_sum(daily_hurst, 10)[-(1:10)]/10,
       name=paste(symbol, "Hurst"))
abline(h=0.5, col="blue", lwd=2)
par(mfrow=c(2,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
# daily seasonality of volume
season_volume <- season_ality(Vo(SPY))
season_volume <- season_volume[-(nrow(season_volume))]
colnames(season_volume) <-
  paste0(colname(get(symbol)), ".season_volume")
plot_theme <- chart_theme()
plot_theme$format.labels <- "%H:%M"
chobj <- chart_Series(x=season_volume,
  name=paste(colnames(season_volume),
  "daily seasonality"), theme=plot_theme,
  plot=FALSE)
ylim <- chobj$get_ylim()
ylim[[2]] <- structure(c(ylim[[2]][1],
        ylim[[2]][2]/2), fixed=TRUE)
chobj$set_ylim(ylim)
plot(chobj)
# daily seasonality of volatility
season_var <- season_ality(vol_ohlc(ohlc=SPY))
season_var <- season_var[-(nrow(season_var))]
par(mfrow=c(2,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
# daily seasonality of liquidity
season_illiquid <- season_ality(
  ifelse(Vo(SPY)==0, 0, sqrt(vol_ohlc(ohlc=SPY)/Vo(SPY))))
colnames(season_illiquid) <-
  paste0(colname(get(symbol)), ".season_illiquid")
chart_Series(x=season_illiquid,
  name=paste(colnames(season_illiquid), "daily seasonality"))
# daily seasonality of volatility
season_var <- season_ality(vol_ohlc(ohlc=SPY))
par(mfrow=c(2,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
# daily seasonality of Hurst exponent
season_hurst <- season_ality(hurst_ohlc(ohlc=SPY[interval, 1:4]))
season_hurst <- season_hurst[-(nrow(season_hurst))]
colnames(season_hurst) <- paste0(colname(get(symbol)), ".season_hurst")
plot_theme <- chart_theme()
plot_theme$format.labels <- "%H:%M"
chobj <- chart_Series(x=season_hurst,
  name=paste(colnames(season_hurst),
  "daily seasonality"), theme=plot_theme,
  plot=FALSE)
ylim <- chobj$get_ylim()
ylim[[2]] <- structure(c(ylim[[2]][1],
        ylim[[2]][2]), fixed=TRUE)
chobj$set_ylim(ylim)
plot(chobj)
abline(h=0.5, col="blue", lwd=2)
# daily seasonality of volatility
season_var <- season_ality(vol_ohlc(ohlc=SPY))
par(mfrow=c(2,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
# intraday seasonality of Hurst exponent
rangev <- range(daily_var)
plot(x=as.vector(daily_var), y=as.vector(daily_hurst), 
     xlab=colnames(daily_var), ylab=colnames(daily_hurst),  
     main="Daily Hurst and variance", 
     xlim=c(rangev[1], rangev[2]/4))
abline(h=0.5, col="blue", lwd=2)
rangev <- range(season_var)
plot(x=as.vector(season_var), y=as.vector(season_hurst), 
     xlab=colnames(season_var), ylab=colnames(season_hurst),  
     main="Intraday seasonal Hurst and variance", 
     xlim=c(rangev[1], rangev[2]/2), ylim=c(0.35, 0.7))
abline(h=0.5, col="blue", lwd=2)
par(mfrow=c(2,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
# rolling variance
var_iance <-
  roll_agg_ohlc(ohlc=SPY, agg_fun="vol_ohlc")
# rolling skew
skew <-
  roll_agg_ohlc(ohlc=SPY, agg_fun="skew_ohlc")
skew <- skew/(var_iance)^(1.5)
skew[1, ] <- 0
skew <- na.locf(skew)
interval <- "2013-11-11/2013-11-15"
chart_Series(var_iance[interval],
      name=paste(symbol, "variance"))
chart_Series(skew[interval],
      name=paste(symbol, "Skew"),
      ylim=c(-1, 1))
par(mfrow=c(2,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
# daily variance and skew
daily_var <- apply.daily(x=SPY, FUN=agg_ohlc,
                  agg_fun="vol_ohlc")
colnames(daily_var) <- paste0(symbol, ".var")
daily_skew <- apply.daily(x=SPY, FUN=agg_ohlc,
                  agg_fun="skew_ohlc")
daily_skew <- daily_skew/(daily_var)^(1.5)
colnames(daily_skew) <- paste0(symbol, ".skew")
interval <- "2013-06-01/"
chart_Series(daily_var[interval],
       name=paste(symbol, "variance"))
chart_Series(daily_skew[interval],
       name=paste(symbol, "skew"))
# skew scatterplot
returns <- calc_rets(xts_data=SPY)
skew <- skew_ohlc(log_ohlc=log(SPY[, -5]))
colnames(skew) <- paste0(symbol, ".skew")
lag_skew <- lag(skew)
lag_skew[1, ] <- 0
datav <- cbind(returns[, 1], sign(lag_skew))
formulav <- as.formula(paste(colnames(datav)[1], 
    paste(paste(colnames(datav)[-1], 
      collapse=" + "), "- 1"), sep="~"))
formulav
l_m <- lm(formulav, data=datav)
summary(l_m)$coef
summary(lm(formulav, data=datav["/2011-01-01"]))$coef
summary(lm(formulav, data=datav["2011-01-01/"]))$coef
interval <- "2013-12-01/"
plot(formulav, data=datav[interval],
     xlim=c(-2e-09, 2e-09),
     cex=0.6, xlab="skew", ylab="rets")
abline(l_m, col="blue", lwd=2)
# contrarian skew trading strategy
# lag the skew to get positions
posit <- -sign(lag_skew)
posit[1, ] <- 0
# cumulative PnL
cumu_pnl <- cumsum(posit*returns[, 1])
# calculate frequency of trades
50*sum(abs(sign(skew)-sign(lag_skew)))/nrow(skew)
# calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
bid_offer*sum(abs(sign(skew)-sign(lag_skew)))
chart_Series(
  cumu_pnl[endpoints(cumu_pnl, on="hours"), ],
  name=paste(symbol, "contrarian skew strategy pnl"))
# vwap plot
vwap_short <-
  vwapv(xtes=SPY, look_back=70)
vwap_long <-
  vwapv(xtes=SPY, look_back=225)
vwap_diff <- vwap_short - vwap_long
colnames(vwap_diff) <- paste0(symbol, ".vwap")
interval <- "2010-05-05/2010-05-07"
invisible(
  chart_Series(x=Cl(SPY[interval]),
         name=paste(symbol, "plus VWAP")))
invisible(
  add_TA(vwap_short[interval],
   on=1, col="red", lwd=2))
invisible(
  add_TA(vwap_long[interval],
   on=1, col="blue", lwd=2))
invisible(
  add_TA(vwap_diff[interval] > 0, on=-1,
   col="lightgreen", border="lightgreen"))
add_TA(vwap_diff[interval] < 0, on=-1,
 col="lightgrey", border="lightgrey")
# vwap scatterplot
# returns <- calc_rets(xts_data=SPY)
vwap_short <- vwapv(xtes=SPY, look_back=70)
vwap_long <- vwapv(xtes=SPY, look_back=225)
vwap_diff <- vwap_short - vwap_long
colnames(vwap_diff) <- paste0(symbol, ".vwap")
lag_vwap <- lag(vwap_diff)
lag_vwap[1, ] <- 0
datav <- cbind(returns[, 1], sign(lag_vwap))
formulav <- as.formula(paste(colnames(datav)[1], 
    paste(paste(colnames(datav)[-1], 
      collapse=" + "), "- 1"), sep="~"))
formulav
l_m <- lm(formulav, data=datav)
summary(l_m)$coef
summary(lm(formulav, data=datav["/2011-01-01"]))$coef
summary(lm(formulav, data=datav["2011-01-01/"]))$coef
interval <- "2013-12-01/"
plot(formulav, data=cbind(returns[, 1], lag_vwap)[interval],
     cex=0.6, xlab="skew", ylab="rets")
abline(l_m, col="blue", lwd=2)
# momentum trading strategy
# cumulative PnL
cumu_pnl <- cumsum(sign(lag_vwap)*returns[, 1])
# calculate frequency of trades
50*sum(abs(sign(vwap_diff)-sign(lag_vwap)))/nrow(vwap_diff)
# calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
bid_offer*sum(abs(sign(vwap_diff)-sign(lag_vwap)))
chart_Series(
  cumu_pnl[endpoints(cumu_pnl, on="hours"), ],
  name=paste(symbol, "VWAP momentum strategy pnl"))
