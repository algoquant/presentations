library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(xts_check_TZ=FALSE)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# load package "HighFreq"
library(HighFreq)
sym_bol <- "SPY"  # define sym_bol
# load OHLC data
re_turns <- calc_rets(xts_data=to.daily(SPY))
len_gth <- nrow(re_turns)  # number of observations
mean_rets <- mean(re_turns[, 1])  # calculate mean
sd_rets <- sd(re_turns[, 1])  # calculate standard deviation
# calculate skew and kurtosis
(sum(((re_turns[, 1] - mean_rets)/sd_rets)^3))/len_gth
(sum(((re_turns[, 1] - mean_rets)/sd_rets)^4))/len_gth
library(PerformanceAnalytics)
chart.Histogram(re_turns[, 1], main="", 
  xlim=c(-6e-5, 6e-5), 
  methods = c("add.density", "add.normal"))
# add title
title(main=paste(sym_bol, "density"), line=-1)
# install package "HighFreq" from github
install.packages("devtools")
library(devtools)
install_github(repo="algoquant/HighFreq")
# load package "HighFreq"
library(HighFreq)
# set data directories
data_dir <- "C:/Develop/data/hfreq/src/"
output_dir <- "C:/Develop/data/hfreq/scrub/"
# define sym_bol
sym_bol <- "SPY"
# load a single day of TAQ data
sym_bol <- load(
  file.path(data_dir,
      paste0(sym_bol, "/2014.05.02.",
             sym_bol, ".RData")))
# scrub, aggregate single day of TAQ data to OHLC
ohlc_data <- scrub_agg(taq_data=get(sym_bol))
# aggregate TAQ data for symbol, save to file
save_scrub_agg(sym_bol,
         data_dir=data_dir,
         output_dir=output_dir,
         period="minutes")
# load package "HighFreq"
library(HighFreq)
# define sym_bol
sym_bol <- "SPY"
# load OHLC data
output_dir <- "C:/Develop/data/hfreq/scrub/"
sym_bol <- load(
  file.path(output_dir,
      paste0(sym_bol, ".RData")))
inter_val <-
  "2013-11-11 09:30:00/2013-11-11 10:30:00"
chart_Series(SPY[inter_val],
      name=sym_bol)
par(mfrow=c(2,1))  # set plot panels
library(quantmod)
library(TTR)
inter_val <- "2013-11-11/2013-11-15"
var_iance <- volatility(OHLC=SPY,
               calc="yang.zhang", n=20)
chart_Series(var_iance[inter_val],
      name=paste(sym_bol, "vol w/ ON spikes"))
var_iance <- volatility(OHLC=SPY,
               calc="rogers.satchell", n=20)
chart_Series(var_iance[inter_val],
      name=paste(sym_bol, "vol w/o ON spikes"))
par(mfrow=c(2,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
# daily variance and volume
daily_var <- apply.daily(x=SPY, FUN=agg_ohlc,
                  agg_fun="vol_ohlc")
colnames(daily_var) <- paste0(sym_bol, ".var")
daily_volume <- apply.daily(x=Vo(SPY), FUN=sum)
chart_Series(daily_var,
       name=paste(sym_bol, "variance"))
chart_Series(daily_volume,
       name=paste(sym_bol, "volume"))
par(mfrow=c(3,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
# daily volume
daily_volu <- apply.daily(x=SPY, 
        FUN=function(da_ta) sum(Vo(da_ta)))
colnames(daily_volu) <- paste0(sym_bol, ".volume")
inter_val <- "2010"
chart_Series(sqrt(daily_var[inter_val]), 
       name=paste(sym_bol, "std dev"))
chart_Series(daily_volu[inter_val],
       name=paste(sym_bol, "volume"))
chart_Series(
  sqrt(daily_var[inter_val]/daily_volu[inter_val]),
  name=paste(sym_bol, "illiquidity"))
library(HighFreq)  # load package "HighFreq"
# daily Hurst exponents
daily_hurst <- apply.daily(x=SPY,
                     FUN=agg_ohlc,
                     agg_fun="hurst_ohlc")
colnames(daily_hurst) <-
  paste(col_name(get(sym_bol)), ".Hurst")
chart_Series(roll_sum(daily_hurst, 10)[-(1:10)]/10,
       name=paste(sym_bol, "Hurst"))
abline(h=0.5, col="blue", lwd=2)
par(mfrow=c(2,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
chart_Series(roll_sum(daily_var, 10)[-(1:10)]/10,
       name=paste(sym_bol, "variance"))
chart_Series(roll_sum(daily_hurst, 10)[-(1:10)]/10,
       name=paste(sym_bol, "Hurst"))
abline(h=0.5, col="blue", lwd=2)
par(mfrow=c(2,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
# daily seasonality of volume
season_volume <- season_ality(Vo(SPY))
season_volume <- season_volume[-(nrow(season_volume))]
colnames(season_volume) <-
  paste0(col_name(get(sym_bol)), ".season_volume")
plot_theme <- chart_theme()
plot_theme$format.labels <- "%H:%M"
ch_ob <- chart_Series(x=season_volume,
  name=paste(colnames(season_volume),
  "daily seasonality"), theme=plot_theme,
  plot=FALSE)
y_lim <- ch_ob$get_ylim()
y_lim[[2]] <- structure(c(y_lim[[2]][1],
        y_lim[[2]][2]/2), fixed=TRUE)
ch_ob$set_ylim(y_lim)
plot(ch_ob)
# daily seasonality of volatility
season_var <- season_ality(vol_ohlc(ohlc=SPY))
season_var <- season_var[-(nrow(season_var))]
par(mfrow=c(2,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
# daily seasonality of liquidity
season_illiquid <- season_ality(
  ifelse(Vo(SPY)==0, 0, sqrt(vol_ohlc(ohlc=SPY)/Vo(SPY))))
colnames(season_illiquid) <-
  paste0(col_name(get(sym_bol)), ".season_illiquid")
chart_Series(x=season_illiquid,
  name=paste(colnames(season_illiquid), "daily seasonality"))
# daily seasonality of volatility
season_var <- season_ality(vol_ohlc(ohlc=SPY))
par(mfrow=c(2,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
# daily seasonality of Hurst exponent
season_hurst <- season_ality(hurst_ohlc(ohlc=SPY[inter_val, 1:4]))
season_hurst <- season_hurst[-(nrow(season_hurst))]
colnames(season_hurst) <- paste0(col_name(get(sym_bol)), ".season_hurst")
plot_theme <- chart_theme()
plot_theme$format.labels <- "%H:%M"
ch_ob <- chart_Series(x=season_hurst,
  name=paste(colnames(season_hurst),
  "daily seasonality"), theme=plot_theme,
  plot=FALSE)
y_lim <- ch_ob$get_ylim()
y_lim[[2]] <- structure(c(y_lim[[2]][1],
        y_lim[[2]][2]), fixed=TRUE)
ch_ob$set_ylim(y_lim)
plot(ch_ob)
abline(h=0.5, col="blue", lwd=2)
# daily seasonality of volatility
season_var <- season_ality(vol_ohlc(ohlc=SPY))
par(mfrow=c(2,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
# intraday seasonality of Hurst exponent
ran_ge <- range(daily_var)
plot(x=as.vector(daily_var), y=as.vector(daily_hurst), 
     xlab=colnames(daily_var), ylab=colnames(daily_hurst),  
     main="Daily Hurst and variance", 
     xlim=c(ran_ge[1], ran_ge[2]/4))
abline(h=0.5, col="blue", lwd=2)
ran_ge <- range(season_var)
plot(x=as.vector(season_var), y=as.vector(season_hurst), 
     xlab=colnames(season_var), ylab=colnames(season_hurst),  
     main="Intraday seasonal Hurst and variance", 
     xlim=c(ran_ge[1], ran_ge[2]/2), ylim=c(0.35, 0.7))
abline(h=0.5, col="blue", lwd=2)
par(mfrow=c(2,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
# rolling variance
var_iance <-
  roll_agg_ohlc(ohlc=SPY, agg_fun="vol_ohlc")
# rolling skew
sk_ew <-
  roll_agg_ohlc(ohlc=SPY, agg_fun="skew_ohlc")
sk_ew <- sk_ew/(var_iance)^(1.5)
sk_ew[1, ] <- 0
sk_ew <- na.locf(sk_ew)
inter_val <- "2013-11-11/2013-11-15"
chart_Series(var_iance[inter_val],
      name=paste(sym_bol, "variance"))
chart_Series(sk_ew[inter_val],
      name=paste(sym_bol, "Skew"),
      ylim=c(-1, 1))
par(mfrow=c(2,1))  # set plot panels
library(HighFreq)  # load package "HighFreq"
# daily variance and skew
daily_var <- apply.daily(x=SPY, FUN=agg_ohlc,
                  agg_fun="vol_ohlc")
colnames(daily_var) <- paste0(sym_bol, ".var")
daily_skew <- apply.daily(x=SPY, FUN=agg_ohlc,
                  agg_fun="skew_ohlc")
daily_skew <- daily_skew/(daily_var)^(1.5)
colnames(daily_skew) <- paste0(sym_bol, ".skew")
inter_val <- "2013-06-01/"
chart_Series(daily_var[inter_val],
       name=paste(sym_bol, "variance"))
chart_Series(daily_skew[inter_val],
       name=paste(sym_bol, "skew"))
# skew scatterplot
re_turns <- calc_rets(xts_data=SPY)
sk_ew <- skew_ohlc(log_ohlc=log(SPY[, -5]))
colnames(sk_ew) <- paste0(sym_bol, ".skew")
lag_skew <- lag(sk_ew)
lag_skew[1, ] <- 0
da_ta <- cbind(re_turns[, 1], sign(lag_skew))
for_mula <- as.formula(paste(colnames(da_ta)[1], 
    paste(paste(colnames(da_ta)[-1], 
      collapse=" + "), "- 1"), sep="~"))
for_mula
l_m <- lm(for_mula, data=da_ta)
summary(l_m)$coef
summary(lm(for_mula, data=da_ta["/2011-01-01"]))$coef
summary(lm(for_mula, data=da_ta["2011-01-01/"]))$coef
inter_val <- "2013-12-01/"
plot(for_mula, data=da_ta[inter_val],
     xlim=c(-2e-09, 2e-09),
     cex=0.6, xlab="skew", ylab="rets")
abline(l_m, col="blue", lwd=2)
# contrarian skew trading strategy
# lag the skew to get positions
position_s <- -sign(lag_skew)
position_s[1, ] <- 0
# cumulative PnL
cumu_pnl <- cumsum(position_s*re_turns[, 1])
# calculate frequency of trades
50*sum(abs(sign(sk_ew)-sign(lag_skew)))/nrow(sk_ew)
# calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
bid_offer*sum(abs(sign(sk_ew)-sign(lag_skew)))
chart_Series(
  cumu_pnl[endpoints(cumu_pnl, on="hours"), ],
  name=paste(sym_bol, "contrarian skew strategy pnl"))
# vwap plot
vwap_short <-
  v_wap(x_ts=SPY, win_dow=70)
vwap_long <-
  v_wap(x_ts=SPY, win_dow=225)
vwap_diff <- vwap_short - vwap_long
colnames(vwap_diff) <- paste0(sym_bol, ".vwap")
inter_val <- "2010-05-05/2010-05-07"
invisible(
  chart_Series(x=Cl(SPY[inter_val]),
         name=paste(sym_bol, "plus VWAP")))
invisible(
  add_TA(vwap_short[inter_val],
   on=1, col="red", lwd=2))
invisible(
  add_TA(vwap_long[inter_val],
   on=1, col="blue", lwd=2))
invisible(
  add_TA(vwap_diff[inter_val] > 0, on=-1,
   col="lightgreen", border="lightgreen"))
add_TA(vwap_diff[inter_val] < 0, on=-1,
 col="lightgrey", border="lightgrey")
# vwap scatterplot
# re_turns <- calc_rets(xts_data=SPY)
vwap_short <- v_wap(x_ts=SPY, win_dow=70)
vwap_long <- v_wap(x_ts=SPY, win_dow=225)
vwap_diff <- vwap_short - vwap_long
colnames(vwap_diff) <- paste0(sym_bol, ".vwap")
lag_vwap <- lag(vwap_diff)
lag_vwap[1, ] <- 0
da_ta <- cbind(re_turns[, 1], sign(lag_vwap))
for_mula <- as.formula(paste(colnames(da_ta)[1], 
    paste(paste(colnames(da_ta)[-1], 
      collapse=" + "), "- 1"), sep="~"))
for_mula
l_m <- lm(for_mula, data=da_ta)
summary(l_m)$coef
summary(lm(for_mula, data=da_ta["/2011-01-01"]))$coef
summary(lm(for_mula, data=da_ta["2011-01-01/"]))$coef
inter_val <- "2013-12-01/"
plot(for_mula, data=cbind(re_turns[, 1], lag_vwap)[inter_val],
     cex=0.6, xlab="skew", ylab="rets")
abline(l_m, col="blue", lwd=2)
# momentum trading strategy
# cumulative PnL
cumu_pnl <- cumsum(sign(lag_vwap)*re_turns[, 1])
# calculate frequency of trades
50*sum(abs(sign(vwap_diff)-sign(lag_vwap)))/nrow(vwap_diff)
# calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
bid_offer*sum(abs(sign(vwap_diff)-sign(lag_vwap)))
chart_Series(
  cumu_pnl[endpoints(cumu_pnl, on="hours"), ],
  name=paste(sym_bol, "VWAP momentum strategy pnl"))
