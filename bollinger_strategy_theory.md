---
title: "The Bollinger Strategy"
author: "Jerzy Pawlowski (jpawlowski@machinetrader.io)"
date: "06/17/2024"
output:
  html_document: default
  pdf_document: default
email: jpawlowski@machinetrader.io
affiliation: MachineTrader.io
---

### The Bollinger Strategy  

The Bollinger Strategy tries to capture the reversion of prices to their equilibrium value, and buys stocks when they're cheap (below the z-score), and sells them when they're expensive (above the z-score).  

The z-score $z_t$ is equal to the difference between the current price $p_t$ minus the equilibrium price $p_e$, divided by the moving average volatility of the prices $\sigma_t$:
$${\normalsize z_t = \frac{p_t - p_e}{\sigma_t}}$$  

The equilibrium price $p_e$ is often chosen to be the moving average price $p_e = \bar{p}_t$.  
You can read more about the moving average prices and volatilities below, and also in 
[**this document**](https://algoquant.github.io/2024/06/13/Moving-Averages-Theory/).  

As an alternative, the equilibrium price can be chosen to be the last trade price $p_f$ - the price at which the last trade was filled (executed).  The advantage of this is that it prevents a subsequent trade at a loss.  For example, a subsequent buy at a higher price than the previous sell, etc.  

The strategy determines if the stock is cheap or expensive depending on the value of the z-score compared to the threshold level $\theta$.  

The strategy buys $1$ share if the stock is cheap (the z-score is less than minus the threshold level $z_t < -\theta$), and it sells $1$ share if the stock is expensive (the z-score is greater than the threshold level $z_t > \theta$).  
The strategy continues to buy or sell shares as long as the absolute z-score is greater than $\theta$.  So it can accumulate an inventory of stock positions, either long or short, up to the position limit.  

Below is a plot of the z-scores for intraday 1-minute SPY stock prices for May 31st, 2024.  
The z-scores oscillate around zero, and they can be positive or negative.  
The horizontal dashed lines are for $\theta = 1$ and $\theta = -1$.  
![alt text](figure/bolllinger_zscores.png "Moving Average Price Volatility
"){width="500" height="350"}  

You can read more about the implementation of the Bollinger strategy in 
[**this document**](https://algoquant.github.io/2024/06/13/Bollinger-Strategy-Implementation/).  


<br>

### The Bollinger Strategy Parameters  

The performance of the *Bollinger strategy* depends on the values of its two parameters: the lambda decay parameter $\lambda$, and the z-score threshold level $\theta$.  

The decay parameter $\lambda$ is used for calculating the moving average prices and volatilities.  

If $\lambda$ is closer to $1$ then the moving average price has a stronger dependence on the past prices, and it adjusts more slowly to the current prices.  
In this case, the difference between the current price and the moving average price can be large.  
The range of the z-score values is also large.  

If $\lambda$ is closer to $0$ then the moving average price adjusts quickly to the current prices, and the difference between the current price and the moving average price is small.  
The range of the z-score values is also small.  

The parameter $\theta$ is used to determine if the z-score value indicates that the stock is mispriced (its price is cheap or expensive).  


### Calibration of the Bollinger Strategy  

For the purpose of calibration, the historical prices can be split into in-sample prices (past training set) and out-of-sample prices (future test set).  

The *Bollinger strategy* is simulated on the training set prices, and the values of the $\lambda$ and $\theta$ parameters are chosen to maximize its performance.  For example, to maximize its cumulative profits (PnLs), or to maximize its Sharpe ratio.  
The *Bollinger strategy* is then simulated on the test set prices, to test the performance of the calibrated model.  

For example, we could select the past 20 days of intraday stock prices for the training set data, and find the values of the $\lambda$ and $\theta$ parameters which maximize the cumulative PnL for that time interval.  
We could then run the calibrated *Bollinger strategy* on live prices on the next day, to test its out-of-sample performance.  

The values of the optimal $\lambda$ and $\theta$ parameters depend on the length of the training set data (the training set).  
A smaller training set (say only 10 past days) results in a greater variance (variability) of the optimal parameters.  
A larger training set (say 30 past days) results in a lower variance of the optimal parameters (thanks to diversification), but at the cost of a larger bias, because the optimal parameters adjust more slowly to the new prices.  

This is an example of the *bias-variance tradeoff* with respect to the size of the training set.  
A smaller training set has a smaller bias (it adjusts quickly to the new prices), but a larger variance (the optimal parameters change significantly over time).  
A larger training set has a smaller variance, but it also has a larger bias (it adjusts more slowly to the new prices).  
To achieve good out-of-sample performance, the optimal parameters should have both a small bias and a small variance.  
Achieving the best *bias-variance tradeoff* is one of the major objectives of *machine learning*.  


Choosing the right model parameters is difficult.  It is the responsibility of the investor to make that choice.  


<br>

### The Moving Average Prices  

The moving average prices can be calculated for a streaming time series of prices $p_t$.  

The Exponential Moving Average (EMA) price $\bar{p}_t$ is calculated using a decay parameter $\lambda$ and the recursive formula: 

$${\normalsize \bar{p}_t = \lambda \bar{p}_{t-1} + (1-\lambda) p_t}$$

The moving average price at time $t$: $p_t$ is equal to the decay parameter $\lambda$ times the moving average price at time $t-1$: $p_{t-1}$, plus $(1-\lambda)$ times the price at time $t-1$: $\bar{p}_{t-1}$.

The decay parameter $\lambda$ determines the persistence of data memory, i.e. how quickly the contribution of the past data to the average value fades over time.  

If the decay parameter $\lambda$ is closer to $1$ then the effect of past data fades slowly over time.
If $\lambda$ is closer to $0$ then the effect of past data fades quickly over time.  

The above recursive formula can be expressed as a series:

$${\normalsize \bar{p}_t = (1-\lambda) \sum\limits_{j=0}^{\infty} {\lambda}^j p_{t-j} = (1-\lambda) (p_t + \lambda p_{t-1} + \lambda^2 p_{t-2} + \ldots)} $$

The formula is called *exponential* because the decay parameter $\lambda$ is raised to the power of $j$, so the weights $\lambda^j$ decrease exponentially as $j$ increases.  

The advantage of the EMA price is that it applies a greater weight to more recent prices than to past ones.  

The EMA calculation is also faster because there's less computation, and it doesn't require maintaining a buffer (queue) of past prices.  

<br>

### The Moving Average Variance of Prices  

The moving average variance of prices $\sigma^2_t$ measures the dispersion of the streaming prices around the moving average price.
It can be calculated for a streaming time series of prices $p_t$.  

The Exponential Moving Average (EMA) variance of prices $\sigma^2_t$ is calculated using the two recursive formulas: 

$${\normalsize \sigma^2_t = \lambda \sigma^2_{t-1} + (1-\lambda) (p_t - \bar{p}_{t-1})^2}$$
$${\normalsize \bar{p}_t = \lambda \bar{p}_{t-1} + (1-\lambda) p_t}$$

The moving average variance at time $t$: $\sigma^2_t$ is equal to the decay parameter $\lambda$ times the moving average variance at time $t-1$: $\sigma^2_{t-1}$, plus $(1-\lambda)$ times the squared difference between the current price $p_t$ minus the moving average price at time $t-1$: $\bar{p}_{t-1}$.


<br>

### Choosing The Decay parameter $\lambda$

The moving average prices are a smoothed version of the streaming prices.  

**The decay parameter $\lambda$ determines the strength of the data smoothing.**  

If $\lambda$ is closer to $1$ then the moving average prices are smoother, but if it's closer to $0$ then the average prices are more variable and they follow the streaming prices more closely.

The effect of the decay parameter $\lambda$ on the moving average prices is illustrated in the animated plot below of VTI stock prices.  When $\lambda = 0.99$ the average prices are very smooth, but as it decreases, they become more variable and they follow the streaming prices more closely.

<p><video src="figure/EMA.mp4" width="600" height="500" controls playsinline autoplay muted loop/></p>

<br>

**The moving average prices are less volatile (smoother), but they also lag behind the streaming prices - they are *biased*.**  
Notice for example that the average prices (red line above) reach their minimum much later than the streaming prices (blue line).  
And the smoother the average prices are, the more biased they are.  
A lower variance is desirable, but it comes at the cost of a larger bias (time lag).  
This is an example of the *bias-variance tradeoff*.  
Achieving the best *bias-variance tradeoff* is one of the major objectives of *machine learning*.  


**The decay parameter $\lambda$ is usually chosen to achieve the best *bias-variance tradeoff*.**  

The best tradeoff depends on the application.  For example, if the streaming prices are very volatile, then a larger decay parameter $\lambda$ is desirable, to reduce the volatility.

**The optimal value of the decay parameter $\lambda$ can be determined using a simulation called *backtesting* (cross-validation).**  

Backtesting is performed on historical data.  
The data is split into in-sample (past training set) and out-of-sample (future test set) data sets.  
Forecasting models often require the smoothing of the data.  
The smoothing is applied to the training set data, and the forecasts are tested on the out-of-sample data.  
The value of $\lambda$ which minimizes the out-of-sample forecast errors is chosen as the optimal value.  

**The backtesting procedure risks *overfitting* to the historical data.**  
The optimal value of $\lambda$ may not be the best choice in live data.
So users should be cautious when selecting parameters using the backtesting procedure.  

