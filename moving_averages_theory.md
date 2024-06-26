---
title: "The Moving Average Prices And Volatilities"
author: "Jerzy Pawlowski (jpawlowski@machinetrader.io)"
date: "06/17/2024"
output:
  html_document: default
  pdf_document: default
email: jpawlowski@machinetrader.io
affiliation: MachineTrader.io
---

### The Moving Average Prices  

The moving average prices can be calculated for a streaming time series of prices $p_i$.  

The Simple Moving Average (SMA) price $p^{SMA}_i$ is the average of the past prices $p_{i-j}$ over a look-back window of length $L$:

$${\normalsize p^{SMA}_i = \frac{1}{L} \sum\limits_{j=0}^{L-1} p_{i-j}}$$
The parameter $L$ is equal to the length of the look-back window.  

**The drawback of the SMA is that it applies the same weight to the recent and past prices.**
Its calculation also requires maintaining a buffer (queue) of past prices, with the length equal to the parameter $L$.

<br>

The Exponential Moving Average (EMA) price $p^{EMA}_i$ is calculated using a decay factor $\lambda$ and the recursive formula: 

$${\normalsize p^{EMA}_i = \lambda p^{EMA}_{i-1} + (1-\lambda) p_i}$$


The above recursive formula can be expressed as a series:

$${\normalsize p^{EMA}_i = (1-\lambda) \sum\limits_{j=0}^{\infty} {\lambda}^j p_{i-j} = (1-\lambda) (p_i + \lambda p_{i-1} + \lambda^2 p_{i-2} + \ldots)} $$
The formula is called *exponential* because the decay factor $\lambda$ is raised to the power of $j$, so the weights $\lambda^j$ decrease exponentially as $j$ increases.  


**The advantage of the EMA is that it applies a greater weight to more recent prices than to past ones.**  

And it doesn't forget the past prices completely - it only applies a smaller weight to them.  
The EMA calculation is also faster because there's less computation, and it doesn't require maintaining a buffer (queue) of past prices.  

**The SMA and EMA averages are not the same, but they are related to each other, and they move in sympathy with each other.**  


**The decay factor $\lambda$ determines the persistence of data memory, i.e. how quickly the contribution of the past data to the average value fades over time.**  

If the decay factor $\lambda$ is closer to $1$ (one) then the effect of past data fades slowly over time.
If $\lambda$ is closer to $0$ (zero) then the effect of past data fades quickly over time.  


**The length of the look-back window also determines the persistence of data memory.**  

In the Simple Moving Average (SMA), the data which is more than $L$ time periods in the past is abruptly forgotten.
If the window is small, then only very recent data is used in the calculation of the average.
If the window is large, then older data is used in the calculation of the average.  


**The decay factor $\lambda$ and the look-back window length $L$ are related to each other.**  

If the decay factor $\lambda$ is closer to $1$ (one) then the EMA adjusts slowly to new prices, corresponding to a larger look-back window length $L$.
If $\lambda$ is closer to $0$ (zero) then the EMA adjusts quickly to new prices, corresponding to a smaller look-back window length $L$.  


<br>

### Choosing The Decay Factor $\lambda$ And The Look-back window length $L$ 

The moving average prices are smoothed versions of the streaming prices.  

**The decay factor $\lambda$ and the look-back window length $L$ determine the strength of the data smoothing.**  

If $\lambda$ is closer to $1$ (one) then the moving average prices are smoother, but if it's closer to $0$ (zero) then the average prices are more variable and they follow the streaming prices more closely.

If the look-back window length $L$ is large, then more prices are used in the calculation of the average and it's therefore smoother.
If the look-back window length $L$ is small, then fewer prices are used and the average follows the streaming prices more closely.

The effect of the decay factor $\lambda$ on the moving average prices is illustrated in the animated plot below of VTI stock prices.  When $\lambda = 0.99$ the average prices are very smooth, and they lag behind the streaming prices.
But as $\lambda$ decreases, the average prices become more variable and they follow the streaming prices more closely.  

<p><video src="figure/EMA.mp4" width="600" height="500" controls playsinline autoplay muted loop/></p>

<br>

**The moving average prices are less volatile (smoother), but they also lag behind the streaming prices - they are *biased*.**  
Notice for example that the average prices (red line above) reach their minimum much later than the streaming prices (blue line).  
And the smoother the average prices are, the more biased they are.  
A lower variance is desirable, but it comes at the cost of a larger bias (time lag).  
This is an example of the *bias-variance tradeoff*.  
Achieving the best *bias-variance tradeoff* is one of the major objectives of *machine learning*.  

**The decay factor $\lambda$ and the look-back window length $L$ are usually chosen to achieve the best *bias-variance tradeoff*.**  

The best tradeoff depends on the application.  For example, if the streaming prices are very volatile, then a larger decay factor $\lambda$ is desirable (or a larger look-back window length $L$), to reduce the volatility.

**The optimal value of the decay factor $\lambda$ can be determined using a simulation called *backtesting* (cross-validation).**  

Backtesting is performed on historical data.  
The data is split into in-sample and out-of-sample sets.  
Forecasting models often require the smoothing of the data.  
The smoothing is applied to the in-sample data, and the forecasts are tested on the out-of-sample data.  
The value of $\lambda$ which minimizes the out-of-sample forecast errors is chosen as the optimal value.  

**The backtesting procedure risks *overfitting* to the historical data.**  
The optimal value of $\lambda$ may not be the best choice in live data.
So users should be cautious when selecting parameters using the backtesting procedure.  


<br>

### The Moving Average Variance of Returns  

The moving average variance of returns $\sigma^2_i$ measures the dispersion of the streaming returns around the moving average return.
It can be calculated for a streaming time series of prices $p_i$.  

The dollar return $r_i$ is the difference between the current price $p_i$ minus the previous price $p_{i-1}$:
$${\normalsize r_i = p_i - p_{i-1}}$$

The Simple Moving Average (SMA) return $r^{SMA}_i$ is equal to the average of the past returns:

$${\normalsize r^{SMA}_i = \frac{1}{L} \sum\limits_{j=0}^{L-1} r_{i-j}}$$


The Simple Moving Average (SMA) variance $\sigma^{2 SMA}_i$ is then given by the sum:

$${\normalsize \sigma^{2 SMA}_i = \frac{1}{L-1} \sum\limits_{j=0}^{L-1} (r_{i-j} - r^{SMA}_{i-1})^2}$$  


Note that the average return $r^{SMA}_{i-1}$ is from the previous time step, to better capture the unexpected return in the current time step.


<br>

The Exponential Moving Average (EMA) variance $\sigma^{2 EMA}_i$ is calculated using the two recursive formulas: 

$${\normalsize \sigma^{2 EMA}_i = \lambda \sigma^{2 EMA}_{i-1} + (1-\lambda) (r_i - r^{EMA}_{i-1})^2}$$
$${\normalsize r^{EMA}_i = \lambda r^{EMA}_{i-1} + (1-\lambda) r_i}$$


**The SMA and EMA variances are not the same, but they are related to each other, and they move in sympathy with each other.**  

If the decay factor $\lambda$ is closer to $1$ (one) then the EMA variance adjusts slowly to new prices, corresponding to a larger look-back window length $L$.
If $\lambda$ is closer to $0$ (zero) then the EMA variance adjusts quickly to new prices, corresponding to a smaller look-back window length $L$.  


<br>


### The Moving Average Variance of Prices  

The moving average variance of prices $\sigma^2_i$ measures the dispersion of the streaming prices around the moving average price.
It can be calculated for a streaming time series of prices $p_i$.  

The Simple Moving Average (SMA) price $p^{SMA}_i$ is equal to the average of the past prices:

$${\normalsize p^{SMA}_i = \frac{1}{L} \sum\limits_{j=0}^{L-1} p_{i-j}}$$


The Simple Moving Average (SMA) variance $\sigma^{2 SMA}_i$ is then given by the sum:

$${\normalsize \sigma^{2 SMA}_i = \frac{1}{L-1} \sum\limits_{j=0}^{L-1} (p_{i-j} - p^{SMA}_{i-1})^2}$$  


Note that the average price $p^{SMA}_{i-1}$ is from the previous time step, to better capture the unexpected price in the current time step.


<br>

The Exponential Moving Average (EMA) variance of prices $\sigma^{2 EMA}_i$ is calculated using the two recursive formulas: 

$${\normalsize \sigma^{2 EMA}_i = \lambda \sigma^{2 EMA}_{i-1} + (1-\lambda) (p_i - p^{EMA}_{i-1})^2}$$
$${\normalsize p^{EMA}_i = \lambda p^{EMA}_{i-1} + (1-\lambda) p_i}$$

**The variance of the returns and of the prices measure different forms of dispersion.**

Take for example the extreme case when the returns are constant $r_i = r$.  
Then the prices $p_i = \sum {r_i}$ increase in a straight line.  The variance of returns is zero, but the variance of prices is not zero.
So if the prices exhibit a significant trend, then the variance of prices is large, even if the variance of returns is small.

Another example is Brownian motion.  The returns are normally distributed, with a constant volatility.  But the variance of prices is not constant, and it increases with the factor $\lambda$, since then prices have more time to drift away from their past average value.

