---
title: "Implementation of the Moving Average Prices And Volatilities"
author: "Jerzy Pawlowski (jpawlowski@machinetrader.io)"
date: "06/17/2024"
output:
  html_document: default
  pdf_document: default
email: jpawlowski@machinetrader.io
affiliation: MachineTrader.io
---

### Implementation of the Moving Average Prices and Volatilities  

The Node-RED tab named *Tech Indicators* contains the implementations of various technical indicators in MachineTrader, including the moving average prices and volatilities.  

The tab named *Tech Indicators* calculates the moving average prices and volatilities from live streaming stock prices for a single selected stock.

You can download the tab named *Tech Indicators* from the MachineTrader-Community repository on GitHub by clicking on this [**link**](https://drive.google.com/uc?export=download&id=1gOpD8O4JOah5xTCVcmQi4qNdmSVliTgr).  

After you download the *JavaScript* file named *Tech_Indicators.json* to your computer, you should then install it in your MachineTrader instance, to create the tab named *Tech Indicators*.  

The tab *Tech Indicators* receives the streaming stock prices from Alpaca, from the tab named *Alpaca Prices*, which is already included in your MachineTrader instance.  
The tab *Tech Indicators* also relies on the global variables produced by the tab named *Globals/Utilities*.  

After you install the tab *Tech Indicators*, you must connect the *link-in node* to the left of the function node named *Get stock price*, to the *link-out node* called *Prices out* in the tab *Alpaca Prices*.  

You can read more about the moving average prices and volatilities in
[**this document**](https://algoquant.github.io/2024/06/17/Moving-Averages-Theory/).  

You can also watch an instructional video about [**Calculating Moving Average Prices And Volatilities**](https://youtu.be/iAZMTEZeZDM) on the MachineTrader YouTube channel.  

<br>

**Let's take a look at the tab named *Tech Indicators*.**  

The function node named *Initialize the parameters* creates flow variables needed for the calculations.
It should be run once before the other nodes are run, by pressing the *inject* node on the left.  

The multiple stock prices stream in from the tab *Alpaca Prices* into the node named *Get stock price*, through the small *link-in node* to its left.
The function node *Get stock price* selects the prices only for the ticker in the tab variable *symboln*.
The single stock prices then stream out through the small *link-out node* to its right, to the nodes which calculate the moving averages.  

The dashed lines are the connectors for passing the live streaming stock prices to the function nodes.  
![alt text](figure/mt_moving_average_price_volatility.png "Moving Average Price Volatility
"){width="500" height="800"}


<br>

**The function node named *Initialize the parameters* creates the flow variables needed for the calculations:**  
![alt text](figure/initialize_parameters.png "Title"){width="400" height="80"}

It should be run once before the other nodes are run, by pressing the *inject* node on the left.
The *inject* node is on a timer, to run on weekdays at 9:30 AM, so that all the flow variables are initialized before the start of stock trading.

The function node named *Initialize the parameters* creates the following flow variables:  

* symboln = the ticker string of the selected stock. 
* lambdaf = the lambda decay factor which multiplies past estimates. 
* volf = the volatility floor. 
* pricec = the current stock price. 
* pricep = the previous stock price, initially set to NaN. 
* pricema =  the moving average price, initially set to NaN. 
* lb = the look-back window, i.e. the number of elements in the data queue. 
* priceq = the data queue (buffer) for storing the recent stock prices. 
* endpq = the position of the end of the price queue. 
* retq = the data queue (buffer) for storing the recent stock returns. 
* endrq = the position of the end of the returns queue. 


<br>

**The data queue is used for calculating the simple moving averages of prices and volatilities.**

The data queue is an array (vector) for storing the recent stock prices or returns.
The data queue only stores the most recent number of elements equal to *lb*.  
The data queue is a first in last out queue.
Data is added one element at a time and it's stored in the order it's received.
New data is written to the end of the queue, and the *endq* variable is shifted to the next to last element.  
The element *P14* at the end of the queue is replaced with new data and becomes the element *P1* at the front of the queue.  
The element *P13* becomes the end of the queue *P14*.
The data elements in the queue are just relabeled - not copied.
This avoids copying the other data elements in the queue, which speeds up the code.

Below is an illustration of how an element is added to the data queue:

![alt text](figure/data_queue.png "Title"){width="500" height="200"}


<br>

**The function node named *Get stock price* obtains the streaming prices for the ticker *symboln* from the tab named *Alpaca Prices*.**

![alt text](figure/get_price.png "Title"){width="400" height="80"}

The prices for all the stocks stream from the tab *Alpaca Prices* into the small *link-in node* on the left.  
The function node *Get stock price* then extracts the prices only for the ticker *symboln*, and it passes them to the small *link-out node* on the right.  The streaming stock prices from the *link-out node* can then be passed to the other nodes and strategy tabs.  

For the live prices to stream, it's necessary to download and install the tabs named *Alpaca Prices* and *Globals*.  Then you must connect the *link-in node* to the left of the function node named *Get stock price* to the *link-out node* called *Prices out* in the tab *Alpaca Prices*.  


<br>

**The function node named *Calculate the SMA prices* calculates the simple moving average prices.**

![alt text](figure/SMA_prices.png "Title"){width="400" height="80"}

The stock prices flow from the node *Get stock price* into the small *link-in node* on the left.  The function node *Calculate the SMA prices* then calculates the simple moving average prices, and it passes them to the small *link-out node* on the right.  The streaming SMA prices from the *link-out node* can then be passed to the other nodes and strategy tabs.  


<br>

**The function node named *Calculate the EMA prices* calculates the exponential moving average prices.**

![alt text](figure/EMA_prices.png "Title"){width="400" height="80"}

The stock prices flow from the node *Get stock price* into the small *link-in node* on the left.  The function node *Calculate the EMA prices* then calculates the exponential moving average prices, and it passes them to the small *link-out node* on the right.  The streaming EMA prices from the *link-out node* can then be passed to the other nodes and strategy tabs.  


<br>

**The function node named *Calculate the SMA return volatility* calculates the simple moving average volatility of the stock returns.**

![alt text](figure/SMA_return_volatility.png "Title"){width="400" height="80"}

The stock prices flow from the node *Get stock price* into the small *link-in node* on the left.  The function node *Calculate the SMA return volatility* then calculates the SMA return volatility, and it passes it to the small *link-out node* on the right.  The streaming SMA return volatility from the *link-out node* can then be passed to the other nodes and strategy tabs.  


<br>

**The function node named *Calculate the EMA return volatility* calculates the exponential moving average volatility of the stock returns.**

![alt text](figure/EMA_return_volatility.png "Title"){width="400" height="80"}

The stock prices flow from the node *Get stock price* into the small *link-in node* on the left.  The function node *Calculate the EMA return volatility* then calculates the EMA return volatility, and it passes it to the small *link-out node* on the right.  The streaming EMA return volatility from the *link-out node* can then be passed to the other nodes and strategy tabs.  

<br>

**The function node named *Calculate the SMA price volatility* calculates the simple moving average volatility of the stock prices.**

![alt text](figure/SMA_price_volatility.png "Title"){width="400" height="80"}

The stock prices flow from the node *Get stock price* into the small *link-in node* on the left.  The function node *Calculate the SMA price volatility* then calculates the SMA price volatility, and it passes it to the small *link-out node* on the right.  The streaming SMA price volatility from the *link-out node* can then be passed to the other nodes and strategy tabs.  

<br>

**The function node named *Calculate the EMA price volatility* calculates the exponential moving average volatility of the stock prices.**

![alt text](figure/EMA_price_volatility.png "Title"){width="400" height="80"}

The stock prices flow from the node *Get stock price* into the small *link-in node* on the left.  The function node *Calculate the EMA price volatility* then calculates the EMA price volatility, and it passes it to the small *link-out node* on the right.  The streaming EMA price volatility from the *link-out node* can then be passed to the other nodes and strategy tabs.  

<br>
