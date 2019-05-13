---
title: Real-time Trading With Packages iBrokers2, Rcpp, and Interactive Brokers
author: Jerzy Pawlowski, NYU Tandon School of Engineering
date: "April 4, 2019"
output: pdf_document
---

### Abstract

We demonstrate how to perform fully automated, algorithmic trading in real-time with packages *iBrokers2*, *Rcpp*, and *Interactive Brokers*.  Real-time trading means trading in a programmatic loop, in which continuous streaming market data is used to update a trading model, and the model outputs are used to place orders via the API.  Real-time trading requires three components: acquisition of streaming market data and account data, trading model execution, and trade order management.  
The package *IBrokers2* contains *R* functions for executing real-time trading via the API of Interactive Brokers.  *IBrokers2* is derived from package *IBrokers*, and is fully backward compatible with it.  This means that all the *IBrokers* functions and variables are preserved exactly in *IBrokers2*, while some additional functions have been added.  The additional functions in *IBrokers2* provide functionality for the three components required for real-time trading.  They provide a *trade wrapper* environment for live data, and a programmatic *callback* loop for executing the trading model and for placing orders via the API.
We provide examples of simple trading strategies and explain how they can be modified by users.
