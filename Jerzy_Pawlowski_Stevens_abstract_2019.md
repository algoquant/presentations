---
title: Real-time Trading via Interactive Brokers with Package iBrokers2
subtitle: Stevens Conference on High-Frequency Finance and Analytics
author: Jerzy Pawlowski, NYU Tandon School of Engineering
date: June 27, 2019
address: Stevens Institute of Technology
output: pdf_document
---

### Abstract

We introduce the *R* package *iBrokers2*, for implementing real-time trading via the API of *Interactive Brokers*.  The package *IBrokers2* provides the essential functionality needed for trading in real time.  
First, it contains functions for collecting live market data, and a *trade wrapper* environment for storing market data and portfolio state information.  
Second, it implements a programmatic *callback* loop for updating the trading model with live market data.  
Third, it contains functions for placing trade orders via the IB API.  
We provide on overview of the package *iBrokers2*.  We also provide examples of how to implement simple trading strategies, and explain how they can be modified by users.
