# S&P500-sector-returns-forecast-using-macroeconomics
### Forecasting sector returns using macroeconomic variables only

Originally a class group project. 
Objective of the project was to compare different modeling approaches - multiple regression, ARIMA and PCA.

This section of the project deals with feature engineering using stepwise regressions. We asked ourselves whether we can use macroeconomic indicators to forecast S&P500 sector returns.

We looked at monthly returns of 10 sectors of the S&P500 separately - Healthcare, Financial, Energy, IT, Consumer discretionary, Comsumer staples, Industrials, Utilities, Telecom and Materials.

![Forecasts vs actuals](https://github.com/Blackkadder/SP500-sector-returns-forecast-using-macroeconomics/blob/master/returns%20actual%20vs%20forecasts.png)

Conclusion: Macroeconomic variables actually do a very good job of tracking and predicting long term trends in the various sector returns - some better than others. But short term price movements are effected mostly by factors other than macroeconomic variables; such as investor sentiment, expectations, earnings releases, information not captured by economic indicators and, of course, randomness. 

As I mentioned above, this is only a single part of a larger analysis. I'll add the PCA models to the repository at a later date. (You'll never guess which models were more accurate!) 
