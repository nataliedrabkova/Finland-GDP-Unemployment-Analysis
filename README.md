# Finland-GDP-Unemployment-Analysis

#   Project Overview
This project examines the relationship between Finland's annual GDP (1980-2023) and quarterly unemployment rates (2009-2023). Using various time series models, we explore structural breaks, forecast future trends, and analyze the causality between GDP and unemployment.

 #   Key Objectives:
* Identify structural breaks in Finland's GDP due to economic crises.
* Model and forecast GDP using ARIMA and other time-series models.
* Examine the relationship between GDP growth and unemployment using VAR models.
* Provide predictions for GDP and unemployment rates for the upcoming years.
#   Dataset
The project utilizes data from Finland on:
* Annual GDP (1980-2023) in millions of Euros.
* Quarterly Unemployment Rate (2009-2023).

#   Analysis Summary
#    GDP Analysis:
Finland's GDP shows an overall upward trend with notable declines during the financial crisis of 1991, 2008, and the COVID-19 pandemic in 2020.
The ARIMA(0,1,1) model was selected to forecast future GDP growth, predicting a steady increase in GDP through 2028.
#    Unemployment and GDP Relationship:
A Vector Autoregression (VAR) model was used to analyze the relationship between GDP and unemployment.
Granger Causality Test reveals that GDP influences unemployment, but not vice versa.
Forecasts indicate that unemployment will gradually increase in the next few quarters.

#   Forecasting:
Using the ARIMA model, the GDP is expected to reach €307 billion by 2028, while unemployment will experience slight increases over the next few years.
#   Files in the Repository
* projekt_drabkova.pdf: Contains the full analysis in Czech, including time-series graphs, model selection, and forecasting results.
* project_part1.R: The R script used for the time series analysis and forecasting.
* project_part2.R: The R script used for the time series analysis and forecasting.
#   Methods Used
* ARIMA for forecasting GDP.
* Vector Autoregression (VAR) for examining the dynamic relationship between GDP and unemployment.
* Granger Causality test to determine the direction of influence.
* Impulse-Response analysis to explore the effect of shocks in GDP and unemployment.
