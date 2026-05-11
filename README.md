# Finland-GDP-Unemployment-Analysis
# Time Series Analysis of Finland's GDP & Unemployment (1980-2023)

##  Project Overview
This repository contains an end-to-end econometric analysis of Finland's Gross Domestic Product (GDP) and its relationship with the unemployment rate. The project applies both univariate and multivariate time series modeling to understand the impact of historical economic shocks (e.g., the 2008 Financial Crisis and the 2020 COVID-19 pandemic) and to forecast future macroeconomic trends.

#   Dataset
The project utilizes data from Finland on:
* Annual GDP (1980-2023) in millions of Euros. _Source:Eurostat_
* Quarterly Unemployment Rate (2009-2023). _Source:Eurostat_

  
 #   Key Objectives:
* Identify structural breaks in Finland's GDP due to economic crises.
* Model and forecast GDP using ARIMA and other time-series models.
* Examine the relationship between GDP growth and unemployment using VAR models.
* Provide predictions for GDP and unemployment rates for the upcoming years.

##  Methodology & Techniques
The analysis was performed in **R** using a rigorous statistical approach, split into two main analytical phases:

**1. Univariate Time Series (ARIMA):**
* **Exploratory Data Analysis:** Trend visualization and variance checking.
* **Stationarity Testing:** Augmented Dickey-Fuller (ADF), Phillips-Perron (PP), and KPSS tests.
* **Structural Break Detection:** Chow test and breakpoint optimization to identify major economic shifts.
* **Modeling:** Manual and automatic lag selection for the optimal ARIMA model.
* **Verificaiton:** Residual diagnostics of selected final ARIMA model.
* **Forecasting:** Forecasting GDP for next 5 years, using final ARIMA model.

**2. Multivariate Time Series (VAR):**
* **Cross-Correlation:** Identifying lead-lag relationships between GDP and Unemployment.
* **Vector Autoregression (VAR):** Model selection based on Information Criteria (AIC, SC, FPE) for examining the dynamic relationship between GDP and unemployment.
* **Granger Causality Testing:** Determining the predictive directionality between the variables.
* **Impulse-Response Function (IRF):** Tracing the effect of a one-time shock to one variable on the other.
* **Forecast Error Variance Decomposition (FEVD):** Analyzing the contribution of each variable's shock to the forecast error.
* **Diagnostics:** Residual checks including Portmanteau (autocorrelation), ARCH (heteroskedasticity), and Jarque-Bera (normality) tests.

## 📊 Key Findings
* **Historical Crises Detected:** Structural breaks were successfully identified around the 1991 recession, the 2008 financial crisis, and the 2020 COVID-19 pandemic.
* **Long-term Growth Forecast:** The best-fitting ARIMA(0,1,1) model predicts continued steady growth in Finland's annual GDP over the next 5 years.
* **Economic Dynamics (Granger Causality):** A unidirectional Granger causality was found where GDP dynamically leads the unemployment rate, but not vice versa.
* **Shock Responses (IRF):** A positive economic shock (an unexpected increase in GDP) leads to a statistically significant decrease in the unemployment rate. Conversely, a sudden shock in unemployment negatively impacts GDP.

## 📈 Visualizations
*(Note: I recommend exporting 2-3 key plots from R as .png files and linking them here to make the repo visually appealing!)*

![GDP Forecast](path/to/your/forecast_image.png)
*Figure 1: 5-Year Forecast of Finland's GDP using ARIMA(0,1,1)*

![Impulse Response](path/to/your/irf_image.png)
*Figure 2: Impulse-Response Analysis showing the effect of a GDP shock on Unemployment*

## 📂 Repository Structure
* `projekt_drabkova.pdf`: Contains the full analysis in Czech, including time-series graphs, model selection, and forecasting results.
* `project_part1.R`: The R script used for the Univariate Time Series (ARIMA) analysis
* `project_part2.R`: The R script used for the Multivariate Time Series (VAR) analysis.
* `finland.xlsx`: The dataset used for the analysis.

For a deep dive into the statistical tests and exact methodology, see the full academic report (in Czech) [projekt_drabkova.pdf]
