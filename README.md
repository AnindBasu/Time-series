**Presentation for the solution of the project can be found at:**  
[https://rpubs.com/vikrambahadur/1216716](https://rpubs.com/vikrambahadur/1216716)

# TOPIC #1  
## FORECASTING FINANCIAL INSTRUMENTS PRICES WITH VECM AND ARIMA MODELS

The aim of the project is to compare the accuracy of forecasts for prices of two cointegrated financial instruments using the VECM model and two independent univariate ARIMA models.  

### Task Breakdown:
- **Download the provided file:** `TSA_2024_project_data_1.csv` containing time series for the prices of ten financial instruments.
- **Identify a cointegrated pair** from the ten time series. You need to find just one cointegrated pair for further analysis.
- **Build a bivariate VECM model** for the identified pair and generate price forecasts for the out-of-sample period.
- **Find univariate ARIMA models** for both instruments and produce forecasts for the same out-of-sample period.
- **Compare the forecast accuracy** using ex-post forecast error measures.
- **Prepare a report** summarizing the results.

### Sample Details:
- **In-sample period:** 280 observations  
- **Out-of-sample period:** 20 observations  

### Grading Criteria (Max: 40 points):
- **Cointegration Check:** 6 pts  
- **VECM Estimation and Forecasting:** 12 pts  
- **ARIMA Model Selection and Forecasting:** 12 pts  
- **Forecast Comparison (VECM vs ARIMA):** 5 pts  
- **Report Structure and Clarity (RMarkdown):** 5 pts  

### Detailed Assessment:
#### 1. **Cointegration Check:**
   - Test cointegration as demonstrated in Lecture #7 and Labs #8.

#### 2. **VECM Estimation and Interpretation:**
   - Perform Johansen cointegration test and determine the number of cointegrating vectors.
   - Estimate the VECM model and interpret the parameters.
   - Analyze if the Error Correction Mechanism is functioning.
   - Comment on parameter signs.
   - Reparametrize the VECM into a VAR model.
   - Calculate and interpret:
     - Impulse Response Function.
     - Forecast Error Variance Decomposition.
   - Check serial autocorrelation and normality of residuals.
   - Calculate and plot forecasts using the VECM model.
   - Compute ex-post forecast errors for both series.

#### 3. **Box-Jenkins Procedure for ARIMA Models:**
   - **ARIMA Steps:**
     - Identification: Plot and interpret ACF, PACF for the series.
     - Estimation: Estimate ARIMA models and interpret results.
     - Diagnostics: Perform ACF, PACF for residuals, and the Ljung-Box test.
     - Forecasting: Calculate ex-post forecast errors on the out-of-sample period.
   - Compare multiple ARIMA models using Information Criteria (avoid only using `auto.arima()`).

#### 4. **Comparison of Forecasts (VECM vs ARIMA):**
   - Calculate ex-post forecast error measures and compare the performance of VECM with ARIMA models.

#### 5. **Report Structure:**
   - Ensure the report is well-structured, clear, and formatted using RMarkdown.

