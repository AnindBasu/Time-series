TOPIC #1
FORECASTING FINANCIAL INSTRUMENTS PRICES WITH VECM AND ARIMA MODELS
 The aim of the project is to compare accuracy of forecasts of prices of two cointegrated financial instruments with VECM model and two independent univariate ARIMA models.
In order to accomplish the task you should:
• Download the provided TSA_2024_project_data_1.csv file with time series including prices of ten financial instruments.
• Find one cointegrated pair out of ten provided time series. There is more than one cointegrated pair but you are supposed to find just one of them. If you found more than one pair, you can choose any of them for further analysis.
• Build a bivariate VECM model for the identified pair and produce forecasts of prices of two instruments for the out-of-sample period.
• Find separately for two instruments the most attractive univariate ARIMA models and produce forecasts for the same out-of-sample period.
• Compare accuracy of forecasts of the prices using the ex-post forecast error measures.
• Prepare a short report on it.
About the sample:
Please consider the in-sample period of 280 observations and out-of-sample period of 20 observations. Detailed grading rules:
The maximum number of points you can get for the project is 40. Detailed grading rules are as follows:
• For checking the cointegration between the chosen time series – 6 pts.
• For estimating VECM, interpreting the estimates along with commenting on model’s diagnostics and calculating forecasts – 12 pts.
• For applying the Box-Jenkins procedure in aim of choosing best possible ARIMA models’ specifications along with calculating forecasts for both series separately – 12 pts.
• For comparing VECM model’s forecasts with ARIMAs alternatives – 5 pts.
• For the structure, order, transparency and clarity of the provided report as well as for using a form of RMarkdown file – 5 pts.
Specific description of the assessment: To avoid any possible misunderstanding:
• By “checking the cointegration between the chosen time series” we mean exactly the procedure for testing the cointegration presented on lecture #7 and labs #8.
• By “estimating VECM, interpreting the estimates along with commenting on model’s diagnostics and calculating forecasts” we mean:
• performing the Johansen cointegration test and concluding about the number of cointegrating vectors,
    
• estimating VECM,
• interpreting all the parameters of the VECM model,
• commenting if the Error Correction Mechanism work,
• commenting about the signs of the estimated parameters,
• reparametrizing VECM model into VAR model,
• calculating and interpreting Impulse Response Function for the reparametrized model,
• calculating and interpreting Forecast Error Variance Decomposition for the reparametrized model,
• checking serial autocorrelation and normality of the residuals obtained from the reparametrized model,
• calculating and plotting a forecast using VECM model,
• calculating ex-post forecast errors for both series on the out-of-sample period.
• By “applying the Box-Jenkins procedure in aim of choosing best possible ARIMA models’
specifications along with calculating forecasts for both series separately” we mean:
• preparing ARIMA model with an application of the Box-Jenkins procedure i.e. going through the following steps:
• identification – plotting and interpreting ACF, PACF for the series,
• estimation – estimating the model and interpreting the results,
• diagnostics – plotting and interpreting ACF, PACF for residuals as well as performing and interpreting the Ljung-Box test,
• forecasting – calculating ex-post forecast errors on the out-of-sample period.
• estimating many ARIMA models when applying the Box-Jenkins procedure and comparing
them with Information Criteria.
Note: when it comes to the above, please do not use only auto.arima() function! This course is not about running some R functions without any deeper consideration. You need to go carefully through the Box- Jenkins procedure.
• By “comparing VECM model’s forecasts with ARIMAs alternatives” we mean calculating forecast ex-post error measures considering the out-of-sample period and comparing them.
• I guess that “the structure, order, transparency and clarity of the provided report as well as for using a form of RMarkdown file” should be clear.
