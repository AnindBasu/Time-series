#common function to prepare time series stationarity test results set data
stationary_test_data <- function(results_adf, title, sample_duration, adf_result, pp_result, bg_result)	{
  rbind(results_adf, data.frame(Data=title, Training_Sample=sample_duration,
                                ADF_Test=paste(sprintf("%.3f", unname(adf_result$statistic)),"(",sprintf("%.3f", adf_result$p.value),")"),
                                PP_Test=paste(sprintf("%.3f", unname(pp_result$statistic)),"(",sprintf("%.3f", pp_result$p.value),")"),
                                BG_Test=paste(sprintf("%.3f", unname(bg_result$statistic)),"(",sprintf("%.3f", bg_result$p.value),")")))
  
}


plot_acf_pacf <- function(cnrw) {
  #par(mar=c(1,1,1,1))
  #par(mfrow = c(2, 1)) # c(rows, columns)
  acf(cnrw, 
      lwd = 5, 
      col = "dark green", 
      main = "ACF for cnrw")
  pacf(cnrw, 
       lwd = 5, 
       col = "dark green", 
       main = "PACF for cnrw", 
       ylim = c(-0.1, 0.1) )
#par(mfrow = c(1, 1)) # c(rows, columns)
  
}
#RMSE = accuracy(model)[2]
#MAPE= accuracy(model)[5]
#MAE = accuracy(model)[6]

#common function to prepare performance matrix result set data
performance_data <- function(forecast_perf, title, sample_duration, model_var)	{
  rbind(forecast_perf, data.frame(Forecast_Model=title, Training_Sample=sample_duration,
                                RMSE=sprintf("%.3f", model_var[2]),
                                MAPE=sprintf("%.3f", model_var[5]),
                                MAE=sprintf("%.3f", model_var[3])))
}

#common function to prepare comparison of models data
performance_compared_data <- function(perf_compared, title, model_var)	{
  rbind(perf_compared, data.frame(Models_Compared=title,
                                  DM_Statistics=sprintf("%.3f", unname(model_var$statistic)),
                                  p_Value=format(unname(model_var$p.value), scientific = TRUE)))
}

model_summary <- function(model_details, title, model_var)	{
  pvalue <- Box.test(resid(model_var),type = "Ljung-Box", lag = 10)$p.value
  rbind(model_details, data.frame(Model_Order=title,
                                  Box_Ljung_P_Value=sprintf("%.10f", pvalue),
                                  AIC_Value=sprintf("%.3f", AIC(model_var)),
                                  BIC_Value=sprintf("%.3f", BIC(model_var))))
  
}

calculate_errors <- function(error_matrix, df_actual, df_forecast, index_name) {
  # Create a new row with the required structure
  new_row <- data.frame(
    MAE=mean(abs(df_actual - df_forecast), na.rm = TRUE),
    MSE=mean((df_actual - df_forecast)^2, na.rm = TRUE),
    MAPE=mean(abs((df_actual - df_forecast)/df_actual), na.rm = TRUE),
    AMAPE=mean(abs((df_actual - df_forecast) / (df_actual + df_forecast)), na.rm = TRUE))
  # Set the row name for the new row
  rownames(new_row) <- index_name
    
  rbind(error_matrix,new_row)
}