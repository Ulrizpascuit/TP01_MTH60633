## ---- f_var_roll_wndw ----
f_var_roll_wndw <- function(logRets, window = 1000, h = 1000, level = 0.95) {
  
  n_indices <- ncol(logRets)
  
  VaR_roll <- matrix(NA_real_, nrow = h, ncol = n_indices)
  
  for (j in 1:n_indices) {
    for (k in 1:h) {
      y_window <- logRets[k:(window + k - 1), j]
      out <- f_forecast_var(y_window, level)
      VaR_roll[k, j] <- as.numeric(tail(out$VaR_Forecast, 1))
    }
  }
  
  colnames(VaR_roll) <- colnames(logRets)
  
  dates_var <- index(logRets)[(window + 1):(window + h)]
  
  VaR_roll_xts <- xts(VaR_roll, order.by = dates_var)
  return(VaR_roll_xts)
}