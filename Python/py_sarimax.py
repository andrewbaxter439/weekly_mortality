import statsmodels.api as sm
import pandas as pd
  
def py_sarimax(df, exog, ar, ma, sar, sma):


  start = df['Date'].min()
  
  index = pd.date_range(start, periods = df.shape[0], freq = "W")
  
  df.index = index
  
  X1 = sm.add_constant(df[exog])
  
  mod = sm.tsa.statespace.SARIMAX(df['adj_rate'], exog = X1, order = (ar, 0, ma), seasonal_order = (sar, 0, sma, 52))
  
  
  initial_fit = mod.fit()
  print(initial_fit.summary())
  # model_fit = mod.fit(initial_fit.params, method='nm', maxiter=1000)
  # print(model_fit.summary())
  # b = model_fit.plot_diagnostics(lags=36, figsize=(15, 12))

  return(initial_fit)
