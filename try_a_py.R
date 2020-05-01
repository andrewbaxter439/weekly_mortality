library(reticulate)
library(stringr)
library(tidyverse)
library(lubridate)

data <- readRDS("data/imported_data.rds")


# reticulate::use_python("C:\\Python")
# reticulate::use_virtualenv("C:\\Python\\.virtualenvs\\shiny_env")

sm <- import("statsmodels.api")


df <- data %>% 
  filter(Sex == "all") %>%
  mutate(t = 1:nrow(.),
    Date = dmy("01/01/2010") + weeks(t),
    covid = ifelse(Date>ymd('20200101'), 1, 0),
    covid_trend = t*covid,
    covid_trend = ifelse(covid_trend == 0, NA, covid_trend),
    covid_trend = ifelse(!is.na(covid_trend), (covid_trend - min(covid_trend, na.rm = TRUE)) + 1, 0)) %>% 
  filter(Date > ymd('20170101')) %>% 
  mutate(t = 1:nrow(.)) %>% 
  as.data.frame()

X1 <- sm$add_constant(df[c('t', 'covid', 'covid_trend')])


ar_lags <- c(1, 3, 53)
ar <- vector("integer", max(ar_lags)+1)
ar[ar_lags] <- 1


ma_lags <- c(0)
ma <- vector("integer", max(ma_lags)+1)
ma[ma_lags] <- 1

sar_lags <- c(0)
sar <- vector("integer", max(sar_lags)+1)
sar[sar_lags] <- 1


sma_lags <- c(1, 2)
sma <- vector("integer", max(sma_lags)+1)
sma[sma_lags] <- 1



mod1 <- sm$tsa$statespace$SARIMAX(df$adj_rate, exog = X1, order = list(ar, 0, ma), seasonal_order = list(sar, 0, sma, 52))


results <- mod1$fit()

results$summary()


source_python('Python/py_sarimax.py')
t1 <- Sys.time()
py_out <- py_sarimax(df, list('t', 'covid', 'covid_trend'), ar, ma, sar, sma)
t2 <- Sys.time()

t2-t1


predict <- py_out$predict()


# gls method --------------------------------------------------------------

library(nlme)

ar[ar_lags] <- NA

# mod <- lm(adj_rate ~ t + covid + covid_trend + tsModel::harmonic(Week, 3, 52), data = df)
mod <- gls(adj_rate ~ t + covid + covid_trend + tsModel::harmonic(Week, 3, 52),
           data = df,
           correlation = corARMA(value = ar,
                                   form = ~ t,
                                 p = 53,
                                 q = 1))
summary(mod)

model1b <- forecast::Arima(as.ts(df$adj_rate), order=c(53,0,0),
                 fixed = c(ar[1:53], NA, NA, NA), 
                 seasonal = list(order = c(0, 0, 2), period = 52),
                 include.constant ="True", 
                 SSinit=c("Rossignol2011"),
                 method = "ML")

summary(model1b)


df %>% 
  # mutate(predict = predict) %>% 
  mutate(predict = predict(mod)) %>%
  ggplot(aes(Date, predict)) +
  geom_line(col = "red") +
  geom_point(aes(y = adj_rate), col = "blue") +
  geom_vline(xintercept = ymd(20200101),
             linetype = "dotted",
             col = "#000000CC"
  )


# testing harmonics -------------------------------------------------------


mod_h1 <- gls(adj_rate ~ t + covid + covid_trend + tsModel::harmonic(Week, 1, 52), data = df)
mod_h2 <- gls(adj_rate ~ t + covid + covid_trend + tsModel::harmonic(Week, 2, 52), data = df)
mod_h3 <- gls(adj_rate ~ t + covid + covid_trend + tsModel::harmonic(Week, 3, 52), data = df)
mod_h4 <- gls(adj_rate ~ t + covid + covid_trend + tsModel::harmonic(Week, 4, 52), data = df)

summary(mod_h1)
summary(mod_h2)
summary(mod_h3)
summary(mod_h4)

anova(mod_h1, mod_h2)
anova(mod_h2, mod_h3)
anova(mod_h3, mod_h4)
