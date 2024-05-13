library("feasts")
library("fable")
library("fabletools")
library("tsibble")
library("readr")
library("magrittr")
library("tidyverse")
library("forecast")

data <- read.csv('CNAGDP.csv')
data<-data%>%
  
  
  CNAGDP1 <- CNAGDP%>%
  mutate(Date = as.Date(paste0(DATE, "-01-01"))) %>%
  as_tsibble(index = Date, key = "MKTGDPCNA646NWDB")

p <- ggplot(CNAGDP1, aes(x = Date, y = MKTGDPCNA646NWDB)) +
  geom_line()

print(p)

cnagdpacf <- acf(CNAGDP1$MKTGDPCNA646NWDB, plot = FALSE)

ggplot(cnagdpacf, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Lag", y = "Autocorrelation", title = "Autocorrelation Function (ACF) Plot") +
  theme_minimal()

plot(cnagdpacf)

if (all(abs(cnagdpacf$acf[-1]) < 0.2)) {
  cat("The ACF suggests weak stationarity.\n")
} else {
  cat("The ACF suggests non-stationarity.\n")
}

ets_model <- ets(CNAGDP1$MKTGDPCNA646NWDB)
forecast_value <- forecast(ets_model, h = 1)
print(forecast_value)
plot(forecast_value)

CNAarima_model <- auto.arima(CNAGDP1$MKTGDPCNA646NWDB)
forecast_value <- forecast(CNAarima_model, h = 1)
print(forecast_value)
plot(forecast_value)
