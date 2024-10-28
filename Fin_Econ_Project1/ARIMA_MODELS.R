library(forecast)
library(readxl)
library(tseries)
library(zoo)
library(writexl)
library(tools)
library(stringr)

# --------------------------------------------------------------------------------------------- #

file_path <- "C:\\Users\\verei\\Documents\\Fin_Econ_Project1\\Forecast_Datasets\\MOEX_21.xlsx"
LEN <- 21

# --------------------------------------------------------------------------------------------- #

file_name <- str_remove(basename(file_path), "\\.xlsx$")

data <- read_excel(file_path, sheet = 1)

Date <- seq(1, LEN)

FORECAST <- data.frame(Date = Date, stringsAsFactors = FALSE)

for (i in 2:ncol(data)) {
  ts_data <- ts(data[,i])
  
  adf_test <- adf.test(ts_data)
  
  if (adf_test$p.value > 0.05) {
    ts_data <- diff(ts_data)
  }
  
  # Определение порядка модели ARIMA
  p <- auto.arima(ts_data)$arma[1]  # Параметр p
  d <- ifelse(adf_test$p.value > 0.05, 1, 0)  # Параметр d
  q <- auto.arima(ts_data)$arma[2]  # Параметр q
  
  # Построение модели ARIMA
  model <- arima(data[, i], order = c(p, d, q))
  
  forecasted_values <- forecast(model, h = LEN)
  
  temp_df <- data.frame(Date = Date, Price = as.numeric(forecasted_values$mean))
  names(temp_df)[2] <- colnames(data)[i]  # Переименовываем колонку с ценой
  
  FORECAST <- merge(FORECAST, temp_df, by = "Date", all = TRUE)
}

name <- paste0("FORECAST_ARIMA_", file_name, ".xlsx")

write_xlsx(FORECAST, name)


















data <- read_excel("C:\\Users\\verei\\Documents\\Fin_Econ_Project1\\closing_prices_MOEX_today_2022_2024.xlsx", sheet = 1)

ARIMA_models <- list()
ARIMA_plots <- list()
ARIMA_orders <- list(list())

# ТРЕБУЕТСЯ УЧИТЫВАТЬ СТАЦИОНАРНОСТЬ!

for (i in 2:ncol(data)) {
  ts_data <- ts(data[,i])
  
  adf_test <- adf.test(ts_data)
  
  if (adf_test$p.value > 0.05) {
    ts_data <- diff(ts_data)
  }
  
  # Определение порядка модели ARIMA
  p <- auto.arima(ts_data)$arma[1]  # Параметр p
  d <- ifelse(adf_test$p.value > 0.05, 1, 0)  # Параметр d
  q <- auto.arima(ts_data)$arma[2]  # Параметр q
  
  # Построение модели ARIMA
  model <- arima(data[, i], order = c(p, d, q))
  ARIMA_orders[[colnames(data)[i]]] <- c(p, d, q)
  
  # Сохранение модели в список
  ARIMA_models[[colnames(data)[i]]] <- model
  
  # Прогнозирование на следующие 5 периодов
  forecasted_values <- forecast(model, h=50)
  
  forecast_df <- data.frame(
    Date = time(forecasted_values$mean),
    Forecast = as.numeric(forecasted_values$mean),
    Lower_80 = as.numeric(forecasted_values$lower[, 1]),
    Upper_80 = as.numeric(forecasted_values$upper[, 1]),
    Lower_95 = as.numeric(forecasted_values$lower[, 2]),
    Upper_95 = as.numeric(forecasted_values$upper[, 2])
  )
  
  # Создание графика прогноза
  plot <- autoplot(forecasted_values) +
   ggtitle(paste("Прогноз для ", colnames(data)[i])) +
    geom_line(data = forecast_df, aes(x = Date, y = Forecast), color = "red") +
    geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower_80, ymax = Upper_80), alpha = 0.1, fill = "cyan") +
    geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower_95, ymax = Upper_95), alpha = 0.05, fill = "cyan") +
    xlab("Дата") +
    ylab("Цена акции") +
    theme_minimal()
  
  # Сохранение графика в список
  ARIMA_plots[[colnames(data)[i]]] <- plot
}

# Выводим результаты
for (name in names(ARIMA_models)) {
  cat("\n Модель для", name, "\n")
  print(summary(ARIMA_models[[name]]))
}

for (name in names(ARIMA_plots)) {
  print(ARIMA_plots[[name]])
}

plot(ARIMA_plots[["GAZP"]])

ts_data = ts(data$SBER[1:560], frequency = 180)


m <- auto.arima(ts_data, test = "adf", seasonal.test = "seas")
plot(forecast(m, h = 20))

plot(ts_data)
fit <- stl(ts_data, s.window=50)
lines(trendcycle(fit),col="red")

##########################################


graphic <- function(data) {
  ts_data <- ts(data$close) 

  # Обучение модели ARIMA
  arima_model <- auto.arima(ts_data)

  # Прогнозирование на 30 дней вперед
  forecasted_values <- forecast(arima_model, h = 30)
  
  # Преобразование прогнозов в датафрейм для ggplot
  forecast_df <- data.frame(
    Date = time(forecasted_values$mean),
    Forecast = as.numeric(forecasted_values$mean),
    Lower_80 = as.numeric(forecasted_values$lower[, 1]),
    Upper_80 = as.numeric(forecasted_values$upper[, 1]),
    Lower_95 = as.numeric(forecasted_values$lower[, 2]),
    Upper_95 = as.numeric(forecasted_values$upper[, 2])
  )

  # Преобразование исторических данных в датафрейм
  historical_df <- data.frame(
    Date = time(ts_data),
    Price = as.numeric(ts_data)
  )

  # Визуализация результатов
  ggplot() +
    geom_line(data = historical_df, aes(x = Date, y = Price), color = "blue") +
    geom_line(data = forecast_df, aes(x = Date, y = Forecast), color = "red") +
    geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower_80, ymax = Upper_80), alpha = 0.2, fill = "orange") +
    geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower_95, ymax = Upper_95), alpha = 0.2, fill = "yellow") +
    labs(title = "ARIMA Forecast", x = "Date", y = "Price") +
    theme(legend.position = "bottom") +
    scale_color_manual(values = c("Historical" = "blue", "Forecast" = "red"))
}

data <- getSymbols.Moex("PLZL", from = "2022-05-01", to = "2024-09-01", period = "day")
plot(ts(data$close))

tsData = ts(data$close)

plot(tsData)

components.ts = decompose(tsData)

plot(components.ts)

stationary = diff(tsData, differences=2)
plot(stationary)
adf.test(stationary)
kpss.test(stationary)
acf(stationary)
pacf(stationary)

ses <- ses(tsData, h = 20)
plot(ses)
checkresiduals(ses)

holt <- holt(tsData, h = 20)
plot(holt)
checkresiduals(holt)

hw <- hw(tsData, h = 20)
plot(hw)
checkresiduals(hw)

arima <- auto.arima(tsData, start.p = 2)
arimafor <- forecast(arima, h = 20)
plot(arimafor)
checkresiduals(arima)

ma <- rollmean(tsData, 7, align = 'right')
ma <- merge(ma, data$close)

plot(ma)

Box.test(tsData)
Box.test(stationary)

library(TSA)

arimax <- arimax(tsData, order = c(0,1,2), method = "ML")
arimaxfor <- forecast(arimax, h = 100)

lagplot(tsData, lag.max = 20, deg = 1, nn = 0.7, method = "both")
######################

bc <- BoxCox(tsData, lambda = 1/3)
plot(bc)
bcinv <- BoxCox(bc, lambda = -1)
plot(bcinv)

library(lmtest)
coeftest(arima)





#################


check = getSymbols.Moex("PLZL", from = "2024-09-01", period = "day")
plot(ts(check$close))


#gazp = data["GAZP"]
#sber = data["SBER"]
plzl = xts(data$close, order.by = as.Date(data$timestamp))
plot(plzl)

plzl_ma <- rollmean(plzl, k = 5, fill = NA)
plot(plzl_ma)
adf.test(na.omit(diff(plzl)))
kpss.test(diff(plzl))

m1 <- auto.arima(plzl)
summary(m1)
plot(forecast(m1, h = 10))

m2 <- auto.arima(ts(sber))
summary(m2)
plot(forecast(m2, h = 100))

m3 <- auto.arima(ts(plzl))
summary(m3)
plot(forecast(m3, h = 100))


hw1 <- HoltWinters(ts(plzl, frequency = 182))
summary(hw1)
plot(forecast(hw1, h = 20))


history <- data.frame(ds = data$Date, y = data$SBER)
m0 <- prophet(history)
summary(m0)

future <- make_future_dataframe(m0, periods = 2, freq = "day")
forecast <- predict(m0, future)
plot(m0, forecast)
