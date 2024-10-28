library(forecast)
library(readxl)
library(tseries)
library(zoo)
library(writexl)
library(tools)
library(stringr)


# --------------------------------------------------------------------------------------------- #

file_path <- "C:\\Users\\verei\\Documents\\Fin_Econ_Project1\\Forecast_Datasets\\MOEX_21.xlsx"
LEN <- 20

# --------------------------------------------------------------------------------------------- #

file_name <- str_remove(basename(file_path), "\\.xlsx$")

data <- read_excel(file_path, sheet = 1)

Date <- seq(1, LEN)

FORECAST <- data.frame(Date = Date, stringsAsFactors = FALSE)

for (i in 2:ncol(data)) {
  ts_data <- msts(data[,i], seasonal.periods=c(180))
  
  model <- stlf(ts_data, method="ets")
  
  # Прогнозирование на следующие 5 периодов
  forecasted_values <- forecast(model, h = LEN)
  
  temp_df <- data.frame(Date = Date, Price = as.numeric(forecasted_values$mean))
  names(temp_df)[2] <- colnames(data)[i]  # Переименовываем колонку с ценой
  
  FORECAST <- merge(FORECAST, temp_df, by = "Date", all = TRUE)
}

name <- paste0("FORECAST_STL&ets_", file_name, ".xlsx")

write_xlsx(FORECAST, name)

####################################################################
Date <- seq(1, LEN)

FORECAST <- data.frame(Date = Date, stringsAsFactors = FALSE)

for (i in 2:ncol(data)) {
  ts_data <- msts(data[,i], seasonal.periods=c(180))
  
  model <- stlf(ts_data, method="arima")
  
  # Прогнозирование на следующие 5 периодов
  forecasted_values <- forecast(model, h = LEN)
  
  temp_df <- data.frame(Date = Date, Price = as.numeric(forecasted_values$mean))
  names(temp_df)[2] <- colnames(data)[i]  # Переименовываем колонку с ценой
  
  FORECAST <- merge(FORECAST, temp_df, by = "Date", all = TRUE)
}

name <- paste0("FORECAST_STL&arima_", file_name, ".xlsx")

write_xlsx(FORECAST, name)

####################################################################
Date <- seq(1, LEN)

FORECAST <- data.frame(Date = Date, stringsAsFactors = FALSE)

for (i in 2:ncol(data)) {
  ts_data <- msts(data[,i], seasonal.periods=c(180))
  
  model <- stlf(ts_data, method="naive")
  
  # Прогнозирование на следующие 5 периодов
  forecasted_values <- forecast(model, h = LEN)
  
  temp_df <- data.frame(Date = Date, Price = as.numeric(forecasted_values$mean))
  names(temp_df)[2] <- colnames(data)[i]  # Переименовываем колонку с ценой
  
  FORECAST <- merge(FORECAST, temp_df, by = "Date", all = TRUE)
}

name <- paste0("FORECAST_STL&naive_", file_name, ".xlsx")

write_xlsx(FORECAST, name)

####################################################################
Date <- seq(1, LEN)

FORECAST <- data.frame(Date = Date, stringsAsFactors = FALSE)

for (i in 2:ncol(data)) {
  ts_data <- msts(data[,i], seasonal.periods=c(180))
  
  model <- stlf(ts_data, method="rwdrift")
  
  # Прогнозирование на следующие 5 периодов
  forecasted_values <- forecast(model, h = LEN)
  
  temp_df <- data.frame(Date = Date, Price = as.numeric(forecasted_values$mean))
  names(temp_df)[2] <- colnames(data)[i]  # Переименовываем колонку с ценой
  
  FORECAST <- merge(FORECAST, temp_df, by = "Date", all = TRUE)
}

name <- paste0("FORECAST_STL&rw_", file_name, ".xlsx")

write_xlsx(FORECAST, name)



###############################################

file_path <- "C:\\Users\\verei\\Documents\\Fin_Econ_Project1\\Forecast_Datasets\\Foreign_21.xlsx"
LEN <- 20

# --------------------------------------------------------------------------------------------- #

data <- read_excel(file_path, sheet = 1)

test_data <- read_excel("C:\\Users\\verei\\Documents\\Fin_Econ_Project1\\Forecast_Datasets\\Foreign_21_residual.xlsx", sheet = 1)

ts_data <- msts(data["TSLA"], seasonal.periods=c(180))
test <- test_data["TSLA"]

full_data <- rbind(data["AAPL"], test)
Date <- seq(1:nrow(full_data))

plot(decompose(ts_data))

adf.test(ts_data)
kpss.test(diff(ts_data))

Box.test(diff(diff(ts_data)), lag = 20, type = 'Ljung-Box')
plot(ts_data)
plot(ts(test))

model <- stlf(ts_data, method="arima")

plot(checkresiduals(model))

acf(diff(ts_data))

# Прогнозирование на следующие 5 периодов
forecasted_values <- forecast(model, h = LEN)
plot(forecasted_values)

plot(Date, full_data[["GAZP"]], type = "l", ylab = "TSLA price")
lines(Date[530:549], forecasted_values$mean, col = "blue")

predictions <- forecasted_values$mean

rmse <- sqrt(mean((ts(test)- ts(predictions))^2))
cat("RMSE:", rmse, "\n")

# Вычисляем метрику MAE
mae <- mean(sqrt((ts(test)- ts(predictions))^2))
cat("MAE:", mae, "\n")

MAPE(obs = test_data$Price, pred = predictions)

MAE(obs = test_data$Price, pred = predictions)
MSE(obs = test_data$Price, pred = predictions)







