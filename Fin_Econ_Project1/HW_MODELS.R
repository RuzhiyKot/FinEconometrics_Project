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
  ts_data <- ts(data[,i], frequency = 180)
  
  model <- HoltWinters(ts_data)
  
  # Прогнозирование на следующие 5 периодов
  forecasted_values <- forecast(model, h = LEN)
  
  temp_df <- data.frame(Date = Date, Price = as.numeric(forecasted_values$mean))
  names(temp_df)[2] <- colnames(data)[i]  # Переименовываем колонку с ценой
  
  FORECAST <- merge(FORECAST, temp_df, by = "Date", all = TRUE)
}

name <- paste0("FORECAST_HW_", file_name, ".xlsx")

write_xlsx(FORECAST, name)















for (ticket in tickets) {
  ts_data <- ts(data[ticket], frequency = 180)
  
  model <- HoltWinters(ts_data)
  
  # Прогнозирование на следующие 5 периодов
  FORECAST[ticket] <- forecasted_values <- forecast(model, h = 41)
}




















data <- read_excel("C:\\Users\\verei\\Documents\\Fin_Econ_Project1\\closing_prices_MOEX_today_2022_2024.xlsx", sheet = 1)

# НЕ ТРЕБУЕТ СТАЦИОНАРНОСТИ!

HW_models <- list()
HW_plots <- list()

for (i in 2:ncol(data)) {
  ts_data <- ts(data[,i], frequency = 180)
  
  model <- HoltWinters(ts_data)
  
  # Сохранение модели в список
  HW_models[[colnames(data)[i]]] <- model
  
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
  HW_plots[[colnames(data)[i]]] <- plot
}

# Выводим результаты
for (name in names(HW_models)) {
  cat("\n Модель для", name, "\n")
  print(summary(HW_models[[name]]))
}

for (name in names(HW_plots)) {
  print(HW_plots[[name]])
}

print(HW_plots[["OZON"]])
