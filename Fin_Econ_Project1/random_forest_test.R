###############################################

# Построение прогноза на основе Random Forest #

###############################################

library(randomForest)
library(xts)
library(ggplot2)
library(rusquant)
library(forecast)
library(metrica)
library(tseries)

# Выкачиваем данные с Мосбиржи
data <- getSymbols.Moex("OZON", from = "2022-05-01", to = "2024-09-01", period = "day")

#data <- 

# Формируем датафрейм (по цене закрытия и дате торгов)
df <- data.frame(Date = data$timestamp, Price = data$close)

# Скалирование данных
#scaled_prices <- (data$close - min(data$close)) / (max(data$close) - min(data$close))
#df <- data.frame(Date = data$timestamp, Price = scaled_prices)

# Преобразуем датафрейм в формат временного ряда (xts -- общий временной ряд)
df$Date <- as.Date(df$Date)
xts <- (xts(df$Price, order.by = df$Date))

#adf.test(diffinv(xts))

#acf(xts)
#pacf(xts)

# Create lag features for time series data
lags <- 1:10  # Число рассматриваемых лаговых-переменных
lagged_data <- lag(xts, k = lags)  # Create lagged data

# Combine the lagged features into one data frame
lagged_df <- data.frame(lagged_data)
colnames(lagged_df) <- paste0("lag_", lags)  # Rename columns with lag prefixes

# Merge the lagged features with the original data
final_data <- cbind(df, lagged_df)  # Combine data frames

# Remove rows with NAs created by lagging
final_data <- final_data[complete.cases(final_data), ]

# Split the data into training and testing sets 
# train_percentage -- какая часть от общей выборки будет использоваться для обучения модели
# ntree -- число ветвлений дерева
train_percentage <- 0.9
train_size <- floor(train_percentage * nrow(final_data)) 
train_data <- final_data[1:train_size, ]
test_data <- final_data[(train_size + 1):nrow(final_data), ]

#Обучение Random Forest
set.seed(0xB0BA)

rf_model <- randomForest(Price ~ ., data = train_data, ntree = 700)

# Прогнозирование на тестовую выборку
predictions <- predict(rf_model, newdata = test_data)


# Если было скалирование!
#predictions <- predictions * (max(data$close) - min(data$close)) + min(data$close)
#final_data$Price <- final_data$Price * (max(data$close) - min(data$close)) + min(data$close)
#train_data$Price <- train_data$Price * (max(data$close) - min(data$close)) + min(data$close)
#test_data$Price <- test_data$Price * (max(data$close) - min(data$close)) + min(data$close)

# или на несколько шагов вперёд -- НЕ РАБОТАЕТ!
# predictions <- predict(rf_model, n.ahead = 20)


# Высчитываем метрику RMSE
 rmse <- sqrt(mean((test_data$Price - predictions)^2))
 cat("RMSE:", rmse, "\n")
 
# Вычисляем метрику MAE
 mae <- mean(sqrt((test_data$Price - predictions)^2))
 cat("MAE:", mae, "\n")
 
 MAPE(obs = test_data$Price, pred = predictions)
 
 MAE(obs = test_data$Price, pred = predictions)
 MSE(obs = test_data$Price, pred = predictions)

# Plot the original time series and the forecast
ggplot(final_data) +
  geom_line(aes(x = Date, y = Price, color = "Original")) +
  geom_line(data = test_data, aes(x = Date, y = predictions, color = "Forecast")) + 
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Time Series Forecasting with Random Forest", x = "Date", y = "Price")


##########################################
#  Прогнозирование с помощью auto-ARIMA  #
##########################################

gazp <- ts(data$close)

#adf.test(gazp)
#kpss.test(gazp)

m1 <- auto.arima(gazp)
summary(m1)
arimafor <- forecast(m1, h = 20)
checkresiduals(m1)

plot(arimafor)

m2 <- arima(gazp, order = c(3,1,3))
summary(m2)
arimafor <- forecast(m2, h = 20)
plot(arimafor$mean)
checkresiduals(m2)

m3 <- naive(gazp)
summary(m3)
plot(forecast(m3, h = 10))

m4 <- rwf(gazp)
summary(m4)
plot(forecast(m4, h = 10))



















