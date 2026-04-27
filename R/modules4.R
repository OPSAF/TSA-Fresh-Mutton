# 模型诊断与效果评估模块

library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(jsonlite)
library(lubridate)

cat("读取月度汇总数据...\n")
monthly_data <- readRDS("json_output/monthly_data.rds")

cat("读取SARIMA模型...\n")
sarima_wholesale <- readRDS("json_output/sarima_wholesale.rds")
sarima_trade <- readRDS("json_output/sarima_trade.rds")
sarima_supermarket <- readRDS("json_output/sarima_supermarket.rds")

ts_wholesale <- ts(monthly_data$批发价格, frequency = 12)
ts_trade <- ts(monthly_data$批农贸零售价格, frequency = 12)
ts_supermarket <- ts(monthly_data$超市贸零售价格, frequency = 12)

diagnose_model <- function(ts_data, sarima_result, price_name, price_col) {
  cat(sprintf("\n=== %s 模型诊断 ===\n", price_name))

  n <- length(ts_data)
  train_size <- floor(n * 0.8)
  ts_time <- time(ts_data)
  train_end_time <- ts_time[train_size]
  test_start_time <- ts_time[train_size + 1]
  train_series <- window(ts_data, end = train_end_time)
  test_series <- window(ts_data, start = test_start_time)

  cat(sprintf("训练集样本数: %d, 测试集样本数: %d\n", length(train_series), length(test_series)))

  model <- sarima_result$model
  residuals_model <- residuals(model)

  png(sprintf("figure1/%s残差时序图.png", price_name), width = 1000, height = 600)
  plot(residuals_model, main = sprintf("%s残差时序图", price_name), xlab = "时间", ylab = "残差")
  abline(h = 0, col = "red", lty = 2)
  dev.off()

  png(sprintf("figure1/%s残差ACF图.png", price_name), width = 1000, height = 600)
  acf(residuals_model, main = sprintf("%s残差ACF图", price_name))
  dev.off()

  png(sprintf("figure1/%s残差PACF图.png", price_name), width = 1000, height = 600)
  pacf(residuals_model, main = sprintf("%s残差PACF图", price_name))
  dev.off()

  png(sprintf("figure1/%s残差分布图.png", price_name), width = 1000, height = 600)
  hist(residuals_model, breaks = 30, main = sprintf("%s残差分布图", price_name), xlab = "残差", ylab = "频数")
  dev.off()

  lb_test <- Box.test(residuals_model, lag = 12, type = "Ljung-Box")
  cat(sprintf("残差LB白噪声检验 p值: %.4f - %s\n",
               lb_test$p.value, ifelse(lb_test$p.value > 0.05, "残差为白噪声，模型通过检验", "残差非白噪声")))

  forecast_result <- forecast(model, h = length(test_series))
  pred_values <- as.numeric(forecast_result$mean)

  mae <- mean(abs(test_series - pred_values), na.rm = TRUE)
  rmse <- sqrt(mean((test_series - pred_values)^2, na.rm = TRUE))
  mape <- mean(abs((test_series - pred_values) / test_series), na.rm = TRUE) * 100
  mase <- mae / mean(abs(diff(train_series)), na.rm = TRUE)

  cat(sprintf("MAE: %.4f, RMSE: %.4f, MAPE: %.4f%%, MASE: %.4f\n", mae, rmse, mape, mase))

  train_fitted <- fitted(model)
  last_train_idx <- length(train_series)

  # 确保train_fitted的长度正确
  if (length(train_fitted) > last_train_idx) {
    train_fitted <- train_fitted[1:last_train_idx]
  }

  # 从月度数据中获取实际日期
  actual_dates <- monthly_data$日期
  date_train <- actual_dates[1:last_train_idx]
  date_test <- actual_dates[(last_train_idx+1):n]

  p_fit <- ggplot() +
    geom_line(data = data.frame(日期 = date_train,
                                值 = as.numeric(ts_data[1:last_train_idx])),
              aes(x = 日期, y = 值, color = "训练集实际值"), linewidth = 0.8) +
    geom_line(data = data.frame(日期 = date_test,
                                值 = as.numeric(ts_data[(last_train_idx+1):n])),
              aes(x = 日期, y = 值, color = "测试集实际值"), linewidth = 0.8) +
    geom_line(data = data.frame(日期 = date_train,
                                值 = as.numeric(train_fitted)),
              aes(x = 日期, y = 值, color = "模型拟合值"), linewidth = 0.8, linetype = "dashed") +
    geom_line(data = data.frame(日期 = date_test,
                                值 = pred_values),
              aes(x = 日期, y = 值, color = "模型预测值"), linewidth = 0.8, linetype = "dashed") +
    geom_ribbon(data = data.frame(日期 = date_test,
                                lower = as.numeric(forecast_result$lower[, 2]),
                                upper = as.numeric(forecast_result$upper[, 2])),
              aes(x = 日期, ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
    labs(title = sprintf("%s模型拟合与预测效果图", price_name),
         x = "时间", y = "价格 (元/公斤)", color = "图例",
         caption = "数据来源: 鲜羊肉价格数据") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), legend.position = "bottom")

  ggsave(sprintf("figure1/%s模型拟合与预测效果图.png", price_name), p_fit, width = 14, height = 7)

  return(list(
    lb_pvalue = lb_test$p.value,
    lb_statistic = as.numeric(lb_test$statistic),
    residual_mean = mean(residuals_model, na.rm = TRUE),
    residual_sd = sd(residuals_model, na.rm = TRUE),
    MAE = mae,
    RMSE = rmse,
    MAPE = mape,
    MASE = mase,
    forecast_data = data.frame(
      日期 = actual_dates[(last_train_idx+1):n],
      实际值 = as.numeric(test_series),
      预测值 = pred_values,
      下界 = as.numeric(forecast_result$lower[, 2]),
      上界 = as.numeric(forecast_result$upper[, 2])
    )
  ))
}

cat("\n=== 批发价格模型诊断 ===\n")
diag_wholesale <- diagnose_model(ts_wholesale, sarima_wholesale, "批发价格", "批发价格")

cat("\n=== 批农贸零售价格模型诊断 ===\n")
diag_trade <- diagnose_model(ts_trade, sarima_trade, "批农贸零售价格", "批农贸零售价格")

cat("\n=== 超市贸零售价格模型诊断 ===\n")
diag_supermarket <- diagnose_model(ts_supermarket, sarima_supermarket, "超市贸零售价格", "超市贸零售价格")

cat("\n=== 基准模型对比 ===\n")

compare_benchmark <- function(ts_data, price_name) {
  n <- length(ts_data)
  train_size <- floor(n * 0.8)
  ts_time <- time(ts_data)
  train_end_time <- ts_time[train_size]
  test_start_time <- ts_time[train_size + 1]
  train_series <- window(ts_data, end = train_end_time)
  test_series <- window(ts_data, start = test_start_time)
  
  naive_pred <- as.numeric(test_series)
  for (i in 1:length(test_series)) {
    idx <- length(train_series) - length(test_series) + i
    if (idx > 0) {
      naive_pred[i] <- train_series[idx]
    } else {
      naive_pred[i] <- mean(train_series, na.rm = TRUE)
    }
  }
  
  mae_naive <- mean(abs(test_series - naive_pred), na.rm = TRUE)
  rmse_naive <- sqrt(mean((test_series - naive_pred)^2, na.rm = TRUE))
  mape_naive <- mean(abs((test_series - naive_pred) / test_series), na.rm = TRUE) * 100
  mase_naive <- mae_naive / mean(abs(diff(train_series)), na.rm = TRUE)
  
  arima_model <- Arima(train_series, order = c(1, 1, 1))
  arima_forecast <- forecast(arima_model, h = length(test_series))
  arima_pred <- as.numeric(arima_forecast$mean)
  
  mae_arima <- mean(abs(test_series - arima_pred), na.rm = TRUE)
  rmse_arima <- sqrt(mean((test_series - arima_pred)^2, na.rm = TRUE))
  mape_arima <- mean(abs((test_series - arima_pred) / test_series), na.rm = TRUE) * 100
  mase_arima <- mae_arima / mean(abs(diff(train_series)), na.rm = TRUE)
  
  cat(sprintf("\n--- %s 基准模型对比 ---\n", price_name))
  cat(sprintf("朴素预测模型 - MAE: %.4f, RMSE: %.4f, MAPE: %.4f%%, MASE: %.4f\n",
               mae_naive, rmse_naive, mape_naive, mase_naive))
  cat(sprintf("普通ARIMA模型 - MAE: %.4f, RMSE: %.4f, MAPE: %.4f%%, MASE: %.4f\n",
               mae_arima, rmse_arima, mape_arima, mase_arima))
  
  return(list(
    naive = list(MAE=mae_naive, RMSE=rmse_naive, MAPE=mape_naive, MASE=mase_naive),
    arima = list(MAE=mae_arima, RMSE=rmse_arima, MAPE=mape_arima, MASE=mase_arima)
  ))
}

bench_wholesale <- compare_benchmark(ts_wholesale, "批发价格")
bench_trade <- compare_benchmark(ts_trade, "批农贸零售价格")
bench_supermarket <- compare_benchmark(ts_supermarket, "超市贸零售价格")

cat("\n=== 导出结果到JSON ===\n")

diagnosis_results <- list(
  批发价格 = list(
    LB检验 = list(p值=diag_wholesale$lb_pvalue, 统计量=diag_wholesale$lb_statistic,
                  结论=ifelse(diag_wholesale$lb_pvalue > 0.05, "残差为白噪声，模型通过检验", "残差非白噪声，模型需要改进")),
    残差均值 = diag_wholesale$residual_mean,
    残差标准差 = diag_wholesale$residual_sd
  ),
  批农贸零售价格 = list(
    LB检验 = list(p值=diag_trade$lb_pvalue, 统计量=diag_trade$lb_statistic,
                  结论=ifelse(diag_trade$lb_pvalue > 0.05, "残差为白噪声，模型通过检验", "残差非白噪声，模型需要改进")),
    残差均值 = diag_trade$residual_mean,
    残差标准差 = diag_trade$residual_sd
  ),
  超市贸零售价格 = list(
    LB检验 = list(p值=diag_supermarket$lb_pvalue, 统计量=diag_supermarket$lb_statistic,
                  结论=ifelse(diag_supermarket$lb_pvalue > 0.05, "残差为白噪声，模型通过检验", "残差非白噪声，模型需要改进")),
    残差均值 = diag_supermarket$residual_mean,
    残差标准差 = diag_supermarket$residual_sd
  )
)

evaluation_results <- list(
  批发价格 = list(
    SARIMA模型 = list(MAE=diag_wholesale$MAE, RMSE=diag_wholesale$RMSE, MAPE=diag_wholesale$MAPE, MASE=diag_wholesale$MASE),
    朴素预测模型 = list(MAE=bench_wholesale$naive$MAE, RMSE=bench_wholesale$naive$RMSE, 
                        MAPE=bench_wholesale$naive$MAPE, MASE=bench_wholesale$naive$MASE),
    普通ARIMA模型 = list(MAE=bench_wholesale$arima$MAE, RMSE=bench_wholesale$arima$RMSE,
                         MAPE=bench_wholesale$arima$MAPE, MASE=bench_wholesale$arima$MASE)
  ),
  批农贸零售价格 = list(
    SARIMA模型 = list(MAE=diag_trade$MAE, RMSE=diag_trade$RMSE, MAPE=diag_trade$MAPE, MASE=diag_trade$MASE),
    朴素预测模型 = list(MAE=bench_trade$naive$MAE, RMSE=bench_trade$naive$RMSE,
                        MAPE=bench_trade$naive$MAPE, MASE=bench_trade$naive$MASE),
    普通ARIMA模型 = list(MAE=bench_trade$arima$MAE, RMSE=bench_trade$arima$RMSE,
                         MAPE=bench_trade$arima$MAPE, MASE=bench_trade$arima$MASE)
  ),
  超市贸零售价格 = list(
    SARIMA模型 = list(MAE=diag_supermarket$MAE, RMSE=diag_supermarket$RMSE, MAPE=diag_supermarket$MAPE, MASE=diag_supermarket$MASE),
    朴素预测模型 = list(MAE=bench_supermarket$naive$MAE, RMSE=bench_supermarket$naive$RMSE,
                        MAPE=bench_supermarket$naive$MAPE, MASE=bench_supermarket$naive$MASE),
    普通ARIMA模型 = list(MAE=bench_supermarket$arima$MAE, RMSE=bench_supermarket$arima$RMSE,
                         MAPE=bench_supermarket$arima$MAPE, MASE=bench_supermarket$arima$MASE)
  )
)

toJSON(diagnosis_results, pretty = TRUE) %>%
  writeLines("json_output/diagnosis_results.json")

toJSON(evaluation_results, pretty = TRUE) %>%
  writeLines("json_output/evaluation_results.json")

saveRDS(diag_wholesale$forecast_data, "json_output/forecast_data_wholesale.rds")
saveRDS(diag_trade$forecast_data, "json_output/forecast_data_trade.rds")
saveRDS(diag_supermarket$forecast_data, "json_output/forecast_data_supermarket.rds")

cat("\n模型诊断与效果评估完成！\n")
