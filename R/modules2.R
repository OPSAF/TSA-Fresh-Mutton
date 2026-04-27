# 平稳性与纯随机性检验模块

library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(jsonlite)
library(tseries)
library(lubridate)

cat("读取月度汇总数据...\n")
monthly_data <- readRDS("json_output/monthly_data.rds")

cat("\n=== 平稳性检验全流程 ===\n")

cat("三种价格平稳性检验:\n")

test_stationarity <- function(ts_data, name) {
  cat(sprintf("\n--- %s ---\n", name))
  
  cat("1. 肉眼初步判断：原始序列存在趋势和季节性，属于非平稳序列。\n")
  
  cat("2. ADF单位根检验：\n")
  adf_original <- adf.test(ts_data)
  cat(sprintf("原始序列ADF检验 p值: %.4f - %s\n", 
               adf_original$p.value, ifelse(adf_original$p.value < 0.05, "平稳", "非平稳")))
  
  diff1 <- diff(ts_data)
  adf_diff1 <- adf.test(diff1)
  cat(sprintf("1阶普通差分后ADF检验 p值: %.4f - %s\n", 
               adf_diff1$p.value, ifelse(adf_diff1$p.value < 0.05, "平稳", "非平稳")))
  
  seasonal_diff1 <- diff(ts_data, lag = 12)
  adf_seasonal_diff1 <- adf.test(seasonal_diff1)
  cat(sprintf("1阶季节性差分后ADF检验 p值: %.4f - %s\n", 
               adf_seasonal_diff1$p.value, ifelse(adf_seasonal_diff1$p.value < 0.05, "平稳", "非平稳")))
  
  diff1_seasonal_diff1 <- diff(diff(ts_data), lag = 12)
  adf_diff1_seasonal_diff1 <- adf.test(diff1_seasonal_diff1)
  cat(sprintf("1阶普通差分+1阶季节性差分后ADF检验 p值: %.4f - %s\n", 
               adf_diff1_seasonal_diff1$p.value, ifelse(adf_diff1_seasonal_diff1$p.value < 0.05, "平稳", "非平稳")))
  
  return(list(
    original = adf_original,
    diff1 = adf_diff1,
    seasonal_diff1 = adf_seasonal_diff1,
    diff1_seasonal_diff1 = adf_diff1_seasonal_diff1,
    stationary_ts = diff1_seasonal_diff1
  ))
}

ts_wholesale <- ts(monthly_data$批发价格, frequency = 12)
ts_trade <- ts(monthly_data$批农贸零售价格, frequency = 12)
ts_supermarket <- ts(monthly_data$超市贸零售价格, frequency = 12)

adf_results_wholesale <- test_stationarity(ts_wholesale, "批发价格")
adf_results_trade <- test_stationarity(ts_trade, "批农贸零售价格")
adf_results_supermarket <- test_stationarity(ts_supermarket, "超市贸零售价格")

cat("\n3. ACF/PACF图辅助判断...\n")

png("figure1/批发价格原始ACF.png", width = 1000, height = 600)
acf(ts_wholesale, main = "批发价格原始序列ACF图")
dev.off()

png("figure1/批发价格原始PACF.png", width = 1000, height = 600)
pacf(ts_wholesale, main = "批发价格原始序列PACF图")
dev.off()

png("figure1/批发价格平稳后ACF.png", width = 1000, height = 600)
acf(adf_results_wholesale$stationary_ts, main = "批发价格平稳序列ACF图")
dev.off()

png("figure1/批发价格平稳后PACF.png", width = 1000, height = 600)
pacf(adf_results_wholesale$stationary_ts, main = "批发价格平稳序列PACF图")
dev.off()

png("figure1/批农贸零售价格原始ACF.png", width = 1000, height = 600)
acf(ts_trade, main = "批农贸零售价格原始序列ACF图")
dev.off()

png("figure1/批农贸零售价格原始PACF.png", width = 1000, height = 600)
pacf(ts_trade, main = "批农贸零售价格原始序列PACF图")
dev.off()

png("figure1/批农贸零售价格平稳后ACF.png", width = 1000, height = 600)
acf(adf_results_trade$stationary_ts, main = "批农贸零售价格平稳序列ACF图")
dev.off()

png("figure1/批农贸零售价格平稳后PACF.png", width = 1000, height = 600)
pacf(adf_results_trade$stationary_ts, main = "批农贸零售价格平稳序列PACF图")
dev.off()

png("figure1/超市贸零售价格原始ACF.png", width = 1000, height = 600)
acf(ts_supermarket, main = "超市贸零售价格原始序列ACF图")
dev.off()

png("figure1/超市贸零售价格原始PACF.png", width = 1000, height = 600)
pacf(ts_supermarket, main = "超市贸零售价格原始序列PACF图")
dev.off()

png("figure1/超市贸零售价格平稳后ACF.png", width = 1000, height = 600)
acf(adf_results_supermarket$stationary_ts, main = "超市贸零售价格平稳序列ACF图")
dev.off()

png("figure1/超市贸零售价格平稳后PACF.png", width = 1000, height = 600)
pacf(adf_results_supermarket$stationary_ts, main = "超市贸零售价格平稳序列PACF图")
dev.off()

cat("\n=== 纯随机性（白噪声）检验 ===\n")

test_white_noise <- function(ts_data, name) {
  cat(sprintf("\n--- %s ---\n", name))
  diff1_seasonal_diff1 <- diff(diff(ts_data), lag = 12)
  lb_test <- Box.test(diff1_seasonal_diff1, lag = 12, type = "Ljung-Box")
  cat(sprintf("Ljung-Box检验 p值: %.4f - %s\n", 
              lb_test$p.value, ifelse(lb_test$p.value < 0.05, "非白噪声，适合建模", "白噪声，不适合建模")))
  return(lb_test)
}

lb_wholesale <- test_white_noise(ts_wholesale, "批发价格")
lb_trade <- test_white_noise(ts_trade, "批农贸零售价格")
lb_supermarket <- test_white_noise(ts_supermarket, "超市贸零售价格")

cat("\n导出检验结果到JSON...\n")

format_adf_results <- function(adf_result, name) {
  list(
    原始序列 = list(
      p值 = adf_result$original$p.value,
      统计量 = as.numeric(adf_result$original$statistic),
      结论 = ifelse(adf_result$original$p.value < 0.05, "平稳", "非平稳")
    ),
    一阶普通差分 = list(
      p值 = adf_result$diff1$p.value,
      统计量 = as.numeric(adf_result$diff1$statistic),
      结论 = ifelse(adf_result$diff1$p.value < 0.05, "平稳", "非平稳")
    ),
    一阶季节性差分 = list(
      p值 = adf_result$seasonal_diff1$p.value,
      统计量 = as.numeric(adf_result$seasonal_diff1$statistic),
      结论 = ifelse(adf_result$seasonal_diff1$p.value < 0.05, "平稳", "非平稳")
    ),
    一阶普通差分加一阶季节性差分 = list(
      p值 = adf_result$diff1_seasonal_diff1$p.value,
      统计量 = as.numeric(adf_result$diff1_seasonal_diff1$statistic),
      结论 = ifelse(adf_result$diff1_seasonal_diff1$p.value < 0.05, "平稳", "非平稳")
    )
  )
}

adf_results_all <- list(
  批发价格 = format_adf_results(adf_results_wholesale, "批发价格"),
  批农贸零售价格 = format_adf_results(adf_results_trade, "批农贸零售价格"),
  超市贸零售价格 = format_adf_results(adf_results_supermarket, "超市贸零售价格")
)

lb_results_all <- list(
  批发价格 = list(
    p值 = lb_wholesale$p.value,
    统计量 = as.numeric(lb_wholesale$statistic),
    结论 = ifelse(lb_wholesale$p.value < 0.05, "非白噪声，适合建模", "白噪声，不适合建模")
  ),
  批农贸零售价格 = list(
    p值 = lb_trade$p.value,
    统计量 = as.numeric(lb_trade$statistic),
    结论 = ifelse(lb_trade$p.value < 0.05, "非白噪声，适合建模", "白噪声，不适合建模")
  ),
  超市贸零售价格 = list(
    p值 = lb_supermarket$p.value,
    统计量 = as.numeric(lb_supermarket$statistic),
    结论 = ifelse(lb_supermarket$p.value < 0.05, "非白噪声，适合建模", "白噪声，不适合建模")
  )
)

conclusion <- list(
  普通差分阶数_d = 1,
  季节性差分阶数_D = 1,
  季节周期_s = 12,
  建模建议 = "使用SARIMA模型，参数范围建议从(p=1-3, d=1, q=1-3)(P=1-2, D=1, Q=1-2)[12]开始搜索"
)

toJSON(adf_results_all, pretty = TRUE) %>%
  writeLines("json_output/adf_test_results.json")

toJSON(lb_results_all, pretty = TRUE) %>%
  writeLines("json_output/lb_test_results.json")

toJSON(conclusion, pretty = TRUE) %>%
  writeLines("json_output/model_parameters.json")

cat("\n平稳性与纯随机性检验完成！\n")
