# 数据预处理与探索性分析 EDA

library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(jsonlite)
library(imputeTS)
library(lubridate)

dir.create("figure1", showWarnings = FALSE)
dir.create("json_output", showWarnings = FALSE)

cat("读取数据...\n")
data <- read_csv("data/鲜羊肉价格.csv")

cat("数据预处理...\n")
names(data) <- c("id", "报告期", "品类编码", "品类", "规格等级",
                 "批发价格", "批农贸零售价格", "超市贸零售价格", "更新时间")

data <- data %>%
  mutate(日期 = as.Date(as.character(报告期), format = "%Y%m%d"))

data <- data %>% arrange(日期)

cat("处理三种价格...\n")
data$批发价格 <- as.numeric(data$批发价格)
data$批农贸零售价格 <- as.numeric(data$批农贸零售价格)
data$超市贸零售价格 <- as.numeric(data$超市贸零售价格)

cat("按月度汇总数据...\n")
monthly_data <- data %>%
  group_by(日期 = floor_date(日期, "month")) %>%
  summarise(
    批发价格 = mean(批发价格, na.rm = TRUE),
    批农贸零售价格 = mean(批农贸零售价格, na.rm = TRUE),
    超市贸零售价格 = mean(超市贸零售价格, na.rm = TRUE),
    .groups = 'drop'
  )

cat("数据清洗与格式标准化...\n")

process_price <- function(price_vec, name) {
  missing_count <- sum(is.na(price_vec))
  cat(sprintf("%s 缺失值数量: %d\n", name, missing_count))

  if (missing_count > 0) {
    price_vec <- na_interpolation(price_vec, option = "linear")
    remaining_missing <- sum(is.na(price_vec))
    if (remaining_missing > 0) {
      price_vec <- na_mean(price_vec, k = 2)
    }
  }

  mean_price <- mean(price_vec, na.rm = TRUE)
  sd_price <- sd(price_vec, na.rm = TRUE)
  lower_bound <- mean_price - 3 * sd_price
  upper_bound <- mean_price + 3 * sd_price

  is_outlier <- (price_vec < lower_bound) | (price_vec > upper_bound)

  for (i in 1:length(price_vec)) {
    if (is_outlier[i]) {
      prev_idx <- max(1, i-1)
      next_idx <- min(length(price_vec), i+1)
      price_vec[i] <- mean(c(price_vec[prev_idx], price_vec[next_idx]), na.rm = TRUE)
    }
  }

  return(price_vec)
}

monthly_data$批发价格 <- process_price(monthly_data$批发价格, "批发价格")
monthly_data$批农贸零售价格 <- process_price(monthly_data$批农贸零售价格, "批农贸零售价格")
monthly_data$超市贸零售价格 <- process_price(monthly_data$超市贸零售价格, "超市贸零售价格")

monthly_data <- monthly_data %>% arrange(日期)

cat("描述性统计分析...\n")
calc_stats <- function(x) {
  list(
    最大值 = max(x, na.rm = TRUE),
    最小值 = min(x, na.rm = TRUE),
    均值 = mean(x, na.rm = TRUE),
    中位数 = median(x, na.rm = TRUE),
    标准差 = sd(x, na.rm = TRUE),
    偏度 = moments::skewness(x, na.rm = TRUE),
    峰度 = moments::kurtosis(x, na.rm = TRUE)
  )
}

stats_wholesale <- calc_stats(monthly_data$批发价格)
stats_trade <- calc_stats(monthly_data$批农贸零售价格)
stats_supermarket <- calc_stats(monthly_data$超市贸零售价格)

cat("\n批发价格统计:\n")
print(stats_wholesale)
cat("\n批农贸零售价格统计:\n")
print(stats_trade)
cat("\n超市贸零售价格统计:\n")
print(stats_supermarket)

cat("\n时序特征可视化...\n")

p_wholesale <- ggplot(monthly_data, aes(x = 日期, y = 批发价格)) +
  geom_line(color = "#1f77b4") +
  labs(title = "批发价格时序变化", x = "时间", y = "价格 (元/公斤)", caption = "数据来源: 鲜羊肉价格数据") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
ggsave("figure1/批发价格时序图.png", p_wholesale, width = 12, height = 6)

p_trade <- ggplot(monthly_data, aes(x = 日期, y = 批农贸零售价格)) +
  geom_line(color = "#ff7f0e") +
  labs(title = "批农贸零售价格时序变化", x = "时间", y = "价格 (元/公斤)", caption = "数据来源: 鲜羊肉价格数据") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
ggsave("figure1/批农贸零售价格时序图.png", p_trade, width = 12, height = 6)

p_supermarket <- ggplot(monthly_data, aes(x = 日期, y = 超市贸零售价格)) +
  geom_line(color = "#2ca02c") +
  labs(title = "超市贸零售价格时序变化", x = "时间", y = "价格 (元/公斤)", caption = "数据来源: 鲜羊肉价格数据") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
ggsave("figure1/超市贸零售价格时序图.png", p_supermarket, width = 12, height = 6)

cat("绘制分布直方图...\n")
p_hist_wholesale <- ggplot(monthly_data, aes(x = 批发价格)) +
  geom_histogram(bins = 30, fill = "#1f77b4", alpha = 0.7) +
  labs(title = "批发价格分布直方图", x = "价格 (元/公斤)", y = "频数", caption = "数据来源: 鲜羊肉价格数据") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
ggsave("figure1/批发价格分布直方图.png", p_hist_wholesale, width = 10, height = 6)

png("figure1/批发价格QQ图.png", width = 1000, height = 600)
qqnorm(monthly_data$批发价格, main = "批发价格QQ图")
qqline(monthly_data$批发价格, col = "#1f77b4")
dev.off()

cat("绘制年度同期价格对比图...\n")
monthly_data$年份 <- format(monthly_data$日期, "%Y")
monthly_data$月份 <- format(monthly_data$日期, "%m")

p_yearly <- ggplot(monthly_data, aes(x = 月份, y = 批农贸零售价格, color = 年份, group = 年份)) +
  geom_line() +
  labs(title = "年度同期批农贸零售价格对比", x = "月份", y = "价格 (元/公斤)", color = "年份", caption = "数据来源: 鲜羊肉价格数据") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), legend.position = "bottom")
ggsave("figure1/年度同期价格对比图.png", p_yearly, width = 12, height = 6)

cat("季节性与趋势分解...\n")
timeseries_wholesale <- ts(monthly_data$批发价格, frequency = 12)
timeseries_trade <- ts(monthly_data$批农贸零售价格, frequency = 12)
timeseries_supermarket <- ts(monthly_data$超市贸零售价格, frequency = 12)

stl_wholesale <- stl(timeseries_wholesale, s.window = "periodic")
stl_trade <- stl(timeseries_trade, s.window = "periodic")
stl_supermarket <- stl(timeseries_supermarket, s.window = "periodic")

png("figure1/批发价格STL分解图.png", width = 1200, height = 800)
plot(stl_wholesale, main = "批发价格STL分解")
dev.off()

png("figure1/批农贸零售价格STL分解图.png", width = 1200, height = 800)
plot(stl_trade, main = "批农贸零售价格STL分解")
dev.off()

png("figure1/超市贸零售价格STL分解图.png", width = 1200, height = 800)
plot(stl_supermarket, main = "超市贸零售价格STL分解")
dev.off()

cat("导出数据到JSON...\n")

monthly_data_json <- monthly_data %>%
  mutate(日期 = as.character(日期)) %>%
  rename(日期_str = 日期) %>%
  select(-年份, -月份)

toJSON(as.data.frame(monthly_data_json), pretty = TRUE, force = TRUE) %>%
  writeLines("json_output/cleaned_data.json")

descriptive_stats_all <- list(
  批发价格 = stats_wholesale,
  批农贸零售价格 = stats_trade,
  超市贸零售价格 = stats_supermarket
)
toJSON(descriptive_stats_all, pretty = TRUE) %>%
  writeLines("json_output/descriptive_stats.json")

decomp_wholesale_df <- as.data.frame(stl_wholesale$time.series)
decomp_wholesale_df$日期 <- monthly_data$日期
decomp_wholesale_df$原始值 <- decomp_wholesale_df$trend + decomp_wholesale_df$seasonal + decomp_wholesale_df$remainder
names(decomp_wholesale_df) <- c("趋势项", "季节项", "残差项", "日期", "原始值")
decomp_wholesale_df <- decomp_wholesale_df[, c("日期", "原始值", "趋势项", "季节项", "残差项")]
toJSON(decomp_wholesale_df, pretty = TRUE, force = TRUE) %>%
  writeLines("json_output/decomposition_wholesale.json")

decomp_trade_df <- as.data.frame(stl_trade$time.series)
decomp_trade_df$日期 <- monthly_data$日期
decomp_trade_df$原始值 <- decomp_trade_df$trend + decomp_trade_df$seasonal + decomp_trade_df$remainder
names(decomp_trade_df) <- c("趋势项", "季节项", "残差项", "日期", "原始值")
decomp_trade_df <- decomp_trade_df[, c("日期", "原始值", "趋势项", "季节项", "残差项")]
toJSON(decomp_trade_df, pretty = TRUE, force = TRUE) %>%
  writeLines("json_output/decomposition_trade.json")

decomp_supermarket_df <- as.data.frame(stl_supermarket$time.series)
decomp_supermarket_df$日期 <- monthly_data$日期
decomp_supermarket_df$原始值 <- decomp_supermarket_df$trend + decomp_supermarket_df$seasonal + decomp_supermarket_df$remainder
names(decomp_supermarket_df) <- c("趋势项", "季节项", "残差项", "日期", "原始值")
decomp_supermarket_df <- decomp_supermarket_df[, c("日期", "原始值", "趋势项", "季节项", "残差项")]
toJSON(decomp_supermarket_df, pretty = TRUE, force = TRUE) %>%
  writeLines("json_output/decomposition_supermarket.json")

saveRDS(monthly_data, "json_output/monthly_data.rds")

cat("分析完成！结果已导出到figure1和json_output文件夹。\n")
