library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(jsonlite)
library(lubridate)

cat("读取月度汇总数据...\n")
monthly_data <- readRDS("json_output/monthly_data.rds")

cat("\n=== 模型选型说明 ===\n")
cat("三种价格均存在明显的长期趋势 + 12个月季节性周期，选用季节性ARIMA模型（SARIMA）。\n")
cat("使用 auto.arima() 自动定阶（基于AIC准则），替代手动网格搜索，大幅提升效率。\n")

cat("\n=== 模型构建与自动定阶 ===\n")

build_sarima_model <- function(ts_data, price_name) {
  cat(sprintf("\n--- %s ---\n", price_name))
  cat("正在使用 auto.arima 自动搜索最优阶数，请稍候...\n")
  
  s <- 12
  
  # 【核心修改】使用 auto.arima 替代4层循环
  # stepwise=FALSE, approximation=FALSE 可以搜索得更全，但稍微慢一点；如果还觉得慢，可以设为 TRUE
  best_model <- auto.arima(ts_data, 
                           seasonal = TRUE, 
                           stepwise = FALSE, 
                           approximation = FALSE,
                           max.p = 3, max.q = 3, 
                           max.P = 3, max.Q = 3,
                           max.d = 1, max.D = 1)
  
  # 提取最优阶数
  model_order <- arimaorder(best_model)
  p <- model_order["p"]
  d <- model_order["d"]
  q <- model_order["q"]
  P <- model_order["P"]
  D <- model_order["D"]
  Q <- model_order["Q"]
  
  cat(sprintf("最优参数: p=%d, d=%d, q=%d, P=%d, D=%d, Q=%d\n", p, d, q, P, D, Q))
  cat(sprintf("最优模型: SARIMA(%d,%d,%d)(%d,%d,%d)[12]\n", p, d, q, P, D, Q))
  cat(sprintf("AIC: %.2f, BIC: %.2f\n", AIC(best_model), BIC(best_model)))
  
  # 稳健提取参数估计表
  param_estimates <- tryCatch({
    coef_mat <- summary(best_model)$coef
    if (is.matrix(coef_mat) || is.data.frame(coef_mat)) {
      needed_cols <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
      available_cols <- intersect(needed_cols, colnames(coef_mat))
      
      if (length(available_cols) >= 1) {
        df <- data.frame(
          参数 = rownames(coef_mat),
          估计值 = as.numeric(coef_mat[, "Estimate"]),
          标准误 = if ("Std. Error" %in% available_cols) as.numeric(coef_mat[, "Std. Error"]) else NA,
          z值 = if ("z value" %in% available_cols) as.numeric(coef_mat[, "z value"]) else NA,
          p值 = if ("Pr(>|z|)" %in% available_cols) as.numeric(coef_mat[, "Pr(>|z|)"]) else NA,
          显著性 = if ("Pr(>|z|)" %in% available_cols) ifelse(coef_mat[, "Pr(>|z|)"] < 0.05, "显著", "不显著") else "未知"
        )
        df
      } else {
        stop("列名不匹配")
      }
    } else {
      stop("非矩阵结构")
    }
  }, error = function(e) {
    cat("警告：无法获取完整参数统计量，仅输出系数估计值\n")
    simple_coefs <- coef(best_model)
    data.frame(
      参数 = names(simple_coefs),
      估计值 = as.numeric(simple_coefs),
      标准误 = NA,
      z值 = NA,
      p值 = NA,
      显著性 = "未知"
    )
  })
  
  # 构造一个简单的网格结果data.frame（兼容旧代码的JSON导出）
  grid_results <- data.frame(
    p = p, q = q, P = P, Q = Q,
    AIC = AIC(best_model), BIC = BIC(best_model)
  )
  
  return(list(
    model = best_model,
    params = list(p=p, d=d, q=q, P=P, D=D, Q=Q),
    grid_results = grid_results,
    param_estimates = param_estimates,
    AIC = AIC(best_model),
    BIC = BIC(best_model)
  ))
}

ts_wholesale <- ts(monthly_data$批发价格, frequency = 12)
ts_trade <- ts(monthly_data$批农贸零售价格, frequency = 12)
ts_supermarket <- ts(monthly_data$超市贸零售价格, frequency = 12)

cat("\n=== 批发价格SARIMA模型 ===\n")
sarima_wholesale <- build_sarima_model(ts_wholesale, "批发价格")

cat("\n=== 批农贸零售价格SARIMA模型 ===\n")
sarima_trade <- build_sarima_model(ts_trade, "批农贸零售价格")

cat("\n=== 超市贸零售价格SARIMA模型 ===\n")
sarima_supermarket <- build_sarima_model(ts_supermarket, "超市贸零售价格")

cat("\n=== 导出结果到JSON ===\n")

format_model_info <- function(sarima_result, price_name) {
  if (is.null(sarima_result)) {
    return(list(
      模型阶数 = paste0(price_name, "-SARIMA(1,1,1)(1,1,1)[12]"),
      AIC = NA,
      BIC = NA,
      参数估计 = list(),
      模型表达式 = paste0(price_name, "-SARIMA(1,1,1)(1,1,1)[12]")
    ))
  }
  
  params <- sarima_result$params
  model_info <- list(
    模型阶数 = sprintf("SARIMA(%d,%d,%d)(%d,%d,%d)[12]", params$p, params$d, params$q, params$P, params$D, params$Q),
    AIC = as.numeric(sarima_result$AIC),
    BIC = as.numeric(sarima_result$BIC),
    参数估计 = list(),
    模型表达式 = sprintf("SARIMA(%d,%d,%d)(%d,%d,%d)[12]", params$p, params$d, params$q, params$P, params$D, params$Q)
  )
  
  if (!is.null(sarima_result$param_estimates) && nrow(sarima_result$param_estimates) > 0) {
    for (i in 1:nrow(sarima_result$param_estimates)) {
      row <- sarima_result$param_estimates[i, ]
      model_info$参数估计[[i]] <- list(
        参数 = as.character(row$参数),
        估计值 = as.numeric(row$估计值),
        标准误 = as.numeric(row$标准误),
        z值 = as.numeric(row$z值),
        p值 = as.numeric(row$p值),
        显著性 = as.character(row$显著性)
      )
    }
  }
  
  return(model_info)
}

best_model_info_all <- list(
  批发价格 = format_model_info(sarima_wholesale, "批发价格"),
  批农贸零售价格 = format_model_info(sarima_trade, "批农贸零售价格"),
  超市贸零售价格 = format_model_info(sarima_supermarket, "超市贸零售价格")
)

toJSON(best_model_info_all, pretty = TRUE) %>%
  writeLines("json_output/best_model_info.json")

if (!is.null(sarima_wholesale$grid_results)) {
  # 兼容旧代码，只导出最优结果
  grid_wholesale <- list(list(
    p=sarima_wholesale$grid_results$p, 
    q=sarima_wholesale$grid_results$q, 
    P=sarima_wholesale$grid_results$P, 
    Q=sarima_wholesale$grid_results$Q, 
    AIC=as.numeric(sarima_wholesale$grid_results$AIC), 
    BIC=as.numeric(sarima_wholesale$grid_results$BIC)
  ))
  toJSON(grid_wholesale, pretty = TRUE) %>%
    writeLines("json_output/grid_search_wholesale.json")
}

saveRDS(sarima_wholesale, "json_output/sarima_wholesale.rds")
saveRDS(sarima_trade, "json_output/sarima_trade.rds")
saveRDS(sarima_supermarket, "json_output/sarima_supermarket.rds")

cat("\nSARIMA模型构建与最优定阶完成！\n")