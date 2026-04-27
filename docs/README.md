# 鲜羊肉价格分析与预测系统
# Fresh Mutton Price Analysis and Prediction System

## 项目介绍
## Project Introduction

本项目是一个基于时间序列分析的鲜羊肉价格分析与预测系统，旨在帮助相关部门和企业了解鲜羊肉价格的历史变化趋势，并预测未来价格走势，为决策提供数据支持。

This project is a time series analysis-based system for analyzing and predicting fresh mutton prices. It aims to help relevant departments and enterprises understand historical price trends and predict future price movements, providing data support for decision-making.

## 功能模块
## Functional Modules

### 1. 数据处理与预处理模块 (modules1.R)
### 1. Data Processing and Preprocessing Module (modules1.R)
- **数据读取**：从CSV文件中读取鲜羊肉价格数据
- **数据清洗**：处理缺失值和异常值
- **数据转换**：将数据转换为时间序列格式
- **数据导出**：将清洗后的数据导出为JSON格式，供前端可视化使用

- **Data Reading**: Read fresh mutton price data from CSV files
- **Data Cleaning**: Handle missing values and outliers
- **Data Conversion**: Convert data to time series format
- **Data Export**: Export cleaned data to JSON format for front-end visualization

### 2. 探索性数据分析模块 (modules2.R)
### 2. Exploratory Data Analysis Module (modules2.R)
- **描述性统计分析**：计算价格的均值、中位数、标准差等统计指标
- **时间序列可视化**：绘制价格的时序变化图
- **平稳性检验**：使用ADF检验判断时间序列的平稳性
- **自相关分析**：分析价格的自相关和偏自相关特性

- **Descriptive Statistical Analysis**: Calculate statistical indicators such as mean, median, and standard deviation of prices
- **Time Series Visualization**: Plot time series change graphs of prices
- **Stationarity Test**: Use ADF test to determine the stationarity of time series
- **Autocorrelation Analysis**: Analyze autocorrelation and partial autocorrelation characteristics of prices

### 3. 模型构建与优化模块 (modules3.R)
### 3. Model Building and Optimization Module (modules3.R)
- **模型选择**：构建SARIMA模型
- **参数优化**：使用网格搜索和AIC准则选择最优模型参数
- **模型诊断**：检查模型残差的白噪声特性
- **模型保存**：将最优模型保存为RDS文件

- **Model Selection**: Build SARIMA model
- **Parameter Optimization**: Use grid search and AIC criterion to select optimal model parameters
- **Model Diagnosis**: Check white noise characteristics of model residuals
- **Model Saving**: Save the optimal model as RDS file

### 4. 模型诊断与效果评估模块 (modules4.R)
### 4. Model Diagnosis and Effect Evaluation Module (modules4.R)
- **模型诊断**：分析模型的残差特性
- **基准模型对比**：与朴素预测、季节性朴素预测等基准模型进行对比
- **误差指标计算**：计算MAE、RMSE、MAPE、MASE等误差指标
- **模型评估**：评估模型的预测效果

- **Model Diagnosis**: Analyze residual characteristics of the model
- **Benchmark Model Comparison**: Compare with benchmark models such as naive prediction and seasonal naive prediction
- **Error Indicator Calculation**: Calculate error indicators such as MAE, RMSE, MAPE, and MASE
- **Model Evaluation**: Evaluate the prediction effect of the model

### 5. 短期预测与业务深度解读模块 (modules5.R)
### 5. Short-term Prediction and Business In-depth Interpretation Module (modules5.R)
- **短期预测**：预测未来6个月的价格走势
- **预测可视化**：绘制预测结果和置信区间
- **业务分析**：分析价格变化的可能原因和影响因素
- **报告生成**：生成详细的分析报告

- **Short-term Prediction**: Predict price trends for the next 6 months
- **Prediction Visualization**: Plot prediction results and confidence intervals
- **Business Analysis**: Analyze possible causes and influencing factors of price changes
- **Report Generation**: Generate detailed analysis reports

## 技术栈
## Technology Stack

- **R语言**：用于数据处理、时间序列分析和模型构建
- **Python**：用于辅助数据处理和服务器启动
- **HTML/CSS/JavaScript**：用于前端可视化
- **Chart.js**：用于绘制交互式图表
- **Bootstrap**：用于构建响应式网页

- **R Language**: Used for data processing, time series analysis, and model building
- **Python**: Used for auxiliary data processing and server startup
- **HTML/CSS/JavaScript**: Used for front-end visualization
- **Chart.js**: Used for drawing interactive charts
- **Bootstrap**: Used for building responsive web pages

## 项目结构
## Project Structure

```
TSA/
├── R/
│   ├── modules1.R          # 数据处理与预处理模块
│   ├── modules2.R          # 探索性数据分析模块
│   ├── modules3.R          # 模型构建与优化模块
│   ├── modules4.R          # 模型诊断与效果评估模块
│   └── modules5.R          # 短期预测与业务深度解读模块
├── json_output/
│   ├── cleaned_data.json         # 清洗后的数据
│   ├── descriptive_stats.json    # 描述性统计结果
│   ├── adf_test_results.json     # ADF检验结果
│   ├── lb_test_results.json      # 白噪声检验结果
│   ├── best_model_info.json      # 最优模型信息
│   ├── diagnosis_results.json    # 模型诊断结果
│   ├── evaluation_results.json   # 模型评估结果
│   ├── forecast_results.json     # 预测结果
│   ├── decomposition_wholesale.json    # 批发价格分解结果
│   ├── decomposition_trade.json        # 批农贸零售价格分解结果
│   └── decomposition_supermarket.json  # 超市贸零售价格分解结果
├── figure1/                 # 图表输出目录
├── model/                   # 模型保存目录
├── data/                    # 数据目录
│   └── monthly_data.csv     # 原始数据文件
├── index.html               # 前端可视化页面
├── test_chart.html          # 图表测试页面
├── start_server.py          # 服务器启动脚本
└── README.md                # 项目文档
```

```
TSA/
├── R/
│   ├── modules1.R          # Data Processing and Preprocessing Module
│   ├── modules2.R          # Exploratory Data Analysis Module
│   ├── modules3.R          # Model Building and Optimization Module
│   ├── modules4.R          # Model Diagnosis and Effect Evaluation Module
│   └── modules5.R          # Short-term Prediction and Business In-depth Interpretation Module
├── json_output/
│   ├── cleaned_data.json         # Cleaned data
│   ├── descriptive_stats.json    # Descriptive statistics results
│   ├── adf_test_results.json     # ADF test results
│   ├── lb_test_results.json      # White noise test results
│   ├── best_model_info.json      # Optimal model information
│   ├── diagnosis_results.json    # Model diagnosis results
│   ├── evaluation_results.json   # Model evaluation results
│   ├── forecast_results.json     # Forecast results
│   ├── decomposition_wholesale.json    # Wholesale price decomposition results
│   ├── decomposition_trade.json        # Trade retail price decomposition results
│   └── decomposition_supermarket.json  # Supermarket retail price decomposition results
├── figure1/                 # Chart output directory
├── model/                   # Model saving directory
├── data/                    # Data directory
│   └── monthly_data.csv     # Original data file
├── index.html               # Front-end visualization page
├── test_chart.html          # Chart test page
├── start_server.py          # Server startup script
└── README.md                # Project documentation
```

## 数据说明
## Data Description

本项目使用的数据集包含北京市2018年1月至2026年4月的鲜羊肉价格数据，具体包括：

The dataset used in this project contains fresh mutton price data from January 2018 to April 2026 in Beijing, including:

- **批发价格**：鲜羊肉的批发价格（元/公斤）
- **批农贸零售价格**：批发市场的零售价格（元/公斤）
- **超市贸零售价格**：超市的零售价格（元/公斤）

- **Wholesale Price**: Wholesale price of fresh mutton (yuan/kg)
- **Trade Retail Price**: Retail price in wholesale markets (yuan/kg)
- **Supermarket Retail Price**: Retail price in supermarkets (yuan/kg)

数据来源：北京市农产品价格监测系统

Data Source: Beijing Agricultural Product Price Monitoring System

## 使用方法
## Usage Instructions

### 1. 环境准备
### 1. Environment Preparation

- **R环境**：安装R 4.0或以上版本
- **R包依赖**：
  - `tidyverse`：数据处理
  - `forecast`：时间序列分析和预测
  - `tseries`：时间序列检验
  - `jsonlite`：JSON数据处理
  - `lubridate`：日期处理
  - `ggplot2`：数据可视化

- **R Environment**: Install R version 4.0 or above
- **R Package Dependencies**:
  - `tidyverse`: Data processing
  - `forecast`: Time series analysis and prediction
  - `tseries`: Time series testing
  - `jsonlite`: JSON data processing
  - `lubridate`: Date processing
  - `ggplot2`: Data visualization

- **Python环境**（可选）：安装Python 3.6或以上版本

- **Python Environment** (optional): Install Python version 3.6 or above

### 2. 运行步骤
### 2. Running Steps

1. **数据处理**：运行 `modules1.R` 进行数据清洗和预处理
2. **探索性分析**：运行 `modules2.R` 进行数据探索和统计分析
3. **模型构建**：运行 `modules3.R` 构建和优化SARIMA模型
4. **模型评估**：运行 `modules4.R` 评估模型效果
5. **预测分析**：运行 `modules5.R` 进行短期预测和业务分析
6. **可视化**：打开 `index.html` 查看分析结果

1. **Data Processing**: Run `modules1.R` for data cleaning and preprocessing
2. **Exploratory Analysis**: Run `modules2.R` for data exploration and statistical analysis
3. **Model Building**: Run `modules3.R` to build and optimize SARIMA model
4. **Model Evaluation**: Run `modules4.R` to evaluate model performance
5. **Prediction Analysis**: Run `modules5.R` for short-term prediction and business analysis
6. **Visualization**: Open `index.html` to view analysis results

### 3. 启动本地服务器
### 3. Start Local Server

如果直接打开 `index.html` 无法正常加载数据，可以启动本地服务器：

If directly opening `index.html` cannot load data properly, you can start a local server:

```bash
# 使用Python启动服务器
cd TSA
python -m http.server 8080

# 或使用start_server.py脚本
cd TSA
python start_server.py
```

```bash
# Start server using Python
cd TSA
python -m http.server 8080

# Or use start_server.py script
cd TSA
python start_server.py
```

然后在浏览器中打开 `http://localhost:8080` 查看可视化结果。

Then open `http://localhost:8080` in your browser to view the visualization results.

## 模型说明
## Model Description

本项目使用SARIMA（Seasonal Autoregressive Integrated Moving Average）模型进行时间序列分析和预测。SARIMA模型是ARIMA模型的扩展，能够处理具有季节性的时间序列数据。

This project uses the SARIMA (Seasonal Autoregressive Integrated Moving Average) model for time series analysis and prediction. SARIMA is an extension of the ARIMA model that can handle time series data with seasonality.

### 模型参数
### Model Parameters

- **p**：自回归阶数
- **d**：差分阶数
- **q**：移动平均阶数
- **P**：季节性自回归阶数
- **D**：季节性差分阶数
- **Q**：季节性移动平均阶数
- **s**：季节性周期长度（本项目中为12，即月度数据）

- **p**: Autoregressive order
- **d**: Differencing order
- **q**: Moving average order
- **P**: Seasonal autoregressive order
- **D**: Seasonal differencing order
- **Q**: Seasonal moving average order
- **s**: Seasonal cycle length (12 in this project, i.e., monthly data)

### 模型选择
### Model Selection

通过网格搜索和AIC准则，为每种价格类型选择最优的SARIMA模型参数。

Optimal SARIMA model parameters are selected for each price type through grid search and AIC criterion.

## 预测结果
## Prediction Results

系统会预测未来6个月的鲜羊肉价格，并提供95%的置信区间。预测结果会保存到 `json_output/forecast_results.json` 文件中，并在前端页面中可视化展示。

The system will predict fresh mutton prices for the next 6 months and provide 95% confidence intervals. The prediction results will be saved to the `json_output/forecast_results.json` file and visualized on the front-end page.

## 业务建议
## Business Recommendations

基于价格分析和预测结果，系统会提供以下业务建议：

Based on price analysis and prediction results, the system will provide the following business recommendations:

1. **价格趋势分析**：分析价格的长期趋势和季节性变化
2. **风险预警**：识别可能的价格异常波动
3. **采购策略**：根据预测结果制定合理的采购计划
4. **库存管理**：优化库存水平，降低成本

1. **Price Trend Analysis**: Analyze long-term trends and seasonal changes in prices
2. **Risk Early Warning**: Identify possible abnormal price fluctuations
3. **Procurement Strategy**: Develop reasonable procurement plans based on prediction results
4. **Inventory Management**: Optimize inventory levels and reduce costs

## 注意事项
## Notes

1. **数据更新**：当有新数据时，需要重新运行所有模块以更新分析结果
2. **模型调优**：随着数据量的增加，可能需要重新调整模型参数
3. **外部因素**：预测结果未考虑外部因素（如政策变化、突发事件等）的影响
4. **预测局限性**：长期预测的准确性会降低，建议关注短期预测结果

1. **Data Update**: When there is new data, all modules need to be re-run to update analysis results
2. **Model Tuning**: As data volume increases, model parameters may need to be readjusted
3. **External Factors**: Prediction results do not consider the impact of external factors (such as policy changes, emergencies, etc.)
4. **Prediction Limitations**: The accuracy of long-term predictions will decrease, so it is recommended to focus on short-term prediction results

## 未来改进方向
## Future Improvement Directions

1. **多模型集成**：结合多种预测模型，提高预测准确性
2. **外部因素纳入**：考虑将外部因素（如饲料价格、天气等）纳入模型
3. **实时数据更新**：建立实时数据更新机制，及时反映市场变化
4. **交互式分析**：增强前端交互功能，支持用户自定义分析参数
5. **机器学习模型**：探索使用机器学习模型（如LSTM、Prophet等）提高预测性能

1. **Multi-model Integration**: Combine multiple prediction models to improve prediction accuracy
2. **Incorporation of External Factors**: Consider incorporating external factors (such as feed prices, weather, etc.) into the model
3. **Real-time Data Update**: Establish a real-time data update mechanism to reflect market changes in a timely manner
4. **Interactive Analysis**: Enhance front-end interaction functions to support user-defined analysis parameters
5. **Machine Learning Models**: Explore the use of machine learning models (such as LSTM, Prophet, etc.) to improve prediction performance

## 结论
## Conclusion

本项目通过时间序列分析和预测技术，为鲜羊肉价格的分析和预测提供了一个完整的解决方案。系统不仅能够分析历史价格数据，还能预测未来价格走势，为相关部门和企业的决策提供科学依据。

This project provides a complete solution for the analysis and prediction of fresh mutton prices through time series analysis and prediction techniques. The system can not only analyze historical price data but also predict future price trends, providing a scientific basis for decision-making by relevant departments and enterprises.

---

**项目维护**：北京市农产品价格监测系统
**最后更新**：2026年4月27日

**Project Maintenance**: Beijing Agricultural Product Price Monitoring System
**Last Update**: April 27, 2026