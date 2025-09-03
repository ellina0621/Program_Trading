rm(list=ls());gc()

options(scipen=999)

# 讀取套件
library(data.table)
library(tidyquant)
library(magrittr)
library(quantmod)
library(RMySQL)
library(parallel)
library(xgboost)
library(caret)
library(readxl)
library(dplyr)
library(TTR)
library(zoo)
library(ggplot2)
library(xts)
library(tidyr)

setwd("D:/NSYSU FIN/金融交易/20241212")

load(file="sp20241211.rdata")
load(file="K_bar_Result_20241212.rdata")

financial <- read_excel("D:/NSYSU FIN/金融交易/new/OOP_5/NEWW.xlsx")
price<-read_excel("D:/NSYSU FIN/金融交易/new/OOP_5/newprice.xlsx")
# 更改欄位名稱
colnames(price) <- c("code", "date", "open", "high", "low", "close", "volume")

# 2. 提取圖 1 中的年月
price <- price %>%
  mutate(date_ym = substr(date, 1, 6))  # 提取年月
# 4. 匹配數據
# 將兩個 date_ym 都轉換為字符類型
price <- price %>%
  mutate(date_ym = as.character(date_ym))

financial <- financial %>%
  mutate(date_ym = as.character(date_ym))
merged_data <- price %>%
  left_join(financial, by = c("code", "date_ym"))

# 5. 計算每日報酬
merged_data <- merged_data %>%
  group_by(code) %>%
  arrange(date) %>%
  mutate(daily_return = (close - lag(close)) / lag(close)) %>%
  ungroup()
factor_columns <- colnames(merged_data)[grepl("Tobins_Q|growth|rank|PEG|Debt_Ratio|ROE|EPS|turnover|marketvalue", colnames(merged_data))]
print(factor_columns)

for (factor in factor_columns) {
  merged_data <- merged_data %>%
    mutate(!!paste0(factor, "_quantile") := ntile(!!sym(factor), 10))  # 分為 10 分位數
}
merged_data <- merged_data %>% 
  drop_na()

# 修改因子方向
merged_data <- merged_data %>%
  mutate(Debt_Ratio = -Debt_Ratio)


# 1. 設定因子名稱與權重
factor_columns <- c("turnover", "Tobins_Q", "BS", "EPS_growth", 
                    "PEG", "ROE_growth", "marketvalue", "Debt_Ratio")

# 權重，從最高的 Turnover 到最低的 Debt_Ratio
factor_weights <- c(0.4, 0.05, 0.1, 0.15, 0.15, 0.075, 0.05, 0.025)

# 2. 計算綜合因子
merged_data <- merged_data %>%
  mutate(composite_factor = rowSums(cbind(
    turnover * factor_weights[1],
    Tobins_Q * factor_weights[2],
    BS * factor_weights[3],
    EPS_growth * factor_weights[4],
    PEG * factor_weights[5],
    ROE_growth * factor_weights[6],
    marketvalue * factor_weights[7],
    Debt_Ratio * factor_weights[8]
  ), na.rm = TRUE))

# 每季選出前10分位的股票作為 stockpool
stockpool <- merged_data %>%
  group_by(quarter, code) %>%
  summarise(composite_factor = mean(composite_factor, na.rm = TRUE)) %>%
  group_by(quarter) %>%
  filter(ntile(composite_factor, 10) == 10) %>%
  ungroup()

#######技術面##########
########技術分析#############
# 定義標準K棒所需的交易日數
stop_loss_rate<-0.05  # 下跌5%停損 
standardKdays <- 60   # 標準K棒平均日數
standardKbar <- 1.1
standardupline <- 0.1
stop_loss_rate <- 0.05
MA5vol_threshold <- 1000
ss <- 0.01  # 一星二陽的誤差範圍

# 過濾技術分析的價格數據僅限於 stockpool
merged_data <- merged_data %>%
  filter(code %in% stockpool$code)

# 定義技術分析所需的參數
stop_loss_rate <- 0.05  # 下跌5%停損 
standardKdays <- 60     # 標準K棒平均日數
standardKbar <- 1.1
standardupline <- 0.1
MA5vol_threshold <- 1000
ss <- 0.01  # 一星二陽的誤差範圍

# 計算技術分析指標
merged_data <- merged_data %>%
  group_by(code) %>%
  arrange(code, date) %>%
  filter(n() >= standardKdays) %>%        # 股票交易日數需大於標準K棒平均日數                                      
  mutate(MA5 = SMA(close, 5),              # 5日移動平均線
         MA20 = SMA(close, 20),            # 20日移動平均線
         MA60 = SMA(close, 60),            # 60日移動平均線
         kbar = abs(close / open - 1),     # K棒絕對值大小 
         standardKbar = runMean(x = kbar, n = standardKdays), # 計算標準K棒值
         lagkbar1 = lag(kbar, 1),          # 前一天K棒大小
         lagkbar2 = lag(kbar, 2),          # 前二天K棒大小
         lagOpen1 = lag(open, 1),          # 前一天開盤價
         lagOpen2 = lag(open, 2),          # 前二天開盤價
         lagClose1 = lag(close, 1),        # 前一天收盤價
         lagClose2 = lag(close, 2),        # 前二天收盤價
         leadOpen1 = lead(open, 1),        # 隔日開盤價
         MA5vol = SMA(volume, 5),    # 五日移動平均成交量
         Buy_date = lead(date, 1),         # 買進日期(隔日)
         LagMA60 = lag(MA60, 1),           # 前一天MA60
         LagMA20 = lag(MA20, 1),           # 前一天MA20
         LagMA5 = lag(MA5, 1),             # 前一天MA5
         upline = (high - pmax(open, close)) / pmax(open, close),  # 上影線佔實體最高價,
         standardupline = runMean(x = upline, n = standardKdays)   # 計算上影線比例
  ) %>% 
  ungroup()


########進場交易策略###########
library(dplyr)

# 初始化交易資訊表
tradeTargetTable <- data.frame()

# 設定參數
standardKbar <- 1.1
standardupline <- 0.1
stop_loss_rate <- 0.05
MA5vol_threshold <- 1000
ss <- 0.01  # 一星二陽的誤差範圍


# ####### 低檔轉折向上組合 #######
library(dplyr)
library(TTR) # 用於移動平均 (SMA)

# 將日期轉換為數字以進行比較（如果尚未完成）
filtered_price <- merged_data
# 策略 1: 長紅吞噬
tradeTargetTable1 <- filtered_price %>%
  filter(
    close > lagOpen1,
    open < lagClose1,
    close > open,
    lagClose1 < lagOpen1,
    kbar >= standardKbar * 1.1,
    lagkbar1 >= standardKbar * 0.3,
    MA5vol > MA5vol_threshold
  ) %>%
  select(code, signalDate = date, inDate = Buy_date, inPrice = leadOpen1) %>%
  mutate(kbar_type = "swa") %>%
  na.omit()

tradeTargetTable <- bind_rows(tradeTargetTable, tradeTargetTable1)

# 策略 2: 旭日東昇
tradeTargetTable1 <- filtered_price %>%
  filter(
    close > open,
    lagClose1 < lagOpen1,
    close > lagOpen1,
    open >= (lagOpen1 + lagClose1) / 2,
    open < lagOpen1,
    kbar >= standardKbar * 1.2,
    lagkbar1 >= standardKbar * 0.8,
    upline <= standardupline,
    MA5vol > MA5vol_threshold,
    volume >= MA5vol * 1.2
  ) %>%
  select(code, signalDate = date, inDate = Buy_date, inPrice = leadOpen1) %>%
  mutate(kbar_type = "sun") %>%
  na.omit()

tradeTargetTable <- bind_rows(tradeTargetTable, tradeTargetTable1)

# 策略 3: 長紅跳空
tradeTargetTable1 <- filtered_price %>%
  filter(
    close > open,
    lagClose1 < lagOpen1,
    open > lagOpen1,
    MA5vol > MA5vol_threshold
  ) %>%
  select(code, signalDate = date, inDate = Buy_date, inPrice = leadOpen1) %>%
  mutate(kbar_type = "jump") %>%
  na.omit()

tradeTargetTable <- bind_rows(tradeTargetTable, tradeTargetTable1)

# 策略 4: 一星二陽
tradeTargetTable1 <- filtered_price %>%
  filter(
    lagClose2 > lagOpen2,                   # 第t-2天的收盤價>開盤價 (紅K)
    close > open,                           # 第t天的收盤價>開盤價 (紅K)
    lagClose2 > (lagOpen1 * (1 - ss)),      # 第t-2天的收盤價接近第t-1天的開盤價
    lagClose2 < (lagOpen1 * (1 + ss)),      
    open > (lagClose1 * (1 - ss)),          # 第t天的開盤價接近第t-1天的收盤價
    open < (lagClose1 * (1 + ss)),
    kbar >= standardKbar * 1.1,             # 第t天的K棒大小
    lagkbar2 >= standardKbar,               # 第t-2天的K棒大小
    lagkbar1 <= (standardKbar * 0.3),       # 第t-1天的K棒小於標準K棒
    upline <= 0.1,                    # 上影線比例
    volume >= MA5vol * 1.5,           # 爆量條件
    MA5vol >= MA5vol_threshold
  ) %>%
  select(code, signalDate = date, inDate = Buy_date, inPrice = leadOpen1) %>%
  mutate(kbar_type = "one") %>%
  na.omit()

tradeTargetTable <- bind_rows(tradeTargetTable, tradeTargetTable1)

# 策略 5: 紅三兵
tradeTargetTable1 <- filtered_price %>%
  filter(
    close > open,                           # 第t日為紅K
    lagClose1 > lagOpen1,                   # 第t-1日為紅K
    lagClose2 > lagOpen2,                   # 第t-2日為紅K
    lagClose1 > lagClose2,                  # 第t-1日收盤價大於第t-2日收盤價
    close > lagClose1,                      # 第t日收盤價大於第t-1日收盤價
    lagkbar2 >= standardKbar,               # 第t-2日黑K大於標準K棒
    kbar >= standardKbar * 1.1,             # 第t日紅K大於標準K棒
    lag(upline, 0) <= 0.1,
    lag(upline, 1) <= 0.1,
    lag(upline, 2) <= 0.1,
    MA5vol > MA5vol_threshold
  ) %>%
  select(code, signalDate = date, inDate = Buy_date, inPrice = leadOpen1) %>%
  mutate(kbar_type = "redsoldier") %>%
  na.omit()

tradeTargetTable <- bind_rows(tradeTargetTable, tradeTargetTable1)
####策略6 內困三紅
tradeTargetTable1 <- filtered_price  %>%
  filter(
    close > open,                    # 第t日為紅K 
    lagClose1 > lagOpen1,            # 第t-1日為紅K
    lagClose2 < lagOpen2,            # 第t-2日為黑K
    lagClose2 < lagOpen1,            # 第t-2日的長黑K包住第t-1日紅K
    lagOpen2 > lagClose1,            
    close > lagOpen2,                # 第t日收盤價大於第t-2日開盤價  
    open > lagOpen1,                 
    open < lagClose1,            
    upline<=standardupline ,
    #upline_ratio<=0.1,               
    kbar >= lagkbar2,                    # 第t日K棒大小要大於第t-2日K棒大小
    lagkbar2 >= standardKbar*1.1,        # 第t-2日K棒要大於標準K棒
    # kbar >= standardKbar*1.2,
    volume>=MA5vol*1.2 ,  #爆量
    MA5vol>100
  ) %>% 
  select(code, signalDate=date,inDate=Buy_date,inPrice=leadOpen1) %>% 
  mutate(kbar_type = "trap") %>% 
  na.omit()
# 併入交易資訊表
tradeTargetTable <- bind_rows(tradeTargetTable,tradeTargetTable1)


# 策略7 雙腳大量反彈突破月線
vol_multiplier <- 1.5    # 爆大量的倍數
MA20_period <- 20        # 月線（20日均線）
MA5vol_threshold <- 1000 # 最低平均成交量標準

# 策略 7: 雙腳大量反彈突破月線（無 low 欄位）
tradeTargetTable1 <- filtered_price %>%
  filter(
    # 第一隻腳條件
    lag(close, 1) < lag(close, 2),          # 前一天收盤價低於前二天收盤價（模擬第一隻腳）
    lag(volume, 1) > lag(MA5vol, 1) * vol_multiplier, # 前一天爆量條件
    
    # 第二隻腳條件
    close > lag(close, 1),                  # 當天收盤價高於前一天收盤價（模擬第二隻腳）
    volume > MA5vol * vol_multiplier, # 當天成交量再次爆量
    
    # 收盤價突破月線
    close > LagMA20,                        # 收盤價突破20日均線（月線）
    
    # 成交量門檻
    MA5vol > MA5vol_threshold
  ) %>%
  select(
    code,                     # 股票代碼
    signalDate = date,        # 訊號產生日期
    inDate = Buy_date,        # 進場日期
    inPrice = leadOpen1       # 隔日開盤價作為進場價格
  ) %>%
  mutate(kbar_type = "double_bottom_vol") %>% # 添加策略標記
  na.omit()

# 如果需要合併到主交易表
tradeTargetTable <- bind_rows(tradeTargetTable, tradeTargetTable1)

# ####### 查看結果 #######


# ####### 初始化交易明細表 #######
tradeDetailTable <- data.frame()

# 停利率與停損率
take_profit_rate <- 0.08  # 停利點 8%
stop_loss_rate <- 0.05    # 停損點 5%

# 遍歷每種 K 棒型態
kbar_type_list <- unique(tradeTargetTable$kbar_type)

ptm <- Sys.time()

for (kbar_type in kbar_type_list) {
  # 篩選出當前 K 棒型態的交易目標
  tradeTarget <- tradeTargetTable %>%
    filter(kbar_type == kbar_type) %>%
    arrange(signalDate)
  
  for (ix in 1:nrow(tradeTarget)) {
    inDate <- tradeTarget$inDate[ix]
    stockCode <- tradeTarget$code[ix]
    inPrice <- tradeTarget$inPrice[ix]
    
    # 初始化數據，顯式設定 sell_date 為 character 類型
    outData <- filtered_price %>%
      filter(code == stockCode, date >= inDate) %>%
      select(code, date, open, close, MA5) %>%
      mutate(
        sell_date = as.character(NA),  # 顯式設定為 character
        sell_price = NA,
        sell_close = NA
      )
    
    stop_price <- inPrice * (1 - stop_loss_rate)
    take_profit_price <- inPrice * (1 + take_profit_rate)
    nm <- nrow(outData)
    
    for (iDate in 1:nm) {
      current_close <- outData$close[iDate]
      
      if (iDate >= nm) {
        # 最後一日未觸發停損或停利條件
        outData$sell_date[nm] <- as.character(NA)
        outData$sell_close[nm] <- NA
        outData$sell_price[nm] <- NA
        outData <- outData %>%
          select(sell_date, sell_close, sell_price) %>%
          na.omit()
      } else if (current_close >= take_profit_price) {
        # 停利條件觸發
        outData$sell_date[iDate] <- outData$date[iDate]
        outData$sell_close[iDate] <- current_close
        outData$sell_price[iDate] <- take_profit_price
        outData <- outData %>%
          select(sell_date, sell_close, sell_price) %>%
          na.omit()
        break
      } else if (current_close <= stop_price) {
        # 停損條件觸發
        outData$sell_date[iDate] <- outData$date[iDate]
        outData$sell_close[iDate] <- current_close
        outData$sell_price[iDate] <- stop_price
        outData <- outData %>%
          select(sell_date, sell_close, sell_price) %>%
          na.omit()
        break
      } else if (iDate >= 10) {
        # 超過 10 天直接以當日收盤價出場
        outData$sell_date[iDate] <- outData$date[iDate]
        outData$sell_close[iDate] <- current_close
        outData$sell_price[iDate] <- current_close
        outData <- outData %>%
          select(sell_date, sell_close, sell_price) %>%
          na.omit()
        break
      } else {
        # 更新停損價格
        stop_price <- max(stop_price, current_close * (1 - stop_loss_rate))
      }
    }
    
    # 確認是否已有重複記錄，避免新增多次
    if (!any(tradeDetailTable$code == stockCode & tradeDetailTable$inDate == inDate)) {
      tradeDetailTable <- bind_rows(tradeDetailTable, bind_cols(tradeTarget[ix, ], outData))
    }
  }
}

print(tradeDetailTable)

# ####### 計算績效 #######
# 確認 tradeDetailTable 並正確選擇欄位
tradeDetailTable <- tradeDetailTable %>%
  select(code, signalDate, inDate, inPrice, kbar_type, sell_date, sell_close, sell_price)

# 計算交易績效
buyCostR <- 0.001425 * 0.25
sellCostR <- 0.003 + 0.001425 * 0.25

# 計算報酬率
tradeDetailTable <- tradeDetailTable %>%
  mutate(
    ret = (sell_close * (1 - sellCostR)) / (inPrice * (1 + buyCostR)) - 1
  )

# 彙總績效結果
tradePerformance <- tradeDetailTable %>%
  group_by(kbar_type) %>%
  summarise(
    total_trades = n(),
    avg_return = mean(ret, na.rm = TRUE),
    prob_win = mean(ret > 0, na.rm = TRUE)
  )

# 查看交易績效
print(tradePerformance)

# 查看交易詳細資料
print(tradeDetailTable)


# 初始化結果表
rolling_results <- data.frame()

# 定義樣本年份範圍，切成3:1
start_year <- 2013
end_year <- 2024
window_size <- 3  # 樣本內窗口大小

# 滾動式迴圈
for (train_start in start_year:(end_year - window_size)) {
  
  # 定義樣本內與樣本外年份
  train_end <- train_start + window_size - 1
  test_year <- train_end + 1
  
  # 樣本內數據
  train_data <- tradeDetailTable %>%
    filter(as.numeric(substr(inDate, 1, 4)) >= train_start,
           as.numeric(substr(inDate, 1, 4)) <= train_end)
  
  # 樣本外數據
  test_data <- tradeDetailTable %>%
    filter(as.numeric(substr(inDate, 1, 4)) == test_year)
  
  # 如果樣本內或樣本外資料不足，跳過
  if (nrow(train_data) < 10 || nrow(test_data) < 10) {
    next
  }
  
  # 準備特徵和標籤
  train_x <- train_data %>%
    select(-c(code, signalDate, inDate, sell_date, kbar_type, ret)) %>%
    as.matrix()
  train_y <- ifelse(train_data$ret > 0, 1, 0)  # 將目標設為分類（正報酬 vs 負報酬）
  
  test_x <- test_data %>%
    select(-c(code, signalDate, inDate, sell_date, kbar_type, ret)) %>%
    as.matrix()
  test_y <- ifelse(test_data$ret > 0, 1, 0)
  
  # 訓練 XGBoost 模型
  dtrain <- xgb.DMatrix(data = train_x, label = train_y)
  dtest <- xgb.DMatrix(data = test_x, label = test_y)
  
  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  xgb_model <- xgb.train(params, dtrain, nrounds = 100, watchlist = list(train = dtrain, test = dtest), verbose = 0)
  
  # 預測樣本外
  predictions <- predict(xgb_model, dtest)
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  
  # 計算樣本外績效
  accuracy <- mean(predicted_classes == test_y)
  profit <- sum(test_data$ret * (predicted_classes == 1), na.rm = TRUE)
  
  # 儲存結果
  rolling_results <- rbind(rolling_results, data.frame(
    train_start = train_start,
    train_end = train_end,
    test_year = test_year,
    accuracy = accuracy,
    total_profit = profit
  ))
}

# 查看滾動結果
print(rolling_results)

# 視覺化滾動績效
library(ggplot2)
ggplot(rolling_results, aes(x = test_year, y = accuracy)) +
  geom_line() +
  geom_point() +
  labs(title = "滾動式樣本外預測準確率", x = "測試年份", y = "準確率")