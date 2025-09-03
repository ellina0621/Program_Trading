library(dplyr)
library(lubridate) 
library(readxl)
library(ggplot2)
library(tidyr)
library(stats)
library(data.table)
library(TTR)  
library(zoo)
library(xts)
library(quantmod)
library(parallel)
# 2021~2023
dataStartDate <- 20220101
dataEndDate = 20241231
setwd("D:/NSYSU FIN/金融交易")
# load data
#price <- read_excel("D:/NSYSU FIN/金融交易/price.xlsx")
price <- fread("price2024_utf8.csv",header = T,stringsAsFactors = F)
# rename
new_names_price <- c("code", "date", "open", 'high', 'low', 'close', 'trade_volume', 'marketvalue')
colnames(price) = new_names_price

# data structure
df = data.frame(price)
df = df[order(price$code), ]

#time
df <- df %>%
  filter(date > 20220000 & date < 20250000)

# now only need close
df = df %>%
  select('code', 'date', 'close', 'open', 'low', 'high', 'trade_volume')
# 將 date 轉為數字格式並新增季度欄位
# 滯後財報邏輯：根據當前月份匹配相應的財報季度
get_previous_quarter <- function(date) {
  month <- as.numeric(substr(date, 5, 6))
  year <- as.numeric(substr(date, 1, 4))
  
  case_when(
    month %in% c(1, 2, 3) ~ paste0(year - 1, "Q3"), 
    month %in% c(4, 5, 6) ~ paste0(year-1 ,"Q4"),    
    month %in% c(7, 8, 9) ~ paste0(year, "Q1"),    
    month %in% c(10, 11, 12) ~ paste0(year, "Q2") 
  )
}
df<- df %>%
  mutate(quarter = get_previous_quarter(date))

###基本面資料整理###########
data_FP <- fread(file="fp20241024.csv",header = T,stringsAsFactors = F)
#data_FP <- fread(file="2024fin_utf8.csv",header = T,stringsAsFactors = F)
# 查看讀取的數據結構
str(data_FP)

colnames(data_FP)
# [1] "證券代碼"       "年月"           "每股盈餘"       "ROE(A)－稅後"  
# [5] "ROA(A)稅後息前" "負債比率"       "營業毛利率"     "營業利益率"    
# [9] "稅前淨利率"     "稅後淨利率"    
colnames(data_FP)<-c("Code","YM","EPS","ROE",
                     "ROA","Debtratio",
                     "Marginratio","ORratio",
                     "EBTratio","NIratio")
data_FP_date<-data_FP %>% arrange(desc(YM)) %>% pull(YM) %>% unique()
# 定義季度
data_FP <- data_FP %>%
  mutate(
    quarter = case_when(
      substr(YM, 5, 6) %in% c("01", "02", "03") ~ paste0(substr(YM, 1, 4), "Q1"),
      substr(YM, 5, 6) %in% c("04", "05", "06") ~ paste0(substr(YM, 1, 4), "Q2"),
      substr(YM, 5, 6) %in% c("07", "08", "09") ~ paste0(substr(YM, 1, 4), "Q3"),
      substr(YM, 5, 6) %in% c("10", "11", "12") ~ paste0(substr(YM, 1, 4), "Q4")
    )
  )

# 合併股價資料與基本面資料，基於滯後季度
merged_data <- df %>%
  inner_join(data_FP, by = c("code" = "Code", "quarter" = "quarter"))

# 計算成長率並篩選符合條件的股票
merged_data <- merged_data %>%
  group_by(code) %>% # 按股票代碼與日期分組
  mutate(
    EPS_growth = (EPS - lag(EPS)) / abs(lag(EPS)), # EPS成長率計算
    ROE_growth = (ROE - lag(ROE)) / abs(lag(ROE)) # ROE成長率計算
  ) %>%
  ungroup() %>% # 解除分組以避免後續操作受限
  filter(EPS>3, ROE_growth > 0.1, EPS_growth > 0.1) 

# 檢視篩選後的結果
head(merged_data)
# 按季度排序
sorted_data <- merged_data %>%
  arrange(quarter, code, date) # 按季度、股票代碼、日期進行排序
# 確保日期為數字
df <- df %>% mutate(date = as.numeric(date))
sorted_data <- sorted_data %>% mutate(date = as.numeric(date))

# 創建每支股票的起始日期查詢表
start_date_lookup <- sorted_data %>%
  group_by(code) %>%
  summarise(start_date = min(date)) # 每支股票的最早日期
extended_start_date_lookup <- start_date_lookup %>%
  mutate(extended_start_date = as.numeric(format(ymd(start_date) - year(1), "%Y%m%d"))) # 向前延伸一年
library(dplyr)
library(lubridate)
# 創建每支股票的起始日期查詢表
start_date_lookup <- sorted_data %>%
  group_by(code) %>%
  summarise(start_date = min(date)) # 每支股票的最早日期

# 計算延伸日期範圍
library(lubridate)

extended_start_date_lookup <- start_date_lookup %>%
  mutate(
    # 延伸的開始日期：往前一年
    extended_start_date = as.numeric(format(ymd(start_date) - years(1), "%Y%m%d")),
    # 交易開始日期：固定為 20210101
    trading_start_date = as.numeric(pmax(start_date, 20210101)),
    # 延伸的結束日期：下一季度的最後一天
    end_date = case_when(
      substr(start_date, 5, 6) %in% c("01", "02", "03") ~ paste0(substr(start_date, 1, 4), "0331"),
      substr(start_date, 5, 6) %in% c("04", "05", "06") ~ paste0(substr(start_date, 1, 4), "0630"),
      substr(start_date, 5, 6) %in% c("07", "08", "09") ~ paste0(substr(start_date, 1, 4), "0930"),
      substr(start_date, 5, 6) %in% c("10", "11", "12") ~ paste0(substr(start_date, 1, 4), "1231")
    ) %>% as.numeric()
  )

# 查看結果
print(head(extended_start_date_lookup))
# 篩選符合條件的歷史股價數據
filtered_price <- df %>%
  inner_join(extended_start_date_lookup, by = "code") %>% # 加入股票的 `extended_start_date` 和 `end_date`
  filter(date >= extended_start_date & date <= end_date) %>% # 使用延伸日期作為篩選條件
  arrange(code, date) # 按代碼和日期排序

# 查看結果
print(head(filtered_price))

########技術分析#############
# 定義標準K棒所需的交易日數
stop_loss_rate<-0.05  # 下跌5%停損 
standardKdays <- 60   # 標準K棒平均日數
standardKbar <- 1.1
standardupline <- 0.1
stop_loss_rate <- 0.05
MA5vol_threshold <- 1000
ss <- 0.01  # 一星二陽的誤差範圍

library(dplyr)
library(TTR) # 提供 SMA 函數
library(zoo) # 提供 rollmean 函數
filtered_price <- filtered_price %>%
  group_by(code) %>%
  arrange(code,date) %>%
  filter(n()>=standardKdays) %>%        # 股票交易日數需大於標準K棒平均日數                                      
  mutate(MA5=SMA(close,5),              # 5日移動平均線
         MA20=SMA(close,20),            # 20日移動平均線
         MA60=SMA(close,60),            # 60日移動平均線
         kbar=abs(close/open-1),        # K棒絕對值大小 
         #standardKbar=rollmean(x=kbar, k=standardKdays,fill=NA, align = "right"), # 計算標準K棒值
         
         standardKbar=runMean(x=kbar, n=standardKdays), # 計算標準K棒值
         
         lagkbar1=lag(kbar,1),          # 前一天K棒大小
         lagkbar2=lag(kbar,2),          # 前二天K棒大小
         lagOpen1=lag(open,1),          # 前一天開盤價
         lagOpen2=lag(open,2),          # 前二天開盤價
         lagClose1=lag(close,1),        # 前一天收盤價
         lagClose2=lag(close,2),        # 前二天收盤價
         leadOpen1=lead(open,1),        # 隔日開盤價
         MA5vol = SMA(trade_volume,5),  # 五日移動平均成交量
         Buy_date=lead(date,1),         # 買進日期(隔日)
         LagMA60 = lag(MA60,1),         # 前一天MA60
         LagMA20 = lag(MA20,1),         # 前一天MA20
         LagMA5 = lag(MA5,1),            # 前一天MA5
         #upline_ratio = (high-pmax(open,close))/(pmax(open,close)-pmin(open,close))  #上影線佔實體最高價
         upline = (high-pmax(open,close))/pmax(open,close),  #上影線佔實體最高價,
         
         standardupline=runMean(x=upline, n=standardKdays) # 計算上影線比例
         
         
         
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
filtered_price <- filtered_price %>%
  mutate(signalDate = as.numeric(start_date))  # 確保日期為數字格式

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
    trade_volume >= MA5vol * 1.2
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
    trade_volume >= MA5vol * 1.5,           # 爆量條件
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
         trade_volume>=MA5vol*1.2 ,  #爆量
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
    lag(trade_volume, 1) > lag(MA5vol, 1) * vol_multiplier, # 前一天爆量條件
    
    # 第二隻腳條件
    close > lag(close, 1),                  # 當天收盤價高於前一天收盤價（模擬第二隻腳）
    trade_volume > MA5vol * vol_multiplier, # 當天成交量再次爆量
    
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

######策略9##########
tradeTargetTable1 <- filtered_price %>% 
  filter(
    # 第一隻腳：收盤價低於前兩天的最低點（模擬第一隻腳的底部）
    close < lag(close, 1) & close < lag(close, 2),
    # 第二隻腳：收盤價高於前一天的收盤價（模擬第二隻腳）
    close > lag(close, 1),
    # 第二隻腳的最低點大於第一隻腳的最低點
    lag(close, 1) > lag(close, 2),
    # 成交量條件：第二隻腳成交量需要爆量
    trade_volume > MA5vol * 1.5,  # 成交量為5日均量的1.5倍以上
    MA5vol > MA5vol_threshold,    # 5日均量門檻
    # 確保W型的第二腳比第一腳高
    close > lag(close, 2),       # 第二腳的收盤價要高於第一腳的收盤價
    # 進一步確認底部的K棒形態
    kbar >= standardKbar * 1.1,  # K棒大於標準K棒
    lagkbar1 <= standardKbar * 0.3,  # 前一天K棒小於標準K棒
    # 上影線比例小於標準比例
    )%>%
    select(
      code,                      # 股票代碼
      signalDate = date,         # 訊號產生日期
      inDate = Buy_date,         # 進場日期
      inPrice = leadOpen1        # 隔日開盤價作為進場價格
      ) %>%
      mutate(kbar_type = "double_bottom") %>%  # 標記策略名稱
      na.omit()  # 刪除缺失值
    
    # 如果需要合併到主交易表
tradeTargetTable <- bind_rows(tradeTargetTable, tradeTargetTable1)
print(tradeTargetTable)







# ####### 查看結果 #######
print(tradeTargetTable)

# 篩選出2021年及之後的交易資訊
tradeTargetTable <- tradeTargetTable %>%
  filter(signalDate >= 20240101)

# 查看篩選後的交易資訊
print(tradeTargetTable)

# ####### 出場邏輯 #######

# 初始化交易明細表
tradeDetailTable <- data.frame()

# 停利率與停損率
take_profit_rate <- 0.08  # 停利點 8%(想說漲差不多9.多%就漲停了，因此用8%較保險，以免不掉)
stop_loss_rate <- 0.05    # 停損點 5%

# 遍歷每種 K 棒型態
kbar_type_list <- unique(tradeTargetTable$kbar_type)
ptm <- Sys.time()

for (kbar_type in kbar_type_list) {
  tradeTarget <- tradeTargetTable %>%
    filter(kbar_type == kbar_type) %>%
    arrange(signalDate)
  
  for (ix in 1:nrow(tradeTarget)) {
    inDate <- tradeTarget$inDate[ix]
    stockCode <- tradeTarget$code[ix]
    inPrice <- tradeTarget$inPrice[ix]
    
    outData <- filtered_price %>%
      filter(code == stockCode, date >= inDate) %>%
      select(code, date, open, close, MA5) %>%
      mutate(
        sell_date = NA,
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
        outData$sell_date[nm] <- NA
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

# 移除重複項目，保留第一筆出現的交易
tradeDetailTable <- tradeDetailTable %>%
  distinct(code, inDate, .keep_all = TRUE)

cat("執行時間: ", Sys.time() - ptm, "\n")

# 查看結果
print(head(tradeDetailTable))


# ####### 計算績效 #######
buyCostR <- 0.001425 * 0.25
sellCostR <- 0.003 + 0.001425 * 0.25

tradeDetailTable1 <- tradeDetailTable %>%
  mutate(
    ret = sell_close * (1 - sellCostR) / (inPrice * (1 + buyCostR)) - 1
  ) %>%
  group_by(kbar_type) %>%
  summarise(
    Count = n(),
    Avg_return = mean(ret, na.rm = TRUE),
    Prob_win = mean(ret >= 0, na.rm = TRUE)
  )

# 查看績效結果
print(tradeDetailTable1)


### 日期轉換函數
DateConvert <- function(x){
  return(as.Date(paste(substring(x,1,4),substring(x,5,6),substring(x,7,8),sep="-",format="%Y%m%d")))
}
###### 持有期間日數
tradeDetailTable$holdDays <- as.numeric(DateConvert(tradeDetailTable$sell_date)-DateConvert(tradeDetailTable$inDate))


###### 儲存歷史交易明細表
write.csv(tradeDetailTable,"tradeDetailTable_14K_10D.RData",row.names = F)

### 繪製交易技術分析圖形
PlotGraph <- function(plotSample){
  
  # 繪製交易的股票代碼
  plotCode <- tradeDetailTable$code[plotSample]
  inDate <- tradeDetailTable$inDate[plotSample]
  outDate <- tradeDetailTable$sell_date[plotSample]
  
  # 整理該股票的股價資料
  stockData <- filtered_price[which(filtered_price$code==plotCode),]
  stockData <- stockData[,c("date","open","high","low","close","trade_volume","MA5","MA20","MA60")] # 取出繪圖所需資料(開高收低成交量)
  
  # 繪圖起始日
  matchSite <- which(stockData$date==inDate)-20
  plotStartDate <- stockData$date[ifelse(matchSite<1, 1, matchSite)]  # 此處用ifelse避免資料超出邊界
  
  # 繪圖結束日
  matchSite <- which(stockData$date==outDate)+20
  plotEndDate <- stockData$date[ifelse(matchSite>nrow(stockData), nrow(stockData), matchSite)] # 此處用ifelse避免資料超出邊界
  
  # 要繪製的股價資料範圍
  plotData <- stockData[which((stockData$date>=plotStartDate)&(stockData$date<=plotEndDate)),]
  
  # 加入進場位置資訊
  plotData$inSite <- rep(NA, nrow(plotData))
  plotData$inSite[which(plotData$date==inDate)] <- plotData$open[which(plotData$date==inDate)]
  
  # 加入出場位置資訊
  plotData$outSite <- rep(NA, nrow(plotData))
  plotData$outSite[which(plotData$date==outDate)] <- plotData$close[which(plotData$date==outDate)]
  
  # 日期轉換函數
  DateConvert <- function(x){
    return(as.Date(paste(substring(x,1,4),substring(x,5,6),substring(x,7,8),sep="-",format="%Y%m%d")))
  }
  # 加入持有天數資訊
  plotData$holdDays <- as.numeric(DateConvert(plotData$date[which(plotData$date==outDate)]) -DateConvert(plotData$date[which(plotData$date==inDate)])) 
  
  
  # 將plotData資料由data.frame格式轉為xts，符合繪圖資料格式要求
  plotData <- xts(plotData[,-1], order.by= as.Date(ISOdate(year=substring(plotData$date,1,4),
                                                           month=substring(plotData$date,5,6),
                                                           day=substring(plotData$date,7,8)),format="%Y%m%d")) 
  
  # 繪製技術分析圖形
  # 設定K棒顏色
  myTheme <- chart_theme()
  myTheme$col$dn.col <- c("chartreuse3")  # 跌K棒顏色
  myTheme$col$up.col <- c("firebrick3")   # 漲K棒顏色
  
  # 繪製主圖形
  chart_Series(plotData[,1:5], name=paste0(plotCode," 技術分析圖"), theme=myTheme)
  # 加入成交量
  add_Vo()
  # 加入5日移動平均線
  add_TA(plotData$MA5, on=1, type="l", col="blue", lwd=1.5)
  # 加入20日移動平均線
  add_TA(plotData$MA20, on=1, type="l", col="orange", lwd=1.5)
  # 加入60日移動平均線
  add_TA(plotData$MA60, on=1, type="l", col="green", lwd=1.5)
  # 標註進場位置
  add_TA(plotData$inSite, on=1, type="p", col="red", pch=2, cex=5, lwd=3)
  # 標註出場位置
  add_TA(plotData$outSite, on=1, type="p", col="green", pch=6, cex=5, lwd=3)
  # 標註持有天數
  # graphics::text(0, 10.00, paste0("持有天數 :",plotData$holdDays[1]," 天"), adj=0,pch=10,cex=2)
  
}

#### 修正異常交易
###### (1) 持有天數大於20天
tradeDetailTable[which(tradeDetailTable$holdDays>20),]

# code  signalDate   inDate inPrice kbar_type  sell_date sell_close sell_price     ret holdDays
# <chr>      <int>    <int>   <dbl> <chr>          <int>      <dbl>      <dbl>   <dbl>    <dbl>
# 1 2341    20101223 20101224    1.53 swa         20110803       1.99       1.77  0.293       222
# 2 2341    20101223 20101224    1.53 single      20110803       1.99       1.77  0.293       222
# 3 2025    20160130 20160201    1.77 single      20160316       1.74       1.72 -0.0227       44
# 4 4530    20101102 20101103   12.8  gravestone  20101227      19.1       17.0   0.492        54
# 5 2025    20180910 20180911    2.15 gravestone  20190408       1.94       2.04 -0.103       209

PlotGraph(plotSample = 10)


HolidayTrade <- tradeDetailTable %>% filter(holdDays>8) %>% mutate(YYYYMM = substring(inDate,1,6))

HolidayTrade$YYYYMM %>% table()
# 200709 200801 200802 200812 200901 200905 201002 201009 201101 201104 201201 201212 201302 201303 
# 97    346    162    125    402    176    850      2    774    201    687    258    861    168 
# 201309 201401 201412 201502 201509 201601 201602 201604 201606 201609 201701 201702 201703 201710 
# 152    940    317    830     62    215    495    133    220    263    874    162    263    885 
# 201802 201803 201804 201812 201901 201902 201903 201910 
# 761    408    397    135    874    196    215    314 

tradeDetailTable[which(tradeDetailTable$holdDays>11),10] <- 10

###### 檢視原始策略績效
tradeDetailTable <- tradeDetailTable %>%
  mutate(
    ret=sell_close*(1-sellCostR)/(inPrice*(1+buyCostR))-1
  )
tradeDetailperformance <- tradeDetailTable %>% 
  mutate(Year = substring(inDate,1,4)) %>% 
  group_by(kbar_type) %>%
  summarise(Avg_Ret = round(mean(ret),4)*100,
            prob_win = round(mean(ret > 0),4)*100, 
            avg_HoldDays = round(mean(holdDays),2),
            Count = n(),
            Max_Ret = round(max(ret),4)*100,
            Min_Ret = round(min(ret),4)*100,
            sd_Ret = round(sd(ret),4)*100,
            max_HoldDays = round(max(holdDays),2)
  )

# 看各K棒型態交易次數比例
tradeTargetTable %>% 
  group_by(kbar_type) %>%
  summarise(Count = n()) %>% 
  mutate(per = Count / sum(Count)) %>% 
  ggplot(aes(x="", y=per, fill=kbar_type)) +
  geom_bar(stat = "identity")+     # 先畫bar plot
  coord_polar(theta = "y")+        # 再沿著Y，轉軸成圓餅圖
  # geom_text(aes(x = 1.3, y = cumsum(per) - per/2 , label = kbar_type))+
  labs(title = "各K棒型態交易次數比例")+
  theme_void()


write.csv(tradeDetailperformance,"tradeDetailperformance_byType.csv",row.names = F)

#########年化報酬率#############
# 計算個別策略的年化報酬率
selected_strategies <- tradeDetailTable %>%
  filter(kbar_type %in% c("double_bottom_vol", "jump", "trap","swa")) %>%
  group_by(kbar_type) %>%
  summarise(
    Avg_Ret = mean(ret, na.rm = TRUE), 
    Avg_HoldDays = mean(holdDays, na.rm = TRUE)
  ) %>%
  mutate(
    Annualized_Ret = round(((1 + Avg_Ret)^(365 / Avg_HoldDays) - 1) * 100, 2)
  )

# 顯示個別策略的年化報酬率
print(selected_strategies)

# 計算合併策略的年化報酬率
combined_avg_ret <- mean(selected_strategies$Avg_Ret, na.rm = TRUE)
combined_avg_days <- mean(selected_strategies$Avg_HoldDays, na.rm = TRUE)
combined_annualized_ret <- round(((1 + combined_avg_ret)^(365 / combined_avg_days) - 1) * 100, 2)

# 顯示合併策略的年化報酬率
cat("合併策略的年化報酬率為:", combined_annualized_ret, "%\n")
