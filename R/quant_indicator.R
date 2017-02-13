

## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
################################################################################
#########################      BackTesting #6:    ##############################
#########################       KPI Analysis      ##############################
#########################      策略回测指标计算   ##############################
################################################################################
#` @
#` @
#
## -----------------------------------------------------------------------------



dataInfor <- backTestingInfor
dataInfor$Date <- as.Date(dataInfor$Date)
str(dataInfor)

dataInfor

## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
################################################################################
#########################      BackTesting #6.1:    ############################
#########################       KPI Analysis      ##############################
#########################       交易概况.sheet    ##############################
################################################################################
#` @交易概况.sheet
## -----------------------------------------------------------------------------

#` @交易概况.sheet
#``````````````````````````````````````````````````````````````````````````````

## 回测开始时间
start_time <- dataInfor[1,Date]
## 回测结束时间
end_time <- dataInfor[.N,Date]
## 初始资金
initial_capital <- initial_capital %>% as.numeric(.)
## 期末资金
ending_capital <- dataInfor[.N,Net_Value]
## 回测年数
years_count <- unique(substr(dataInfor$Date, 1, 4)) %>% length(.)
## 回测月数
months_count <- unique(substr(dataInfor$Date, 1, 7)) %>% length(.)
## 回测天数
## days_conunt <- as.POSIXct(end_time, format = "%Y%m%d") -
##   as.POSIXct(start_time, format = "%Y%m%d")
days_conunt <- nrow(dataInfor)

year <- unique(substr(dataInfor$Date, 1, 4))
month <- unique(substr(dataInfor$Date, 1, 7))
day <- unique(dataInfor$Date)


## summary_abount <- data.table("指标" = c("回测开始时间", "回测结束时间",
##                                       "初始资金", "期末资金",
##                                       "回测年数", "回测月数", "回测天数"),
##                              "内容" = c(as.character(start_time), 
##                                       as.character(end_time),
##                                       sprintf("%.2f", initial_capital), sprintf("%.2f", ending_capital),
##                                       years_count, months_count, days_conunt))
## -----------------------------------------------------------------------------
## 收益统计.sheet

## 净收益
net_revenue <- ending_capital - initial_capital
net_return <- net_revenue / initial_capital
annual_return <- net_return / as.numeric(days_conunt) * 365
compounding_annual_return <- (1+net_return)^{1 / (as.numeric(days_conunt) / 250)} - 1



## 读取每笔交易的盈亏数据
return.infor <- returnInfor

## 交易次数：平仓算一次
transactions <- nrow(return.infor)
## 盈利次数
winners <- return.infor[Return >= 0] %>% nrow()
## 亏损次数
losers <- return.infor[Return < 0] %>% nrow()

## 胜率
winning_rate <- winners / transactions
##赔率

## 盈利交易的盈利总额
total_profit <- return.infor[Profit >=0][,sum(Profit)]
## 亏损交易的亏损总额
total_loss <- return.infor[Profit <0][,sum(Profit)]
## 交易费用总额
## total_fee

## 盈利系数
profit_factor <- total_profit / total_loss %>% abs(.)

## 单笔平均盈利金额
average_profit <- total_profit / winners
##单笔平均亏损金额
average_loss <- total_loss / losers
## 赔率
payoff_ratio <- average_profit / abs(average_loss)

## 单笔最大盈利额
max_profit <- return.infor[,max(Profit)]
## 单笔最大收益率
max_profit_percentage <- return.infor[,max(Return)]
## 单笔最大亏损额
max_loss <- return.infor[,min(Profit)]
## 单笔最大亏损率
max_loss_percentage <- return.infor[,min(Return)]

## 最大回撤金额
max_DD_amount <- dataInfor[,max(Draw_Down)]
## 最大回撤比例
max_DD_percentage <- dataInfor[,max(Per_Draw_Down)]
## 恢复比例
recovery_factor <- net_revenue / max_DD_amount

compounding_annual_return_max_DD_percentage <- compounding_annual_return /
  max_DD_percentage

## 返回前期净值高点的时长
draw.down.infor <- dataInfor[,.(Date,NAV,Draw_Down,Per_Draw_Down,
                                DD_Duration = rep(0))]

for(i in 2:nrow(dataInfor)){
  if(draw.down.infor[i,Draw_Down] == 0){draw.down.infor$DD_Duration[i] <- 0} 
  else{draw.down.infor$DD_Duration[i] <- draw.down.infor$DD_Duration[i-1] + 1} 
}

max.dd.duration <- max(draw.down.infor$DD_Duration)

## 计算每日的收益情况
rtn <- dataInfor[,.(Date, Fund_NAV = Net_Value,
                    SH_NAV = SH_index,
                    HS300_NAV = HS300_index,
                    Fund_rtn = rep(0),
                    SH_rtn = rep(0),
                    HS300_rtn = rep(0),
                    Diff_SH = rep(0), Diff_HS300 = rep(0)
)
]

rtn[2:nrow(rtn)]$Fund_rtn <- diff(log(rtn$Fund_NAV), lag = 1)
rtn[2:nrow(rtn)]$SH_rtn <- diff(log(rtn$SH_NAV), lag = 1)
rtn[2:nrow(rtn)]$HS300_rtn <- diff(log(rtn$HS300_NAV), lag = 1)

rtn[,Diff_SH := Fund_rtn - SH_rtn]
rtn[,Diff_HS300 := Fund_rtn - HS300_rtn]

sharpe.ratio.mean <- (1 + mean(rtn$Fund_rtn))^250 - 1
sharpe.ratio.std <- sd(rtn$Fund_rtn) * sqrt(250)
sharpe.ratio <- (sharpe.ratio.mean - rtn_free) / sharpe.ratio.std

information.ratio.mean <- (1 + mean(rtn$Diff_SH))^250 - 1
information.ratio.std <- sd(rtn$Diff_SH) * sqrt(250)
information.ratio.sh <- information.ratio.mean / information.ratio.std

information.ratio.mean <- (1 + mean(rtn$Diff_HS300))^250 - 1
information.ratio.std <- sd(rtn$Diff_HS300) * sqrt(250)
information.ratio.hs300 <- information.ratio.mean / information.ratio.std

information.ratio.sh 
information.ratio.hs300 

rtn[,max(Fund_rtn)]
rtn[,max(Diff_SH)]
rtn[,max(Diff_HS300)]

#` @abnormal.infor.volatility
##``````````````````````````````````````````````````````````````````````````````
# 异常波动情况
abnormal.infor.volatility <- rtn[Fund_rtn >= 0.08 | Fund_rtn <= -0.08 |
                                   Diff_SH >= 0.08 | Diff_SH <= - 0.08 |
                                   Diff_HS300 >= 0.08 | Diff_HS300 <= -0.08,]

## `````````````````````````````````````````````````````````````````````````````
#` @Fund_rtn


## 连续盈利/亏损
## 连续盈利/亏损

continuous_rtn <- return.infor[,.(Date,Code,Volume,Return,
                                  Continuous_Mark = ifelse(Return >=0,
                                                           1,0)
)]

continuous_rtn_positive <- rle(continuous_rtn[,Continuous_Mark])$
  length[rle(continuous_rtn[,Continuous_Mark])$value == 1]

continuous_rtn_negative <- rle(continuous_rtn[,Continuous_Mark])$
  length[rle(continuous_rtn[,Continuous_Mark])$value == 0]

continuous_positive <- data.table(table(continuous_rtn_positive))
colnames(continuous_positive) <- c("连续盈利次数", "交易次数")
continuous_positive$连续盈利次数 <- as.numeric(continuous_positive$连续盈利次数)

continuous_negative <- data.table(table(continuous_rtn_negative))
colnames(continuous_negative) <- c("连续亏损次数", "交易次数")
continuous_negative$连续亏损次数 <- as.numeric(continuous_negative$连续亏损次数)

## 最大连续盈利次数
max_num_consecutive_winners <- continuous_positive[.N,连续盈利次数]
## 最大连续亏损次数
max_num_consecutive_losers <- continuous_negative[.N,连续亏损次数]


continuous_positive <- 
  continuous_positive[, .(连续盈利次数.汇总=rep("--",max(c(nrow(continuous_positive),15))),
                                交易次数.汇总=rep(0),
                                连续盈利次数,交易次数)]


for(i in 1:10){
  continuous_positive$连续盈利次数.汇总[i] <- paste0(i,"D")
  ## continuous_positive[i,交易次数.汇总:= continuous_positive[连续盈利次数 == i, 交易次数]]
  continuous_positive$交易次数.汇总[i] <- ifelse(length(continuous_positive[连续盈利次数 == i, 交易次数]) == 0,0,
                                           continuous_positive[连续盈利次数 == i, 交易次数])
}
continuous_positive$连续盈利次数.汇总[11] <- paste0("(10,15]")
continuous_positive[11,交易次数.汇总:= 
                      sum(continuous_positive[连续盈利次数 %between% c(11,15), 交易次数])]

continuous_positive$连续盈利次数.汇总[12] <- paste0("(15,20]")
continuous_positive[12,交易次数.汇总:= 
                      sum(continuous_positive[连续盈利次数 %between% c(16,20), 交易次数])]

continuous_positive$连续盈利次数.汇总[13] <- paste0("(20,30]")
continuous_positive[13,交易次数.汇总:= 
                      sum(continuous_positive[连续盈利次数 %between% c(21,30), 交易次数])]


continuous_positive$连续盈利次数.汇总[14] <- paste0("(30,50]")
continuous_positive[14,交易次数.汇总:= 
                      sum(continuous_positive[连续盈利次数 %between% c(31,50), 交易次数])]


continuous_positive$连续盈利次数.汇总[15] <- paste0("(50+,)")
continuous_positive[15,交易次数.汇总:= 
                      sum(continuous_positive[连续盈利次数 > 50, 交易次数])]


continuous_positive <- continuous_positive[1:15,1:2, with=FALSE]

continuous_negative <- 
  continuous_negative[, .(连续亏损次数.汇总=rep("--",max(c(nrow(continuous_negative),15))),
                                交易次数.汇总=rep(0),
                                连续亏损次数,交易次数)]



for(i in 1:10){
  print(i)
  continuous_negative$连续亏损次数.汇总[i] <- paste0(i,"D")
  continuous_negative$交易次数.汇总[i] <- ifelse(length(continuous_negative[连续亏损次数 == i, 交易次数]) == 0,0,
                                           continuous_negative[连续亏损次数 == i, 交易次数])
}
continuous_negative$连续亏损次数.汇总[11] <- paste0("(10,15]")
continuous_negative[11,交易次数.汇总:= 
                      sum(continuous_negative[连续亏损次数 %between% c(11,15), 交易次数])]

continuous_negative$连续亏损次数.汇总[12] <- paste0("(15,20]")
continuous_negative[12,交易次数.汇总:= 
                      sum(continuous_negative[连续亏损次数 %between% c(16,20), 交易次数])]

continuous_negative$连续亏损次数.汇总[13] <- paste0("(20,30]")
continuous_negative[13,交易次数.汇总:= 
                      sum(continuous_negative[连续亏损次数 %between% c(21,30), 交易次数])]


continuous_negative$连续亏损次数.汇总[14] <- paste0("(30,50]")
continuous_negative[14,交易次数.汇总:= 
                      sum(continuous_negative[连续亏损次数 %between% c(31,50), 交易次数])]


continuous_negative$连续亏损次数.汇总[15] <- paste0("(50+,)")
continuous_negative[15,交易次数.汇总:= 
                      sum(continuous_negative[连续亏损次数 > 50, 交易次数])]

continuous_negative <- continuous_negative[1:15,1:2, with=FALSE]



summary_return <- data.frame("指标" = c("回测开始时间", "回测结束时间",
                                      "初始资金", "期末资金",
                                      "回测交易年数", "回测交易月数", "回测交易天数",
                                      "净收益", "净收益率",
                                      "年化收益率(CAR)", "交易次数",
                                      "盈利次数", "亏损次数", "胜率",
                                      "盈利交易的盈利总额", "亏损交易的亏损总额",
                                      "盈利系数", "单笔平均盈利额", "单笔平均亏损额",
                                      "赔率",
                                      "单笔最大盈利额", "单笔最大盈利比",
                                      "单笔最大亏损额", "单笔最大亏损比", "最大回撤金额",
                                      "最大回撤比例", "最长回撤期间", "恢复系数",   # max.dd.duration
                                      "夏普比率","信息比率(SH)","信息比率(HS300)",
                                      "年化收益率/最大回撤比例",
                                      "最大连续盈利次数",
                                      "最大连续亏损次数"),
                             "内容" = c(as.character(start_time), 
                                      as.character(end_time),
                                      sprintf("%.2f", initial_capital), sprintf("%.2f", ending_capital),
                                      years_count, months_count, days_conunt,
                                      sprintf("%.2f", net_revenue), sprintf("%.2f%%", net_return * 100),
                                      sprintf("%.2f%%", compounding_annual_return * 100), transactions,
                                      winners, losers, sprintf("%.2f%%", winning_rate * 100),
                                      sprintf("%.2f", total_profit), sprintf("%.2f", total_loss),
                                      sprintf("%.2f", profit_factor), sprintf("%.2f", average_profit), sprintf("%.2f", average_loss),
                                      sprintf("%.2f", payoff_ratio),
                                      sprintf("%.2f", max_profit), sprintf("%.2f%%", max_profit_percentage * 100),
                                      sprintf("%.2f", max_loss), sprintf("%.2f%%", max_loss_percentage * 100), sprintf("%.2f", max_DD_amount),
                                      sprintf("%.2f%%", max_DD_percentage * 100), paste(max.dd.duration,"天"),sprintf("%.2f", recovery_factor),
                                      sprintf("%.2f", sharpe.ratio),sprintf("%.2f", information.ratio.sh),sprintf("%.2f", information.ratio.hs300),
                                      sprintf("%.2f", compounding_annual_return_max_DD_percentage),
                                      max_num_consecutive_winners,
                                      max_num_consecutive_losers))
knitr::kable(summary_return)
## sprintf("%.2f%%", singleYearDD * 100)
## -----------------------------------------------------------------------------
## 按年份汇总
## Yearly
## -----------------------------------------------------------------------------
calYearReturn <- function(x){
  temp <- dataInfor$Net_Value[grep(x, dataInfor$Date)][length(dataInfor$Net_Value[grep(x, dataInfor$Date)])] /
    dataInfor$Net_Value[grep(x, dataInfor$Date)][1] - 1
}
#m# 每年的收益
singleYearReturn <- sapply(year, calYearReturn)

##
calYearDD <- function(x){
  temp <- max(dataInfor$Per_Draw_Down[grep(x, dataInfor$Date)])
}
## 每年的DD
singleYearDD <- sapply(year, calYearDD)

## 每年的收益 / 每年的 DD
singleYearReturn_DD <- singleYearReturn / singleYearDD

summary_year <- data.frame(Year = year, 
                           年化收益率 = sprintf("%.2f%%", singleYearReturn * 100),
                           最大回撤比 = sprintf("%.2f%%", singleYearDD * 100),
                           "年化收益率/最大回撤比" = c(round(singleYearReturn_DD, 2)))



## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
## 对每只股票的持仓周期进行统计，形成：
## holdingInfor
## -----------------------------------------------------------------------------

holdingDays <- return.infor[,Holding_Days]
holdingDays_1_7 <- which(holdingDays < 7) %>% length(.)
holdingDays_7_15 <- which(holdingDays >= 7 & holdingDays < 14) %>% length(.)
holdingDays_15_30 <- which(holdingDays >= 14 & holdingDays < 30) %>% length(.)
holdingDays_30_60 <- which(holdingDays >= 30 & holdingDays < 60) %>% length(.)
holdingDays_60_90 <- which(holdingDays >= 60 & holdingDays < 90) %>% length(.)
holdingDays_90_180 <- which(holdingDays >= 90 & holdingDays < 180) %>% length(.)
holdingDays_180_365 <- which(holdingDays >= 180 & holdingDays < 365) %>% length(.)
holdingDays_365 <- which(holdingDays >= 365) %>% length(.)

holdingDays_dis <- data.frame("持股周期(天)" = c("[1,7)", "[7,15)", "[15,30)", "[30,60)",
                                            "[60,90)", "[90,180)", "[180,365)",
                                            "[365,)"),
                              "交易次数" = c(holdingDays_1_7, holdingDays_7_15, holdingDays_15_30, holdingDays_30_60,
                                         holdingDays_60_90, holdingDays_90_180, holdingDays_180_365,
                                         holdingDays_365))
holdingDays_dis


## 交易盈亏分布
DD_0_5 <- length(which(return.infor[,Return] >= 0 &
                         return.infor[,Return] < .05))
DD_5_10 <- length(which((return.infor[,Return] >= .05) &
                          (return.infor[,Return] < .1)))
DD_10_15 <- length(which((return.infor[,Return]  >=  .1)
                         & (return.infor[,Return] < .15)))
DD_15_20 <- length(which((return.infor[,Return] >=  .15)
                         & (return.infor[,Return] < .2)))
DD_20_25 <- length(which((return.infor[,Return]  >=  .2)
                         & (return.infor[,Return] < .25)))
DD_25_30 <- length(which((return.infor[,Return]  >=  .25)
                         & (return.infor[,Return] < .30)))
DD_30_40 <- length(which((return.infor[,Return]  >=  .30)
                         & (return.infor[,Return] < .40)))
DD_40_50 <- length(which((return.infor[,Return]  >=  .40)
                         & (return.infor[,Return] < .50)))
DD_50 <- length(which(return.infor[,Return]  >=  .50))

DD_N_5_0 <- length(which((return.infor[,Return] < 0) &
                           (return.infor[,Return] >= -.05)))
DD_N_10_5 <- length(which((return.infor[,Return] < -.05) &
                            (return.infor[,Return] >= -.1)))
DD_N_15_10 <- length(which((return.infor[,Return]  <  -.1)
                           & (return.infor[,Return] >= -.15)))
DD_N_20_15 <- length(which((return.infor[,Return] <  -.15)
                           & (return.infor[,Return] >= -.20)))
DD_N_25_20 <- length(which((return.infor[,Return]  <  -.20)
                           & (return.infor[,Return] >= -.25)))
DD_N_30_25 <- length(which((return.infor[,Return]  <  -.25)
                           & (return.infor[,Return] >= -.30)))
DD_N_40_30 <- length(which((return.infor[,Return]  < -.30)
                           & (return.infor[,Return] >= -.40)))
DD_N_50_40 <- length(which((return.infor[,Return]  <  -.40)
                           & (return.infor[,Return] >= -.50)))
DD_N_50 <- length(which(return.infor[,Return] < -.50))


DD <- data.frame(c("<=-50%", "[-50%,-40%)", "[-40%, -30%)", "[-30%, -25%)",
                   "[-25%,-20%)",  "[-20%,-15%)",  "[-15%,-10%)",  "[-10%,-5%)", "[-5%, 0)",
                   "[0%, 5%)", "[5%,10%)", "[10%,15%)", "[15%,20%)", "[20%,25%)",
                   "[25%,30%)", "[30%,40%)", "[40%,50%)", ">=50%"),
                 c(DD_N_50, DD_N_50_40, DD_N_40_30, DD_N_30_25, DD_N_25_20, DD_N_20_15,
                   DD_N_15_10, DD_N_10_5, DD_N_5_0,
                   DD_0_5, DD_5_10, DD_10_15, DD_15_20, DD_20_25, DD_25_30, DD_30_40,
                   DD_40_50, DD_50))
colnames(DD) <- c("交易盈亏", "交易次数")

## 交易亏损次数统计
DD

## 回撤区间分布
pdd <- dataInfor[,Per_Draw_Down]

pdd_0_5 <- which(pdd < 0.05) %>% length(.)
pdd_5_10 <- which(pdd >= 0.05 & pdd < 0.10) %>% length(.)
pdd_10_15 <- which(pdd >= 0.10 & pdd < 0.15) %>% length(.)
pdd_15_20 <- which(pdd >= 0.15 & pdd < 0.20) %>% length(.)
pdd_20_30 <- which(pdd >= 0.20 & pdd < 0.30) %>% length(.)
pdd_30_40 <- which(pdd >= 0.30 & pdd < 0.40) %>% length(.)
pdd_40_50 <- which(pdd >= 0.40 & pdd < 0.50) %>% length(.)
pdd_50 <- which(pdd >= 0.50) %>% length(.)

pdd_dis <- data.frame("最大回撤区间" = 
                        c("[0,5%)", "[5%,10%)", "[10%,15%)", "[15%,20%)",
                          "[20%,30%)", "[30%,40%)", "[40%,50%)","[50%+,)"),
                      "交易次数" = 
                        c(pdd_0_5, pdd_5_10, pdd_10_15, pdd_15_20,
                          pdd_20_30, pdd_30_40, pdd_40_50, pdd_50))
pdd_dis



## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
################################################################################
#########################      BackTesting #7:    ##############################
#########################           Report        ##############################
#########################       策略回测报告      ##############################
################################################################################
#` @
#` @
#
## -----------------------------------------------------------------------------


l <- list("收益统计" = summary_return,
          "按年份汇总" = summary_year,
          
          "回测净值" = backTestingInfor,
          "单日净值波动" = rtn,
          "异常波动情况" = abnormal.infor.volatility,
          
          "交割单" = trading.infor.ohlc,
          "建仓情况" = buy.in.infor[,.(Date, Code, Volume)],
          "平仓情况" = sell.out.infor[,.(Date, Code, Volume)],
          
          "每日持仓盈亏情况" = profitInfor,
          "收益情况" = returnInfor,
          "异常收益情况" = abnormal.infor.return,
          "异常买卖情况" = abnormal.infor.position,
          
          "持仓周期分布" = holdingDays_dis,
          "交易盈亏分布" = DD,
          "连续盈利分布" = continuous_positive,
          "连续亏损分布" = continuous_negative,
          "回撤区间分布" = pdd_dis,
          
          "分布图" = list())

openxlsx::write.xlsx(l, file = paste0("../output/",
                                      file_name,"_回测结果_", data_name, ".xlsx"),
                     colNames = TRUE, rowNames=FALSE)



## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
################################################################################
#########################          Plotting       ##############################
#########################           Report        ##############################
#########################       各种画图展示      ##############################
################################################################################
## dataInfor <- read_excel("DC6220e6g1_回测结果.xlsx", sheet = "回测净值") %>% as.data.table()
#` @
#` @draw.down.infor：净值回撤情况
#
## -----------------------------------------------------------------------------
