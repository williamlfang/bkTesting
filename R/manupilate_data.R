
#! 回测开始时间
start.time <- dataFile[1,Date]
#！回测结束时间
end.time <- dataFile[.N,Date]

#! sh.xlsx 日期格式为:2010-01-01
exchange.date <- paste0(r.code.path, "SH.xlsx")%>%
  read_excel(.) %>%
  .[[1]] %>%
  as.Date() %>%
  as.data.table()
names(exchange.date) <- "Date"

#! 按照交易所正常交易日期重新划分的
#! 交易日
trading.date <- exchange.date[Date %between% c(start.time,end.time),]

## <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<




## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
################################################################################
#########################      BackTesting #2:    ##############################
#########################         account         ##############################
#########################      统计持仓账户情况   ##############################
################################################################################
#` @trading.infor
#` @account.infor
## -----------------------------------------------------------------------------

#` @tradingInfor
## `````````````````````````````````````````````````````````````````````````````

cl <- makeCluster(no.cores)
clusterExport(cl, c("dataFile","trading.date"))
clusterEvalQ(cl, {library(data.table);library(magrittr)})

trading.infor <- parLapply(cl, 1:nrow(trading.date),
                           function(i) 
                             dataFile[Date == trading.date[i,][,Date],])
names(trading.infor) <- trading.date$Date
stopCluster(cl)


## `````````````````````````````````````````````````````````````````````````````
#! -- 用来测试是否在正常的交易日期内成交
buy.in <- dataFile[Volume > 0,]
sell.out <- dataFile[Volume < 0,]

x <- rbindlist(trading.infor)

#! -- 测试买入的情况
x[Volume > 0] -> a1
buy.in -> a2
different.names <- (!a2$Date %in% a1$Date)
a2[different.names,]

#! -- 测试卖出的情况
x[Volume < 0] -> a1
sell.out -> a2
different.names <- (!a2$Date %in% a1$Date)
a2[different.names,]


#` @accountInfor
## `````````````````````````````````````````````````````````````````````````````
account.infor <- list()
for(i in nrow(trading.date)){
  account.infor[i] <- list(i = rep(NA))
}

account.infor[[1]] <- trading.infor[[1]][, .(Date = rep(trading.date[1,Date]), 
                                             Code, Volume)]
for(i in 2:length(trading.infor)){
  long.infor <- trading.infor[[i]][Volume > 0]
  
  short.infor <- trading.infor[[i]][Volume < 0]
  
  ##
  ##===============================================================================  
  ## 按照相同的股票，把其持仓数量进行合并
  account.infor[[i]] <- rbind(account.infor[[i-1]][,.(Code, Volume)], 
                              long.infor[,.(Code, Volume)]) %>%
    .[, list(Volume = sum(Volume)), by = Code] %>%
    .[!duplicated(.$Code)] %>% .[order(Code)]
  
  ## 先进行平仓处理
  ##===============================================================================
  if(nrow(short.infor) != 0){
    for(k in 1:nrow(short.infor)){
      the.position <- which(short.infor[k,Code] == account.infor[[i]][,Code])
      account.infor[[i]][the.position, Volume := 
                           account.infor[[i]][the.position, Volume] + short.infor[k,Volume]
                         ]
    }
  }
  
  account.infor[[i]] <- account.infor[[i]][Volume !=0]
  
  if(nrow(account.infor[[i]]) != 0){
    account.infor[[i]] <- cbind(Date = rep(trading.date[i,Date]),
                                account.infor[[i]])
  }else{
    account.infor[[i]] <- data.table(Date = trading.date[i,Date],
                                     Code = rep(0),
                                     Volume = rep(0))
  }
  
  account.infor[[i]] <- account.infor[[i]][order(Code),] %>%  .[Volume != 0]
  
  ## Progress bar
  pb <- txtProgressBar(min = 0, max = length(trading.infor), style = 3)
  # update progress bar
  setTxtProgressBar(pb, i)
}

if(nrow(account.infor[length(account.infor)][[1]]) != 0){
  #! 如果最后一天的持仓不为零
  #! 多加一个交易日，用于全部清仓
  end.time.plus <- exchange.date %>% .[which(.$Date == end.time) + 1,] %>%.[,Date]
  
  trading.date.plus <- rbind(trading.date,list(end.time.plus))
  
  trading.infor.added <- account.infor[length(account.infor)] %>% .[[1]] %>%
    .[, .(Date = end.time.plus,
          Code,
          Volume = - Volume)]
  
  trading.infor <- c(trading.infor,list(trading.infor.added))
  
  ohlc.infor <- c(account.infor, list(trading.infor.added))
}else{
  #！ 如果最后一天的持仓为零
  end.time.plus <- exchange.date %>% .[which(.$Date == end.time),] %>%.[,Date]
  
  trading.date.plus <- trading.date
  
  trading.infor.added <- account.infor[length(account.infor)-1] %>% .[[1]] %>%
    .[, .(Date = end.time,
          Code,
          Volume=0)]
  
  ohlc.infor <- c(account.infor[1:(length(account.infor)-1)], list(trading.infor.added))
}
























#` @ohlc
## `````````````````````````````````````````````````````````````````````````````
#` @ohlc
## `````````````````````````````````````````````````````````````````````````````
#if(!file.exists(paste0("../output/", file_name_main,"_ohlc", ".xlsx"))){
ohlc.infor <- rbindlist(ohlc.infor)
ohlc.date <- exchange.date[(which(exchange.date$Date == start.time) - no_ohlc_backward):
                             (which(exchange.date$Date == end.time) + no_ohlc_backward)
                           ]
ohlc <- list()
cl <- makeCluster(no.cores)
registerDoParallel(cl)
ohlc <- foreach(i = 1:nrow(ohlc.date), .packages=c("data.table", "magrittr")) %dopar% {
  d1 <- ohlc.date[i,Date]
  
  d2 <- ohlc.infor[Date >= d1][,unique(Date)] %>% 
    .[1:min(length(.),2)]
  
  temp <- ohlc.infor[Date %in% d2, .(Date, Code, Volume)] %>% 
    .[!duplicated(Code)]
  
  if(nrow(temp) == 0){
    ohlc[[i]] <- data.table()
  }else{
    ohlc[[i]] <- data.table(Exchange_Date = d1,temp)
  }
}
stopCluster(cl)

temp <- dataFile[Volume < 0][,Exchange_Date := Date]
setcolorder(temp,c("Exchange_Date", colnames(temp)[1:(ncol(temp)-1)]))

ohlc <- rbindlist(ohlc[!is.na(ohlc)])[Code != 0] %>% 
  list(temp, .) %>% rbindlist() %>% 
  .[order(Exchange_Date)]

#Sys.setenv("R_ZIPCMD" = "D:/Program Files/Rtools/bin/zip.exe") ## path to zip.exe
#openxlsx::write.xlsx(ohlc, file = paste0("../output/",file_name_main,"_ohlc", ".xlsx"), colNames = TRUE, rowNames=FALSE)
#}
ohlc[,":="(Exchange_Date = as.Date(Exchange_Date),
           Date = as.Date(Date),
           Volume = NULL)]

mysql <- mysqlFetch('dev')
dbListTables(mysql)
dbSendQuery(mysql,"TRUNCATE TABLE fl_temp")
system.time(
  dbWriteTable(mysql,'fl_temp',ohlc, row.names=F, append=T)
)

system.time({
  ohlc <- dbGetQuery(mysql,"
                     SELECT a.TradingDay AS Exchange_Date, a.stockID AS Code,
                     a.open AS Open, a.high AS High,
                     a.low AS Low, a.close AS Close
                     FROM dev.wind_daily AS a, dev.fl_temp AS b
                     WHERE a.TradingDay = b.Exchange_Date
                     AND left(a.stockID,6) = b.Code") %>% as.data.table() %>% 
    .[, ":="(Exchange_Date = as.Date(Exchange_Date),
             Code = substr(Code,1,6))]
})

#` @trading.infor.ohlc
##``````````````````````````````````````````````````````````````````````````````
trading.infor <- rbindlist(trading.infor)

trading.infor.ohlc <- merge(trading.infor, ohlc, by.x = c('Date','Code'), 
                            by.y = c('Exchange_Date','Code'), all.x = T)

buy.in.infor <- trading.infor.ohlc[Volume > 0]
sell.out.infor <- trading.infor.ohlc[Volume < 0]



#` @account.infor.ohlc
##``````````````````````````````````````````````````````````````````````````````
account.infor  <- rbindlist(account.infor) 
accountInfor_ohlc <-  merge(account.infor, ohlc, by.x = c('Date','Code'), 
                            by.y = c('Exchange_Date','Code'), all.x = T)


#` @profitInfor
##``````````````````````````````````````````````````````````````````````````````
system.time({
  profitInfor <- list()
  for(i in nrow(trading.date)){
    profitInfor[i] <- list(i = rep(NA))
  }
  
  profitInfor[[1]] <- accountInfor_ohlc[Date==start.time] %>%
    .[, .(Date, Code, Volume, Close,
          Cost_Per_Share = Close * (1+fee),
          Cost_Total = Close * (1+fee) * Volume,
          Profit_Per_Share = rep(0),
          Profit_Total = rep(0),
          Return = rep(0),
          Holding_Days = rep(0))] %>%
    .[, .(Date, Code, Volume, Close,
          Cost_Per_Share,
          Cost_Total,
          Profit_Per_Share = Close - Cost_Per_Share,
          Profit_Total = (Close - Cost_Per_Share) * Volume,
          Return = (Close - Cost_Per_Share) / Cost_Per_Share,
          Holding_Days),]
  profitInfor[[1]] <- profitInfor[[1]] %>% .[order(Code)]
  
  
  
  for(i in 2:nrow(trading.date)){
    longInfor <- trading.infor.ohlc[Date == trading.date[i,Date] &
                                      Volume > 0,
                                    .(Date, Code, Volume, Close)]
    
    shortInfor <- trading.infor.ohlc[Date == trading.date[i,Date] &
                                       Volume < 0,
                                     .(Date, Code, Volume, Close)]
    
    ## 继承上一日的持仓情况
    ##===============================================================================
    if(profitInfor[[i-1]]$Code[1] == 0 | nrow(profitInfor[[i-1]]) == 0){
      ## 如果上一日的持仓为空
      profitInfor[[i]] <- data.table(Date = trading.date.plus[i][,Date],Code = rep(0),Volume = rep(0),
                                     Close = rep(0),
                                     Cost_Per_Share = rep(0),
                                     Cost_Total = rep(0),
                                     Profit_Per_Share = rep(0),
                                     Profit_Total = rep(0),
                                     Return = rep(0),
                                     Holding_Days = rep(0)
      )
    }else{
      ## 如果上一日有持仓，则继承原来的持仓账户
      ## 同时，持仓天数累计加 1 天
      profitInfor[[i]] <- profitInfor[[i-1]][, .(Date, Code, Volume,
                                                 Close = rep(0),
                                                 Cost_Per_Share,
                                                 Cost_Total,
                                                 Profit_Per_Share,
                                                 Profit_Total,
                                                 Return,
                                                 Holding_Days = Holding_Days + 1)]
    }
    
    
    ## 先进行卖出的处理
    ## 1.更新持股的数量
    ## 2.更新持股的成本
    ##===============================================================================
    if(nrow(shortInfor) != 0){
      for(k in 1:nrow(shortInfor)){
        the.position1 <- which(shortInfor$Code[k] == profitInfor[[i-1]]$Code)
        the.position2 <- which(shortInfor$Code[k] == profitInfor[[i]]$Code)
        ## 1.更新持股的数量
        profitInfor[[i]][the.position2, 
                         Volume := profitInfor[[i]]$Volume[the.position2] + shortInfor$Volume[k]
                         ]
        
        profitInfor[[i]][the.position2,Close := shortInfor$Close[k]]
        
        ## 2.更新持股的成本
        ##
        profitInfor[[i]][the.position2, Cost_Per_Share :=
                           ifelse(profitInfor[[i]]$Volume[the.position2] == 0,
                                  0,
                                  profitInfor[[i-1]]$Cost_Per_Share[the.position1]
                           )]
        
        profitInfor[[i]][the.position2, Cost_Total :=
                           ifelse(profitInfor[[i]]$Volume[the.position2] == 0,
                                  0,
                                  profitInfor[[i]]$Cost_Per_Share[the.position2] *
                                    profitInfor[[i]]$Volume[the.position2]
                           )]
        
        
        profitInfor[[i]][the.position2, Profit_Per_Share :=
                           (1 - fee) * profitInfor[[i]]$Close[the.position2] - profitInfor[[i-1]]$Cost_Per_Share[the.position1]
                         ]
        
        profitInfor[[i]][the.position2, Profit_Total :=
                           profitInfor[[i]]$Profit_Per_Share[the.position2] *
                           abs(shortInfor$Volume[k])
                         ]
        
        profitInfor[[i]][the.position2, Return :=
                           profitInfor[[i]]$Profit_Per_Share[the.position2] /
                           profitInfor[[i-1]]$Cost_Per_Share[the.position1]
                         ]
        
      }
    }
    
    
    ##
    ##===============================================================================
    ## 按照相同的股票，把其持仓数量进行合并
    ##
    if (nrow(longInfor) == 0) { 
      #! 没有买入的情况
      if(nrow(profitInfor[[i]]) == 0 | profitInfor[[i]]$Code[1] == 0 ){ 
        #！ 没有买入，且是空仓
        profitInfor[[i]] <- data.table(Date = trading.date[i,Date],
                                       Code = rep(0),Volume = rep(0),
                                       Close = rep(0),
                                       Cost_Per_Share = rep(0),
                                       Cost_Total = rep(0),
                                       Profit_Per_Share = rep(0),
                                       Profit_Total = rep(0),
                                       Return = rep(0),
                                       Holding_Days = rep(0)
        )
      }else{ 
        #！ 没有买入，但有仓位
        #！ 需要更新仓位情况
        for (k in 1:nrow(profitInfor[[i]])) {
          #! Close
          profitInfor[[i]][k, Close := ohlc[Exchange_Date == profitInfor[[i]]$Date[k] &
                                              Code == profitInfor[[i]]$Code[k]][,Close]
                           ]
          
          #! Cost_Per_Share
          profitInfor[[i]][k, Cost_Per_Share := profitInfor[[i]]$Cost_Per_Share[k]
                           ]
          
          #! Cost_Total
          profitInfor[[i]][k, Cost_Total :=
                             profitInfor[[i]]$Cost_Per_Share[k] *
                             profitInfor[[i]]$Volume[k]
                           ]
          
          #! Profit_Per_Share
          profitInfor[[i]][k, Profit_Per_Share :=
                             profitInfor[[i]]$Close[k] - profitInfor[[i]]$Cost_Per_Share[k]
                           ]
          
          #! Cost_Total
          profitInfor[[i]][k, Profit_Total :=
                             profitInfor[[i]]$Profit_Per_Share[k] *
                             profitInfor[[i]]$Volume[k]
                           ]
          
          #! Return
          profitInfor[[i]][k, Return :=
                             profitInfor[[i]]$Profit_Per_Share[k] /
                             profitInfor[[i]]$Cost_Per_Share[k]
                           ]
          
        }
      }
    }else{ 
      #! 如果有买入的情况
      profitInfor[[i]] <- cbind(longInfor,
                                Cost_Per_Share =rep(0),
                                Cost_Total = rep(0),
                                Profit_Per_Share = rep(0),
                                Profit_Total = rep(0),
                                Return = rep(0),
                                Holding_Days = rep(0)) %>%
        rbind(profitInfor[[i]], .) %>%
        .[, Date:= trading.date[i,Date]]
      
      profitInfor[[i]] <- profitInfor[[i]][, Volume := sum(Volume), by = Code] %>%
        .[!duplicated(.$Code)] %>% .[order(Code)]
      
      for(k in 1:nrow(longInfor)){
        #! 在 i-1
        the.position.1 <- which(longInfor$Code[k] == profitInfor[[i-1]]$Code)
        #！在 i
        the.position.2 <- which(longInfor$Code[k] == profitInfor[[i]]$Code)
        
        if(length(the.position.1) ==0){
          profitInfor[[i]][the.position.2, Cost_Per_Share := (1+fee) * longInfor$Close[k]
                           ]
        }
        if(length(the.position.1) !=0){
          profitInfor[[i]][the.position.2, Cost_Per_Share := (profitInfor[[i-1]]$Cost_Per_Share[the.position.1] *
                                                                profitInfor[[i-1]]$Volume[the.position.1]
                                                              + (1+fee) * longInfor$Close[k] * longInfor$Volume[k] ) /
                             (profitInfor[[i-1]]$Volume[the.position.1] + longInfor$Volume[k])
                           ]
        }
        ##
      }
      
      profitInfor[[i]] <-  profitInfor[[i]][Volume != 0]
      
      for (k in 1:nrow(profitInfor[[i]])) {
        #! Close
        profitInfor[[i]][k,Close := ohlc[Exchange_Date == profitInfor[[i]]$Date[k] &
                                           Code == profitInfor[[i]]$Code[k]][,Close]
                         ]
        
        #! Cost_Total
        profitInfor[[i]][k,Cost_Total :=
                           profitInfor[[i]]$Cost_Per_Share[k] *
                           profitInfor[[i]]$Volume[k]
                         ]
        
        #! Profit_Per_Share
        profitInfor[[i]][k,Profit_Per_Share :=
                           profitInfor[[i]]$Close[k] - profitInfor[[i]]$Cost_Per_Share[k]
                         ]
        
        #! Cost_Total
        profitInfor[[i]][k,Profit_Total :=
                           profitInfor[[i]]$Profit_Per_Share[k] *
                           profitInfor[[i]]$Volume[k]
                         ]
        
        #! Return
        profitInfor[[i]][k,Return :=
                           profitInfor[[i]]$Profit_Per_Share[k] /
                           profitInfor[[i]]$Cost_Per_Share[k]
                         ]
        
      }
    }
    
    profitInfor[[i]] <- profitInfor[[i]] %>% .[order(Code)] %>% .[Volume != 0] %>%
      .[,Date :=trading.date[i,Date] ]
    
    ## Progress bar
    pb <- txtProgressBar(min = 0, max = nrow(trading.date), style = 3)
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  
  
  profitInfor <- rbindlist(profitInfor)
  
})


#` @returnInfor
##``````````````````````````````````````````````````````````````````````````````

cl <- makeCluster(no.cores)
registerDoParallel(cl)
returnInfor <-
  foreach(i = 1:nrow(sell.out.infor), .combine = "rbind",.packages = c("data.table")) %dopar% {
    infor <- profitInfor[Date < sell.out.infor[i,Date] & Code == sell.out.infor[i,Code],][.N]
    
    return(sell.out.infor[i, .(Date,Code,Volume,Close,
                               Cost = infor[,Cost_Per_Share],
                               Price = (1 - fee) * Close,
                               Return = (1 - fee) * Close / infor[,Cost_Per_Share] - 1,
                               Profit = ((1 - fee) * Close / infor[,Cost_Per_Share] - 1) * infor[,Cost_Per_Share] * abs(Volume), #((1 - fee) * Close / infor[,Cost_Per_Share] - 1) * abs(Volume) * Cost,
                               Holding_Days = infor[,Holding_Days + 1])]
    )
  }
stopCluster(cl)
## <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
################################################################################
#########################      BackTesting #4:    ##############################
#########################       abnormal.infor    ##############################
#########################     检测数据异常的情况  ##############################
################################################################################
#` @abnormal.infor.position
#` @abnormal.infor.return
#` @abnormal.infor.volatility
## -----------------------------------------------------------------------------
abnormal.infor.buy <- cbind(buy.in.infor, Close.Pre = rep(0),Open_Pct = rep(0), 
                            Close_Pct = rep(0), Limited = rep("--"))
for(i in 1:nrow(abnormal.infor.buy)){
  the.infor <- ohlc[Exchange_Date < abnormal.infor.buy[i,Date] &
                      Code == abnormal.infor.buy[i,Code]
                    ][.N]  ## 查看昨天的 OHLC
  abnormal.infor.buy[i,Close.Pre:= the.infor[1,Close]]
  abnormal.infor.buy[i,Open_Pct:= (Open/the.infor[1,Close] -1)]
  abnormal.infor.buy[i,Close_Pct:= (Close/the.infor[1,Close] -1)]
  abnormal.infor.buy[i,Limited:= ifelse(Open_Pct >=0.095 | Close_Pct >= 0.095,"涨停买入","")]
  ## Progress bar
  pb <- txtProgressBar(min = 0, max = nrow(abnormal.infor.buy), style = 3)
  # update progress bar
  setTxtProgressBar(pb, i)
}

abnormal.infor.buy[Limited == "涨停买入"]


## -----------------------------------------------------------------------------
abnormal.infor.sell <- cbind(sell.out.infor, Close.Pre = rep(0),Open_Pct = rep(0), 
                             Close_Pct = rep(0), Limited = rep("--"))
for(i in 1:nrow(abnormal.infor.sell)){
  the.infor <- ohlc[Exchange_Date < abnormal.infor.sell[i,Date] &
                      Code == abnormal.infor.sell[i,Code]
                    ][.N]  ## 查看昨天的 OHLC
  abnormal.infor.sell[i,Close.Pre:= the.infor[1,Close]]
  abnormal.infor.sell[i,Open_Pct:= (Open/the.infor[1,Close] -1)]
  abnormal.infor.sell[i,Close_Pct:= (Close/the.infor[1,Close] -1)]
  abnormal.infor.sell[i,Limited:= ifelse(Open_Pct <= -0.095 | Close_Pct <= -0.095,"跌停卖出","")]
  ## Progress bar
  pb <- txtProgressBar(min = 0, max = nrow(abnormal.infor.sell), style = 3)
  # update progress bar
  setTxtProgressBar(pb, i)
}

abnormal.infor.sell[Limited == "跌停卖出"]

#` @abnormal.infor.position
##``````````````````````````````````````````````````````````````````````````````
# 异常买卖情况
abnormal.infor.position <- rbind(abnormal.infor.buy[Limited == "涨停买入"], 
                                 abnormal.infor.sell[Limited == "跌停卖出"])

#` @abnormal.infor.return
##``````````````````````````````````````````````````````````````````````````````
# 异常收益情况
abnormal.infor.return <- rbind(returnInfor[Return > 0.50 | Return < -0.50],
                               returnInfor[Holding_Days > 50])




## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
################################################################################
#########################      BackTesting #5:    ##############################
#########################     Net Asset Value     ##############################
#########################      计算基金净值       ##############################
################################################################################
#` @strategyInfor
#` @backTestingInfor
## -----------------------------------------------------------------------------

#` @strategyInfor
##``````````````````````````````````````````````````````````````````````````````
stockInfor <- accountInfor_ohlc

head(stockInfor, 20)
strategyInfor <- data.table(Date = trading.date.plus[,Date],
                            Long = rep(0),                           ## 买入
                            Short = rep(0),                          ## 平仓
                            Stock_Value = rep(0),                    ## 市值
                            Cash = rep(0),                           ## 现金
                            Net_Value = rep(0))                      ## 净值
str(strategyInfor)

for(i in 1:nrow(trading.date.plus)){
  TD <- trading.date.plus[i,Date]
  
  temp <- trading.infor.ohlc[Date == TD]
  ## 计算买入和平仓的交易数量
  long.position <- which(temp$Volume > 0)
  short.position <- which(temp$Volume < 0)
  
  ## 合计总买入的市值
  strategyInfor$Long[i] <- temp$Close[long.position] %*% temp$Volume[long.position]
  ## 合计总平仓的市值
  # strategyInfor$Short[i] <- temp$Price[short.position] %*% temp$Volume[short.position]
  strategyInfor$Short[i] <- temp$Close[short.position] %*% temp$Volume[short.position]
  
  # 合计收益
  ## strategyInfor$Profit[i] <- sum(temp$Profit)
  
  # 合计股票账面市值
  #strategyInfor$Stock_Value[i] <- stockInfor$VolumeAdj[which(stockInfor$Date == TD)] %*%
  #  stockInfor$ClosePrice[which(stockInfor$Date == TD)]
  strategyInfor$Stock_Value[i] <- 
    stockInfor[Date == TD, Close] %*% stockInfor[Date == TD, Volume] 
  
  ## 合计现金余额
  ## 初始资金规模：50,000,000
  strategyInfor$Cash[1] <- initial_capital - strategyInfor$Long[1] * (1+fee)
  
  ###  if(i >= 2){
  ###    strategyInfor$Cash[i] <- strategyInfor$Cash[i-1] - strategyInfor$Long[i] + abs(strategyInfor$Short[i])
  ###  }
  
  if(i >= 2){
    strategyInfor$Cash[i] <- strategyInfor$Cash[i-1] - (1+fee) * strategyInfor$Long[i] + 
      (1-fee) * abs(strategyInfor$Short[i])
  }
  # 合计基金总净值
  strategyInfor$Net_Value[i] <- strategyInfor$Stock_Value[i] + strategyInfor$Cash[i]
  
  ## Progress bar
  pb <- txtProgressBar(min = 0, max = nrow(trading.date.plus), style = 3)
  # update progress bar
  setTxtProgressBar(pb, i)
  
  # print(paste("\n\n ============", i, "of", length(tradingInfor), "is Done!", "============ \n\n"))
  #print(paste("----------------", i, "of", nrow(trading.date.plus), "--------------->>",
  #            format(Sys.time(), "%Y %b %d %a %X"), "<<"))
}

## -----------------------------------------------------------------------------

strategyInfor[.N, Net_Value] / strategyInfor[1, Net_Value]



#` @backTestingInfor
##``````````````````````````````````````````````````````````````````````````````

sh000 <- fread('../output/sh000.csv') %>% 
  .[,c('Volume', 'Turnover') := NULL]
setnames(sh000,'Close','SH000')

hs300 <- fread('../output/hs300.csv') %>% 
  .[,c('Volume', 'Turnover') := NULL]
setnames(hs300,'Close','HS300')

index <- merge(sh000,hs300, by = 'TradingDay', all = TRUE) %>% 
  .[, TradingDay := as.Date(TradingDay)]


backTestingInfor <- 
  strategyInfor[,.(Date = as.Date(Date),Long,Short,Stock_Value,Cash,Net_Value,
                   Max_Value = rep(0),                              ## 最大值
                   Draw_Down = rep(0),                              ## DD 值
                   Per_Draw_Down = rep(0),                          ## 单位 DD 值
                   NAV = rep(0),
                   SH_index = index[TradingDay %between% range(Date)][,SH000],
                   HS300_index = index[TradingDay %between% range(Date)][,HS300],
                   NAV_SH_index = rep(0),                           ## 上证净值
                   NAV_HS300_index = rep(0)
  )]


for(i in 1:nrow(backTestingInfor)){
  
  backTestingInfor[i,Max_Value := 
                     max(backTestingInfor[1:i,Net_Value])
                   ]
  
  backTestingInfor[i,Draw_Down := 
                     backTestingInfor[i,Max_Value] - backTestingInfor[i,Net_Value]
                   ]
  
  backTestingInfor[i,Per_Draw_Down := 
                     backTestingInfor[i,Draw_Down] / backTestingInfor[i,Max_Value]
                   ]
  
  backTestingInfor[i,NAV := backTestingInfor[i,Net_Value] / initial_capital
                   ]
  
  backTestingInfor[i,NAV_SH_index := 
                     backTestingInfor[i,SH_index] / backTestingInfor[1,SH_index]
                   ]
  
  backTestingInfor[i,NAV_HS300_index := 
                     backTestingInfor[i,HS300_index] / backTestingInfor[1,HS300_index]
                   ]
  
  ## Progress bar
  pb <- txtProgressBar(min = 0, max = nrow(backTestingInfor), style = 3)
  # update progress bar
  setTxtProgressBar(pb, i)
}



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

