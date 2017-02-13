library(WindR)
w.start()
w.menu()

# 上证指数: 000001.Sh
sh000 <- w.wsd("000001.SH","close,volume,amt",
               "2000-01-01",
               as.character(Sys.Date())) %>% 
  .$Data %>% as.data.table()
colnames(sh000) <- c('TradingDay', 'Close', 'Volume', 'Turnover')
sh000[, ":="(TradingDay = as.Date(TradingDay))]

# 沪深300指数: 000300.Sh
hs300 <- w.wsd("000300.SH","close,volume,amt",
               "2000-01-01",
               as.character(Sys.Date())) %>% 
  .$Data %>% as.data.table()
colnames(hs300) <- c('TradingDay', 'Close', 'Volume', 'Turnover')
hs300[, ":="(TradingDay = as.Date(TradingDay))]

################################################################################
## 保存到 csv
################################################################################
fwrite(sh000, '../output/sh000.csv')
fwrite(hs300, '../output/hs300.csv')
