## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
################################################################################
#########################        Plotting:#3       #############################
######################### 基金产品净值日内波动情况 ############################
################################################################################
## 
#` @
#` @
#
## -----------------------------------------------------------------------------

png(paste0("../fig/",file_name,"_基金产品净值日内波动情况_",
           data_name,".png"), res = 72*3, height=1350*0.9, width=1500*1.6)
par(mar=c(2.5,4.8,2,1), bg = "#d5e4eb")

nav.return.infor <- read_excel(paste0("../output/",file_name, "_回测结果_",
                                      data_name,".xlsx"), sheet = "单日净值波动") %>% 
  as.data.table()

plot(nav.return.infor$Date, nav.return.infor$Fund_rtn, type="h",
     ylab = "净值日收益率",main = paste0("基金产品净值日内波动情况: ", data_name)
     )
abline(h=c(-0.05,0.05),lty=3, col="gray65")
lines(nav.return.infor$Date, nav.return.infor$Fund_rtn, 
      col = "gray50")
points(x = nav.return.infor[Fund_rtn > 0, Date], 
       y = nav.return.infor[Fund_rtn > 0,Fund_rtn], 
       col = "red", pch=21)
points(nav.return.infor[Fund_rtn < 0, Date], nav.return.infor[Fund_rtn < 0,Fund_rtn], 
       col = "green", pch=21)
points(nav.return.infor[Fund_rtn >= 0.05 | Fund_rtn <= -0.05, Date], 
       nav.return.infor[Fund_rtn >= 0.05 | Fund_rtn <= -0.05, Fund_rtn], 
       cex = 1.1, col = "blue", pch=19)
text(nav.return.infor[Fund_rtn >= 0.05 | Fund_rtn <= -0.05, Date], 
     nav.return.infor[Fund_rtn >= 0.05 | Fund_rtn <= -0.05, Fund_rtn], 
     sprintf("%.2f%%", nav.return.infor[Fund_rtn >= 0.05 | Fund_rtn <= -0.05, Fund_rtn] * 100),
     cex = 0.8, col = "blue", adj=c(-0.15,-0.1))
text(nav.return.infor[Fund_rtn >= 0.05 | Fund_rtn <= -0.05, Date], 
     nav.return.infor[Fund_rtn >= 0.05 | Fund_rtn <= -0.05, Fund_rtn], 
     paste0("(",nav.return.infor[Fund_rtn >= 0.05 | Fund_rtn <= -0.05, Date],")"),
     cex = 0.6, col = "gray20", adj=c(-0.05,1.2))

dev.off()
