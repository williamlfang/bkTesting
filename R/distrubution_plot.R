## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
################################################################################
#########################        Plotting:#4       #############################
######################### 基金产品收益分布图        ############################
################################################################################
## 
#` @
#` @
#
## -----------------------------------------------------------------------------
png(paste0("../fig/",file_name,"_基金产品交易分布统计_",
           data_name,".png"), res = 72*3, height=1350*0.9, width=1500*1.6)

layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE))
################################################################################
## 持股周期分布
################################################################################
dt1 <- read_excel(paste0("../output/",file_name, "_回测结果_",
                         data_name,".xlsx"), sheet = "持仓周期分布") %>% 
  as.data.table()

par(mar=c(2.0,0.5,1.0,0.5), bg = rgb(18,32,47,maxColorValue = 255))
temp <- 1:nrow(dt1)
plot(temp, dt1[,交易次数], type = 'h',xaxt = "n", axes = F, ylab="", xlab = "",
     main = '持股周期分布',col.main = rgb(66,175,182,maxColorValue = 255),cex.main=1.15,
     ylim = c(min(dt1[,交易次数])*0.9,max(dt1[,交易次数])*1.1))
for(i in temp){
  rect(i-0.1, 0, i+0.1, dt1[i,交易次数]+10, col= rgb(66,175,182,maxColorValue = 255), 
       border = NA)
}
mtext(1, at = temp, text = dt1[,持股周期.天.], col = 'gray50', 
      cex = .5,line = 0.8)
axis(1, at = temp, labels = NA, col = 'gray50')

for(i in temp){
  text(i,dt1[i,交易次数]+min(dt1[,max(交易次数)]*0.15,60),paste(dt1[i,交易次数]), 
       cex = 0.75, col = rgb(66,175,182,maxColorValue = 255))
}

################################################################################
## 交易盈亏分布
################################################################################
dt2 <- read_excel(paste0("../output/",file_name, "_回测结果_",
                         data_name,".xlsx"), sheet = "交易盈亏分布") %>% 
  as.data.table() %>% 
  .[,labs := c("<=-50%","-50%","-40%","-30%","-25%","-20%","-15%","-10%","-5%",
              "0%","5%","10%","15%","20%","25%","30%","40%",">=50%")]

par(mar=c(2.0,0.5,1.0,0.5), bg = rgb(18,32,47,maxColorValue = 255))
temp <- 1:nrow(dt2)
plot(temp, dt2[,交易次数], type = 'h',xaxt = "n", axes = F, ylab="", xlab = "",
     main = '交易盈亏分布',col.main = rgb(66,175,182,maxColorValue = 255),cex.main=1.15,
     ylim = c(min(dt2[,交易次数])*0.9,max(dt2[,交易次数])*1.1))
for(i in temp){
  rect(i-0.15, 0, i+0.15, dt2[i,交易次数]+10, col= rgb(66,175,182,maxColorValue = 255), border = NA)
}

mtext(1, at = temp, text = dt2[,labs], col = 'gray50', 
      cex = .5,line = 0.6)
axis(1, at = temp, labels = NA, col = 'gray50')

for(i in temp[1:9]){
  text(i,dt2[i,交易次数]+min(dt1[,max(交易次数)]*0.15,20),paste(dt2[i,交易次数]), cex = 0.70, col = rgb(66,175,182,maxColorValue = 255))
}

for(i in temp[10:18]){
  rect(i-0.15, 0, i+0.15, dt2[i,交易次数]+10, col= 'hotpink', border = NA)
  text(i,dt2[i,交易次数]+min(dt1[,max(交易次数)]*0.15,20),paste(dt2[i,交易次数]), cex = 0.70, col = 'hotpink')
}

################################################################################
## 连续盈利分布
################################################################################

dt3 <- read_excel(paste0("../output/",file_name, "_回测结果_",
                         data_name,".xlsx"), sheet = "连续盈利分布") %>% 
  as.data.table()

par(mar=c(2.0,0.5,2.5,0.5), bg = rgb(18,32,47,maxColorValue = 255))
temp <- 1:nrow(dt3)
plot(temp, dt3[,交易次数.汇总], type = 'h',xaxt = "n", axes = F, ylab="", xlab = "",col=NA,
     main = '连续盈利分布',col.main = rgb(66,175,182,maxColorValue = 255),cex.main=1.15,
     ylim = c(min(dt3$交易次数.汇总)*0.9,max(dt3$交易次数.汇总)*1.1))
for(i in temp){
  rect(i-0.15, 0, i+0.15, dt3[i,交易次数.汇总]+2, col= rgb(66,175,182,maxColorValue = 255), border = NA)
}
mtext(1, at = temp, text = dt3[,连续盈利次数.汇总], col = 'gray50', 
      cex = .5,line = 0.8)
axis(1, at = temp, labels = NA, col = 'gray50')

for(i in temp){
  text(i,dt3[i,交易次数.汇总]+min(max(dt3$交易次数.汇总*0.05),1),pos=3,
       paste(dt3[i,交易次数.汇总]), cex = 0.75, col = rgb(66,175,182,maxColorValue = 255))
}


################################################################################
## 连续亏损分布
################################################################################

dt4 <- read_excel(paste0("../output/",file_name, "_回测结果_",
                         data_name,".xlsx"), sheet = "连续亏损分布") %>% 
  as.data.table()

par(mar=c(2.0,0.5,2.5,0.5), bg = rgb(18,32,47,maxColorValue = 255))
temp <- 1:nrow(dt3)
plot(temp, dt4[,交易次数.汇总], type = 'h',xaxt = "n", axes = F, ylab="", xlab = "",col=NA,
     main = '连续亏损分布',col.main = rgb(66,175,182,maxColorValue = 255),cex.main=1.15,
     ylim = c(min(dt4$交易次数.汇总)*0.90,max(dt4$交易次数.汇总)*1.1))
for(i in temp){
  rect(i-0.15, 0, i+0.15, dt4[i,交易次数.汇总]+2, col= rgb(66,175,182,maxColorValue = 255), border = NA)
}
mtext(1, at = temp, text = dt4[,连续亏损次数.汇总], col = 'gray50', 
      cex = .5,line = 0.8)
axis(1, at = temp, labels = NA, col = 'gray50')

for(i in temp){
  text(i,dt4[i,交易次数.汇总]+min(max(dt4$交易次数.汇总*0.05),1),pos=3,
       paste(dt4[i,交易次数.汇总]), cex = 0.75, col = rgb(66,175,182,maxColorValue = 255))
}

#-------------------------------------------------------------------------------
dev.off()


