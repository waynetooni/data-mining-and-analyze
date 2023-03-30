
setwd("C:/Users/lab602/Desktop/data mining") 

library(bitops)
library(RCurl) 
library(XML)
library(rvest)
library(httr)
library(curl)
library(plyr)
library(stringr)
library(lattice)
library(ggplot2)
library(readr)
library(data.table)

## 輸入CSV檔
data = read.csv("fn00017-3mc.csv")
data <- data.frame(data)

## 刪除無用數據 ##

data <- data[-grep("體育局",data$機關別),] 
data <- data[-grep("資訊局",data$機關別),] 
data <- data[-grep("法務局",data$機關別),] 
data <- data[-grep("捷運工程局",data$機關別),] 

## 轉換格式從character to intege or float ##

data$全年度預算數.金額.千元. <- as.integer(data$全年度預算數.金額.千元.)

#data$全年度預算數.百分比... <- as.numeric(data$全年度預算數.百分比...)
round(as.numeric(data$全年度預算數.百分比...),3)

data$累計分配數.金額.千元. <- as.integer(data$累計分配數.金額.千元.)

#data$累計分配數.占預算數... <- as.numeric(data$累計分配數.占預算數... )
round(as.numeric(data$累計分配數.占預算數... ),3)

data$累計執行數.金額.千元. <- as.integer(data$累計執行數.金額.千元.)

#data$累計執行數.占預算數... <- as.numeric(data$累計執行數.占預算數...)
round(as.numeric(data$累計執行數.占預算數...),3)

#data$累計執行數.占分配數... <- as.numeric(data$累計執行數.占分配數...)
data$累計執行數.占分配數... <- round(as.numeric(data$累計執行數.占分配數...),3)

## 十年使用經費比例的平均 ##
departname <- c("市議會","市政府","民政局","財政局","教育局","產業發展局","工務局","交通局","社會局","勞動局"
                ,"警察局","衛生局","環境保護局","都市發展局","文化局","消防局","翡翠水庫管理局","觀光傳播局","地政局","兵役局","臺北自來水事業處")
yearsmean <- NULL
yearsmean <- data.frame(depart = "市議會", percentage = as.numeric(unname(summary(data$累計執行數.占分配數...[grep("市議會",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("市政府", unname(summary(data$累計執行數.占分配數...[grep("市政府",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("民政局", unname(summary(data$累計執行數.占分配數...[grep("民政局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("財政局", unname(summary(data$累計執行數.占分配數...[grep("財政局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("教育局", unname(summary(data$累計執行數.占分配數...[grep("教育局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("產業發展局", unname(summary(data$累計執行數.占分配數...[grep("產業發展局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("工務局", unname(summary(data$累計執行數.占分配數...[grep("工務局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("交通局", unname(summary(data$累計執行數.占分配數...[grep("交通局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("社會局", unname(summary(data$累計執行數.占分配數...[grep("社會局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("勞動局", unname(summary(data$累計執行數.占分配數...[grep("勞動局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("警察局", unname(summary(data$累計執行數.占分配數...[grep("警察局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("衛生局", unname(summary(data$累計執行數.占分配數...[grep("衛生局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("環境保護局", unname(summary(data$累計執行數.占分配數...[grep("環境保護局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("都市發展局", unname(summary(data$累計執行數.占分配數...[grep("都市發展局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("文化局", unname(summary(data$累計執行數.占分配數...[grep("文化局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("消防局", unname(summary(data$累計執行數.占分配數...[grep("消防局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("翡翠水庫管理局", unname(summary(data$累計執行數.占分配數...[grep("翡翠水庫管理局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("觀光傳播局", unname(summary(data$累計執行數.占分配數...[grep("觀光傳播局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("地政局", unname(summary(data$累計執行數.占分配數...[grep("地政局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("兵役局", unname(summary(data$累計執行數.占分配數...[grep("兵役局",data$機關別)])[4])))
yearsmean <- rbind(yearsmean, c("臺北自來水事業處", unname(summary(data$累計執行數.占分配數...[grep("臺北自來水事業處",data$機關別)])[4])))
#class(as.numeric(unname(summary(data$累計執行數.占分配數...[grep("市議會",data$機關別)])[4])))
#row.names(yearsmean) <- departname
#colnames(yearsmean) <- "percentage"
#rownames(yearsmean)
yearsmean$percentage <- round(as.numeric(yearsmean$percentage),2)

## 畫圖

ggplot(yearsmean,aes( x = depart,y = percentage)) + geom_col() + scale_x_discrete( guide = guide_axis(angle=90) )
qplot( yearsmean$depart, yearsmean$percentage )


## 輸出csv

write.csv(data, "newdata.csv", row.names = FALSE)
write.csv(yearsmean, "yearspercentenge.csv", row.names = FALSE)

