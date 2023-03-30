
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

## ��JCSV��
data = read.csv("fn00017-3mc.csv")
data <- data.frame(data)

## �R���L�μƾ� ##

data <- data[-grep("��|��",data$�����O),] 
data <- data[-grep("��T��",data$�����O),] 
data <- data[-grep("�k�ȧ�",data$�����O),] 
data <- data[-grep("���B�u�{��",data$�����O),] 

## �ഫ�榡�qcharacter to intege or float ##

data$���~�׹w���.���B.�d��. <- as.integer(data$���~�׹w���.���B.�d��.)

#data$���~�׹w���.�ʤ���... <- as.numeric(data$���~�׹w���.�ʤ���...)
round(as.numeric(data$���~�׹w���.�ʤ���...),3)

data$�֭p���t��.���B.�d��. <- as.integer(data$�֭p���t��.���B.�d��.)

#data$�֭p���t��.�e�w���... <- as.numeric(data$�֭p���t��.�e�w���... )
round(as.numeric(data$�֭p���t��.�e�w���... ),3)

data$�֭p�����.���B.�d��. <- as.integer(data$�֭p�����.���B.�d��.)

#data$�֭p�����.�e�w���... <- as.numeric(data$�֭p�����.�e�w���...)
round(as.numeric(data$�֭p�����.�e�w���...),3)

#data$�֭p�����.�e���t��... <- as.numeric(data$�֭p�����.�e���t��...)
data$�֭p�����.�e���t��... <- round(as.numeric(data$�֭p�����.�e���t��...),3)

## �Q�~�ϥθg�O��Ҫ����� ##
departname <- c("��ĳ�|","���F��","���F��","�]�F��","�Ш|��","���~�o�i��","�u�ȧ�","��q��","���|��","�Ұʧ�"
                ,"ĵ�","�åͧ�","���ҫO�@��","�����o�i��","��Ƨ�","������","�B�A���w�޲z��","�[���Ǽ���","�a�F��","�L�Ч�","�O�_�ۨӤ��Ʒ~�B")
yearsmean <- NULL
yearsmean <- data.frame(depart = "��ĳ�|", percentage = as.numeric(unname(summary(data$�֭p�����.�e���t��...[grep("��ĳ�|",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("���F��", unname(summary(data$�֭p�����.�e���t��...[grep("���F��",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("���F��", unname(summary(data$�֭p�����.�e���t��...[grep("���F��",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("�]�F��", unname(summary(data$�֭p�����.�e���t��...[grep("�]�F��",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("�Ш|��", unname(summary(data$�֭p�����.�e���t��...[grep("�Ш|��",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("���~�o�i��", unname(summary(data$�֭p�����.�e���t��...[grep("���~�o�i��",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("�u�ȧ�", unname(summary(data$�֭p�����.�e���t��...[grep("�u�ȧ�",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("��q��", unname(summary(data$�֭p�����.�e���t��...[grep("��q��",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("���|��", unname(summary(data$�֭p�����.�e���t��...[grep("���|��",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("�Ұʧ�", unname(summary(data$�֭p�����.�e���t��...[grep("�Ұʧ�",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("ĵ�", unname(summary(data$�֭p�����.�e���t��...[grep("ĵ�",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("�åͧ�", unname(summary(data$�֭p�����.�e���t��...[grep("�åͧ�",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("���ҫO�@��", unname(summary(data$�֭p�����.�e���t��...[grep("���ҫO�@��",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("�����o�i��", unname(summary(data$�֭p�����.�e���t��...[grep("�����o�i��",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("��Ƨ�", unname(summary(data$�֭p�����.�e���t��...[grep("��Ƨ�",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("������", unname(summary(data$�֭p�����.�e���t��...[grep("������",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("�B�A���w�޲z��", unname(summary(data$�֭p�����.�e���t��...[grep("�B�A���w�޲z��",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("�[���Ǽ���", unname(summary(data$�֭p�����.�e���t��...[grep("�[���Ǽ���",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("�a�F��", unname(summary(data$�֭p�����.�e���t��...[grep("�a�F��",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("�L�Ч�", unname(summary(data$�֭p�����.�e���t��...[grep("�L�Ч�",data$�����O)])[4])))
yearsmean <- rbind(yearsmean, c("�O�_�ۨӤ��Ʒ~�B", unname(summary(data$�֭p�����.�e���t��...[grep("�O�_�ۨӤ��Ʒ~�B",data$�����O)])[4])))
#class(as.numeric(unname(summary(data$�֭p�����.�e���t��...[grep("��ĳ�|",data$�����O)])[4])))
#row.names(yearsmean) <- departname
#colnames(yearsmean) <- "percentage"
#rownames(yearsmean)
yearsmean$percentage <- round(as.numeric(yearsmean$percentage),2)

## �e��

ggplot(yearsmean,aes( x = depart,y = percentage)) + geom_col() + scale_x_discrete( guide = guide_axis(angle=90) )
qplot( yearsmean$depart, yearsmean$percentage )


## ��Xcsv

write.csv(data, "newdata.csv", row.names = FALSE)
write.csv(yearsmean, "yearspercentenge.csv", row.names = FALSE)
