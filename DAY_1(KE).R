setwd("D:/R_DragonBall/R_DragonBall")
library(DT)
library(zoo)
library(plotly)
library(lubridate)
library(rmarkdown)
library(data.table)
library(tidyverse)
library(kableExtra)
options(dplyr.print_max=1e9)
train<-read.csv("train.csv") #讀取資料
head(train) 
dat<-train%>% #房屋外觀
  select(contains("Exterior"))
head(dat)
Exter1st<-train%>% #外觀一
  group_by(Id, Exterior1st)%>% #計算每個id在這些外觀類型中的分佈不
  summarize(count=n())%>%
  spread(Exterior1st,count,fill=0) #將這些外觀類型展開
head(Exter1st)
Exter2nd<-train%>% #外觀二
  group_by(Id, Exterior2nd)%>%
  summarize(count=n())%>%
  spread(Exterior2nd,count,fill=0)
head(Exter2nd)
Exterior<-
  bind_rows(Exter1st,Exter2nd)%>%
  group_by(Id)%>%
  summarize_all(funs(sum(., na.rm=TRUE)))%>%
  rename_all(function(x) paste0("Exterior_", x))
train1<-train%>%
  left_join(Exterior, by=c("Id"="Exterior_Id"))
datatable(train1%>%select(Id, contains("Exterior")))

