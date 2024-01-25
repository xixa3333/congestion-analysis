#install.packages("ggplot2")
#install.packages("openxlsx")
library(openxlsx)
#將5年的車流量丟入串列
car107=read.csv("C:/Users/xixa3/Desktop/X/程式/R/線上上課課程/期末/資料/107年日交通量參考值(主線).csv")
car108=read.csv("C:/Users/xixa3/Desktop/X/程式/R/線上上課課程/期末/資料/108年日交通量參考值(主線)_022573.csv")
car109=read.csv("C:/Users/xixa3/Desktop/X/程式/R/線上上課課程/期末/資料/109年日交通量參考值(主線).csv")
car110=read.csv("C:/Users/xixa3/Desktop/X/程式/R/線上上課課程/期末/資料/110年日交通量參考值(主線).csv")
car111=read.csv("C:/Users/xixa3/Desktop/X/程式/R/線上上課課程/期末/資料/111年日交通量參考值(主線)_修正.csv")
car108=car108[,-(3:4)]
car109=car109[,-(3:4)]
car110=car110[,-(3:4)]
car111=car111[,-(3:4)]
car=list(car107,car108,car109,car110,car111)
samecar=car
temp=data.frame()
file1 <- "C:/Users/xixa3/Desktop/X/程式/R/線上上課課程/期末/107-111車流量.xlsx"
wb <- createWorkbook()
lapply(seq_along(car), function(i) {
  sheet_name <- paste0(106+i)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, x = car[[i]])
})
saveWorkbook(wb, excel_file)

#找出5年每年相同的路段且命名
compare=Reduce(intersect,list(car[[1]][,2],car[[2]][,2],car[[3]][,2],car[[4]][,2],car[[5]][,2]))
for(x in 1:5){
  temp2=car[[x]]
  for (i in 1:180) {
    for (j in 1:length(samecar[[x]][,2])) {
      if (samecar[[x]][j,2] == compare[i]) {
        temp=rbind(temp,samecar[[x]][j,])
        break
      }
    }
  }
  samecar[[x]]=temp
  rownames(samecar[[x]])=c(1:180)
  temp=data.frame()
}
names(samecar)=107:111
names(car)=107:111

#計算5年來相同路段的周六日跟2.4每個路段車流量的平均值
samecar_mean107_111=samecar[[1]]
samecar_mean107_111[,3:5]=0
for(i in 1:180){
  for(j in 1:5){
    samecar_mean107_111[i,3:5]=samecar[[j]][i,3:5]+samecar_mean107_111[i,3:5]
  }
  samecar_mean107_111[i,3:5]=samecar_mean107_111[i,3:5]/5
}
samecar_mean107_111=samecar_mean107_111[order(-samecar_mean107_111$週六),]
rownames(samecar_mean107_111)=c(1:180)
samecar[[6]]=samecar_mean107_111
colnames(samecar[[6]])=c("路線方向","路段","週六","週日","週2.4")
names(samecar)[6]="107-111"

#全部照週六車流量降序
for(x in 1:5){
  car[[x]]=car[[x]][order(-car[[x]]$週六),]
  samecar[[x]]=samecar[[x]][order(-samecar[[x]]$週六),]
  rownames(car[[x]])=c(1:length(car[[x]][,2]))
  rownames(samecar[[x]])=c(1:length(samecar[[x]][,2]))
}
file.create("C:/Users/xixa3/Desktop/X/程式/R/線上上課課程/期末/5年來路段相同車流量.csv")
write.csv(samecar[[6]], file = "C:/Users/xixa3/Desktop/X/程式/R/線上上課課程/期末/5年來路段相同車流量.csv", row.names = FALSE, fileEncoding = "big5")

#可以透過圖表找出5年來佔據車流量的多少
require(ggplot2)
ggplot(data=samecar[["107-111"]])+
  geom_point(aes(x=週六,y=週日,color=路線方向))+
  theme_bw()
#可以透過圖表找出5年來前幾個的車流量的多少
ggplot(data=head(samecar[["107-111"]],n=7))+
  geom_point(aes(x=週六,y=週日,color=路線方向))+
  theme_bw()
ggplot(data=head(samecar[["107-111"]],n=7))+
  geom_point(aes(x=週六,y=週日,color=路段))+
  theme_bw()

#可以透過圖表找出111年前幾個的車流量的多少
ggplot(data=car[["111"]])+
  geom_point(aes(x=週六,y=週日,color=路線方向))+
  theme_bw()
ggplot(data=head(car[["111"]],n=10))+
  geom_point(aes(x=週六,y=週日,color=路線方向))+
  theme_bw()
ggplot(data=head(car[["111"]],n=10))+
  geom_point(aes(x=週六,y=週日,color=路段))+
  theme_bw()

#算出5年每年相同跟全部的路段的車流量平均
#可以透過這個知道因疫情影響的車流量
temp=data.frame(1,2,3)
colnames(temp)=c("週六","週日","平日")
temp=temp[-1,]
for(i in 1:5){
  for(j in 3:5){
    temp[i,j-2]=mean(car[[i]][,(j)])
  }
}
rownames(temp)=c(107:111)
average=temp2
average$年度=107:111
file.create("C:/Users/xixa3/Desktop/X/程式/R/線上上課課程/期末/每年平均車流量.csv")
write.csv(average, file = "C:/Users/xixa3/Desktop/X/程式/R/線上上課課程/期末/每年平均車流量.csv", row.names = FALSE, fileEncoding = "big5")

#可以透過圖表知道因疫情影響的車流量
ggplot(data=average)+
  geom_point(aes(x=週六,y=週日,color=年度))+
  theme_bw()