##必要ライブラリの読み込み
library(tidyverse)
library(lubridate)
##前処理
rm(ddf0)
居住地<-c("中原")
人口<-c(0.262)
ddf0<-data.frame(居住地,人口)

rm(ddf01)
年代<-c(10,20,30,40,50,60,70,80,90)
男性<-c(22863,19480,23274,23292,18345,10925,8359,3615,677)
女性<-c(21467,18449,22130,22015,16350,9982,9362,6061,1909)
ddf01<-data.frame(年代,男性,女性)
ddf01$年代<-as.character(ddf01$年代)
xx <- ddf01 %>% gather(key=性別, value=人数,男性,女性)
ddf02 <- ddf01
ddf02 <- ddf02 %>% mutate(合計 = 男性 + 女性 )



dd<-read.csv("~/COVID19/Kanagawa/Kawasaki/patient2.csv")
df <- as.data.frame(dd)
df <- df %>% filter(居住地=="中原")
df$発表日<-as.POSIXct(df$発表日)
df$居住地<-as.character(df$居住地)
df$年代<-as.character(df$年代)
df$性別<-as.character(df$性別)

df1<- df %>% select(-居住地,-性別)
df2<-count(df1,発表日,年代)　

df3<- spread(df2,key=年代,value=n)
df4 <- as.data.frame(df3)
#NAをzero(0)に修飾
df4[is.na(df4)] <- 0

#年代毎の累積和を集計
df5 <- df4 %>% gather(key="gen",value = "感染者数",2:10) %>% group_by(gen) %>% mutate(cumsum=cumsum(感染者数)) %>% select(-感染者数) %>% spread(key=gen,value=cumsum)
df5 <- as.data.frame(df5)


#世代別規感染者数
p1 <- df5 %>% gather(ftype,n,-発表日) %>% ggplot(aes(x=発表日,y=n))
p1 <- p1 + geom_line(aes(color = ftype))+scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 month")
p1 <- p1 + labs(color = "世代",size=2, title ="新型コロナ感染状況評価指標",subtitle="~世代別規感染者数(人)~", x="発生日",y="感染者数(人)")+theme_grey(base_family = "HiraKakuProN-W3")
p1 <- p1 + annotate(geom="text",x=tail(df5$発表日,n=1),y=tail(df5[,2],n=1),label="10",size=3,hjust=0)
p1 <- p1 + annotate(geom="text",x=tail(df5$発表日,n=1),y=tail(df5[,3],n=1),label="20",size=3,hjust=0)
p1 <- p1 + annotate(geom="text",x=tail(df5$発表日,n=1),y=tail(df5[,4],n=1),label="30",size=3,hjust=0)
p1 <- p1 + annotate(geom="text",x=tail(df5$発表日,n=1),y=tail(df5[,5],n=1),label="40",size=3,hjust=0)
p1 <- p1 + annotate(geom="text",x=tail(df5$発表日,n=1),y=tail(df5[,6],n=1),label="50",size=3,hjust=0)
p1 <- p1 + annotate(geom="text",x=tail(df5$発表日,n=1),y=tail(df5[,7],n=1),label="60",size=3,hjust=0)
p1 <- p1 + annotate(geom="text",x=tail(df5$発表日,n=1),y=tail(df5[,8],n=1),label="70",size=3,hjust=0)
p1 <- p1 + annotate(geom="text",x=tail(df5$発表日,n=1),y=tail(df5[,9],n=1),label="80",size=3,hjust=0)
p1 <- p1 + annotate(geom="text",x=tail(df5$発表日,n=1),y=tail(df5[,10],n=1),label="90",size=3,hjust=0)

#世代別規感染者率
##全年代層
df6<-df5
df6[,2]= df5[,2]/ddf02$合計[ddf02$年代=="10"]*100
df6[,3]= df5[,2]/ddf02$合計[ddf02$年代=="20"]*100
df6[,4]= df5[,2]/ddf02$合計[ddf02$年代=="30"]*100
df6[,5]= df5[,2]/ddf02$合計[ddf02$年代=="40"]*100
df6[,6]= df5[,2]/ddf02$合計[ddf02$年代=="50"]*100
df6[,7]= df5[,2]/ddf02$合計[ddf02$年代=="60"]*100
df6[,8]= df5[,2]/ddf02$合計[ddf02$年代=="70"]*100
df6[,9]= df5[,2]/ddf02$合計[ddf02$年代=="80"]*100
df6[,10]= df5[,2]/ddf02$合計[ddf02$年代=="90"]*100
p2 <- df6 %>% gather(ftype,val,-発表日) %>% ggplot(aes(x=発表日,y=val))
p2 <- p2 + geom_line(aes(color = ftype))+scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 month")
p2 <- p2 + labs(color = "世代",size=2, title ="新型コロナ感染状況評価指標",subtitle="~世代別規感染者率~", x="発生日",y="感染率(%)")+theme_grey(base_family = "HiraKakuProN-W3")
p2 <- p2 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,2],n=1),label="10",size=3,hjust=0)
p2 <- p2 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,3],n=1),label="20",size=3,hjust=0)
p2 <- p2 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,4],n=1),label="30",size=3,hjust=0)
p2 <- p2 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,5],n=1),label="40",size=3,hjust=0)
p2 <- p2 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,6],n=1),label="50",size=3,hjust=0)
p2 <- p2 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,7],n=1),label="60",size=3,hjust=0)
p2 <- p2 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,8],n=1),label="70",size=3,hjust=0)
p2 <- p2 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,9],n=1),label="80",size=3,hjust=0)
p2 <- p2 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,10],n=1),label="90",size=3,hjust=0)


#生産人口(20-50)
df6<-df5
df6[,3]= df5[,2]/ddf02$合計[ddf02$年代=="20"]*100
df6[,4]= df5[,2]/ddf02$合計[ddf02$年代=="30"]*100
df6[,5]= df5[,2]/ddf02$合計[ddf02$年代=="40"]*100
df6[,6]= df5[,2]/ddf02$合計[ddf02$年代=="50"]*100
df6<-df6[,-2]
df6<-df6[,-6:-9]
p3 <- df6 %>% gather(ftype,val,-発表日) %>% ggplot(aes(x=発表日,y=val))
p3 <- p3 + geom_line(aes(color = ftype))+scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 month")
p3 <- p3 + labs(color = "世代",size=2, title ="新型コロナ感染状況評価指標",subtitle="~世代別規感染者率~", x="発生日",y="感染率(%)")+theme_grey(base_family = "HiraKakuProN-W3")
p3 <- p3 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,2],n=1),label="20",size=3,hjust=0)
p3 <- p3 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,3],n=1),label="30",size=3,hjust=0)
p3 <- p3 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,4],n=1),label="40",size=3,hjust=0)
p3 <- p3 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,5],n=1),label="50",size=3,hjust=0)


#若年層(10-20)
df6<-df5
df6[,2]= df5[,2]/ddf02$合計[ddf02$年代=="10"]*100
df6[,3]= df5[,2]/ddf02$合計[ddf02$年代=="20"]*100
df6<-df6[,-4:-10]
p4 <- df6 %>% gather(ftype,val,-発表日) %>% ggplot(aes(x=発表日,y=val))
p4 <- p4 + geom_line(aes(color = ftype))+scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 month")
p4 <- p4 + labs(color = "世代",size=2, title ="新型コロナ感染状況評価指標",subtitle="~世代別規感染者率~", x="発生日",y="感染率(%)")+theme_grey(base_family = "HiraKakuProN-W3")
p4 <- p4 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,2],n=1),label="10",size=3,hjust=0)
p4 <- p4 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,3],n=1),label="20",size=3,hjust=0)


#老年層(60-90)
df6<-df5
df6[,7]= df5[,2]/ddf02$合計[ddf02$年代=="60"]*100
df6[,8]= df5[,2]/ddf02$合計[ddf02$年代=="70"]*100
df6[,9]= df5[,2]/ddf02$合計[ddf02$年代=="80"]*100
df6[,10]= df5[,2]/ddf02$合計[ddf02$年代=="90"]*100
df6<-df6[,-2:-6]
p5 <- df6 %>% gather(ftype,val,-発表日) %>% ggplot(aes(x=発表日,y=val))
p5 <- p5 + geom_line(aes(color = ftype))+scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 month")
p5 <- p5 + labs(color = "世代",size=2, title ="新型コロナ感染状況評価指標",subtitle="~世代別規感染者率~", x="発生日",y="感染率(%)")+theme_grey(base_family = "HiraKakuProN-W3")
p5 <- p5 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,2],n=1),label="60",size=3,hjust=0)
p5 <- p5 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,3],n=1),label="70",size=3,hjust=0)
p5 <- p5 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,4],n=1),label="80",size=3,hjust=0)
p5 <- p5 + annotate(geom="text",x=tail(df6$発表日,n=1),y=tail(df6[,5],n=1),label="90",size=3,hjust=0)
