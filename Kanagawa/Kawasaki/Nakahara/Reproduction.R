##必要ライブラリの読み込み
library(tidyverse)
library(lubridate)
##前処理
##system("/Users/momma/COVID19/Kawasaki/prep.sh")
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


##
df<-read.csv("~/COVID19/Kanagawa/Kawasaki/patient2.csv")
class(df$発表日)
class(df$居住地)
class(df$年代)
class(df$性別)
df$発表日<-as.POSIXct(df$発表日)
df$居住地<-as.character(df$居住地)
df$年代<-as.character(df$年代)
df$性別<-as.character(df$性別)
ddf1<-count(df,発表日)
ddf2<-count(df,居住地)
ddf3<-count(df,年代)
ddf4<-count(df,性別)
ddf1$date<-as.POSIXct(ddf1$発表日)
p01<-ggplot(ddf1,aes(x=date,y=n))+geom_col()+xlab("date")+scale_x_datetime(date_labels = "%m/%d")
P02<-ggplot(ddf3,aes(x=年代,y=n))+geom_col()+xlab("gen")


##保健所別の感染者数をソートし確認。非該当行を削除
#ddf2<-ddf2[order(ddf2$n, decreasing=T),]
#ddf2<-ddf2[-8,]
#ddf2<-ddf2[-1,]
#ddf2<-ddf2[-2:-6,]
ddf2 <- ddf2 %>% filter(居住地 !='')

##感染率を計算
##ddf2$人口<-c(3.75,1.53,0.85,0.72,0.30,0.43,0.63,0.39,0.24,0.34)
ddf2<-merge(ddf2,ddf0)
ddf2$感染率<-(ddf2$n / ddf2$人口) /10

##可視化
p03<-ggplot(ddf2,aes(x=reorder(居住地,-n),n))+geom_col()+theme(text = element_text(family = "HiraKakuPro-W3"))
p04<-ggplot(ddf2,aes(x=reorder(居住地,-感染率),感染率))+geom_col()+theme(text = element_text(family = "HiraKakuPro-W3"))+xlab("居住地")+theme(text = element_text(family = "HiraKakuPro-W3"))
p05<-ggplot(ddf2,aes(x=人口,y=n))+geom_point()+geom_label(aes(label = 居住地), size = 2, family = "HiraKakuPro-W3")+theme(text = element_text(family = "HiraKakuPro-W3"))
p06<-ggplot(ddf2,aes(x=人口,y=感染率))+geom_point()+geom_label(aes(label = 居住地), size = 2, family = "HiraKakuPro-W3")+theme(text = element_text(family = "HiraKakuPro-W3"))

ddf10<-table(df$発表日,df$居住地)
ddf11<-as.data.frame(ddf10)
ddf12<-spread(ddf11,Var2,Freq)
ddf10<-ddf12
ddf10<-ddf10[,-2]
ddf10$発表日<-as.POSIXct(ddf10$Var1)
ddf10<-ddf10[,-1]

ddf10<-ddf10[c(8,6)]
ddf11<-ddf10
ddf11$中原<-cumsum(ddf10$中原)
ddf12<- ddf11
ddf12$中原<-ddf11[,2]/ddf2$人口[ddf2$居住地=="中原"]/10



##tail(ddf12,n=1)
p1 <- ddf12 %>% gather(ftype, val, -発表日) %>% ggplot(aes(x = 発表日, y = val))
p1 <-p1 + geom_line(aes(color = ftype)) + labs(color = "居住地",size=2)+theme_grey(base_family = "HiraKakuProN-W3") +scale_x_datetime(date_labels = "%m/%d")+ylab("PCR+ per  1/10M")
p1 <-p1+annotate(geom="text",x=as.POSIXct("2020-07-25"),y=tail(ddf12$中原区,n=1)+0.4,label="kamakura",size=3)


## add 2020/05/14
##非常事態宣言解除の判断目安
#「直近１週間の新たな感染者数が10万人あたり、0.5人程度以下」になること
##
##
##直近一週間の新たな感染者数の評価
rm(ddf13,ddf14)

##直近一週間の新規感染者数増加を評価
ddf13<-as.data.frame(ddf11[8:nrow(ddf11),1])
ddf13<-cbind(ddf13,diff(ddf11$中原,lag=7))
names(ddf13)<-c("発表日","中原")

##可視化
rm(p2)
p2 <- ddf13 %>% gather(ftype2,val,-発表日)　%>% ggplot(aes(x=発表日, y=val))
p2<-p2 + geom_line(aes(color = ftype2)) + labs(color = "居住地",size=2)+theme_grey(base_family = "HiraKakuProN-W3") +scale_x_datetime(date_labels = "%m/%d")+ylab("#new infections in the last week")
p2<-p2+annotate(geom="text",x=as.POSIXct("2020-07-21"),y=tail(ddf13$中原,n=1),label="Nakahara",size=3)
p2


##直近一週間の新らたな感染者数の人口１０万人当たり評価
ddf14<- ddf13
ddf14$中原<-ddf13[,2]/ddf2$人口[ddf2$居住地=="中原"]/10

##感染者数でソート
aa <- tail(ddf14$中原,1) %>% sort( decreasing = TRUE)
t(aa)
##可視化
rm(p3)
p3 <- ddf14 %>% gather(ftype3,val,-発表日)　%>% ggplot(aes(x=発表日, y=val))
p3<-p3 + geom_line(aes(color = ftype3)) + labs(color = "居住地",size=2, title ="新型コロナ感染状況評価指標",subtitle="~人口10万人あたりの一週間の新規感染者数(人)~", x="発生日",y="感染者数(人)")+theme_grey(base_family = "HiraKakuProN-W3") +scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 month")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$中原,n=1),label="中原",size=3,hjust=0,family="HiraKakuProN-W3")


##感染者数の人口10万人当たり累積数の対数値を評価
ddf41<- ddf11
ddf41$中原<-ddf41$中原/(ddf2$人口*10)

##可視化
rm(p4)
p4 <- ddf41 %>% gather(ftype4,val4,-発表日)　%>% ggplot(aes(x=発表日, y=val4))
p4<-p4 + geom_line(aes(color = ftype4)) + labs(color = "居住地",size=2)+theme_grey(base_family = "HiraKakuProN-W3") +scale_x_datetime(date_labels = "%m/%d",date_breaks = "1 month")+scale_y_continuous(trans = "log1p")+ylab("log10(#infections per 1M)")
p4<-p4+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf41$中原,n=1),label="Nakahara",size=3,hjust=0)


##可視化　年代別クロス集計
cross <- df[df$居住地=="中原",]
df301<-count(cross,年代,性別)

#Pos設定
df301[df301$性別=="女性" & df301$年代=="10","Pos"]<-df301[df301$性別=="男性" & df301$年代=="10","n"] + df301[df301$性別=="女性" & df301$年代=="10","n"]+1
df301[df301$性別=="女性" & df301$年代=="20","Pos"]<-df301[df301$性別=="男性" & df301$年代=="20","n"] + df301[df301$性別=="女性" & df301$年代=="20","n"]+1
df301[df301$性別=="女性" & df301$年代=="30","Pos"]<-df301[df301$性別=="男性" & df301$年代=="30","n"] + df301[df301$性別=="女性" & df301$年代=="30","n"]+1
df301[df301$性別=="女性" & df301$年代=="40","Pos"]<-df301[df301$性別=="男性" & df301$年代=="40","n"] + df301[df301$性別=="女性" & df301$年代=="40","n"]+1
df301[df301$性別=="女性" & df301$年代=="50","Pos"]<-df301[df301$性別=="男性" & df301$年代=="50","n"] + df301[df301$性別=="女性" & df301$年代=="50","n"]+1
df301[df301$性別=="女性" & df301$年代=="60","Pos"]<-df301[df301$性別=="男性" & df301$年代=="60","n"] + df301[df301$性別=="女性" & df301$年代=="60","n"]+1
df301[df301$性別=="女性" & df301$年代=="70","Pos"]<-df301[df301$性別=="男性" & df301$年代=="70","n"] + df301[df301$性別=="女性" & df301$年代=="70","n"]+1
df301[df301$性別=="女性" & df301$年代=="80","Pos"]<-df301[df301$性別=="男性" & df301$年代=="80","n"] + df301[df301$性別=="女性" & df301$年代=="80","n"]+1
df301[df301$性別=="女性" & df301$年代=="90","Pos"]<-df301[df301$性別=="女性" & df301$年代=="90","n"]+1
df301[df301$性別=="男性" ,"Pos"]<-df301[df301$性別=="男性" ,"n"]-1
#可視化
p5 <- df301 %>%　ggplot(aes(x =  年代, y = n, fill = 性別))+geom_bar(stat = "identity", position = "stack")+
geom_text(aes(label = n, y = Pos))+theme_grey(base_family = "HiraKakuProN-W3")

#年代・性別ランク
xx$年代<-as.character(xx$年代)
xxx<-right_join(xx,df301)
xxx$ratio<-(xxx$n / xxx$人数 ) * 100
xxx$ratio %>% sort(decreasing = TRUE)
xxx %>% arrange(desc(xxx$ratio))
