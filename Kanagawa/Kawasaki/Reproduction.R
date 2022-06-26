##必要ライブラリの読み込み
library(tidyverse)
library(lubridate)
library(ggplot2)
##前処理
##system("~/COVID19/Kawasaki/prep.sh")
rm(ddf0)
居住地<-c("川崎","幸","中原","高津","宮前","多摩","麻生")
人口<-c(0.233,0.171,0.262,0.233,0.232,0.220,0.180)
ddf0<-data.frame(居住地,人口)

##
df<-read.csv("~/COVID19/Kanagawa/Kawasaki/patient2.csv")
class(df$発表日)
class(df$居住地)
class(df$年代)
class(df$性別)
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
#ddf2<-ddf2[-1,]
#ddf2<-ddf2[order(ddf2$n, decreasing=T),]
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

##
ddf10<-table(df$発表日,df$居住地)
ddf11<-as.data.frame(ddf10)
ddf12<-spread(ddf11,Var2,Freq)
ddf10<-ddf12
ddf10<-ddf10[,-2]
ddf10$発表日<-as.POSIXct(ddf10$Var1)
ddf10<-ddf10[,-1]
ddf10<-ddf10[c(8,1,2,3,4,5,6,7)]
ddf11<-ddf10
ddf11$川崎<-cumsum(ddf10$川崎)
ddf11$幸<-cumsum(ddf10$幸)
ddf11$中原<-cumsum(ddf10$中原)
ddf11$高津<-cumsum(ddf10$高津)
ddf11$宮前<-cumsum(ddf10$宮前)
ddf11$多摩<-cumsum(ddf10$多摩)
ddf11$麻生<-cumsum(ddf10$麻生)
ddf12<- ddf11
ddf12$宮前<-ddf11[,2]/ddf2$人口[ddf2$居住地=="宮前"]/10
ddf12$幸<-ddf11[,3]/ddf2$人口[ddf2$居住地=="幸"]/10
ddf12$高津<-ddf11[,4]/ddf2$人口[ddf2$居住地=="高津"]/10
ddf12$川崎<-ddf11[,5]/ddf2$人口[ddf2$居住地=="川崎"]/10
ddf12$多摩<-ddf11[,6]/ddf2$人口[ddf2$居住地=="多摩"]/10
ddf12$中原<-ddf11[,7]/ddf2$人口[ddf2$居住地=="中原"]/10
ddf12$麻生<-ddf11[,8]/ddf2$人口[ddf2$居住地=="麻生"]/10

##tail(ddf12,n=1)
p1 <- ddf12 %>% gather(ftype, val, -発表日) %>% ggplot(aes(x = 発表日, y = val))
p1 <-p1 + geom_line(aes(color = ftype)) + labs(color = "居住地",size=2)+theme_grey(base_family = "HiraKakuProN-W3") +scale_x_datetime(date_labels = "%m/%d")+ylab("PCR+ per  1/10M")
p1 <-p1+annotate(geom="text",x=as.POSIXct("2020-07-25"),y=tail(ddf12$中原区,n=1)+0.4,label="nakahara",size=3)


## add 2020/05/14
##非常事態宣言解除の判断目安
#「直近１週間の新たな感染者数が10万人あたり、0.5人程度以下」になること
##
##
##直近一週間の新たな感染者数の評価
rm(ddf13,ddf14)

##直近一週間の新規感染者数増加を評価
ddf13<-as.data.frame(ddf11[8:nrow(ddf11),1])
ddf13<-cbind(ddf13,diff(ddf11$宮前,lag=7))
ddf13<-cbind(ddf13,diff(ddf11$幸,lag=7))
ddf13<-cbind(ddf13,diff(ddf11$高津,lag=7))
ddf13<-cbind(ddf13,diff(ddf11$川崎,lag=7))
ddf13<-cbind(ddf13,diff(ddf11$多摩,lag=7))
ddf13<-cbind(ddf13,diff(ddf11$中原,lag=7))
ddf13<-cbind(ddf13,diff(ddf11$麻生,lag=7))
names(ddf13)<-c("発表日","宮前","幸","高津","川崎","多摩","中原","麻生")

##可視化
rm(p2)
p2 <- ddf13 %>% gather(ftype2,val,-発表日)　%>% ggplot(aes(x=発表日, y=val))
p2<-p2 + geom_line(aes(color = ftype2)) + labs(color = "居住地",size=2)+theme_grey(base_family = "HiraKakuProN-W3") +scale_x_datetime(date_labels = "%m/%d")+ylab("#new infections in the last week")
p2<-p2+annotate(geom="text",x=as.POSIXct("2020-07-21"),y=tail(ddf13$高津,n=1),label="Takatsu",size=3)
p2<-p2+annotate(geom="text",x=as.POSIXct("2020-07-21"),y=tail(ddf13$多摩,n=1),label="Tama",size=3)
p2<-p2+annotate(geom="text",x=as.POSIXct("2020-07-21"),y=tail(ddf13$川崎,n=1),label="Kawasaki",size=3)
p2<-p2+annotate(geom="text",x=as.POSIXct("2020-07-21"),y=tail(ddf13$幸,n=1),label="Saiwai",size=3)
p2<-p2+annotate(geom="text",x=as.POSIXct("2020-07-21"),y=tail(ddf13$中原,n=1),label="Nakahara",size=3)
p2


##直近一週間の新らたな感染者数の人口１０万人当たり評価
ddf14<- ddf13
ddf14$宮前<-ddf13[,2]/ddf2$人口[ddf2$居住地=="宮前"]/10
ddf14$幸<-ddf13[,3]/ddf2$人口[ddf2$居住地=="幸"]/10
ddf14$高津<-ddf13[,4]/ddf2$人口[ddf2$居住地=="高津"]/10
ddf14$川崎<-ddf13[,5]/ddf2$人口[ddf2$居住地=="川崎"]/10
ddf14$多摩<-ddf13[,6]/ddf2$人口[ddf2$居住地=="多摩"]/10
ddf14$中原<-ddf13[,7]/ddf2$人口[ddf2$居住地=="中原"]/10
ddf14$麻生<-ddf13[,8]/ddf2$人口[ddf2$居住地=="麻生"]/10
##感染者数でソート
aa <- tail(ddf14[,2:8],1) %>% sort( decreasing = TRUE)
t(aa)

##可視化
rm(p3)
p3 <- ddf14 %>% gather(ftype3,val,-発表日)　%>% ggplot(aes(x=発表日, y=val))
p3<-p3 + geom_line(aes(color = ftype3)) + labs(color = "居住地",size=2, title ="新型コロナ感染状況評価指標",subtitle="~人口10万人あたりの一週間の新規感染者数(人)~", x="発生日",y="新規感染者数(人)")+theme_grey(base_family = "HiraKakuProN-W3") +scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 month")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$宮前,n=1),label="宮前",size=3,hjust=0,family="HiraKakuProN-W3")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$幸,n=1),label="幸",size=3,hjust=0,family="HiraKakuProN-W3")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$高津,n=1),label="高津",size=3,hjust=0,family="HiraKakuProN-W3")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$川崎,n=1),label="川崎",size=3,hjust=0,family="HiraKakuProN-W3")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$多摩,n=1),label="多摩",size=3,hjust=0,family="HiraKakuProN-W3")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$中原,n=1),label="中原",size=3,hjust=0,family="HiraKakuProN-W3")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$麻生,n=1),label="麻生",size=3,hjust=0,family="HiraKakuProN-W3")
p3<-ggsave("p3.png",dpi=400)

##感染者数の人口10万人当たり累積数の対数値を評価
ddf41<- ddf11
ddf41$宮前<-ddf41[,2]/ddf2$人口[ddf2$居住地=="宮前"]/10
ddf41$幸<-ddf41[,3]/ddf2$人口[ddf2$居住地=="幸"]/10
ddf41$高津<-ddf41[,4]/ddf2$人口[ddf2$居住地=="高津"]/10
ddf41$川崎<-ddf41[,5]/ddf2$人口[ddf2$居住地=="川崎"]/10
ddf41$多摩<-ddf41[,6]/ddf2$人口[ddf2$居住地=="多摩"]/10
ddf41$中原<-ddf41[,7]/ddf2$人口[ddf2$居住地=="中原"]/10
ddf41$麻生<-ddf41[,8]/ddf2$人口[ddf2$居住地=="麻生"]/10

##可視化
rm(p4)
p4 <- ddf41 %>% gather(ftype4,val4,-発表日)　%>% ggplot(aes(x=発表日, y=val4))
p4<-p4 + geom_line(aes(color = ftype4)) + labs(color = "居住地",size=2, title ="新型コロナ感染状況評価指標",subtitle="~人口10万人あたり新規感染者数累積(人)~", x="発生日",y="感染者数累積(人)")+theme_grey(base_family = "HiraKakuProN-W3") +scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 month")+scale_y_continuous(trans = "log1p")
##
p4<-p4+annotate(geom="text",x=tail(ddf41$発表日,n=1),y=tail(ddf41$川崎,n=1),label="Kawasaki",size=3,hjust=0)
p4<-p4+annotate(geom="text",x=tail(ddf41$発表日,n=1),y=tail(ddf41$幸,n=1),label="Saiwai",size=3,hjust=0)
p4<-p4+annotate(geom="text",x=tail(ddf41$発表日,n=1),y=tail(ddf41$中原,n=1),label="Nakahara",size=3,hjust=0)
p4<-p4+annotate(geom="text",x=tail(ddf41$発表日,n=1),y=tail(ddf41$高津,n=1),label="Takatsu",size=3,hjust=0)
p4<-p4+annotate(geom="text",x=tail(ddf41$発表日,n=1),y=tail(ddf41$宮前,n=1),label="Miyamae",size=3,hjust=0)
p4<-p4+annotate(geom="text",x=tail(ddf41$発表日,n=1),y=tail(ddf41$多摩,n=1),label="Tama",size=3,hjust=0)
p4<-p4+annotate(geom="text",x=tail(ddf41$発表日,n=1),y=tail(ddf41$麻生,n=1),label="Asou",size=3,hjust=0)

##P5##
ddf51<-ddf14
ddf51<-ddf51[,-2:-6]
ddf51<-ddf51[,-3]
ddf51[which.max(ddf51[,2]),]
tail(ddf51,n=1)
##
##p5<-ddf51 %>% gather(ftype4,val4,-発表日)　%>% ggplot(aes(x=発表日, y=val4))
##p5<-p5+geom_line(aes(color = ftype4)) + labs(color = "居住地",size=2)+theme_grey(base_family = "HiraKakuProN-W3") +scale_x_datetime(date_labels = "%m/%d")+scale_y_continuous(trans = "log1p")+ylab("log10(#infections per 1M)")


#tail(ddf10,n=1) %>% select(-1) %>% gather(key="区域",value="人数",宮前,幸,高津,川崎,多摩,中原,麻生) %>% arrange(-人数)
#tail(ddf10,n=1) %>% select(-1) %>% pivot_longer(key="区域",value="人数",宮前,幸,高津,川崎,多摩,中原,麻生) %>% arrange(-人数)
