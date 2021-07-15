##必要ライブラリの読み込み
library(tidyverse)
library(lubridate)
##前処理
system("~/COVID19/Kanagawa/prep.sh")
##
rm(ddf0)
居住地<-c("横浜","川崎","厚木","相模原","藤沢","鎌倉","平塚","横須賀","小田原","茅ヶ崎")
人口<-c(3.75,1.53,0.85,0.72,0.43,0.30,0.63,0.39,0.34,0.24)
ddf0<-data.frame(居住地,人口)
##
df<-read.csv("~/COVID19/Kanagawa/patient2.csv")
df$発表日<-as.POSIXct(df$発表日)
df$居住地<-as.character(df$居住地)
df$年代<-as.character(df$年代)
df$性別<-as.character(df$性別)
ddf1<-count(df,発表日)
ddf2<-count(df,居住地)
ddf3<-count(df,年代)
ddf4<-count(df,性別)
ddf1$date<-as.POSIXct(ddf1$発表日)


##可視化
##新規感染者数
p01<-ggplot(ddf1,aes(x=発表日,y=n))+geom_col(aes(color="red"))
p01<-p01+labs(title ="新型コロナ感染状況評価指標",subtitle="新規感染者数(人)~",x="発表日",y="新規感染者数(人)",color="神奈川県")+theme_bw(base_family = "HiraKakuProN-W3")
p01<-p01+scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 month")


##ggplot(ddf3,aes(x=年代,y=n))+geom_col()+xlab("gen")


##保健所別の感染者数をソートし確認。非該当行を削除
#ddf2<-ddf2[order(ddf2$n, decreasing=T),]
#ddf2<-ddf2[-11,]
ddf2 <- ddf2 %>% filter(居住地 !='')

##感染率を計算
##ddf2$人口<-c(3.75,1.53,0.85,0.72,0.30,0.43,0.63,0.39,0.24,0.34)



ddf2<-merge(ddf2,ddf0)
ddf2$感染率<-(ddf2$n / ddf2$人口) /10

##可視化
##ggplot(ddf2,aes(x=reorder(居住地,-n),n))+geom_col()+theme(text = element_text(family = "HiraKakuPro-W3"))
#ggplot(ddf2,aes(x=reorder(居住地,-感染率),感染率))+geom_col()+theme(text = element_text(family = "HiraKakuPro-W3"))+xlab("居住地")+theme(text = element_text(family = "HiraKakuPro-W3"))
##ggplot(ddf2,aes(x=人口,y=n))+geom_point()+geom_label(aes(label = 居住地), size = 2, family = "HiraKakuPro-W3")+theme(text = element_text(family = "HiraKakuPro-W3"))
##ggplot(ddf2,aes(x=人口,y=感染率))+geom_point()+geom_label(aes(label = 居住地), size = 2, family = "HiraKakuPro-W3")+theme(text = element_text(family = "HiraKakuPro-W3"))

ddf10<-table(df$発表日,df$居住地)
ddf11<-as.data.frame(ddf10)
ddf12<-spread(ddf11,Var2,Freq)
ddf10<-ddf12
ddf10<-ddf10[,-2]
ddf10$発表日<-as.POSIXct(ddf10$Var1)
ddf10<-ddf10[,-1]
ddf10<-ddf10[c(11,1,2,3,4,5,6,7,8,9,10)]
ddf11<-ddf10
ddf11$横須賀<-cumsum(ddf10$横須賀)
ddf11$横浜<-cumsum(ddf10$横浜)
ddf11$鎌倉<-cumsum(ddf10$鎌倉)
ddf11$茅ヶ崎<-cumsum(ddf10$茅ヶ崎)
ddf11$厚木<-cumsum(ddf10$厚木)
ddf11$小田原<-cumsum(ddf10$小田原)
ddf11$川崎<-cumsum(ddf10$川崎)
ddf11$相模原<-cumsum(ddf10$相模原)
ddf11$藤沢<-cumsum(ddf10$藤沢)
ddf11$平塚<-cumsum(ddf10$平塚)
ddf12<- ddf11
ddf12$横須賀<-ddf11[,2]/ddf2$人口[ddf2$居住地=="横須賀"]/10
ddf12$横浜<-ddf11[,3]/ddf2$人口[ddf2$居住地=="横浜"]/10
ddf12$鎌倉<-ddf11[,4]/ddf2$人口[ddf2$居住地=="鎌倉"]/10
ddf12$茅ヶ崎<-ddf11[,5]/ddf2$人口[ddf2$居住地=="茅ヶ崎"]/10
ddf12$厚木<-ddf11[,6]/ddf2$人口[ddf2$居住地=="厚木"]/10
ddf12$小田原<-ddf11[,7]/ddf2$人口[ddf2$居住地=="小田原"]/10
ddf12$川崎<-ddf11[,8]/ddf2$人口[ddf2$居住地=="川崎"]/10
ddf12$相模原<-ddf11[,9]/ddf2$人口[ddf2$居住地=="相模原"]/10
ddf12$藤沢<-ddf11[,10]/ddf2$人口[ddf2$居住地=="藤沢"]/10
ddf12$平塚<-ddf11[,11]/ddf2$人口[ddf2$居住地=="平塚"]/10
##tail(ddf12,n=1)
p <- ddf12 %>% gather(ftype, val, -発表日) %>% ggplot(aes(x = 発表日, y = val))
p<-p + geom_line(aes(color = ftype)) + labs(color = "居住地",size=2)+theme_grey(base_family = "HiraKakuProN-W3") +scale_x_datetime(date_labels = "%m/%d")+ylab("PCR+ per  1/10M")
p<-p+annotate(geom="text",x=tail(ddf12$発表日,n=1),y=tail(ddf12$鎌倉,n=1)+0.4,label="kamakura",size=3,hjust=0)
p<-p+annotate(geom="text",x=tail(ddf12$発表日,n=1),y=tail(ddf12$川崎,n=1)+0.4,label="kawasaki",size=3,hjust=0)
p<-p+annotate(geom="text",x=tail(ddf12$発表日,n=1),y=tail(ddf12$小田原,n=1)+0.4,label="odawara",size=3,hjust=0)
p

## add 2020/05/14
##非常事態宣言解除の判断目安
#「直近１週間の新たな感染者数が10万人あたり、0.5人程度以下」になること
##
##
##直近一週間の新たな感染者数の評価
rm(ddf13,ddf14)

##直近一週間の新規感染者数の評価
ddf13<-as.data.frame(ddf11[8:nrow(ddf11),1])
ddf13<-cbind(ddf13,diff(ddf11$横須賀,lag=7))
ddf13<-cbind(ddf13,diff(ddf11$横浜,lag=7))
ddf13<-cbind(ddf13,diff(ddf11$鎌倉,lag=7))
ddf13<-cbind(ddf13,diff(ddf11$茅ヶ崎,lag=7))
ddf13<-cbind(ddf13,diff(ddf11$厚木,lag=7))
ddf13<-cbind(ddf13,diff(ddf11$小田原,lag=7))
ddf13<-cbind(ddf13,diff(ddf11$川崎,lag=7))
ddf13<-cbind(ddf13,diff(ddf11$相模原,lag=7))
ddf13<-cbind(ddf13,diff(ddf11$藤沢,lag=7))
ddf13<-cbind(ddf13,diff(ddf11$平塚,lag=7))
names(ddf13)<-c("発表日","横須賀","横浜","鎌倉","茅ヶ崎","厚木","小田原","川崎","相模原","藤沢","平塚")

##可視化
rm(p2)
p2 <- ddf13 %>% gather(ftype2,val,-発表日)　%>% ggplot(aes(x=発表日, y=val))
p2<-p2 + geom_line(aes(color = ftype2)) + labs(color = "居住地",size=2)+theme_grey(base_family = "HiraKakuProN-W3") +ylab("#new infections in the last week")+scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 month")
p2


##直近一週間の新らたな感染者数の人口１０万人当たり評価
ddf14<- ddf13
ddf14$横須賀<-ddf13[,2]/ddf2$人口[ddf2$居住地=="横須賀"]/10
ddf14$横浜<-ddf13[,3]/ddf2$人口[ddf2$居住地=="横浜"]/10
ddf14$鎌倉<-ddf13[,4]/ddf2$人口[ddf2$居住地=="鎌倉"]/10
ddf14$茅ヶ崎<-ddf13[,5]/ddf2$人口[ddf2$居住地=="茅ヶ崎"]/10
ddf14$厚木<-ddf13[,6]/ddf2$人口[ddf2$居住地=="厚木"]/10
ddf14$小田原<-ddf13[,7]/ddf2$人口[ddf2$居住地=="小田原"]/10
ddf14$川崎<-ddf13[,8]/ddf2$人口[ddf2$居住地=="川崎"]/10
ddf14$相模原<-ddf13[,9]/ddf2$人口[ddf2$居住地=="相模原"]/10
ddf14$藤沢<-ddf13[,10]/ddf2$人口[ddf2$居住地=="藤沢"]/10
ddf14$平塚<-ddf13[,11]/ddf2$人口[ddf2$居住地=="平塚"]/10
##感染者数でソート
aa <- tail(ddf14[,2:11],1) %>% sort( decreasing = TRUE)
t(aa)
##可視化()新型コロナ感染状況評価指標/人口10万人あたりの一週間の新規感染者数(
#https://www.asahi.com/articles/ASN876R5PN87UTFK012.html
rm(p3)
p3 <- ddf14 %>% gather(ftype3,val,-発表日)　%>% ggplot(aes(x=発表日, y=val))
p3<-p3 + geom_line(aes(color = ftype3)) + labs(color = "居住地",size=2, title ="新型コロナ感染状況評価指標",subtitle="~人口10万人あたりの一週間の新規感染者数(人)~", x="発生日",y="感染者数(人)")+theme_grey(base_family = "HiraKakuProN-W3") +scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 month")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$相模原,n=1),label="相模原",size=3,hjust=0,family="HiraKakuProN-W3")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$厚木,n=1),label="厚木",size=3,hjust=0,family="HiraKakuProN-W3")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$鎌倉,n=1),label="鎌倉",size=3,hjust=0,family="HiraKakuProN-W3")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$川崎,n=1),label="川崎",size=3,hjust=0,family="HiraKakuProN-W3")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$横浜,n=1),label="横浜",size=3,hjust=0,family="HiraKakuProN-W3")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$藤沢,n=1),label="藤沢",size=3,hjust=0,family="HiraKakuProN-W3")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$茅ヶ崎,n=1),label="茅ヶ崎",size=3,hjust=0,family="HiraKakuProN-W3")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$横須賀,n=1),label="横須賀",size=3,hjust=0,family="HiraKakuProN-W3")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$小田原,n=1),label="小田原",size=3,hjust=0,family="HiraKakuProN-W3")
p3<-p3+annotate(geom="text",x=tail(ddf14$発表日,n=1),y=tail(ddf14$平塚,n=1),label="平塚",size=3,hjust=0,family="HiraKakuProN-W3")

##感染者数の人口10万人当たり累積数の対数値を評価
ddf41<- ddf11
ddf41$横須賀<-ddf41[,2]/ddf2$人口[ddf2$居住地=="横須賀"]/10
ddf41$横浜<-ddf41[,3]/ddf2$人口[ddf2$居住地=="横浜"]/10
ddf41$鎌倉<-ddf41[,4]/ddf2$人口[ddf2$居住地=="鎌倉"]/10
ddf41$茅ヶ崎<-ddf41[,5]/ddf2$人口[ddf2$居住地=="茅ヶ崎"]/10
ddf41$厚木<-ddf41[,6]/ddf2$人口[ddf2$居住地=="厚木"]/10
ddf41$小田原<-ddf41[,7]/ddf2$人口[ddf2$居住地=="小田原"]/10
ddf41$川崎<-ddf41[,8]/ddf2$人口[ddf2$居住地=="川崎"]/10
ddf41$相模原<-ddf41[,9]/ddf2$人口[ddf2$居住地=="相模原"]/10
ddf41$藤沢<-ddf41[,10]/ddf2$人口[ddf2$居住地=="藤沢"]/10
ddf41$平塚<-ddf41[,11]/ddf2$人口[ddf2$居住地=="平塚"]/10
##可視化
##新型コロナ感染状況評価指標/人口10万人あたり新規感染者数累積(人)
#https://www.asahi.com/articles/ASN876R5PN87UTFK012.html
rm(p4)
p4 <- ddf41 %>% gather(ftype4,val4,-発表日)　%>% ggplot(aes(x=発表日, y=val4))
p4<-p4 + geom_line(aes(color = ftype4)) + labs(color = "居住地",size=2, title ="新型コロナ感染状況評価指標",subtitle="~人口10万人あたり新規感染者数累積(人)~", x="発生日",y="感染者数累積(人)")+theme_grey(base_family = "HiraKakuProN-W3") +scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 month")+scale_y_continuous(trans = "log1p")
##
p4<-p4+annotate(geom="text",x=tail(ddf41$発表日,n=1),y=tail(ddf41$相模原,n=1),label="相模原",size=3,hjust=0,family="HiraKakuProN-W3")
p4<-p4+annotate(geom="text",x=tail(ddf41$発表日,n=1),y=tail(ddf41$鎌倉,n=1),label="鎌倉",size=3,hjust=0,family="HiraKakuProN-W3")
p4<-p4+annotate(geom="text",x=tail(ddf41$発表日,n=1),y=tail(ddf41$小田原,n=1),label="小田原",size=3,hjust=0,family="HiraKakuProN-W3")
p4<-p4+annotate(geom="text",x=tail(ddf41$発表日,n=1),y=tail(ddf41$厚木,n=1),label="厚木",size=3,hjust=0,family="HiraKakuProN-W3")
p4<-p4+annotate(geom="text",x=tail(ddf41$発表日,n=1),y=tail(ddf41$平塚,n=1),label="平塚",size=3,hjust=0,family="HiraKakuProN-W3")
p4<-p4+annotate(geom="text",x=tail(ddf41$発表日,n=1),y=tail(ddf41$横須賀,n=1),label="横須賀",size=3,hjust=0,family="HiraKakuProN-W3")
p4<-p4+annotate(geom="text",x=tail(ddf41$発表日,n=1),y=tail(ddf41$川崎,n=1),label="川崎",size=3,hjust=0,family="HiraKakuProN-W3")
p4<-p4+annotate(geom="text",x=tail(ddf41$発表日,n=1),y=tail(ddf41$横浜,n=1),label="横浜",size=3,hjust=0,family="HiraKakuProN-W3")
p4<-p4+annotate(geom="text",x=tail(ddf41$発表日,n=1),y=tail(ddf41$茅ヶ崎,n=1),label="茅ヶ崎",size=3,hjust=0,family="HiraKakuProN-W3")
p4<-p4+annotate(geom="text",x=tail(ddf41$発表日,n=1),y=tail(ddf41$藤沢,n=1),label="藤沢",size=3,hjust=0,family="HiraKakuProN-W3")
##P5##
ddf51<-ddf14
ddf51<-ddf51[,-2:-7]
ddf51<-ddf51[,-3:-5]
ddf51[which.max(ddf51[,2]),]
tail(ddf51,n=1)
##
p5<-ddf51 %>% gather(ftype4,val4,-発表日)　%>% ggplot(aes(x=発表日, y=val4))
p5<-p5+geom_line(aes(color = ftype4)) + labs(color = "居住地",size=2)+theme_grey(base_family = "HiraKakuProN-W3") +scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 month")+scale_y_continuous(trans = "log1p")+ylab("log10(#infections per 1M)")




ddf15<-ddf10 %>% gather(area,n,-発表日) %>% group_by(発表日) %>% summarise(d.total = sum(n))
##可視化
##神奈川県
##新型コロナ感染状況評価指標/新規感染者数累積(人)
#p5 <- ddf15 %>% gather(ftype4,val4,-発表日)　%>% ggplot(aes(x=発表日, y=val4))
#p4<-p4 + geom_line(aes(color = ftype4)) + labs(color = "居住地",size=2, title ="新型コロナ感染状況評価指標",subtitle="新規感染者数累積(人)~", x="発生日",y="感染者数(人)")+theme_grey(base_family = "HiraKakuProN-W3") +scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 month")
