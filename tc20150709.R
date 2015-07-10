#######################
157
decompose ts according to frequency for zfb and amount for bank(card)
#######################

library("forecast")
library("TSA")
library("xts")
library("wmtsa")
library("lmtest")
d<-read.csv("purchase_fs_49.csv")
#data<-read.csv("purchase_by_type.csv")
data<-d[1:153,]
#plot(as.zoo(cbind(data$total_purchase_amt,data$zfb1,data$zfb2,data$zfb3,data$bank1,data$bank2,data$bank3,data$bank4,data$bank5,data$bank6,data$bank7,data$bank8,data$bank9,data$bank10,data$bank11,data$share)),col=1:16)
#par(mfcol=c(4,4))
#plot(ts(data$total_purchase_amt))
#plot(ts(data$zfb1))
#plot(ts(data$zfb2))
#plot(ts(data$zfb3))
#plot(ts(data$bank1))
#plot(ts(data$bank2))
#plot(ts(data$bank3))
#plot(ts(data$bank4))
#plot(ts(data$bank5))
#plot(ts(data$bank6))
#plot(ts(data$bank7))
#plot(ts(data$bank8))
#plot(ts(data$bank9))
#plot(ts(data$bank10))
#plot(ts(data$bank11))
#plot(ts(data$share))

report_date         <-data$report_date
total_purchase_amt  <-log(data$total_purchase_amt )
zfb1                <-log(data$zfb1)
zfb2                <-log(data$zfb2)
zfb3                <-log(data$zfb3)
ibank1               <-log(data$bank1)
ibank2               <-log(data$bank2)
ibank3               <-log(data$bank3)
ibank4               <-log(data$bank4 )
ibank5               <-log(data$bank5 )
ibank6               <-log(data$bank6 )
ibank7               <-log(data$bank7 )
ibank8               <-log(data$bank8 )
ibank9               <-log(data$bank9 )
ibank10              <-log(data$bank10)
ibank11              <-log(data$bank11)
share               <-log(data$share)

bank1<-log(data$bank1+data$bank2+data$bank3+data$bank4)
bank2<-log(data$bank5+data$bank6)
bank3<-log(data$bank7+data$bank8+data$bank9+data$bank10+data$bank11)
xregpre<-data.frame(d[154:183,20:NCOL(d)]);
xregfit<-data.frame(data[,20:NCOL(data)]);
zfb1 <-wavShrink(zfb1 ,n.level=1)
zfb2 <-wavShrink(zfb2 ,n.level=1)
zfb3 <-wavShrink(zfb3 ,n.level=1)
bank1<-wavShrink(bank1,n.level=1)
bank2<-wavShrink(bank2,n.level=1)
bank3<-wavShrink(bank3,n.level=1)
share<-wavShrink(share,n.level=1)

tslist<-list(zfb1,zfb2,zfb3,bank1,bank2,bank3,share)
orderlist<-list(c(0,0,0),c(0,0,0),c(1,0,2),c(0,0,1),c(0,0,1),c(0,0,1),c(1,0,0))
result<-rep(0,30)
fittedValue<-rep(0,153)
par(mfcol=c(4,2))
type<-list("zfb1","zfb2","zfb3","bank1","bank2","bank3","share")
for(ti in 1:6)
{
    st<-seq(0,1,0.05);
    #st<-0.05
    ts<-tslist[[ti]]
    or<-orderlist[[ti]]
    legend<-type[[ti]]
    aicb<-Inf;
    for(si in st)
    {
        if(si==0) next;
        #tsam<-auto.arima(ts,xreg=xregfit)
        tsam<-arima(ts,order=or,xreg=xregfit)
        aico<-tsam$aic;
        fixedv<-rep(NA,length(tsam$coef));
        needFix<-TRUE;
        #cat(si,"---------------------------->\n");
        #print(tsam)
        while(needFix)
        {
            needFix<-FALSE;
            hasNaN<-FALSE;
        for (ci in 1:length(tsam$coef))
        {
            #if(is.na(coeftest(tsam)[ci,4])) 
            if(is.na(sqrt(diag(tsam$var.coef))[ci])) 
            {
                hasNaN<-TRUE;
                break; 
            }
        }
        if(!hasNaN)
        {
            vi<-1;
            for (ci in 1:length(tsam$coef))
            {
                if(tsam$coef[ci]==0)
                {
                    next
                }else
                {
                    if( (1-pnorm(abs(tsam$coef[ci])/sqrt(diag(tsam$var.coef))[vi]))*2 > si )
                    {
                        fixedv[ci]=0;
                        needFix<-TRUE;
                    }
                    vi<-vi+1
                }

            }

        }
            if(needFix)
            {
                #cat("Fixing...\n")
                tsam<-arima(ts,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
                #print(tsam)
            }
        }
        ##print(tsam)
        if(tsam$aic<aicb)
        {
            ki<-si;
            fixb<-fixedv;
            aicb<-tsam$aic;
        }
    }
    #tsam.bestfit<-tsam;
    tsam.bestfit<-arima(ts,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE);
    cat("========>",legend,"<======\n")
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
    print(tsam.bestfit);
    tsam.p<-predict(tsam.bestfit,newxreg =xregpre);
    tsamp<-tsam.p$pred;
    #result<-cbind(result,tsamp)
    #print(tsamp)
    result<-result+exp(tsamp)
    fittedValue<-fittedValue+exp(fitted(tsam.bestfit))
    tsam.data<-ts(c(exp(ts),rep(NA,30)),fre=7)               
    tsam.fore<-ts(exp(c(fitted(tsam.bestfit),tsamp)),fre=7)    
    tsam.data.xts<-xts(tsam.data,seq(as.POSIXct("2014-04-01"),len=length(tsam.data),by='day'))
    tsam.fore.xts<-xts(tsam.fore,seq(as.POSIXct("2014-04-01"),len=length(tsam.fore),by='day'))
    plot(as.zoo(cbind(tsam.data.xts,tsam.fore.xts)),col=1:2,lty=1:2,screens=1,xlab="Time")
    legend(x="topright",legend=c("Observed",legend),lty=1:2,col=1:2)
}
print(result)
    total.data<-ts(c(exp(total_purchase_amt),rep(NA,30)),fre=7)               
    total.fore<-ts(c(fittedValue,result),fre=7)    
    total.data.xts<-xts(total.data,seq(as.POSIXct("2014-04-01"),len=length(total.data),by='day'))
    total.fore.xts<-xts(total.fore,seq(as.POSIXct("2014-04-01"),len=length(total.fore),by='day'))
    plot(as.zoo(cbind(total.data.xts,total.fore.xts)),col=1:2,lty=1:2,screens=1,xlab="Time")
    legend(x="topright",legend=c("Observed","total"),lty=1:2,col=1:2)
    -----

library("forecast")
library("TSA")
library("xts")
library("wmtsa")
d<-read.csv("redeem_fs_49.csv")
data<-d[1:153,]
#par(mfcol=c(4,5))
#plot(ts(data$total_redeem_amt))
#plot(ts(data$tftobal_amt))
#plot(ts(data$category1))
#plot(ts(data$category2))
#plot(ts(data$category3))
#plot(ts(data$category4))
#plot(ts(data$card1))
#plot(ts(data$card2))
#plot(ts(data$card3))
#plot(ts(data$card4))
#plot(ts(data$card5))
#plot(ts(data$card6))
#plot(ts(data$card7))
#plot(ts(data$card8))
#plot(ts(data$card9))
#plot(ts(data$card10))
#plot(ts(data$card11))

report_date      <-data$report_date
total_redeem_amt <-log(data$total_redeem_amt )
tftobal_amt      <-log(data$tftobal_amt      )
category1        <-log(data$category1        )
category2        <-log(data$category2        )
category3        <-log(data$category3        )
category4        <-log(data$category4        )
icard0            <-log(data$card0            )
icard1            <-log(data$card1            )
icard2            <-log(data$card2            )
icard3             <-log(data$card3             )
icard4             <-log(data$card4             )
icard5             <-log(data$card5             )
icard6             <-log(data$card6             )
icard7             <-log(data$card7             )
icard8             <-log(data$card8             )
icard9             <-log(data$card9             )
icard10            <-log(data$card10            )
icard11            <-log(data$card11            )
consume<-log(data$category1+data$category2+data$category3+data$category4)
card1<-log(data$card1+data$card2+data$card3)
card2<-log(data$card4+data$card5+data$card6)
card3<-log(data$card7+data$card8+data$card9+data$card10+data$card11)

xregfit<-data.frame(data[,21:NCOL(data)]);
xregpre<-data.frame(d[154:183,21:NCOL(d)]);
tftobal_amt<-wavShrink(tftobal_amt,n.level=1)
consume    <-wavShrink(consume    ,n.level=1)
card1      <-wavShrink(card1      ,n.level=1)
card2      <-wavShrink(card2      ,n.level=1)
card3      <-wavShrink(card3      ,n.level=1)
tslist<-list(tftobal_amt,consume,card1,card2,card3)
orderlist<-list(c(2,0,0),c(0,0,2),c(1,0,2),c(0,0,0),c(1,0,0))
result<-rep(0,30)
fittedValue<-rep(0,153)
par(mfcol=c(3,2))
type<-list("tftobal_amt","consume","card1","card2","card3")
for(ti in 1:5)
{
    st<-seq(0,1,0.05);
    #st<-0.05
    ts<-tslist[[ti]]
    or<-orderlist[[ti]]
    legend<-type[[ti]]
    aicb<-Inf;
    for(si in st)
    {
        if(si==0) next;
        #tsam<-auto.arima(ts,xreg=xregfit)
        tsam<-arima(ts,order=or,xreg=xregfit)
        aico<-tsam$aic;
        fixedv<-rep(NA,length(tsam$coef));
        needFix<-TRUE;
        #cat(si,"---------------------------->\n");
        #print(tsam)
        while(needFix)
        {
            needFix<-FALSE;
            hasNaN<-FALSE;
        for (ci in 1:length(tsam$coef))
        {
            #if(is.na(coeftest(tsam)[ci,4])) 
            if(is.na(sqrt(diag(tsam$var.coef))[ci])) 
            {
                hasNaN<-TRUE;
                break; 
            }
        }
        if(!hasNaN)
        {
            vi<-1;
            for (ci in 1:length(tsam$coef))
            {
                if(tsam$coef[ci]==0)
                {
                    next
                }else
                {
                    if( (1-pnorm(abs(tsam$coef[ci])/sqrt(diag(tsam$var.coef))[vi]))*2 > si )
                    {
                        fixedv[ci]=0;
                        needFix<-TRUE;
                    }
                    vi<-vi+1
                }

            }
        }
            if(needFix)
            {
                #cat("Fixing...\n")
                tsam<-arima(ts,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
                #print(tsam)
            }
        }
        ##print(tsam)
        if(tsam$aic<aicb)
        {
            ki<-si;
            fixb<-fixedv;
            aicb<-tsam$aic;
        }
    }
    #tsam.bestfit<-tsam;
    tsam.bestfit<-arima(ts,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE);
    cat("========>",legend,"<======\n")
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
    print(tsam.bestfit);
    tsam.p<-predict(tsam.bestfit,newxreg =xregpre);
    tsamp<-tsam.p$pred;
    #result<-cbind(result,tsamp)
    #print(tsamp)
    result<-result+exp(tsamp)
    fittedValue<-fittedValue+exp(fitted(tsam.bestfit))
    tsam.data<-ts(c(exp(ts),rep(NA,30)),fre=7)               
    tsam.fore<-ts(exp(c(fitted(tsam.bestfit),tsamp)),fre=7)    
    tsam.data.xts<-xts(tsam.data,seq(as.POSIXct("2014-04-01"),len=length(tsam.data),by='day'))
    tsam.fore.xts<-xts(tsam.fore,seq(as.POSIXct("2014-04-01"),len=length(tsam.fore),by='day'))
    plot(as.zoo(cbind(tsam.data.xts,tsam.fore.xts)),col=1:2,lty=1:2,screens=1,xlab="Time")
    legend(x="topright",legend=c("Observed",legend),lty=1:2,col=1:2)
}
print(result)
    total.data<-ts(c(exp(total_redeem_amt),rep(NA,30)),fre=7)               
    total.fore<-ts(c(fittedValue,result),fre=7)    
    total.data.xts<-xts(total.data,seq(as.POSIXct("2014-04-01"),len=length(total.data),by='day'))
    total.fore.xts<-xts(total.fore,seq(as.POSIXct("2014-04-01"),len=length(total.fore),by='day'))
    plot(as.zoo(cbind(total.data.xts,total.fore.xts)),col=1:2,lty=1:2,screens=1,xlab="Time")
    legend(x="topright",legend=c("Observed","total"),lty=1:2,col=1:2)
#######################
report_date = c(  
  as.integer(20140901)
, as.integer(20140902)
, as.integer(20140903)
, as.integer(20140904)
, as.integer(20140905)
, as.integer(20140906)
, as.integer(20140907)
, as.integer(20140908)
, as.integer(20140909)
, as.integer(20140910)
, as.integer(20140911)
, as.integer(20140912)
, as.integer(20140913)
, as.integer(20140914)
, as.integer(20140915)
, as.integer(20140916)
, as.integer(20140917)
, as.integer(20140918)
, as.integer(20140919)
, as.integer(20140920)
, as.integer(20140921)
, as.integer(20140922)
, as.integer(20140923)
, as.integer(20140924)
, as.integer(20140925)
, as.integer(20140926)
, as.integer(20140927)
, as.integer(20140928)
, as.integer(20140929)
, as.integer(20140930)
)

purchase= c(  
  as.integer(1309556359)
, as.integer(902832004 )
, as.integer(975601643 )
, as.integer(938713588 )
, as.integer(746820260 )
, as.integer(386534688 )
, as.integer(424530085 )
, as.integer(752935648 )
, as.integer(992127670 )
, as.integer(892724529 )
, as.integer(937758698 )
, as.integer(809696126 )
, as.integer(537746936 )
, as.integer(561947071 )
, as.integer(877737341 )
, as.integer(907113046 )
, as.integer(886138076 )
, as.integer(815610979 )
, as.integer(717158431 )
, as.integer(511375768 )
, as.integer(557977827 )
, as.integer(871418697 )
, as.integer(900824104 )
, as.integer(879690131 )
, as.integer(809724254 )
, as.integer(705801498 )
, as.integer(487910742 )
, as.integer(554093103 )
, as.integer(903156388 )
, as.integer(897900063 )
)
redeem= c(  
  as.integer(1035938832)
, as.integer(998291585 )
, as.integer(1389800910)
, as.integer(995325395 )
, as.integer(960218908 )
, as.integer(469104415 )
, as.integer(545593503 )
, as.integer(882183829 )
, as.integer(1211474031)
, as.integer(1046139371)
, as.integer(1002208057)
, as.integer(964087576 )
, as.integer(628702334 )
, as.integer(953548728 )
, as.integer(954411356 )
, as.integer(1218977608)
, as.integer(1052160466)
, as.integer(1009232579)
, as.integer(995450121 )
, as.integer(651689562 )
, as.integer(959639753 )
, as.integer(960519994 )
, as.integer(1226636553)
, as.integer(1058304488)
, as.integer(1016401961)
, as.integer(1095837238)
, as.integer(659451128 )
, as.integer(1338154731)
, as.integer(966753399 )
, as.integer(1272166839)
)


1309556359
902832004
975601643
938713588
746820260
386534688
424530085
752935648
992127670
892724529
937758698
809696126
537746936
561947071
877737341
907113046
886138076
815610979
717158431
511375768
557977827
871418697
900824104
879690131
809724254
705801498
487910742
554093103
903156388
897900063

1035938832
998291585
1389800910
995325395
960218908
469104415
545593503
882183829
1211474031
1046139371
1002208057
964087576
628702334
953548728
954411356
1218977608
1052160466
1009232579
995450121
651689562
959639753
960519994
1226636553
1058304488
1016401961
1095837238
659451128
1338154731
966753399
1272166839
