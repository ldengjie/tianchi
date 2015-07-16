#######################
140
小波减噪后，分类，然后arima，没有加任何特征.
#######################
library("forecast")
library("TSA")
library("xts")
library("wmtsa")
d<-read.csv("redeem_fs_49.csv")
data<-d[1:153,]

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

zfb1<-log(exp(tftobal_amt)+exp(consume))
card1<-log(exp(card1))
card2<-log(exp(card2)+exp(card3))

total_redeem_amt<-wavShrink(total_redeem_amt,n.level=1)
zfb1<-wavShrink(zfb1,n.level=1)
card1      <-wavShrink(card1      ,n.level=1)
card2      <-wavShrink(card2      ,n.level=1)


cat("... begin to loop ...")

xregfit<-data.frame(data[,27:NCOL(data)]);
xregpre<-data.frame(d[154:183,27:NCOL(d)]);
total_redeem_amt <-ts(total_redeem_amt ,fre=7)
zfb1             <-ts(zfb1             ,fre=7)
card1            <-ts(card1            ,fre=7)
card2            <-ts(card2            ,fre=7)


tslist<-list(total_redeem_amt,zfb1,card1,card2)
orderlist<-list(c(4,0,1),c(4,0,2),c(5,0,1),c(5,0,1))
sorderlist<-list(c(0,1,1),c(0,1,1),c(0,1,1),c(0,1,1))
result<-rep(0,30)
fittedValue<-rep(0,153)
par(mfcol=c(2,2))
type<-list("total_redeem_amt","zfb1","card1","card2")
for(ti in 2:4)
{
    st<-seq(0,0.15,0.05);
    ts<-tslist[[ti]]
    or<-orderlist[[ti]]
    sor<-sorderlist[[ti]]
    legend<-type[[ti]]
    aicb<-Inf;
    for(si in st)
    {
        if(si==0) next;
        tsam<-arima(ts,order=or,seasonal=list(order=sor,period=7))
        aico<-tsam$aic;
        fixedv<-rep(NA,length(tsam$coef));
        needFix<-TRUE;
        cat(si,"---------------------------->\n");
        while(needFix)
        {
            needFix<-FALSE;
            hasNaN<-FALSE;
            print(sqrt(diag(tsam$var.coef)))
            for (ci in 1:length(tsam$coef))
            {
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
                cat("Fixing...\n")
                tsam<-arima(ts,order=or,seasonal=list(order=sor,period=7),fixed=fixedv,transform.pars = FALSE)
            }
        }
        if(tsam$aic<aicb)
        {
            ki<-si;
            fixb<-fixedv;
            aicb<-tsam$aic;
        }
    }
    tsam.bestfit<-arima(ts,order=or,seasonal=list(order=sor,period=7),fixed=fixb,transform.pars = FALSE);
    cat("========>",legend,"<======\n")
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
    print(tsam.bestfit);
    tsam.p<-predict(tsam.bestfit,n.ahead=30);
    tsamp<-tsam.p$pred;
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
---
library("forecast")
library("TSA")
library("xts")
library("wmtsa")
d<-read.csv("redeem_fs_49.csv")
data<-d[1:153,]

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

zfb1<-log(exp(tftobal_amt)+exp(consume))
card1<-log(exp(card1))
card2<-log(exp(card2)+exp(card3))

total_redeem_amt<-wavShrink(total_redeem_amt,n.level=1)
zfb1<-wavShrink(zfb1,n.level=1)
card1      <-wavShrink(card1      ,n.level=1)
card2      <-wavShrink(card2      ,n.level=1)


cat("... begin to loop ...")

xregfit<-data.frame(data[,27:NCOL(data)]);
xregpre<-data.frame(d[154:183,27:NCOL(d)]);
total_redeem_amt <-ts(total_redeem_amt ,fre=7)
zfb1             <-ts(zfb1             ,fre=7)
card1            <-ts(card1            ,fre=7)
card2            <-ts(card2            ,fre=7)


tslist<-list(total_redeem_amt,zfb1,card1,card2)
orderlist<-list(c(4,0,1),c(4,0,2),c(5,0,1),c(5,0,1))
sorderlist<-list(c(0,1,1),c(0,1,1),c(0,1,1),c(0,1,1))
result<-rep(0,30)
fittedValue<-rep(0,153)
par(mfcol=c(2,2))
type<-list("total_redeem_amt","zfb1","card1","card2")
for(ti in 2:4)
{
    st<-seq(0,0.15,0.05);
    ts<-tslist[[ti]]
    or<-orderlist[[ti]]
    sor<-sorderlist[[ti]]
    legend<-type[[ti]]
    aicb<-Inf;
    for(si in st)
    {
        if(si==0) next;
        tsam<-arima(ts,order=or,seasonal=list(order=sor,period=7))
        aico<-tsam$aic;
        fixedv<-rep(NA,length(tsam$coef));
        needFix<-TRUE;
        cat(si,"---------------------------->\n");
        while(needFix)
        {
            needFix<-FALSE;
            hasNaN<-FALSE;
            print(sqrt(diag(tsam$var.coef)))
            for (ci in 1:length(tsam$coef))
            {
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
                cat("Fixing...\n")
                tsam<-arima(ts,order=or,seasonal=list(order=sor,period=7),fixed=fixedv,transform.pars = FALSE)
            }
        }
        if(tsam$aic<aicb)
        {
            ki<-si;
            fixb<-fixedv;
            aicb<-tsam$aic;
        }
    }
    tsam.bestfit<-arima(ts,order=or,seasonal=list(order=sor,period=7),fixed=fixb,transform.pars = FALSE);
    cat("========>",legend,"<======\n")
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
    print(tsam.bestfit);
    tsam.p<-predict(tsam.bestfit,n.ahead=30);
    tsamp<-tsam.p$pred;
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
  as.integer(672944720)
, as.integer(795027151)
, as.integer(891110880)
, as.integer(878416517)
, as.integer(713470676)
, as.integer(564390414)
, as.integer(593070724)
, as.integer(796539294)
, as.integer(962308962)
, as.integer(937309920)
, as.integer(840178704)
, as.integer(710294186)
, as.integer(597555997)
, as.integer(612192422)
, as.integer(769214546)
, as.integer(918053181)
, as.integer(936001223)
, as.integer(865774204)
, as.integer(719217687)
, as.integer(588993386)
, as.integer(607235335)
, as.integer(778976106)
, as.integer(930720231)
, as.integer(932864218)
, as.integer(855915744)
, as.integer(717133321)
, as.integer(592267700)
, as.integer(607611108)
, as.integer(774872935)
, as.integer(928829032)
)
redeem= c(  
  as.integer(657345289 )
, as.integer(811323533 )
, as.integer(1035121084)
, as.integer(1090949693)
, as.integer(826071359 )
, as.integer(611164648 )
, as.integer(620245972 )
, as.integer(787665532 )
, as.integer(932288985 )
, as.integer(999888190 )
, as.integer(1004627922)
, as.integer(830319713 )
, as.integer(644995374 )
, as.integer(642781976 )
, as.integer(809374252 )
, as.integer(974821629 )
, as.integer(1021904981)
, as.integer(968419153 )
, as.integer(793983891 )
, as.integer(643106691 )
, as.integer(664079258 )
, as.integer(835450208 )
, as.integer(987875802 )
, as.integer(1028222454)
, as.integer(968362840 )
, as.integer(786248122 )
, as.integer(638185306 )
, as.integer(669052460 )
, as.integer(853563554 )
, as.integer(1003525045)
)

672944720
795027151
891110880
878416517
713470676
564390414
593070724
796539294
962308962
937309920
840178704
710294186
597555997
612192422
769214546
918053181
936001223
865774204
719217687
588993386
607235335
778976106
930720231
932864218
855915744
717133321
592267700
607611108
774872935
928829032


657345289
811323533
1035121084
1090949693
826071359
611164648
620245972
787665532
932288985
999888190
1004627922
830319713
644995374
642781976
809374252
974821629
1021904981
968419153
793983891
643106691
664079258
835450208
987875802
1028222454
968362840
786248122
638185306
669052460
853563554
1003525045

