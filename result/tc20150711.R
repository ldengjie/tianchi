#######################
193.445
合并分类后+features~（000）得到pq~arima
#######################
library("forecast")
library("TSA")
library("xts")
library("wmtsa")
library("lmtest")
d<-read.csv("purchase_fs_49.csv")
fitend<-153;
preend<-183;
prebeg<-fitend+1;
data<-d[1:preend,]

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

zfb1<-log(exp(zfb1))
zfb2<-log(exp(zfb2)+exp(zfb3))
bank1<-log(exp(bank1)+exp(bank2))
bank2<-log(exp(bank3)+exp(share))

cat("... begin to loop ...")
xregfit<-data.frame(data[1:fitend,20:(NCOL(data)-1)]);
xregpre<-data.frame(d[prebeg:preend,20:(NCOL(d)-1)]);
tslist<-list(total_purchase_amt,zfb1,zfb2,bank1,bank2)
orderlist<-list(c(4,0,4),c(0,0,0),c(4,0,4),c(1,0,1),c(1,0,5))
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
result<-rep(0,30)
fittedValue<-rep(0,fitend)
par(mfcol=c(5,3))
type<-list("total_purchase_amt","zfb1","zfb2","bank1","bank2")
for(ti in 2:5)
{
    st<-0.05
    ts<-tslist[[ti]][1:fitend]
    or<-orderlist[[ti]]
    sor<-sorderlist[[ti]]
    legend<-type[[ti]]
    aicb<-Inf;
    for(si in st)
    {
        if(si==0) next;
        tsam<-arima(ts,order=or,xreg=xregfit)
        aico<-tsam$aic;
        fixedv<-rep(NA,length(tsam$coef));
        needFix<-TRUE;
        cat(si,"---------------------------->\n");
        while(needFix)
        {
            needFix<-FALSE;
            hasNaN<-FALSE;
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
                        if(!is.na(sqrt(diag(tsam$var.coef))[vi]))
                        {
                            if( (1-pnorm(abs(tsam$coef[ci])/sqrt(diag(tsam$var.coef))[vi]))*2 > si )
                            {
                                fixedv[ci]=0;
                                needFix<-TRUE;
                            }
                        }
                        vi<-vi+1
                    }

                }
            }
            if(needFix)
            {
                cat("Fixing...\n")
                tsam<-arima(ts,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
            }
        }
        if(tsam$aic<aicb)
        {
            ki<-si;
            fixb<-fixedv;
            aicb<-tsam$aic;
        }
    }
    tsam.bestfit<-arima(ts,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE);
    cat("========>",legend,"<======\n")
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
    print(tsam.bestfit);
    tsam.p<-predict(tsam.bestfit,newxreg =xregpre);
    tsamp<-tsam.p$pred;
    result<-result+exp(tsamp)
    fittedValue<-fittedValue+exp(fitted(tsam.bestfit))
    tsam.data<-ts(c(exp(ts),exp(tslist[[ti]][prebeg:preend])),fre=7)               
    tsam.fore<-ts(exp(c(fitted(tsam.bestfit),tsamp)),fre=7)    
    tsam.data.xts<-xts(tsam.data,seq(as.POSIXct("2014-04-01"),len=length(tsam.data),by='day'))
    tsam.fore.xts<-xts(tsam.fore,seq(as.POSIXct("2014-04-01"),len=length(tsam.fore),by='day'))
    plot(as.zoo(cbind(tsam.data.xts,tsam.fore.xts)),col=1:2,lty=1:2,screens=1,xlab="Time")
    legend(x="topright",legend=c("Observed",legend),lty=1:2,col=1:2)
    acf(residuals(tsam.bestfit))
    pacf(residuals(tsam.bestfit))
    tsm1<-msts(ts,seasonal.periods=c(7,30.44))
    tstbats1<-tbats(tsm1)
    tstbats71<-forecast(tstbats1,30)
}
print(result)
total.data<-ts(c(exp(total_purchase_amt[1:fitend]),exp(total_purchase_amt[prebeg:preend])),fre=7)               
total.fore<-ts(c(fittedValue,result),fre=7)    
total.data.xts<-xts(total.data,seq(as.POSIXct("2014-04-01"),len=length(total.data),by='day'))
total.fore.xts<-xts(total.fore,seq(as.POSIXct("2014-04-01"),len=length(total.fore),by='day'))
plot(as.zoo(cbind(total.data.xts,total.fore.xts)),col=1:2,lty=1:2,screens=1,xlab="Time")
legend(x="topright",legend=c("Observed","total"),lty=1:2,col=1:2)
-------
library("forecast")
library("TSA")
library("xts")
library("wmtsa")
d<-read.csv("redeem_fs_49.csv")
fitend<-153;
preend<-183;
prebeg<-fitend+1;
data<-d[1:preend,]

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

#{
#}


cat("... begin to loop ...")

total_redeem_amt <-ts(total_redeem_amt ,fre=7)
zfb1             <-ts(zfb1             ,fre=7)
card1            <-ts(card1            ,fre=7)
card2            <-ts(card2            ,fre=7)


tslist<-list(total_redeem_amt,zfb1,card1,card2)
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
orderlist<-list(c(0,0,0),c(2,0,8),c(3,0,12),c(1,0,13))
xregfit<-data.frame(data[1:fitend,20:(NCOL(data)-1)]);
xregpre<-data.frame(d[prebeg:preend,20:(NCOL(d)-1)]);
result<-rep(0,30)
fittedValue<-rep(0,fitend)
par(mfcol=c(4,3))
type<-list("total_redeem_amt","zfb1","card1","card2")
for(ti in 2:4)
{
    st<-0.05
    ts<-tslist[[ti]][1:fitend]
    or<-orderlist[[ti]]
    sor<-sorderlist[[ti]]
    legend<-type[[ti]]
    aicb<-Inf;
    for(si in st)
    {
        if(si==0) next;
        tsam<-arima(ts,order=or,xreg=xregfit)
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
            if(1)
            {
                vi<-1;
                for (ci in 1:length(tsam$coef))
                {
                    if(tsam$coef[ci]==0)
                    {
                        next
                    }else
                    {
                        if(!is.na(sqrt(diag(tsam$var.coef))[vi]))
                        {
                            if( (1-pnorm(abs(tsam$coef[ci])/sqrt(diag(tsam$var.coef))[vi]))*2 > si )
                            {
                                fixedv[ci]=0;
                                needFix<-TRUE;
                            }
                        }
                        vi<-vi+1
                    }

                }
            }
            if(needFix)
            {
                cat("Fixing...\n")
                tsam<-arima(ts,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
            }
        }
        if(tsam$aic<aicb)
        {
            ki<-si;
            fixb<-fixedv;
            aicb<-tsam$aic;
        }
    }
    tsam.bestfit<-arima(ts,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE);
    cat("========>",legend,"<======\n")
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
    print(tsam.bestfit);
    tsam.p<-predict(tsam.bestfit,newxreg =xregpre);
    tsamp<-tsam.p$pred;
    result<-result+exp(tsamp)
    fittedValue<-fittedValue+exp(fitted(tsam.bestfit))
    tsam.data<-ts(c(exp(ts),exp(tslist[[ti]][prebeg:preend])),fre=7)               
    tsam.fore<-ts(exp(c(fitted(tsam.bestfit),tsamp)),fre=7)    
    tsam.data.xts<-xts(tsam.data,seq(as.POSIXct("2014-04-01"),len=length(tsam.data),by='day'))
    tsam.fore.xts<-xts(tsam.fore,seq(as.POSIXct("2014-04-01"),len=length(tsam.fore),by='day'))
    plot(as.zoo(cbind(tsam.data.xts,tsam.fore.xts)),col=1:2,lty=1:2,screens=1,xlab="Time")
    legend(x="topright",legend=c("Observed",legend),lty=1:2,col=1:2)
    acf(residuals(tsam.bestfit))
    pacf(residuals(tsam.bestfit))
}
print(result)
total.data<-ts(c(exp(total_redeem_amt[1:fitend]),exp(total_redeem_amt[prebeg:preend])),fre=7)               
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
  as.integer(1374144155)
, as.integer(978982648)
, as.integer(1030862311)
, as.integer(1004897304)
, as.integer(860051791)
, as.integer(465281288)
, as.integer(536595838)
, as.integer(946573101)
, as.integer(1247021241)
, as.integer(993638677)
, as.integer(1085839965)
, as.integer(936648660)
, as.integer(642208536)
, as.integer(620077189)
, as.integer(979017884)
, as.integer(1004401014)
, as.integer(993638677)
, as.integer(904310603)
, as.integer(785957939)
, as.integer(564110111)
, as.integer(620077189)
, as.integer(979017884)
, as.integer(1004401014)
, as.integer(993638677)
, as.integer(904310603)
, as.integer(718670211)
, as.integer(546962104)
, as.integer(641458297)
, as.integer(1029289216)
, as.integer(1026863480)
)
redeem= c(  
  as.integer(1136472347)
, as.integer(969257513)
, as.integer(1000975808)
, as.integer(954555932)
, as.integer(819316716)
, as.integer(466065576)
, as.integer(483676095)
, as.integer(962557751)
, as.integer(1075983329)
, as.integer(1000975808)
, as.integer(954555932)
, as.integer(856585167)
, as.integer(632111535)
, as.integer(727206014)
, as.integer(1136472347)
, as.integer(1075983329)
, as.integer(1000975808)
, as.integer(954555932)
, as.integer(856585167)
, as.integer(632111535)
, as.integer(727206014)
, as.integer(1136472347)
, as.integer(1075983329)
, as.integer(1000975808)
, as.integer(954555932)
, as.integer(856585167)
, as.integer(669189025)
, as.integer(758338908)
, as.integer(1136472347)
, as.integer(1091155151)
)
