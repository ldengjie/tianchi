#######################
150
#######################
library("forecast")
library("TSA")
library("xts")
library("wmtsa")
od<-read.csv("purchase_by_type.csv")
of<-read.csv("totalfs.csv")
prebeg<-fitend+1;
fd<-od[fitbeg:fitend,]


report_date         <-fd$report_date
total_purchase_amt  <-log(fd$total_purchase_amt )
zfb1                <-log(fd$zfb1)
zfb2                <-log(fd$zfb2)
zfb3                <-log(fd$zfb3)
ibank1               <-log(fd$bank1)
ibank2               <-log(fd$bank2)
ibank3               <-log(fd$bank3)
ibank4               <-log(fd$bank4 )
ibank5               <-log(fd$bank5 )
ibank6               <-log(fd$bank6 )
ibank7               <-log(fd$bank7 )
ibank8               <-log(fd$bank8 )
ibank9               <-log(fd$bank9 )
ibank10              <-log(fd$bank10)
ibank11              <-log(fd$bank11)
share               <-log(fd$share)

bank1<-log(fd$bank1+fd$bank2+fd$bank3+fd$bank4)
bank2<-log(fd$bank5+fd$bank6)
bank3<-log(fd$bank7+fd$bank8+fd$bank9+fd$bank10+fd$bank11)
zfb1<-log(exp(zfb1))
zfb2<-log(exp(zfb2)+exp(zfb3))
bank1<-log(exp(bank1)+exp(bank2))
bank2<-log(exp(bank3)+exp(share))



cat("... begin to loop ...\n")
par(mfcol=c(3,2))
tslist<-list(total_purchase_amt,zfb1,zfb2,bank1,bank2)
type<-list("total_purchase_amt","zfb1","zfb2","bank1","bank2")
orderlist<-list(c(4,0,4),c(0,0,0),c(4,0,4),c(1,0,1),c(1,0,5))
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))


result<-rep(0,30)
fittedValue<-rep(0,fitend-fitbeg+1)
result.stlf<-rep(0,30)
fittedValue.stlf<-rep(0,fitend-fitbeg+1)

xregfit<-data.frame(of[fitbeg:fitend,2:(NCOL(of)-21)]);
xregpre<-data.frame(of[prebeg:preend,2:(NCOL(of)-21)]);
for(ti in 2:5)
{
    st<-0.05
    ts<-tslist[[ti]]
    or<-orderlist[[ti]]
    sor<-sorderlist[[ti]]
    legend<-type[[ti]]
    aicb<-Inf;
    for(si in st)
    {
        if(si==0) next;
        if(ti==4)
        {
            tsam<-arima(ts,order=or,xreg=data.frame(xregfit),io=c(33))
        }else if(ti==5)
        {
            tsam<-arima(ts,order=or,xreg=xregfit,io=c(137))
        }else
        {
            tsam<-arima(ts,order=or,xreg=xregfit)
        }
        aico<-tsam$aic;
        fixedv<-rep(NA,length(tsam$coef));
        needFix<-TRUE;
        cat(si,"---------------------------->\n");
        while(needFix)
        {
            needFix<-FALSE;
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
            if(needFix)
            {
                cat("Fixing...\n")
                if(ti==4)
                {
                    tsam<-arima(ts,order=or,xreg=data.frame(xregfit),io=c(33),fixed=fixedv,transform.pars = FALSE)
                }else if(ti==5)
                {
                    tsam<-arima(ts,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE,io=c(137))
                }else
                {
                    tsam<-arima(ts,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
                }
            }
        }
        if(tsam$aic<aicb)
        {
            ki<-si;
            fixb<-fixedv;
            aicb<-tsam$aic;
        }
    }
    cat("========>",legend,"<======\n")
    if(ti==4)
    {
        tsam.bestfit<-arima(ts,order=or,xreg=data.frame(xregfit),io=c(33),fixed=fixb,transform.pars = FALSE);
        fittedValueTmp<-fitted(tsam.bestfit)
        print(tsam.bestfit);
        tsam.bestfit$coef<-tsam.bestfit$coef[1:(length(tsam.bestfit$coef)-1)]
    }else if(ti==5)
    {
        tsam.bestfit<-arima(ts,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE,io=c(137));
        fittedValueTmp<-fitted(tsam.bestfit)
        print(tsam.bestfit);
        tsam.bestfit$coef<-tsam.bestfit$coef[1:(length(tsam.bestfit$coef)-1)]
    }else
    {
        tsam.bestfit<-arima(ts,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE);
        fittedValueTmp<-fitted(tsam.bestfit)
        print(tsam.bestfit);
    }
    detectAO(tsam.bestfit);
    detectIO(tsam.bestfit);
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
        tsam.p<-predict(tsam.bestfit,newxreg =xregpre);
    tsamp<-tsam.p$pred;
    print(exp(tsamp))
    result<-result+exp(tsamp)
    fittedValue<-fittedValue+exp(fittedValueTmp)
    tsam.data<-ts(c(exp(ts),od[prebeg:preend,2]),fre=7)               
    tsam.fore<-ts(exp(c(fittedValueTmp,tsamp)),fre=7)    
    tsam.data.xts<-xts(tsam.data,seq(as.POSIXct("2014-04-01"),len=length(tsam.data),by='day'))
    tsam.fore.xts<-xts(tsam.fore,seq(as.POSIXct("2014-04-01"),len=length(tsam.fore),by='day'))
    plot(as.zoo(cbind(tsam.data.xts,tsam.fore.xts)),col=1:2,lty=1:2,screens=1,xlab="Time")
    legend(x="topright",legend=c("Observed",legend),lty=1:2,col=1:2)
}
print(result)
total.data<-ts(c(od[fitbeg:preend,2]),fre=7)               
total.fore<-ts(c(fittedValue,result),fre=7)    
total.data.xts<-xts(total.data,seq(as.POSIXct("2014-04-01"),len=length(total.data),by='day'))
total.fore.xts<-xts(total.fore,seq(as.POSIXct("2014-04-01"),len=length(total.fore),by='day'))
plot(as.zoo(cbind(total.data.xts,total.fore.xts)),col=1:2,lty=1:2,screens=1,xlab="Time")
legend(x="topright",legend=c("Observed","total"),lty=1:2,col=1:2)

------
library("forecast")
library("TSA")
library("xts")
library("wmtsa")
library("lmtest")
od<-read.csv("redeem_by_type.csv")
of<-read.csv("totalfs.csv")
prebeg<-fitend+1;
fd<-od[fitbeg:fitend,]

report_date      <-fd$report_date
total_redeem_amt <-log(fd$total_redeem_amt )
tftobal_amt      <-log(fd$tftobal_amt      )
category1        <-log(fd$category1        )
category2        <-log(fd$category2        )
category3        <-log(fd$category3        )
category4        <-log(fd$category4        )
icard0            <-log(fd$card0            )
icard1            <-log(fd$card1            )
icard2            <-log(fd$card2            )
icard3             <-log(fd$card3             )
icard4             <-log(fd$card4             )
icard5             <-log(fd$card5             )
icard6             <-log(fd$card6             )
icard7             <-log(fd$card7             )
icard8             <-log(fd$card8             )
icard9             <-log(fd$card9             )
icard10            <-log(fd$card10            )
icard11            <-log(fd$card11            )
consume<-log(fd$category1+fd$category2+fd$category3+fd$category4)
card1<-log(fd$card1+fd$card2+fd$card3)
card2<-log(fd$card4+fd$card5+fd$card6)
card3<-log(fd$card7+fd$card8+fd$card9+fd$card10+fd$card11)

zfb1<-log(exp(tftobal_amt)+exp(consume))
card1<-log(exp(card1))
card2<-log(exp(card2)+exp(card3))


cat("... begin to loop ...")

total_redeem_amt <-ts(total_redeem_amt ,fre=7)
zfb1             <-ts(zfb1             ,fre=7)
card1            <-ts(card1            ,fre=7)
card2            <-ts(card2            ,fre=7)


par(mfcol=c(3,2))
type<-list("total_redeem_amt","zfb1","card1","card2")
tslist<-list(total_redeem_amt,zfb1,card1,card2)
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
orderlist<-list(c(0,0,0),c(2,0,8),c(3,0,12),c(1,0,13))

result<-rep(0,30)
fittedValue<-rep(0,fitend-fitbeg+1)
result.st<-rep(0,30)
fittedValue.st<-rep(0,fitend-fitbeg+1)

xregfit<-data.frame(of[fitbeg:fitend,2:(NCOL(of)-21)]);
xregpre<-data.frame(of[prebeg:preend,2:(NCOL(of)-21)]);
for(ti in 2:4)
{
    st<-0.05
    ts<-tslist[[ti]]
    or<-orderlist[[ti]]
    sor<-sorderlist[[ti]]
    legend<-type[[ti]]
    aicb<-Inf;
    for(si in st)
    {
        if(si==0) next;
        if(ti==3)
        {
            tsam<-arima(ts,order=or,xreg=data.frame(xregfit,AO=1*(seq(ts)==150)))
        }else
        {
            tsam<-arima(ts,order=or,xreg=xregfit)
        }
        aico<-tsam$aic;
        fixedv<-rep(NA,length(tsam$coef));
        needFix<-TRUE;
        cat(si,"---------------------------->\n");
        while(needFix)
        {
            needFix<-FALSE;
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
            if(needFix)
            {
                cat("Fixing...\n")
                if(ti==3)
                {
                    tsam<-arima(ts,order=or,xreg=data.frame(xregfit,AO=1*(seq(ts)==150)),fixed=fixedv,transform.pars = FALSE)
                }else
                {
                    tsam<-arima(ts,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
                }
            }
        }
        if(tsam$aic<aicb)
        {
            ki<-si;
            fixb<-fixedv;
            aicb<-tsam$aic;
        }
    }
    cat("========>",legend,"<======\n")
    if(ti==3)
    {
        tsam.bestfit<-arima(ts,order=or,xreg=data.frame(xregfit,AO=1*(seq(ts)==150)),fixed=fixb,transform.pars = FALSE);
        fittedValueTmp<-fitted(tsam.bestfit)
        print(tsam.bestfit);
    }else
    {
        tsam.bestfit<-arima(ts,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE);
        fittedValueTmp<-fitted(tsam.bestfit)
        print(tsam.bestfit);
    }
    detectAO(tsam.bestfit);
    detectIO(tsam.bestfit);
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
    if(ti==3)
    {
        tsam.p<-predict(tsam.bestfit,newxreg =data.frame(xregpre,rep(0,30)));
    }else
    {
        tsam.p<-predict(tsam.bestfit,newxreg =xregpre);
    }
    tsamp<-tsam.p$pred;
    print(exp(tsamp))
    result<-result+exp(tsamp)
    fittedValue<-fittedValue+exp(fittedValueTmp)
    tsam.data<-ts(c(exp(ts),od[prebeg:preend,2]),fre=7)               
    tsam.fore<-ts(exp(c(fittedValueTmp,tsamp)),fre=7)    
    tsam.data.xts<-xts(tsam.data,seq(as.POSIXct("2014-04-01"),len=length(tsam.data),by='day'))
    tsam.fore.xts<-xts(tsam.fore,seq(as.POSIXct("2014-04-01"),len=length(tsam.fore),by='day'))
    plot(as.zoo(cbind(tsam.data.xts,tsam.fore.xts)),col=1:2,lty=1:2,screens=1,xlab="Time")
    legend(x="topright",legend=c("Observed",legend),lty=1:2,col=1:2)
}
print(result)
total.data<-ts(c(od[fitbeg:preend,2]),fre=7)               
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
  as.integer(1391315719)
, as.integer(1046376444)
, as.integer(1034379856)
, as.integer(994383862 )
, as.integer(905830314 )
, as.integer(488164895 )
, as.integer(552735654 )
, as.integer(812232423 )
, as.integer(1066173700)
, as.integer(1034379856)
, as.integer(1063130522)
, as.integer(824616585 )
, as.integer(614797406 )
, as.integer(631723723 )
, as.integer(1029148762)
, as.integer(1066173700)
, as.integer(1034379856)
, as.integer(949836354 )
, as.integer(824616585 )
, as.integer(576330706 )
, as.integer(631723723 )
, as.integer(1029148762)
, as.integer(1066173700)
, as.integer(1034379856)
, as.integer(949836354 )
, as.integer(791658829 )
, as.integer(563057638 )
, as.integer(823803799 )
, as.integer(1079480911)
, as.integer(1139698550)
)
redeem= c(  
  as.integer(978709815)
, as.integer(834522873)
, as.integer(975820645)
, as.integer(882514657)
, as.integer(768928248)
, as.integer(362478949)
, as.integer(412049181)
, as.integer(703644755)
, as.integer(928775842)
, as.integer(804566099)
, as.integer(820399780)
, as.integer(712955418)
, as.integer(564284718)
, as.integer(709163529)
, as.integer(978709815)
, as.integer(928775842)
, as.integer(868998595)
, as.integer(820399780)
, as.integer(764881671)
, as.integer(564284718)
, as.integer(588772576)
, as.integer(978709815)
, as.integer(928775842)
, as.integer(868998595)
, as.integer(820399780)
, as.integer(764881671)
, as.integer(591105161)
, as.integer(988093337)
, as.integer(848189227)
, as.integer(985727519)
)
