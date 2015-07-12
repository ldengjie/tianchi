library("forecast")
library("TSA")
library("xts")
library("wmtsa")
d<-read.csv("redeem_fs_49.csv")
fitend<-153;
preend<-183;
prebeg<-fitend+1;
data<-d[1:preend,]


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

zfb1<-log(exp(tftobal_amt)+exp(consume))
card1<-log(exp(card1))
card2<-log(exp(card2)+exp(card3))

#total_redeem_amt<-wavShrink(total_redeem_amt[1:fitend],n.level=1)
#zfb1<-wavShrink(zfb1[1:fitend],n.level=1)
#card1      <-wavShrink(card1[1:fitend]      ,n.level=1)
#card2      <-wavShrink(card2[1:fitend]      ,n.level=1)
#for(i in 1:fitend)
#{
#total_redeem_amt[i]<-itotal_redeem_amt[i]
#zfb1[i]<-izfb1[i]
#card1[i]<-icard1[i]
#card2[i]<-icard2[i]
#}

#par(mfcol=c(5,5))
#plot(ts(total_redeem_amt))
#acf(total_redeem_amt,lag=60)
#pacf(total_redeem_amt,lag=60)
#acf(diff(total_redeem_amt),lag=60) 
#pacf(diff(total_redeem_amt),lag=60)
#acf(diff(total_redeem_amt,lag=7),lag=60)
#pacf(diff(total_redeem_amt,lag=7),lag=60)

#plot(ts(zfb1))
#acf(zfb1,lag=60)
#pacf(zfb1,lag=60)
#acf(diff(zfb1),lag=60) 
#pacf(diff(zfb1),lag=60)
#acf(diff(zfb1,lag=7),lag=60)
#pacf(diff(zfb1,lag=7),lag=60)
#
#plot(ts(card1))
#acf(card1,lag=60)
#pacf(card1,lag=60)
#acf(diff(card1),lag=60) 
#pacf(diff(card1),lag=60)
#acf(diff(card1,lag=7),lag=60)
#pacf(diff(card1,lag=7),lag=60)
#
#plot(ts(card2))
#acf(card2,lag=60)
#pacf(card2,lag=60)
#acf(diff(card2),lag=60) 
#pacf(diff(card2),lag=60)
#acf(diff(card2,lag=7),lag=60)
#pacf(diff(card2,lag=7),lag=60)
cat("... begin to loop ...")

#xregpre<-rep(1,30);
total_redeem_amt <-ts(total_redeem_amt ,fre=7)
zfb1             <-ts(zfb1             ,fre=7)
card1            <-ts(card1            ,fre=7)
card2            <-ts(card2            ,fre=7)


tslist<-list(total_redeem_amt,zfb1,card1,card2)
#orderlist<-list(c(0,0,0),c(0,0,0),c(1,0,1))
#sorderlist<-list(c(6,1,1),c(1,1,1),c(1,1,1))
#orignal+features
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
#orderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
#orderlist<-list(c(0,0,0),c(1,0,1),c(2,0,5),c(1,0,5))
orderlist<-list(c(0,0,0),c(2,0,8),c(3,0,12),c(1,0,13))
#denoised
#orderlist<-list(c(4,0,1),c(4,0,2),c(5,0,1),c(5,0,1))
#sorderlist<-list(c(0,1,1),c(0,1,1),c(0,1,1),c(0,1,1))
#orignal
#orderlist<-list(c(0,0,0),c(2,0,4),c(5,0,1),c(5,0,2))
#sorderlist<-list(c(0,0,0),c(1,1,0),c(0,1,1),c(0,1,1))
xregfit<-data.frame(data[1:fitend,20:(NCOL(data)-1)]);
xregpre<-data.frame(d[prebeg:preend,20:(NCOL(d)-1)]);
#xregfit<-data.frame(data[,60]);
#xregpre<-data.frame(d[prebeg:preend,60]);
#xregfit<-data.frame(data[,27:NCOL(data)]);
#xregpre<-data.frame(d[prebeg:preend,27:NCOL(d)]);
#xregpre<-rep(1,30);
result<-rep(0,30)
fittedValue<-rep(0,fitend)
#par(mfcol=c(6,4))
#par(mfcol=c(2,2))
par(mfcol=c(4,3))
type<-list("total_redeem_amt","zfb1","card1","card2")

for(ti in 2:4)
{
    #st<-seq(0,0.15,0.05);
    st<-0.05
    ts<-tslist[[ti]][1:fitend]
    or<-orderlist[[ti]]
    sor<-sorderlist[[ti]]
    legend<-type[[ti]]
    aicb<-Inf;
    for(si in st)
    {
        if(si==0) next;
        #tsam<-auto.arima(ts,xreg=xregfit)
        tsam<-arima(ts,order=or,xreg=xregfit)
        #tsam<-arima(ts,order=or,seasonal=list(order=sor,period=7),xreg=xregfit)
        #tsam<-arima(ts,order=or,seasonal=list(order=sor,period=7))
        aico<-tsam$aic;
        fixedv<-rep(NA,length(tsam$coef));
        needFix<-TRUE;
        cat(si,"---------------------------->\n");
        #print(tsam)
        while(needFix)
        {
            needFix<-FALSE;
            hasNaN<-FALSE;
            print(sqrt(diag(tsam$var.coef)))
            for (ci in 1:length(tsam$coef))
            {
                #if(is.na(coeftest(tsam)[ci,4])) 
                if(is.na(sqrt(diag(tsam$var.coef))[ci])) 
                {
                #cat("fnidddddddddddddddddddddd\n")
                    hasNaN<-TRUE;
                    break; 
                }
            }
            #if(!hasNaN)
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
                #tsam<-arima(ts,order=or,seasonal=list(order=sor,period=7),xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
                tsam<-arima(ts,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
                #tsam<-arima(ts,order=or,seasonal=list(order=sor,period=7),fixed=fixedv,transform.pars = FALSE)
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
    #tsam.bestfit<-arima(ts,order=or,seasonal=list(order=sor,period=7),xreg=xregfit,fixed=fixb,transform.pars = FALSE);
    tsam.bestfit<-arima(ts,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE);
    #tsam.bestfit<-arima(ts,order=or,seasonal=list(order=sor,period=7),xreg=xregfit);
    #tsam.bestfit<-arima(ts,order=or,seasonal=list(order=sor,period=7),fixed=fixb,transform.pars = FALSE);
    #tsam.bestfit<-arima(ts,order=or,seasonal=list(order=sor,period=7));
    cat("========>",legend,"<======\n")
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
    print(tsam.bestfit);
    tsam.p<-predict(tsam.bestfit,newxreg =xregpre);
    #tsam.p<-predict(tsam.bestfit,n.ahead=30);
    tsamp<-tsam.p$pred;
    #result<-cbind(result,tsamp)
    #print(tsamp)
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
    #acf(abs(residuals(tsam.bestfit)))
    #pacf(abs(residuals(tsam.bestfit)))
    #acf(residuals(tsam.bestfit)^2)
    #pacf(residuals(tsam.bestfit)^2)
}
print(result)
total.data<-ts(c(exp(total_redeem_amt[1:fitend]),exp(total_redeem_amt[prebeg:preend])),fre=7)               
total.fore<-ts(c(fittedValue,result),fre=7)    
total.data.xts<-xts(total.data,seq(as.POSIXct("2014-04-01"),len=length(total.data),by='day'))
total.fore.xts<-xts(total.fore,seq(as.POSIXct("2014-04-01"),len=length(total.fore),by='day'))
plot(as.zoo(cbind(total.data.xts,total.fore.xts)),col=1:2,lty=1:2,screens=1,xlab="Time")
legend(x="topright",legend=c("Observed","total"),lty=1:2,col=1:2)
