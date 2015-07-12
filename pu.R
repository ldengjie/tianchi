library("forecast")
library("TSA")
library("xts")
library("wmtsa")
library("lmtest")
od<-read.csv("purchase_by_type.csv")
of<-read.csv("totalfs.csv")
fitbeg<-275;#20140401
fitend<-427;#20140831
prebeg<-fitend+1;
preend<-457;#20140930
fd<-od[fitbeg:fitend,]


#plot(as.zoo(cbind(fd$total_purchase_amt,fd$zfb1,fd$zfb2,fd$zfb3,fd$bank1,fd$bank2,fd$bank3,fd$bank4,fd$bank5,fd$bank6,fd$bank7,fd$bank8,fd$bank9,fd$bank10,fd$bank11,fd$share)),col=1:16)
#par(mfcol=c(4,4))
#plot(ts(fd$total_purchase_amt))
#plot(ts(fd$zfb1))
#plot(ts(fd$zfb2))
#plot(ts(fd$zfb3))
#plot(ts(fd$bank1))
#plot(ts(fd$bank2))
#plot(ts(fd$bank3))
#plot(ts(fd$bank4))
#plot(ts(fd$bank5))
#plot(ts(fd$bank6))
#plot(ts(fd$bank7))
#plot(ts(fd$bank8))
#plot(ts(fd$bank9))
#plot(ts(fd$bank10))
#plot(ts(fd$bank11))
#plot(ts(fd$share))

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

#total_purchase_amt<-wavShrink(total_purchase_amt[fitbeg:fitend],n.level=1)
#zfb1 <-wavShrink(zfb1[fitbeg:fitend] ,n.level=1)
#zfb2 <-wavShrink(zfb2[fitbeg:fitend] ,n.level=1)
#bank1<-wavShrink(bank1[fitbeg:fitend],n.level=1)
#bank2<-wavShrink(bank2[fitbeg:fitend],n.level=1)

#par(mfcol=c(5,6))
#plot(ts(total_purchase_amt))
#acf(total_purchase_amt,lag=60)
#pacf(total_purchase_amt,lag=60)
#acf(diff(total_purchase_amt),lag=60) 
#pacf(diff(total_purchase_amt),lag=60)
#acf(diff(total_purchase_amt,lag=7),lag=60)
#pacf(diff(total_purchase_amt,lag=7),lag=60)

#plot(ts(zfb1))
#acf(zfb1,lag=60)
#pacf(zfb1,lag=60)
#acf(diff(zfb1),lag=60) 
#pacf(diff(zfb1),lag=60)
#acf(diff(zfb1,lag=7),lag=60)
#pacf(diff(zfb1,lag=7),lag=60)
#
#plot(ts(zfb2))
#acf(zfb2,lag=60)
#pacf(zfb2,lag=60)
#acf(diff(zfb2),lag=60) 
#pacf(diff(zfb2),lag=60)
#acf(diff(zfb2,lag=7),lag=60)
#pacf(diff(zfb2,lag=7),lag=60)
#
#plot(ts(bank1))
#acf(bank1,lag=60)
#pacf(bank1,lag=60)
#acf(diff(bank1),lag=60) 
#pacf(diff(bank1),lag=60)
#acf(diff(bank1,lag=7),lag=60)
#pacf(diff(bank1,lag=7),lag=60)
#
#plot(ts(bank2))
#acf(bank2,lag=60)
#pacf(bank2,lag=60)
#acf(diff(bank2),lag=60) 
#pacf(diff(bank2),lag=60)
#acf(diff(bank2,lag=7),lag=60)
#pacf(diff(bank2,lag=7),lag=60)

cat("... begin to loop ...")
#xregpre<-data.frame(od[prebeg:preend,26:NCOL(od)]);
#xregfit<-data.frame(fd[,26:NCOL(fd)]);
#zfb1<-exp(zfb1)
#zfb2<-exp(zfb2)
#bank1<-exp(bank1)
#bank1<-exp(bank1)
tslist<-list(total_purchase_amt,zfb1,zfb2,bank1,bank2)
#orignal+features
orderlist<-list(c(4,0,4),c(0,0,0),c(4,0,4),c(1,0,1),c(1,0,5))
#orderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
#denoised
#orderlist<-list(c(4,0,1),c(4,0,1),c(5,0,1),c(5,0,1),c(4,0,1))
#sorderlist<-list(c(0,1,2),c(0,1,1),c(0,1,1),c(0,1,1),c(0,1,1))
#orignal
#orderlist<-list(c(4,0,1),c(0,0,0),c(4,0,4),c(1,0,1),c(5,0,1))
#sorderlist<-list(c(0,1,2),c(2,1,1),c(2,1,1),c(3,1,2),c(1,1,2))
result<-rep(0,30)
fittedValue<-rep(0,fitend-fitbeg+1)
result.st<-rep(0,30)
fittedValue.st<-rep(0,fitend-fitbeg+1)
#par(mfcol=c(6,5))
#par(mfcol=c(3,2))
par(mfcol=c(5,3))
type<-list("total_purchase_amt","zfb1","zfb2","bank1","bank2")

xregfit<-of[fitbeg:fitend,2]
xregpre<-of[prebeg:preend,2]
for(xi in 3:NCOL(of))
{
    if(sum(of[fitbeg:fitend,xi])!=0)
    {
        xregfit<-cbind(xregfit,of[fitbeg:fitend,xi])
        xregpre<-cbind(xregpre,of[prebeg:preend,xi])
    }
}
for(ti in 2:5)
{
    #st<-seq(0,0.15,0.05);
    st<-0.05
    ts<-tslist[[ti]]
    or<-orderlist[[ti]]
    sor<-sorderlist[[ti]]
    legend<-type[[ti]]
    aicb<-Inf;
    #vm<-list(model="iGARCH",garchOrder=c(8,8))
    #mm<-list(armaOrder=c(1,1),include.mean=T,arfima=F,archpow = 1,external.regressors=xregfit[,60])
    #spec<-ugarchspec(variance.mode=vm,mean.mode=mm,distribution.mode="norm")
    #ts.igar=ugarchfit(fd=ts,spec=spec,out.sample=0,solver="solnp")
    for(si in st)
    {
        if(si==0) next;
        #tsam<-auto.arima(ts,xreg=xregfit)
        #tsam<-arima(ts,order=or,seasonal=list(order=sor,period=7),xreg=xregfit)
        #tsam<-arima(ts,order=or,seasonal=list(order=sor,period=7))
        tsam<-arima(ts,order=or,xreg=xregfit)
        aico<-tsam$aic;
        fixedv<-rep(NA,length(tsam$coef));
        needFix<-TRUE;
        cat(si,"---------------------------->\n");
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
                #if(0)
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
    tsam.data<-ts(c(exp(ts),od[prebeg:preend,2]),fre=7)               
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
    #######
    #tsst<-stlf(ts(ts[1:153],fre=7),h=30,s.window=7,method='arima',ic='bic',xreg=xregfit,newxreg=xregpre)
    #result.st<-result.st+exp(tsst$mean)
    #fittedValue.st<-fittedValue.st+exp(fitted(tsst))
    #plot(tsst)
    ########
    tsm1<-msts(ts,seasonal.periods=c(7,30.44))
    tstbats1<-tbats(tsm1)
    tstbats71<-forecast(tstbats1,30)
    #plot(tstbats1)
}
print(result)
total.data<-ts(c(od[fitbeg:preend,2]),fre=7)               
total.fore<-ts(c(fittedValue,result),fre=7)    
total.data.xts<-xts(total.data,seq(as.POSIXct("2014-04-01"),len=length(total.data),by='day'))
total.fore.xts<-xts(total.fore,seq(as.POSIXct("2014-04-01"),len=length(total.fore),by='day'))
plot(as.zoo(cbind(total.data.xts,total.fore.xts)),col=1:2,lty=1:2,screens=1,xlab="Time")
legend(x="topright",legend=c("Observed","total"),lty=1:2,col=1:2)

print(result.st)
total.data.st<-ts(c(exp(total_purchase_amt[fitbeg:fitend]),exp(total_purchase_amt[prebeg:preend])),fre=7)               
total.fore.st<-ts(c(fittedValue.st,result.st),fre=7)    
total.data.st.xts<-xts(total.data.st,seq(as.POSIXct("2014-04-01"),len=length(total.data.st),by='day'))
total.fore.st.xts<-xts(total.fore.st,seq(as.POSIXct("2014-04-01"),len=length(total.fore.st),by='day'))
plot(as.zoo(cbind(total.data.st.xts,total.fore.st.xts)),col=1:2,lty=1:2,screens=1,xlab="Time")
legend(x="topright",legend=c("Observed","total"),lty=1:2,col=1:2)
