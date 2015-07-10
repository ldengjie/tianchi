library("forecast")
library("TSA")
library("xts")
data<-read.csv("456789fs.csv")
startdate<-'2014-04-01';
#data<-read.csv("totalfs.csv")
#startdate<-'2013-07-01';
fs<-data.frame(data[,4:NCOL(data)]);
dstr<-c("Puchase /Day","Redeem /Day");
iaorder<-cbind(c(4,0,5),c(4,0,9));
di<-2;
d17<-ts(data[1:(NROW(data)-30),(di+1)],fre=7)
d17l<-log(d17);
#lm2<-lm(log(total_purchase_amt)~ w1+w2+w3+w4+w5+w6+mb1+mb2+mb3+mb4+mb5+ma1+ma2+ma3+ma4+ma5+fb1+fb2+fb3+fb4+fb5+fi1+fi2+fi3+fa1+fa2+fa3+fa4+fa5+sb1+sb2+sb3+sb4+sb5+si1+sa1+sa2+sa3+sa4+sa5 ,data=data)
#ts.plot(d17l,fitted(lm2));
#r<-d17l-fitted(lm2);
le<-NROW(d17l);
cat("################## seasonal arima \n")
aicb<-Inf;
xregfit<-data.frame(fs[1:le,]);
st<-seq(0,1,0.05);
#st<-0.45;
for(si in st)
{
    if(si==0) next;
    d1l.f7t<-stats::arima(d17l,order=iaorder[,di],xreg=xregfit)
    fixedv<-rep(NA,length(d1l.f7t$coef));
    needFix<-TRUE;
    cat(si,"---------------------------->\n");
    print(d1l.f7t)
    while(needFix)
    {
    #cat(1,"\n");
        #print(d1l.f7t);
        needFix<-FALSE;
        hasNaN<-FALSE;
        for (ci in 1:length(d1l.f7t$coef))
        {
            if(is.na(coeftest(d1l.f7t)[ci,4])) 
            {
                hasNaN<-TRUE;
                break; 
            }
        }
        if(!hasNaN)
        {
    #cat(3,"\n");
            vi<-1;
            for (ci in 1:length(d1l.f7t$coef))
            {
    #cat(4,"\n");
                #cat(d1l.f7t$coef[ci],"\n")
                #cat(coeftest(d1l.f7t)[ci,4],"\n")
                #cat("\n")
                #if((d1l.f7t$coef[ci]!=0)&&(coeftest(d1l.f7t)[ci,4]>si))
                #(1-pnorm(abs(d1l.f7t$coef)/sqrt(diag(d1l.f7t$var.coef))))*2
                if(d1l.f7t$coef[ci]==0)
                {
                    next
                }else
                {
                    if( (1-pnorm(abs(d1l.f7t$coef[ci])/sqrt(diag(d1l.f7t$var.coef))[vi]))*2 > si )
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
    #cat(5,"\n");
            cat("Fixing...\n")
            d1l.f7t<-stats::arima(d17l,order=iaorder[,di],xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
            print(d1l.f7t)
        }
    }
    #print(d1l.f7t)
    if(d1l.f7t$aic<aicb)
    {
        ki<-si;
        fixb<-fixedv;
        aicb<-d1l.f7t$aic;
    }
}
cat("si=",ki,"aicb=",aicb)
#d1l.fm.bestfit<-d1l.f7t;
d1l.fm.bestfit<-arima(d17l,order=iaorder[,di],xreg=xregfit,fixed=fixb,transform.pars = FALSE);
print(d1l.fm.bestfit);
xregpre<-data.frame(fs[(le+1):(le+30),]);
d1l.fm.p<-predict(d1l.fm.bestfit,newxreg =xregpre);
d1l.fmp<-d1l.fm.p$pred;

cat("################## arima \n")
aorder<-cbind(c(4,0,1),c(0,0,6));
sorder<-cbind(c(3,1,1),c(0,1,2));
d1l.m1<-arimax(d17l,order=aorder[,di],seasonal=list(order=sorder[,di],period=7))
d1l.m1.d<-predict(d1l.m1,n.ahead=30)
d1l.m1p<-d1l.m1.d$pred
d1l.m1p
cat("################## lm \n")
dataw<-read.csv("48w.csv")
dataw9<-read.csv("9w.csv")
d1.lm1 = lm(total_purchase_amt ~ mon + tue + wed + thu + fri + sat + sun,dataw)
d1.lm1p <-predict(d1.lm1,dataw9,interval ="prediction",level=0.95,se.fit=FALSE)
d1.lm1p
cat("################## ets \n")
d17ets<-ets(d17)
d17etsp<-forecast(d17ets,30)                                          
d17etsp
cat("################## tbats 7+30 \n")
d17m<-msts(d17,seasonal.periods=c(7,30))
d17tbats<-tbats(d17m)
d17tbatsp<-forecast(d17tbats,30)
d17tbatsp
cat("################## tbats 7+30.44 \n")
d17m1<-msts(d17,seasonal.periods=c(7,30.44))
d17tbats1<-tbats(d17m1)
d17tbatsp1<-forecast(d17tbats1,30)
d17tbatsp1
cat("################## hw \n")
d17hw<-hw(d17,h=30,seasonal=("multiplicative"))
d17hw
d1l.m1.data<-ts(c(d17,rep(NA,30)),fre=7)               
d1l.m1.fore<-ts(exp(c(fitted(d1l.m1),d1l.m1p)),fre=7)
d1l.fm.fore<-ts(exp(c(fitted(d1l.fm.bestfit),d1l.fmp)),fre=7)    
d1.lm1.fore<-ts(c(fitted(d1.lm1),d1.lm1p[,1]),fre=7)    
d1.ets.fore<-ts(c(fitted(d17ets),d17etsp$mean),fre=7)    
d1.tba.fore<-ts(c(fitted(d17tbats),d17tbatsp$mean),fre=7)    
d1.tb1.fore<-ts(c(fitted(d17tbats1),d17tbatsp1$mean),fre=7)    
d1.hw1.fore<-ts(c(fitted(d17hw),d17hw$mean),fre=7)    
d1l.m1.data.xts<-xts(d1l.m1.data,seq(as.POSIXct(startdate),len=length(d1l.m1.data),by='day'))
d1l.m1.fore.xts<-xts(d1l.m1.fore,seq(as.POSIXct(startdate),len=length(d1l.m1.fore),by='day'))
d1l.fm.fore.xts<-xts(d1l.fm.fore,seq(as.POSIXct(startdate),len=length(d1l.fm.fore),by='day'))
d1.lm1.fore.xts<-xts(d1.lm1.fore,seq(as.POSIXct(startdate),len=length(d1.lm1.fore),by='day'))
d1.ets.fore.xts<-xts(d1.ets.fore,seq(as.POSIXct(startdate),len=length(d1.ets.fore),by='day'))
d1.tba.fore.xts<-xts(d1.tba.fore,seq(as.POSIXct(startdate),len=length(d1.tba.fore),by='day'))
d1.tb1.fore.xts<-xts(d1.tb1.fore,seq(as.POSIXct(startdate),len=length(d1.tb1.fore),by='day'))
d1.hw1.fore.xts<-xts(d1.hw1.fore,seq(as.POSIXct(startdate),len=length(d1.hw1.fore),by='day'))
#d1l.result<-(exp(d1l.fmp)+d1.lm1p[,1]+d17tbatsp1$mean+d17hw$mean)/4;
d1l.result<-exp(d1l.fmp);
d1l.result.xts<-xts(d1l.result,seq(as.POSIXct('2014-09-01'),len=length(d1l.fmp),by='day'))
plot(as.zoo(cbind(d1l.m1.data.xts,d1l.m1.fore.xts,d1l.fm.fore.xts,d1.lm1.fore.xts,d1.ets.fore.xts,d1.tba.fore.xts,d1.tb1.fore.xts,d1.hw1.fore.xts,d1l.result.xts)),col=1:9,lty=1:9,screens=1,ylab=dstr[di],xlab="Time")
legend(x="topright",legend=c("observed","seasonal arima","seasonal arima correction","lm","ets","tbats 7+30","tbats 7+30.44","hw","result"),lty=1:9,col=1:9)
cat(">>>> ",dstr[di]," <<<<\n")
print(d1l.result.xts);
cat("========\n")
print(exp(d1l.fmp));
