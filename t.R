library("forecast")
library("TSA")
library("xts")
require("lmtest")
data<-read.csv("48.csv");
fs<-read.csv("456789fs.csv")
startdate<-'2014-04-01';
#fs<-read.csv("totalfs.csv")
#data<-read.csv("total.csv");
#startdate<-'2013-07-01'
d<-data.frame(data$total_purchase_amt,data$total_redeem_amt);
dstr<-c("Puchase /Day","Redeem /Day");
aorder<-cbind(c(4,0,1),c(0,0,6));
sorder<-cbind(c(3,1,1),c(0,1,2));
#sorder<-cbind(c(3,0,2),c(0,0,2));
#ij<-cbind(c(3,15),c(3,15));
ij<-cbind(c(1,2),c(1,2));
#fixc<-list(c(NA,0,NA,NA,0,0,NA,NA,NA,0),c(NA,NA,0,0,NA,0,NA,0));
#fixc<-list(c(NA,0,NA,NA,0,0,0,NA,0,0,NA),c(NA,NA,0,0,NA,0,NA,0));
di<-2;
d17<-ts(d[di],fre=7)
d130<-ts(d[di],fre=30)
d1306<-ts(d[di],fre=30.44)
d131<-ts(d[di],fre=31)
d17l<-log(d17);
d17lo<-log(d17);
#d17l<-ts(c(rep(NA,7),diff(d17lo,lag=7)),fre=7);
#d17l<-ts(diff(d17lo,lag=7),fre=7);
#d17l<-ts(diff(diff(d17,lag=6),lag=7),fre=7);
#d17l<-ts(diff(log(d17),lag=7),fre=7);
le<-NROW(d17l);
#d17lf<-ts(c(log(d17),rep(NA,30)),fre=7)
d17lf<-ts(c(diff(log(d17),lag=7),rep(NA,30)),fre=7)
#d130lf<-ts(c(log(d130),rep(NA,30)),fre=30)
#d1306lf<-ts(c(log(d130),rep(NA,30)),fre=30.44)
d1306lf<-ts(c(diff(log(d1306),lag=7),rep(NA,30)),fre=30.44)
#d131lf<-ts(c(log(d131),rep(NA,30)),fre=31)
##arima
cat("################## seasonal arima \n")
#d1l.fm.bestfit<- list(aic=Inf);
aicb<-Inf;
#for(i in 1:1){#1:3
#for(i in ij[1,di]){
    #for(j in 1:1){#1:15
    #for(j in ij[2,di]){
        #cat("i:",i," j:",j,"\n")
        #xregfit<-data.frame(fourier(d17lf,K=i)[1:le,],fourier(d1306lf,K=j)[1:le,]);
        #xregfit<-data.frame(fourier(d17lf,K=i)[1:le,],fourier(d1306lf,K=j)[1:le,],fs[1:le+7,]);
        #xregfit<-data.frame(fourier(d17lf,K=i)[1:le,],fourier(d1306lf,K=j)[1:le,],fs[1:le,]);
        xregfit<-data.frame(fs[1:le,]);
        #xregfit<-data.frame(fourier(d17,K=3),fourier(d1306,K=15),fs[1:183,]);
        #xregfit<-data.frame(fourier(d17lf,K=i)[1:le,],fourier(d130lf,K=j)[1:le,],fourier(d131lf,K=j)[1:le,],fs[1:le,]);
        #d1l.f7t<-arimax(d17l,order=aorder[,di],xreg=xregfit,xtransf=data.frame(wy=1*(seq(d17l)==27)),transfer=list(c(1,0)));
        #d1l.f7t<-arimax(d17l,order=aorder[,di],xreg=xregfit);
        d1l.f7t<-arimax(d17l,order=aorder[,di],seasonal=list(order=sorder[,di],period=7),xreg=xregfit)
        fixedv<-rep(NA,length(d1l.f7t$coef));
        #needFix<-TRUE;
        #while(needFix )
        #while(needFix && !(di==1 && ((i==2 && j==10)||(i==3 && j==14))))
        #while(needFix && !(di==1 && ((i==2 && j==1)||(i==3 && j==1)||(i==3 && j==9)||(i==3 && j==10))))
        #while(0)
        #{
            needFix<-FALSE;
            cat(1);
            for (ci in 1:length(d1l.f7t$coef))
            {
                #if((d1l.f7t$coef[ci]!=0)&&(abs(confint(d1l.f7t)[ci,2]-confint(d1l.f7t)[ci,1])/4/0.978 - abs(d1l.f7t$coef[ci])) >= 0 )
                if(coeftest(d1l.f7t)[ci,4]>0.05)
                {
                    fixedv[ci]=0;
                    needFix<-TRUE;
                }
                #print(d1l.f7t)
            }
            #d1l.f7t<-arimax(d17l,order=aorder[,di],xreg=xregfit,xtransf=data.frame(wy=1*(seq(d17l)==27)),transfer=list(c(1,0)),fixed=fixedv,transform.pars = FALSE)                                          
            #d1l.f7t<-arimax(d17l,order=aorder[,di],xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
            if(needFix)
            {
                d1l.f7t<-arimax(d17l,order=aorder[,di],seasonal=list(order=sorder[,di],period=7),xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
            }
        #}
        #cat("i:",i," j:",j," aic:",d1l.f7t$aic,"\n")
        #if(d1l.f7t$aic<aicb)
        #{
            #ki<-i;
            #kj<-j;
            #fixb<-fixedv;
            #aicb<-d1l.f7t$aic;
            #xregb<-xregfit;
        #}
    #}
#}
#cat("ki:",ki," kj:",kj," aic:",aicb,"\n");
#d1l.fm.bestfit<-arimax(d17l,order=aorder[,di],xreg=xregb,xtransf=data.frame(wy=1*(seq(d17l)==27)),transfer=list(c(1,0)),fixed=fixb,transform.pars = FALSE)                                          
#d1l.fm.bestfit<-arimax(d17l,order=aorder[,di],seasonal=list(order=sorder[,di],period=7),xreg=xregb,fixed=fixb,transform.pars = FALSE)                                          
d1l.fm.bestfit<-d1l.f7t;
#d1l.fm.bestfit<-arimax(d17l,order=aorder[,di],xreg=xregb)                                          
print(d1l.fm.bestfit);
#xregpre<-data.frame(fourier(d17lf,K=ki)[(le+1):(le+30),],fourier(d1306lf,K=kj)[(le+1):(le+30),]);
#xregpre<-data.frame(fourier(d17lf,K=ki)[(le+1):(le+30),],fourier(d1306lf,K=kj)[(le+1):(le+30),],fs[(le+1):(le+30),]);
#xregpre<-data.frame(fs[(le+1):(le+30),]);
#xregpre<-data.frame(fourier(d17lf,K=ki)[(le+1):(le+30),],fourier(d130lf,K=kj)[(le+1):(le+30),],fourier(d131lf,K=kj)[(le+1):(le+30),],fs[(le+1):(le+30),]);
#d1l.fm.p<-predict(d1l.fm.bestfit,newxreg =xregpre,xtransf=data.frame(wy=1*(seq(d17l)==27)),transfer=list(c(1,0)));
#d1l.fm.p<-predict(d1l.fm.bestfit,newxreg =xregpre);
#d1l.fmp<-d1l.fm.p$pred;
#d1l.fmp<-exp(c(diffinv((d1l.fm.p$pred)[8:length(d1l.fm.p$pred)],lag=7,xi=d17lo[1:7])));
#d1l.fmp<-exp(c(diffinv(d1l.fm.p$pred,lag=7,xi=d17lo[1:7])));
##7.1 arima
cat("################## arima \n")
#d1l.m1<-arimax(d17l,order=aorder[,di],seasonal=list(order=sorder[,di],period=7),fixed=fixc[[di]],transform.pars = FALSE)
d1l.m1<-arimax(d17l,order=aorder[,di],seasonal=list(order=sorder[,di],period=7))
d1l.m1.d<-predict(d1l.m1,n.ahead=30)
d1l.m1p<-d1l.m1.d$pred
d1l.m1p
m1fitted<-exp(fitted(d1l.m1));
fb1=(                                     (exp(d1l.f7t$coef[22])-1)*m1fitted[26]+(exp(d1l.f7t$coef[35])-1)*m1fitted[56])/2;
fb2=((exp(d1l.f7t$coef[10])-1)*m1fitted[1]+(exp(d1l.f7t$coef[23])-1)*m1fitted[27]+(exp(d1l.f7t$coef[36])-1)*m1fitted[57])/3;
fb3=((exp(d1l.f7t$coef[11])-1)*m1fitted[2]+(exp(d1l.f7t$coef[24])-1)*m1fitted[28]+(exp(d1l.f7t$coef[37])-1)*m1fitted[58])/3;
fb4=((exp(d1l.f7t$coef[12])-1)*m1fitted[3]+(exp(d1l.f7t$coef[25])-1)*m1fitted[29]+(exp(d1l.f7t$coef[38])-1)*m1fitted[59])/3;
fb5=((exp(d1l.f7t$coef[13])-1)*m1fitted[4]+(exp(d1l.f7t$coef[26])-1)*m1fitted[30]+(exp(d1l.f7t$coef[39])-1)*m1fitted[60])/3;
fi1=((exp(d1l.f7t$coef[14])-1)*m1fitted[5]+(exp(d1l.f7t$coef[27])-1)*m1fitted[31]+(exp(d1l.f7t$coef[40])-1)*m1fitted[61])/3;
fi2=((exp(d1l.f7t$coef[15])-1)*m1fitted[6]+(exp(d1l.f7t$coef[28])-1)*m1fitted[32]+(exp(d1l.f7t$coef[41])-1)*m1fitted[62])/3;
fi3=((exp(d1l.f7t$coef[16])-1)*m1fitted[7]+(exp(d1l.f7t$coef[29])-1)*m1fitted[33]+(exp(d1l.f7t$coef[42])-1)*m1fitted[63])/3;
fa1=((exp(d1l.f7t$coef[17])-1)*m1fitted[8]+(exp(d1l.f7t$coef[30])-1)*m1fitted[34]+(exp(d1l.f7t$coef[43])-1)*m1fitted[64])/3;
fa2=((exp(d1l.f7t$coef[18])-1)*m1fitted[9]+(exp(d1l.f7t$coef[31])-1)*m1fitted[35]+(exp(d1l.f7t$coef[44])-1)*m1fitted[65])/3;
fa3=((exp(d1l.f7t$coef[19])-1)*m1fitted[10]+(exp(d1l.f7t$coef[32])-1)*m1fitted[36]+(exp(d1l.f7t$coef[45])-1)*m1fitted[66])/3;
fa4=((exp(d1l.f7t$coef[20])-1)*m1fitted[11]+(exp(d1l.f7t$coef[33])-1)*m1fitted[37]+(exp(d1l.f7t$coef[46])-1)*m1fitted[67])/3;
fa5=((exp(d1l.f7t$coef[21])-1)*m1fitted[12]+(exp(d1l.f7t$coef[34])-1)*m1fitted[38]+(exp(d1l.f7t$coef[47])-1)*m1fitted[68])/3;
cat("fb1",fb1,"\n");
cat("fb2",fb2,"\n");
cat("fb3",fb3,"\n");
cat("fb4",fb4,"\n");
cat("fb5",fb5,"\n");
cat("fi1",fi1,"\n");
cat("fi2",fi2,"\n");
cat("fi3",fi3,"\n");
cat("fa1",fa1,"\n");
cat("fa2",fa2,"\n");
cat("fa3",fa3,"\n");
cat("fa4",fa4,"\n");
cat("fa5",fa5,"\n");
d1l.m1pe<-exp(d1l.m1p)
d1l.m1pee<-d1l.m1pe
d1l.m1pee[1 ]<-d1l.m1pee[1 ]+fb1
d1l.m1pee[2 ]<-d1l.m1pee[2 ]+fb2
d1l.m1pee[3 ]<-d1l.m1pee[3 ]+fb3
d1l.m1pee[4 ]<-d1l.m1pee[4 ]+fb4
d1l.m1pee[5 ]<-d1l.m1pee[5 ]+fb5
d1l.m1pee[6 ]<-d1l.m1pee[6 ]+fi1
d1l.m1pee[7 ]<-d1l.m1pee[7 ]+fi2
d1l.m1pee[8 ]<-d1l.m1pee[8 ]+fi3
d1l.m1pee[9 ]<-d1l.m1pee[9 ]+fa1
d1l.m1pee[10]<-d1l.m1pee[10]+fa2
d1l.m1pee[11]<-d1l.m1pee[11]+fa3
d1l.m1pee[12]<-d1l.m1pee[12]+fa4
d1l.m1pee[13]<-d1l.m1pee[13]+fa5
d1l.m1pee[26]<-d1l.m1pee[26]+fb1
d1l.m1pee[27]<-d1l.m1pee[27]+fb2
d1l.m1pee[28]<-d1l.m1pee[28]+fb3
d1l.m1pee[29]<-d1l.m1pee[29]+fb4
d1l.m1pee[30]<-d1l.m1pee[30]+fb5
##lm
cat("################## lm \n")
dataw<-read.csv("48w.csv")
dataw9<-read.csv("9w.csv")
d1.lm1 = lm(total_purchase_amt ~ mon + tue + wed + thu + fri + sat + sun,dataw)
d1.lm1p <-predict(d1.lm1,dataw9,interval ="prediction",level=0.95,se.fit=FALSE)
d1.lm1p
##ets
cat("################## ets \n")
d17ets<-ets(d17)
d17etsp<-forecast(d17ets,30)                                          
d17etsp
##tbats30
cat("################## tbats 7+30 \n")
d17m<-msts(d17,seasonal.periods=c(7,30))
d17tbats<-tbats(d17m)
d17tbatsp<-forecast(d17tbats,30)
d17tbatsp
##tbats30.6
cat("################## tbats 7+30.44 \n")
d17m1<-msts(d17,seasonal.periods=c(7,30.44))
d17tbats1<-tbats(d17m1)
d17tbatsp1<-forecast(d17tbats1,30)
d17tbatsp1
##hw
cat("################## hw \n")
d17hw<-hw(d17,h=30,seasonal=("multiplicative"))
d17hw
##stlf
#cat("################## stlf \n")
#d17stlf<-stlf(d17,h=30)
#d17stlf
##plot
d1l.m1.data<-ts(c(d17,rep(NA,30)),fre=7)               
#d1l.m1.fore<-ts(exp(diffinv(c(fitted(d1l.m1),d1l.m1p),lag=7,xi=d17lo[1:7])),fre=7)
d1l.m1.fore<-ts(exp(c(fitted(d1l.m1),d1l.m1p)),fre=7)
d1l.m1ee.fore<-ts(c(exp(fitted(d1l.m1)),d1l.m1pee),fre=7)
#d1l.m1.fore<-ts(c(fitted(d1l.m1),d1l.m1p),fre=7)
#d1l.fm.fore<-ts(exp(c(fitted(d1l.fm.bestfit),d1l.fmp)),fre=7)    
#d1l.fm.fore<-ts(c(exp(c(diffinv(fitted(d1l.fm.bestfit)[8:length(fitted(d1l.fm.bestfit))],lag=7,xi=d17lo[1:7]))),d1l.fmp),fre=7)    
#d1l.fm.fore<-ts(exp(diffinv(c(fitted(d1l.fm.bestfit),d1l.fmp),lag=7,xi=d17lo[1:7])),fre=7)    
#d1l.fm.fore<-ts(c(diffinv(c(fitted(d1l.fm.bestfit),d1l.fmp),lag=7,xi=d17lo[1:7])),fre=7)    
d1.lm1.fore<-ts(c(fitted(d1.lm1),d1.lm1p[,1]),fre=7)    
d1.ets.fore<-ts(c(fitted(d17ets),d17etsp$mean),fre=7)    
d1.tba.fore<-ts(c(fitted(d17tbats),d17tbatsp$mean),fre=7)    
d1.tb1.fore<-ts(c(fitted(d17tbats1),d17tbatsp1$mean),fre=7)    
d1.hw1.fore<-ts(c(fitted(d17hw),d17hw$mean),fre=7)    
d1l.m1.data.xts<-xts(d1l.m1.data,seq(as.POSIXct(startdate),len=length(d1l.m1.data),by='day'))
d1l.m1.fore.xts<-xts(d1l.m1.fore,seq(as.POSIXct(startdate),len=length(d1l.m1.fore),by='day'))
d1l.m1ee.fore.xts<-xts(d1l.m1ee.fore,seq(as.POSIXct(startdate),len=length(d1l.m1ee.fore),by='day'))
#d1l.fm.fore.xts<-xts(d1l.fm.fore,seq(as.POSIXct(startdate),len=length(d1l.fm.fore),by='day'))
d1.lm1.fore.xts<-xts(d1.lm1.fore,seq(as.POSIXct(startdate),len=length(d1.lm1.fore),by='day'))
d1.ets.fore.xts<-xts(d1.ets.fore,seq(as.POSIXct(startdate),len=length(d1.ets.fore),by='day'))
d1.tba.fore.xts<-xts(d1.tba.fore,seq(as.POSIXct(startdate),len=length(d1.tba.fore),by='day'))
d1.tb1.fore.xts<-xts(d1.tb1.fore,seq(as.POSIXct(startdate),len=length(d1.tb1.fore),by='day'))
d1.hw1.fore.xts<-xts(d1.hw1.fore,seq(as.POSIXct(startdate),len=length(d1.hw1.fore),by='day'))
d1l.result<-(d1l.m1pee+d1.lm1p[,1]+d17tbatsp1$mean+d17hw$mean)/4;
#d1l.result<-d1l.fmp;
#d1l.result<-d1l.fmp;
d1l.result.xts<-xts(d1l.result,seq(as.POSIXct(startdate),len=length(d1l.m1pee),by='day'))
plot(as.zoo(cbind(d1l.m1.data.xts,d1l.m1.fore.xts,d1l.m1ee.fore.xts,d1.lm1.fore.xts,d1.ets.fore.xts,d1.tba.fore.xts,d1.tb1.fore.xts,d1.hw1.fore.xts,d1l.result.xts)),col=1:9,lty=1:9,screens=1,ylab=dstr[di],xlab="Time")
legend(x="topright",legend=c("observed","seasonal arima","seasonal arima correction","lm","ets","tbats 7+30","tbats 7+30.44","hw","result"),lty=1:9,col=1:9)
##print result
cat(">>>> ",dstr[di]," <<<<\n")
print(d1l.result.xts);
cat("========\n")
print(d1l.m1pee);
#print(d1l.fm.bestfit);
#cat("ki:",ki," kj:",kj," aic:",aicb,"\n");
