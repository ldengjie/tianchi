#######################

#######################
library("forecast")
library("TSA")
library("xts")
library("wmtsa")
od<-read.csv("purchase_by_type_p1_v1.csv")
of<-read.csv("totalfs.csv")
#fitbeg<-1;#20140401
fitbeg<-275;#20140401
fitend<-427;#20140831
prebeg<-fitend+1;
preend<-457;#20140930
fd<-od[fitbeg:fitend,]

report_date         <-ts(fd$report_date,fre=7)
total_purchase_amt  <-ts(log(fd$total_purchase_amt ),fre=7)
zfb1                <-ts(log(fd$zfb1),fre=7)
zfb2                <-ts(log(fd$zfb2),fre=7)
zfb3                <-ts(log(fd$zfb3),fre=7)
ibank1               <-ts(log(fd$bank1),fre=7)
ibank2               <-ts(log(fd$bank2),fre=7)
ibank3               <-ts(log(fd$bank3),fre=7)
ibank4               <-ts(log(fd$bank4 ),fre=7)
ibank5               <-ts(log(fd$bank5 ),fre=7)
ibank6               <-ts(log(fd$bank6 ),fre=7)
ibank7               <-ts(log(fd$bank7 ),fre=7)
ibank8               <-ts(log(fd$bank8 ),fre=7)
ibank9               <-ts(log(fd$bank9 ),fre=7)
ibank10              <-ts(log(fd$bank10),fre=7)
ibank11              <-ts(log(fd$bank11),fre=7)
share               <-ts(log(fd$share),fre=7)
bank1<-ts(log(fd$bank1+fd$bank2+fd$bank3+fd$bank4),fre=7)
bank2<-ts(log(fd$bank5+fd$bank6),fre=7)
bank3<-ts(log(fd$bank7+fd$bank8+fd$bank9+fd$bank10+fd$bank11),fre=7)

#total_purchase_amt<-wavShrink(total_purchase_amt[fitbeg:fitend],n.level=1)
#zfb1 <-wavShrink(zfb1[fitbeg:fitend] ,n.level=1)
#zfb2 <-wavShrink(zfb2[fitbeg:fitend] ,n.level=1)
#bank1<-wavShrink(bank1[fitbeg:fitend],n.level=1)
#bank2<-wavShrink(bank2[fitbeg:fitend],n.level=1)

cat("... begin to loop ...\n")

par(mfcol=c(6,4))
zfb1<-log(exp(zfb1))
zfb2<-log(exp(zfb2)+exp(zfb3))
bank1<-log(exp(bank1)+exp(bank2))
bank2<-log(exp(bank3)+exp(share))
tslist<-list(total_purchase_amt,zfb1,zfb2,bank1,bank2)
type<-list("total_purchase_amt","zfb1","zfb2","bank1","bank2")
#orignal+features
#orderlist<-list(c(0,0,0),c(0,0,0),c(3,1,4),c(1,0,1),c(1,0,5))
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
orderlist<-list(c(0,0,0),c(0,0,0),c(7,1,1),c(1,0,4),c(4,1,1))
#orignal
#orderlist<-list(c(4,0,1),c(0,0,0),c(4,0,4),c(1,0,1),c(5,0,1))
#sorderlist<-list(c(0,1,2),c(2,1,1),c(2,1,1),c(3,1,2),c(1,1,2))

result.tsam      <-rep(0,30)
result.stlf.arima<-rep(0,30)
result.stlf.ets  <-rep(0,30)
result.tbats     <-rep(0,30)
result.tsw       <-rep(0,30)
result.fourier   <-rep(0,30)
result.sam       <-rep(0,30)
result.hw        <-rep(0,30)
fittedValue.tsam       <-rep(0,fitend-fitbeg+1)
fittedValue.stlf.arima <-rep(0,fitend-fitbeg+1)
fittedValue.stlf.ets   <-rep(0,fitend-fitbeg+1)
fittedValue.tbats      <-rep(0,fitend-fitbeg+1)
fittedValue.tsw        <-rep(0,fitend-fitbeg+1)
fittedValue.fourier    <-rep(0,fitend-fitbeg+1)
fittedValue.sam        <-rep(0,fitend-fitbeg+1)
fittedValue.hw         <-rep(0,fitend-fitbeg+1)
#(1)report_date, (2)w1, (3)w2, (4)w3, (5)w4, (6)w5, (7)w6, (8)mb1, (9)mb2, (10)mb3, (11)mb4, (12)mb5, (13)ma1, (14)ma2, (15)ma3, (16)ma4, (17)ma5, (18)fb1, (19)fb2, (20)fb3, (21)fb4, (22)fb5, (23)fb6, (24)fb7, (25)fi1, (26)fi2, (27)fi3, (28)fa1, (29)fa2, (30)fa3, (31)fa4, (32)fa5, (33)fa6, (34)fa7, (35)tx, (36)sb1, (37)sb2, (38)sb3, (39)sb4, (40)sb5, (41)sb6, (42)sb7, (43)si1, (44)sa1, (45)sa2, (46)sa3, (47)sa4, (48)sa5, (49)sa6, (50)sa7, (51)cb1, (52)cb2, (53)cb3, (54)cb4, (55)cb5, (56)cb6, (57)cb7, (58)ci1, (59)ci2, (60)ci3, (61)ci4, (62)ci5, (63)ci6, (64)ci7, (65)ca1, (66)ca2, (67)ca3, (68)ca4, (69)ca5, (70)ca6, (71)ca7
#5
xregfit<-data.frame(of[fitbeg:fitend,2:17],of[fitbeg:fitend,20:32],of[fitbeg:fitend,35],of[fitbeg:fitend,38:48]);
xregpre<-data.frame(of[prebeg:preend,2:17],of[prebeg:preend,20:32],of[prebeg:preend,35],of[prebeg:preend,38:48]);
for(ti in 2:5)
{
    cat("###arima-lm###\n")
    st<-0.05
    tsdata<-tslist[[ti]]
    or<-orderlist[[ti]]
    legend<-type[[ti]]
    aicb<-Inf;
    #vm<-list(model="sGARCH",garchOrder=c(7,6),submodel = NULL, external.regressors = NULL, variance.targeting = FALSE)
    #mm<-list(armaOrder=c(5,1),include.mean=FALSE,external.regressors=as.matrix(xregfit))
    #mm<-list(armaOrder=c(5,1))
    #spec<-ugarchspec(variance.mode=vm,mean.mode=mm,distribution.mode="norm",start.pars = list(), fixed.pars = list())
    #ts.igar=ugarchfit(data=tsdata,spec=spec,solver.control=list(trace=0))
    for(si in st)
    {
        if(si==0) next;
        #if(ti==4)
        #{
            #tsam<-arima(tsdata,order=or,xreg=data.frame(xregfit),io=c(33))
        #}else
        if(ti==5)
        {
            tsam<-arima(tsdata,order=or,xreg=xregfit,io=c(33,101,137))
        }else
        {
            tsam<-arima(tsdata,order=or,xreg=xregfit)
        }
        aico<-tsam$aic;
        fixedv<-rep(NA,length(tsam$coef));
        needFix<-TRUE;
        #cat(si,"---------------------------->\n");
        #print(tsam)
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
                #if(ti==4)
                #{
                    #tsam<-arima(tsdata,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE,io=c(33))
                #}else
                if(ti==5)
                {
                    tsam<-arima(tsdata,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE,io=c(33,101,137))
                }else
                {
                    tsam<-arima(tsdata,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
                }
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
    cat("========>",legend,"<======\n")
    #if(ti==4)
    #{
        #tsam.bestfit<-arima(tsdata,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE,io=c(33));
        #fittedValueTmp<-fitted(tsam.bestfit)
        #print(tsam.bestfit);
        #tsam.bestfit$coef<-tsam.bestfit$coef[1:(length(tsam.bestfit$coef)-1)]
    #}else 
    if(ti==5)
    {
        tsam.bestfit<-arima(tsdata,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE,io=c(33,101,137));
        fittedValueTmp<-fitted(tsam.bestfit)
        print(tsam.bestfit);
        tsam.bestfit$coef<-tsam.bestfit$coef[1:(length(tsam.bestfit$coef)-3)]
    }else
    {
        tsam.bestfit<-arima(tsdata,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE);
        fittedValueTmp<-fitted(tsam.bestfit)
        print(tsam.bestfit);
    }
        #acf(residuals(tsam.bestfit),lag=60)
        #pacf(residuals(tsam.bestfit),lag=60)
        #acf(abs(residuals(tsam.bestfit)),lag=60)
        #pacf(abs(residuals(tsam.bestfit)),lag=60)
        #acf(residuals(tsam.bestfit)^2,lag=60)
        #pacf(residuals(tsam.bestfit)^2,lag=60)
        #acf(diff(residuals(tsam.bestfit)),lag=60) 
        #pacf(diff(residuals(tsam.bestfit)),lag=60)
        #acf(diff(residuals(tsam.bestfit),lag=7),lag=60)
        #pacf(diff(residuals(tsam.bestfit),lag=7),lag=60)
    detectAO(tsam.bestfit);
    detectIO(tsam.bestfit);
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
    tsam.p<-predict(tsam.bestfit,newxreg =xregpre);
    tsamp<-tsam.p$pred;
    result.tsam<-result.tsam+exp(tsamp)
    fittedValue.tsam<-fittedValue.tsam+exp(fittedValueTmp)

#    #obtain month/holiday features from arima.
#    coefs <- tsam.bestfit$coef
#    narma<-sum(tsam.bestfit$arma[1:4])
#    if(names(coefs)[narma + 1L] == "intercept") narma<-narma+1
#    shift<-coefs[-(1:narma)]
#    cat("###holidayshift###\n")
#    holidayshift<-rep(1,30)
#    for(fi in 17:NCOL(xregpre))
#    {
#        holidayshift<-holidayshift*exp(xregpre[,fi]*shift[fi])
#    }
#    print(holidayshift)
#    cat("###monthshift###\n")
#    monthshift<-rep(1,30)
#    for(fi in 7:16)
#    {
#        monthshift<-monthshift*exp(xregpre[,fi]*shift[fi])
#    }
#    print(monthshift)
#    cat("###stlf.ets###\n")
#    tsst.ets<-stlf(tsdata,h=30,s.window=7,method='ets',ic='bic',opt.crit='mae')
#    result.stlf.ets<-result.stlf.ets+exp(tsst.ets$mean)*holidayshift*monthshift
#    fittedValue.stlf.ets<-fittedValue.stlf.ets+exp(fitted(tsst.ets))
#    cat("###stlf.arima###\n")
#    tsst.arima<-stlf(tsdata,h=30,s.window=7,method='arima',ic='bic',xreg=xregfit,newxreg=xregpre)
#    result.stlf.arima<-result.stlf.arima+exp(tsst.arima$mean)
#    fittedValue.stlf.arima<-fittedValue.stlf.arima+exp(fitted(tsst.arima))
#    cat("###tbats###\n")
#    tsm1<-msts(tsdata,seasonal.periods=c(7,30.44))
#    tbats<-tbats(tsm1)
#    tbatsp<-forecast(tbats,30)
#    result.tbats<-result.tbats+exp(tbatsp$mean)*holidayshift
#    fittedValue.tbats<-fittedValue.tbats+exp(fitted(tbats))
#    cat("###fourier###\n")
#    ff<-od[fitbeg:preend,2]
#    d7<-ts(ff,fre=7)
#    d30<-ts(ff,fre=30.44)
#    #tsfo<-arimax(tsdata,order=c(5,0,5),xreg=data.frame(fourier(d7,K=3)[1:(fitend-fitbeg+1)],fourier(d30,K=15)[1:(fitend-fitbeg+1)]));
#    tsfo<-auto.arima(tsdata,xreg=data.frame(fourier(d7,K=3)[1:(fitend-fitbeg+1)],fourier(d30,K=15)[1:(fitend-fitbeg+1)]));
#    tsfo.p<-predict(tsfo,newxreg = data.frame(fourier(d7,K=3)[(prebeg-fitbeg+1):(preend-fitbeg+1)],fourier(d30,K=15)[(prebeg-fitbeg+1):(preend-fitbeg+1)]));
#    tsfop<-exp(tsfo.p$pred)*holidayshift;
#    result.fourier<-result.fourier+tsfop;
#    fittedValue.fourier<-fittedValue.fourier+exp(tsdata-residuals(tsfo));
#    cat("###wavelet###\n")
#    nl<-3
#    tsw3<-wavMODWT(tsdata,n.level=nl)
#    tswp<-rep(0,30)
#    to<-rep(0,153)
#    tsw31<-wavMRD(tsw3)
#    tfi<-rep(0,153)
#    wor<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,1,0))
#    for(tswi in 1:(nl+1))
#    {
#        tsw<-arima(tsw31[[tswi]],order=wor[[tswi]],xreg=xregfit)
#        to<-to+tsw31[[tswi]]
#        tfi<-tfi+tsw31[[tswi]]-residuals(tsw)
#        tsw.p<-predict(tsw,newxreg=xregpre)
#        tswp<-tswp+tsw.p$pred
#        #tsw.datat.xts<-xts(c(tsw31[[tswi]],rep(NA,30)),seq(as.POSIXct("2014-04-01"),len=183,by='day'))
#        #tsw.foret<-ts(c((tsw31[[tswi]]-residuals(tsw)),tsw.p$pred),fre=7)    
#        #tsw.foret.xts<-xts(tsw.foret,seq(as.POSIXct("2014-04-01"),len=length(tsw.foret),by='day'))
#        #plot(as.zoo(cbind(tsw.datat.xts,tsw.foret.xts)),col=1:3,lty=1:3,screens=1)
#    }
#    result.tsw<-result.tsw+exp(tswp)*holidayshift
#    fittedValue.tsw<-fittedValue.tsw+exp(tfi)
#    cat("#seasonal arima + holiday shift \n")
#    nor<-list(c(0,0,0),c(0,0,0),c(4,0,4),c(1,0,2),c(6,0,6))
#    sor<-list(c(0,0,0),c(1,1,1),c(2,1,1),c(3,1,2),c(1,1,2))
#    if(ti==2 || ti==3)
#    {
#        tssam<-arima(tsdata,order=nor[[ti]],seasonal=list(order=sor[[ti]],period=7),io=c(50))
#    }else if(ti==4)
#    {
#        tssam<-arima(tsdata,order=nor[[ti]],seasonal=list(order=sor[[ti]],period=7),io=c(34))
#    }else
#    {
#        tssam<-arima(tsdata,order=nor[[ti]],seasonal=list(order=sor[[ti]],period=7))
#    }
#    print(tssam)
#    detectAO(tssam);
#    detectIO(tssam);
#    aicb2<-Inf;
#    aico2<-tssam$aic;
#    needFix<-TRUE;
#    while(needFix)
#    {
#        fix<-tssam$call$fixed
#        fixedv2<-if(!is.null(fix)) eval.parent(fix) else rep(NA,length(tssam$coef));
#        needFix<-FALSE;
#        vi<-1;
#        for (ci in 1:length(tssam$coef))
#        {
#            if(tssam$coef[ci]==0)
#            {
#                next
#            }else
#            {
#                if(!is.na(sqrt(diag(tssam$var.coef))[vi]))
#                {
#                    if( (1-pnorm(abs(tssam$coef[ci])/sqrt(diag(tssam$var.coef))[vi]))*2 > st )
#                    {
#                        fixedv2[ci]=0;
#                        needFix<-TRUE;
#                    }
#                }
#                vi<-vi+1
#            }
#        } 
#        if(needFix)
#        {
#            cat("Fixing...\n")
#            if(ti==2||ti==3)
#            {
#                tssam<-arima(tsdata,order=nor[[ti]],seasonal=list(order=sor[[ti]],period=7),fixed=fixedv2,transform.pars = FALSE,io=c(50))
#            }else if(ti==4)
#            {
#                tssam<-arima(tsdata,order=nor[[ti]],seasonal=list(order=sor[[ti]],period=7),fixed=fixedv2,transform.pars = FALSE,io=c(34))
#            }else
#            {
#                tssam<-arima(tsdata,order=nor[[ti]],seasonal=list(order=sor[[ti]],period=7),fixed=fixedv2,transform.pars = FALSE)
#            }
#        }
#    }
#    if(ti==2 || ti==3 | ti==4)
#    {
#        tssam$coef<-tssam$coef[1:(length(tssam$coef)-1)]
#    }
#    tssam.p<-predict(tssam,30);
#    tssamp<-tssam.p$pred;
#    result.sam<-result.sam+exp(tssamp)*holidayshift*monthshift
#    fittedValue.sam<-fittedValue.sam+exp(fitted(tssam))
#    cat("###hw###\n")
#    tshw<-hw(exp(tsdata),h=30,seasonal='multiplicative',damped=TRUE)
#    result.hw<-result.hw+tshw$mean*holidayshift*monthshift
#    fittedValue.hw<-fittedValue.hw+fitted(tshw)
#
#    tsam.data<-ts(c(exp(tsdata),od[prebeg:preend,2]),fre=7)               
#    tsam.data.xts<-xts(tsam.data,seq(as.POSIXct("2014-04-01"),len=length(tsam.data),by='day'))
#
#    tsam.fore      <-ts(exp(c(fittedValueTmp,tsamp)),fre=7)    
#    stlf.arima.fore<-ts(c(exp(fitted(tsst.arima)),exp(tsst.arima$mean)),fre=7)    
#    stlf.ets.fore  <-ts(c(exp(fitted(tsst.ets)),exp(tsst.ets$mean)*holidayshift*monthshift),fre=7)    
#    tbats.fore     <-ts(c(exp(fitted(tbats)),exp(tbatsp$mean)*holidayshift),fre=7)    
#    tsfo.fore      <-ts(c(exp(tsdata-residuals(tsfo)),tsfop),fre=7)    
#    tsw.fore       <-ts(c(exp(tfi),exp(tswp)*holidayshift),fre=7)    
#    tssam.fore     <-ts(c(exp(fitted(tssam)),exp(tssamp)*holidayshift*monthshift),fre=7)    
#    tshw.fore      <-ts(c(fitted(tshw),tshw$mean*holidayshift*monthshift),fre=7)    
#    tsam.fore.xts      <-xts(tsam.fore,seq(as.POSIXct("2014-04-01"),len=length(tsam.fore),by='day'))
#    stlf.arima.fore.xts<-xts(stlf.arima.fore,seq(as.POSIXct("2014-04-01"),len=length(stlf.arima.fore),by='day'))
#    stlf.ets.fore.xts  <-xts(stlf.ets.fore,seq(as.POSIXct("2014-04-01"),len=length(stlf.ets.fore),by='day'))
#    tbats.fore.xts     <-xts(tbats.fore,seq(as.POSIXct("2014-04-01"),len=length(tbats.fore),by='day'))
#    tsfo.fore.xts      <-xts(tsfo.fore,seq(as.POSIXct("2014-04-01"),len=length(tsfo.fore),by='day'))
#    tsw.fore.xts       <-xts(tsw.fore,seq(as.POSIXct("2014-04-01"),len=length(tsw.fore),by='day'))
#    tssam.fore.xts     <-xts(tssam.fore,seq(as.POSIXct("2014-04-01"),len=length(tssam.fore),by='day'))
#    tshw.fore.xts      <-xts(tshw.fore,seq(as.POSIXct("2014-04-01"),len=length(tshw.fore),by='day'))
#    #plot(as.zoo(cbind(tsam.data.xts,tsam.fore.xts,stlf.arima.fore.xts,stlf.ets.fore.xts,tbats.fore.xts,tsfo.fore.xts,tsw.fore.xts,tssam.fore.xts,tshw.fore.xts)),col=1:9,lty=1:9,screens=1)
#    #legend(x="topleft",legend=c("observed","arima+lm","stlf.arima","stlf.ets","tbats","fourier arima","wavelet arima","seasonal arima","hw"),lty=1:9,col=1:9)
}
#
#totalFittedValue<-( ts(fittedValue.tsam,fre=7)
#                   +ts(fittedValue.stlf.arima,fre=7)
#                   +ts(fittedValue.stlf.ets  ,fre=7)
#                   #+ts(fittedValue.tbats     ,fre=7)
#                   +ts(fittedValue.tsw       ,fre=7)
#                   +ts(fittedValue.fourier   ,fre=7)
#                   +ts(fittedValue.sam       ,fre=7)
#                   +ts(fittedValue.hw        ,fre=7)
                   #)/1
totalResult<-( ts(result.tsam,fre=7)
#              +ts(result.stlf.arima,fre=7)
#              +ts(result.stlf.ets  ,fre=7)
#              #+ts(result.tbats     ,fre=7)
#              +ts(result.tsw       ,fre=7)
#              +ts(result.fourier   ,fre=7)
#              +ts(result.sam       ,fre=7)
#              +ts(result.hw        ,fre=7)
              )/1
#
#cat(">>>> Purchase <<<<\n")
print(totalResult)
#
#total.data           <-ts(c(od[fitbeg:preend,2]),fre=7)               
#total.fore           <-ts(c(totalFittedValue,totalResult),fre=7)    
#total.fore.tsam      <-ts(c(fittedValue.tsam      ,result.tsam      ),fre=7)    
#total.fore.stlf.arima<-ts(c(fittedValue.stlf.arima,result.stlf.arima),fre=7)    
#total.fore.stlf.ets  <-ts(c(fittedValue.stlf.ets  ,result.stlf.ets  ),fre=7)    
#total.fore.tbats     <-ts(c(fittedValue.tbats     ,result.tbats     ),fre=7)    
#total.fore.tsw       <-ts(c(fittedValue.tsw       ,result.tsw       ),fre=7)    
#total.fore.fourier   <-ts(c(fittedValue.fourier   ,result.fourier   ),fre=7)    
#total.fore.sam       <-ts(c(fittedValue.sam       ,result.sam       ),fre=7)    
#total.fore.hw        <-ts(c(fittedValue.hw        ,result.hw        ),fre=7)    
#total.data.xts           <-xts(total.data,seq(as.POSIXct("2014-04-01"),len=length(total.data),by='day'))
#total.fore.xts           <-xts(total.fore,seq(as.POSIXct("2014-04-01"),len=length(total.fore),by='day'))
#total.fore.tsam.xts      <-xts(total.fore.tsam      ,seq(as.POSIXct("2014-04-01"),len=length(total.fore.tsam      ),by='day'))
#total.fore.stlf.arima.xts<-xts(total.fore.stlf.arima,seq(as.POSIXct("2014-04-01"),len=length(total.fore.stlf.arima),by='day'))
#total.fore.stlf.ets.xts  <-xts(total.fore.stlf.ets  ,seq(as.POSIXct("2014-04-01"),len=length(total.fore.stlf.ets  ),by='day'))
#total.fore.tbats.xts     <-xts(total.fore.tbats     ,seq(as.POSIXct("2014-04-01"),len=length(total.fore.tbats     ),by='day'))
#total.fore.tsw.xts       <-xts(total.fore.tsw       ,seq(as.POSIXct("2014-04-01"),len=length(total.fore.tsw       ),by='day'))
#total.fore.fourier.xts   <-xts(total.fore.fourier   ,seq(as.POSIXct("2014-04-01"),len=length(total.fore.fourier   ),by='day'))
#total.fore.sam.xts       <-xts(total.fore.sam       ,seq(as.POSIXct("2014-04-01"),len=length(total.fore.sam       ),by='day'))
#total.fore.hw.xts        <-xts(total.fore.hw        ,seq(as.POSIXct("2014-04-01"),len=length(total.fore.hw        ),by='day'))
#
#plot(as.zoo(cbind(total.data.xts,total.fore.tsam.xts,total.fore.stlf.arima.xts,total.fore.stlf.ets.xts,total.fore.tbats.xts,total.fore.tsw.xts,total.fore.fourier.xts,total.fore.sam.xts,total.fore.hw.xts,total.fore.xts)),col=1:9,lty=1:9,screens=1)
#legend(x="topleft",legend=c("observed","arima+lm","stlf.arima","stlf.ets","tbats","fourier arima","wavelet arima","seasonal arima","hw","result"),lty=1:9,col=1:9)
-----
library("forecast")
library("TSA")
library("xts")
library("wmtsa")
library("rugarch")
od<-read.csv("redeem_by_type_p1_v1.csv")
of<-read.csv("totalfs.csv")
fitbeg<-275;#20140401
fitend<-427;#20140831
prebeg<-fitend+1;
preend<-457;#20140930
fd<-od[fitbeg:fitend,]

report_date      <-ts(fd$report_date,fre=7)
total_redeem_amt <-ts(log(fd$total_redeem_amt ),fre=7)
tftobal_amt      <-ts(log(fd$tftobal_amt      ),fre=7)
category1        <-ts(log(fd$category1        ),fre=7)
category2        <-ts(log(fd$category2        ),fre=7)
category3        <-ts(log(fd$category3        ),fre=7)
category4        <-ts(log(fd$category4        ),fre=7)
icard0            <-ts(log(fd$card0            ),fre=7)
icard1            <-ts(log(fd$card1            ),fre=7)
icard2            <-ts(log(fd$card2            ),fre=7)
icard3             <-ts(log(fd$card3             ),fre=7)
icard4             <-ts(log(fd$card4             ),fre=7)
icard5             <-ts(log(fd$card5             ),fre=7)
icard6             <-ts(log(fd$card6             ),fre=7)
icard7             <-ts(log(fd$card7             ),fre=7)
icard8             <-ts(log(fd$card8             ),fre=7)
icard9             <-ts(log(fd$card9             ),fre=7)
icard10            <-ts(log(fd$card10            ),fre=7)
icard11            <-ts(log(fd$card11            ),fre=7)
consume<-ts(log(fd$category1+fd$category2+fd$category3+fd$category4),fre=7)
card1<-ts(log(fd$card1+fd$card2+fd$card3),fre=7)
card2<-ts(log(fd$card4+fd$card5+fd$card6),fre=7)
card3<-ts(log(fd$card7+fd$card8+fd$card9+fd$card10+fd$card11),fre=7)


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

cat("... begin to loop ...\n")

#xregpre<-rep(1,30);
#total_redeem_amt <-ts(total_redeem_amt ,fre=7)
#zfb1             <-ts(zfb1             ,fre=7)
#card1            <-ts(card1            ,fre=7)
#card2            <-ts(card2            ,fre=7)


#par(mfcol=c(2,2))
par(mfcol=c(6,3))
zfb1<-log(exp(tftobal_amt)+exp(consume))
card1<-log(exp(card1))
card2<-log(exp(card2)+exp(card3))
type<-list("total_redeem_amt","zfb1","card1","card2")
tslist<-list(total_redeem_amt,zfb1,card1,card2)
#orignal+features
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
orderlist<-list(c(0,0,0),c(1,1,1),c(1,0,3),c(2,1,1))
#orderlist<-list(c(0,0,0),c(4,1,1),c(3,1,1),c(3,1,1))
#type<-list("total_redeem_amt","zfb1","card1","card2","card3")
#tslist<-list(total_redeem_amt,zfb1,card1,card2,card3)
#sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
#orderlist<-list(c(0,0,0),c(1,0,2),c(3,1,1),c(1,0,1),c(5,1,1))
#orignal
#orderlist<-list(c(0,0,0),c(2,0,4),c(5,0,1),c(5,0,2))
#sorderlist<-list(c(0,0,0),c(1,1,0),c(0,1,1),c(0,1,1))

result.tsam      <-rep(0,30)
result.stlf.arima<-rep(0,30)
result.stlf.ets  <-rep(0,30)
result.tbats     <-rep(0,30)
result.tsw       <-rep(0,30)
result.fourier   <-rep(0,30)
result.sam       <-rep(0,30)
result.hw        <-rep(0,30)
fittedValue.tsam       <-rep(0,fitend-fitbeg+1)
fittedValue.stlf.arima <-rep(0,fitend-fitbeg+1)
fittedValue.stlf.ets   <-rep(0,fitend-fitbeg+1)
fittedValue.tbats      <-rep(0,fitend-fitbeg+1)
fittedValue.tsw        <-rep(0,fitend-fitbeg+1)
fittedValue.fourier    <-rep(0,fitend-fitbeg+1)
fittedValue.sam        <-rep(0,fitend-fitbeg+1)
fittedValue.hw         <-rep(0,fitend-fitbeg+1)
#5
xregfit<-data.frame(of[fitbeg:fitend,2:17],of[fitbeg:fitend,20:32],of[fitbeg:fitend,35],of[fitbeg:fitend,38:48]);
xregpre<-data.frame(of[prebeg:preend,2:17],of[prebeg:preend,20:32],of[prebeg:preend,35],of[prebeg:preend,38:48]);

#xregfit<-of[fitbeg:fitend,2]
#xregpre<-of[prebeg:preend,2]
#for(xi in 3:NCOL(of))
#{
#if(sum(of[fitbeg:fitend,xi])!=0)
#{
#xregfit<-cbind(xregfit,of[fitbeg:fitend,xi])
#xregpre<-cbind(xregpre,of[prebeg:preend,xi])
#}
#}
for(ti in 2:4)
{
    #st<-seq(0,0.4,0.05);
    st<-0.05
    tsdata<-tslist[[ti]]
    or<-orderlist[[ti]]
    sor<-sorderlist[[ti]]
    legend<-type[[ti]]
    aicb<-Inf;
    #vm<-list(model="iGARCH",garchOrder=c(4,1),submodel = NULL, external.regressors = NULL, variance.targeting = FALSE)
    #mm<-list(armaOrder=c(5,1),include.mean=FALSE,external.regressors=as.matrix(xregfit))
    #mm<-list(armaOrder=c(5,1),external.regressors=as.matrix(xregfit[,1]))
    #mm<-list(armaOrder=c(1,1))
    #spec<-ugarchspec(variance.mode=vm,mean.mode=mm,distribution.mode="norm",start.pars = list(), fixed.pars = list())
    #ts.igar=ugarchfit(data=tsdata,spec=spec,solver.control=list(trace=0))
    #next
    for(si in st)
    {
        if(si==0) next;
        tsam<-arima(tsdata,order=or,xreg=xregfit)
        aico<-tsam$aic;
        fixedv<-rep(NA,length(tsam$coef));
        needFix<-TRUE;
        cat(si,"---------------------------->\n");
        #print(tsam)
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
                tsam<-arima(tsdata,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
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
    cat("========>",legend,"<======\n")
    tsam.bestfit<-arima(tsdata,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE);
    fittedValueTmp<-fitted(tsam.bestfit)
    print(tsam.bestfit);
    detectAO(tsam.bestfit);
    detectIO(tsam.bestfit);
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
    tsam.p<-predict(tsam.bestfit,newxreg =xregpre);
    tsamp<-tsam.p$pred;
    print(exp(tsamp))
    result.tsam<-result.tsam+exp(tsamp)
    fittedValue.tsam<-fittedValue.tsam+exp(fittedValueTmp)
        #acf(residuals(tsam.bestfit),lag=60)
        #pacf(residuals(tsam.bestfit),lag=60)
        #acf(abs(residuals(tsam.bestfit)),lag=60)
        #pacf(abs(residuals(tsam.bestfit)),lag=60)
        #acf(residuals(tsam.bestfit)^2,lag=60)
        #pacf(residuals(tsam.bestfit)^2,lag=60)
        #acf(diff(residuals(tsam.bestfit)),lag=60) 
        #pacf(diff(residuals(tsam.bestfit)),lag=60)
        #acf(diff(residuals(tsam.bestfit),lag=7),lag=60)
        #pacf(diff(residuals(tsam.bestfit),lag=7),lag=60)

#    #obtain month/holiday features from arima.
#    coefs <- tsam.bestfit$coef
#    narma<-sum(tsam.bestfit$arma[1:4])
#    if(names(coefs)[narma + 1L] == "intercept") narma<-narma+1
#    shift<-coefs[-(1:narma)]
#    cat("###holidayshift###\n")
#    holidayshift<-rep(1,30)
#    for(fi in 17:NCOL(xregpre))
#    {
#        holidayshift<-holidayshift*exp(xregpre[,fi]*shift[fi])
#    }
#    print(holidayshift)
#    cat("###monthshift###\n")
#    monthshift<-rep(1,30)
#    for(fi in 7:16)
#    {
#        monthshift<-monthshift*exp(xregpre[,fi]*shift[fi])
#    }
#    print(monthshift)
#
#    cat("###stlf.ets###\n")
#    tsst.ets<-stlf(tsdata,h=30,s.window=7,method='ets',ic='bic',opt.crit='mae')
#    result.stlf.ets<-result.stlf.ets+exp(tsst.ets$mean)*holidayshift*monthshift
#    fittedValue.stlf.ets<-fittedValue.stlf.ets+exp(fitted(tsst.ets))
#    cat("###stlf.arima###\n")
#    tsst.arima<-stlf(tsdata,h=30,s.window=7,method='arima',ic='bic',xreg=xregfit,newxreg=xregpre)
#    result.stlf.arima<-result.stlf.arima+exp(tsst.arima$mean)
#    fittedValue.stlf.arima<-fittedValue.stlf.arima+exp(fitted(tsst.arima))
#    cat("###tbats###\n")
#    tsm1<-msts(tsdata,seasonal.periods=c(7,30.44))
#    tbats<-tbats(tsm1)
#    tbatsp<-forecast(tbats,30)
#    result.tbats<-result.tbats+exp(tbatsp$mean)*holidayshift
#    fittedValue.tbats<-fittedValue.tbats+exp(fitted(tbats))
#    cat("###fourier###\n")
#    ff<-od[fitbeg:preend,2]
#    d7<-ts(ff,fre=7)
#    d30<-ts(ff,fre=30.44)
#    #tsfo<-arimax(tsdata,order=c(5,0,5),xreg=data.frame(fourier(d7,K=3)[1:(fitend-fitbeg+1)],fourier(d30,K=15)[1:(fitend-fitbeg+1)]));
#    tsfo<-auto.arima(tsdata,xreg=data.frame(fourier(d7,K=3)[1:(fitend-fitbeg+1)],fourier(d30,K=15)[1:(fitend-fitbeg+1)]),allowdrift=F);
#    tsfo.p<-predict(tsfo,newxreg = data.frame(fourier(d7,K=3)[(prebeg-fitbeg+1):(preend-fitbeg+1)],fourier(d30,K=15)[(prebeg-fitbeg+1):(preend-fitbeg+1)]));
#    tsfop<-exp(tsfo.p$pred)*holidayshift;
#    result.fourier<-result.fourier+tsfop;
#    fittedValue.fourier<-fittedValue.fourier+exp(tsdata-residuals(tsfo));
#    cat("###wavelet###\n")
#    nl<-3
#    tsw3<-wavMODWT(tsdata,n.level=nl)
#    tswp<-rep(0,30)
#    to<-rep(0,153)
#    tsw31<-wavMRD(tsw3)
#    tfi<-rep(0,153)
#    wor<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,1,0))
#    for(tswi in 1:(nl+1))
#    {
#        tsw<-arima(tsw31[[tswi]],order=wor[[tswi]],xreg=xregfit)
#        to<-to+tsw31[[tswi]]
#        tfi<-tfi+tsw31[[tswi]]-residuals(tsw)
#        tsw.p<-predict(tsw,newxreg=xregpre)
#        tswp<-tswp+tsw.p$pred
#        #tsw.datat.xts<-xts(c(tsw31[[tswi]],rep(NA,30)),seq(as.POSIXct("2014-04-01"),len=183,by='day'))
#        #tsw.foret<-ts(c((tsw31[[tswi]]-residuals(tsw)),tsw.p$pred),fre=7)    
#        #tsw.foret.xts<-xts(tsw.foret,seq(as.POSIXct("2014-04-01"),len=length(tsw.foret),by='day'))
#        #plot(as.zoo(cbind(tsw.datat.xts,tsw.foret.xts)),col=1:3,lty=1:3,screens=1)
#    }
#    result.tsw<-result.tsw+exp(tswp)*holidayshift
#    fittedValue.tsw<-fittedValue.tsw+exp(tfi)
#
#    cat("#seasonal arima + holiday shift \n")
#    nor<-list(c(0,0,0),c(2,0,4),c(5,0,1),c(5,0,2))
#    sor<-list(c(0,0,0),c(1,1,0),c(0,1,1),c(0,1,1))
#    tssam<-arima(tsdata,order=nor[[ti]],seasonal=list(order=sor[[ti]],period=7))
#    print(tssam)
#    detectAO(tssam);
#    detectIO(tssam);
#    aicb2<-Inf;
#    aico2<-tssam$aic;
#    needFix<-TRUE;
#    st<-0.05
#    while(needFix)
#    {
#        fix<-tssam$call$fixed
#        fixedv2<-if(!is.null(fix)) eval.parent(fix) else rep(NA,length(tssam$coef));
#        needFix<-FALSE;
#        vi<-1;
#        for (ci in 1:length(tssam$coef))
#        {
#            if(tssam$coef[ci]==0)
#            {
#                next
#            }else
#            {
#                if(!is.na(sqrt(diag(tssam$var.coef))[vi]))
#                {
#                    if( (1-pnorm(abs(tssam$coef[ci])/sqrt(diag(tssam$var.coef))[vi]))*2 > st )
#                    {
#                        fixedv2[ci]=0;
#                        needFix<-TRUE;
#                    }
#                }
#                vi<-vi+1
#            }
#        } 
#        if(needFix)
#        {
#            cat("Fixing...\n")
#            tssam<-arima(tsdata,order=nor[[ti]],seasonal=list(order=sor[[ti]],period=7),fixed=fixedv2,transform.pars = FALSE)
#            print(tssam)
#        }
#    }
#    tssam.p<-predict(tssam,30);
#    tssamp<-tssam.p$pred;
#    result.sam<-result.sam+exp(tssamp)*holidayshift*monthshift
#    fittedValue.sam<-fittedValue.sam+exp(fitted(tssam))
#
#    cat("###hw###\n")
#    tshw<-hw(exp(tsdata),h=30,seasonal='multiplicative',damped=TRUE)
#    result.hw<-result.hw+tshw$mean*holidayshift*monthshift
#    fittedValue.hw<-fittedValue.hw+fitted(tshw)
#
#    tsam.data<-ts(c(exp(tsdata),od[prebeg:preend,2]),fre=7)               
#    tsam.data.xts<-xts(tsam.data,seq(as.POSIXct("2014-04-01"),len=length(tsam.data),by='day'))
#
#    tsam.fore      <-ts(exp(c(fittedValueTmp,tsamp)),fre=7)    
#    stlf.arima.fore<-ts(c(exp(fitted(tsst.arima)),exp(tsst.arima$mean)),fre=7)    
#    stlf.ets.fore  <-ts(c(exp(fitted(tsst.ets)),exp(tsst.ets$mean)*holidayshift*monthshift),fre=7)    
#    tbats.fore     <-ts(c(exp(fitted(tbats)),exp(tbatsp$mean)*holidayshift),fre=7)    
#    tsfo.fore      <-ts(c(exp(tsdata-residuals(tsfo)),tsfop),fre=7)    
#    tsw.fore       <-ts(c(exp(tfi),exp(tswp)*holidayshift),fre=7)    
#    tssam.fore     <-ts(c(exp(fitted(tssam)),exp(tssamp)*holidayshift*monthshift),fre=7)    
#    tshw.fore      <-ts(c(fitted(tshw),tshw$mean*holidayshift*monthshift),fre=7)    
#    tsam.fore.xts      <-xts(tsam.fore,seq(as.POSIXct("2014-04-01"),len=length(tsam.fore),by='day'))
#    stlf.arima.fore.xts<-xts(stlf.arima.fore,seq(as.POSIXct("2014-04-01"),len=length(stlf.arima.fore),by='day'))
#    stlf.ets.fore.xts  <-xts(stlf.ets.fore,seq(as.POSIXct("2014-04-01"),len=length(stlf.ets.fore),by='day'))
#    tbats.fore.xts     <-xts(tbats.fore,seq(as.POSIXct("2014-04-01"),len=length(tbats.fore),by='day'))
#    tsfo.fore.xts      <-xts(tsfo.fore,seq(as.POSIXct("2014-04-01"),len=length(tsfo.fore),by='day'))
#    tsw.fore.xts       <-xts(tsw.fore,seq(as.POSIXct("2014-04-01"),len=length(tsw.fore),by='day'))
#    tssam.fore.xts     <-xts(tssam.fore,seq(as.POSIXct("2014-04-01"),len=length(tssam.fore),by='day'))
#    tshw.fore.xts      <-xts(tshw.fore,seq(as.POSIXct("2014-04-01"),len=length(tshw.fore),by='day'))
#    #plot(as.zoo(cbind(tsam.data.xts,tsam.fore.xts,stlf.arima.fore.xts,stlf.ets.fore.xts,tbats.fore.xts,tsfo.fore.xts,tsw.fore.xts,tssam.fore.xts,tshw.fore.xts)),col=1:9,lty=1:9,screens=1)
#    #legend(x="topleft",legend=c("observed","arima+lm","stlf.arima","stlf.ets","tbats","fourier arima","wavelet arima","seasonal arima","hw"),lty=1:9,col=1:9)
#
}
#
#totalFittedValue<-( ts(fittedValue.tsam,fre=7)
#                   +ts(fittedValue.stlf.arima,fre=7)
#                   +ts(fittedValue.stlf.ets  ,fre=7)
#                   #+ts(fittedValue.tbats     ,fre=7)
#                   +ts(fittedValue.tsw       ,fre=7)
#                   +ts(fittedValue.fourier   ,fre=7)
#                   +ts(fittedValue.sam       ,fre=7)
#                   +ts(fittedValue.hw        ,fre=7)
#                   )/7
totalResult<-( ts(result.tsam,fre=7)
#              +ts(result.stlf.arima,fre=7)
#              +ts(result.stlf.ets  ,fre=7)
#              #+ts(result.tbats     ,fre=7)
#              +ts(result.tsw       ,fre=7)
#              +ts(result.fourier   ,fre=7)
#              +ts(result.sam       ,fre=7)
#              +ts(result.hw        ,fre=7)
              )/1
#
#cat(">>>> Redeem <<<<\n")
print(totalResult)
#
#total.data           <-ts(c(od[fitbeg:preend,2]),fre=7)               
#total.fore           <-ts(c(totalFittedValue,totalResult),fre=7)    
#total.fore.tsam      <-ts(c(fittedValue.tsam      ,result.tsam      ),fre=7)    
#total.fore.stlf.arima<-ts(c(fittedValue.stlf.arima,result.stlf.arima),fre=7)    
#total.fore.stlf.ets  <-ts(c(fittedValue.stlf.ets  ,result.stlf.ets  ),fre=7)    
#total.fore.tbats     <-ts(c(fittedValue.tbats     ,result.tbats     ),fre=7)    
#total.fore.tsw       <-ts(c(fittedValue.tsw       ,result.tsw       ),fre=7)    
#total.fore.fourier   <-ts(c(fittedValue.fourier   ,result.fourier   ),fre=7)    
#total.fore.sam       <-ts(c(fittedValue.sam       ,result.sam       ),fre=7)    
#total.fore.hw        <-ts(c(fittedValue.hw        ,result.hw        ),fre=7)    
#total.data.xts           <-xts(total.data,seq(as.POSIXct("2014-04-01"),len=length(total.data),by='day'))
#total.fore.xts           <-xts(total.fore,seq(as.POSIXct("2014-04-01"),len=length(total.fore),by='day'))
#total.fore.tsam.xts      <-xts(total.fore.tsam      ,seq(as.POSIXct("2014-04-01"),len=length(total.fore.tsam      ),by='day'))
#total.fore.stlf.arima.xts<-xts(total.fore.stlf.arima,seq(as.POSIXct("2014-04-01"),len=length(total.fore.stlf.arima),by='day'))
#total.fore.stlf.ets.xts  <-xts(total.fore.stlf.ets  ,seq(as.POSIXct("2014-04-01"),len=length(total.fore.stlf.ets  ),by='day'))
#total.fore.tbats.xts     <-xts(total.fore.tbats     ,seq(as.POSIXct("2014-04-01"),len=length(total.fore.tbats     ),by='day'))
#total.fore.tsw.xts       <-xts(total.fore.tsw       ,seq(as.POSIXct("2014-04-01"),len=length(total.fore.tsw       ),by='day'))
#total.fore.fourier.xts   <-xts(total.fore.fourier   ,seq(as.POSIXct("2014-04-01"),len=length(total.fore.fourier   ),by='day'))
#total.fore.sam.xts       <-xts(total.fore.sam       ,seq(as.POSIXct("2014-04-01"),len=length(total.fore.sam       ),by='day'))
#total.fore.hw.xts        <-xts(total.fore.hw        ,seq(as.POSIXct("2014-04-01"),len=length(total.fore.hw        ),by='day'))
#
#plot(as.zoo(cbind(total.data.xts,total.fore.tsam.xts,total.fore.stlf.arima.xts,total.fore.stlf.ets.xts,total.fore.tbats.xts,total.fore.tsw.xts,total.fore.fourier.xts,total.fore.sam.xts,total.fore.hw.xts,total.fore.xts)),col=1:9,lty=1:9,screens=1)
#legend(x="topleft",legend=c("observed","arima+lm","stlf.arima","stlf.ets","tbats","fourier arima","wavelet arima","seasonal arima","hw","result"),lty=1:9,col=1:9)
#
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
  as.integer(1121196856)
, as.integer(950412530 )
, as.integer(1014373911)
, as.integer(828318032 )
, as.integer(755028136 )
, as.integer(402504687 )
, as.integer(436808473 )
, as.integer(665289253 )
, as.integer(950412530 )
, as.integer(1035054357)
, as.integer(943098974 )
, as.integer(791038367 )
, as.integer(551243507 )
, as.integer(551243507 )
, as.integer(933645639 )
, as.integer(950412530 )
, as.integer(923104200 )
, as.integer(828318032 )
, as.integer(755028136 )
, as.integer(551243507 )
, as.integer(551243507 )
, as.integer(933645639 )
, as.integer(950412530 )
, as.integer(923104200 )
, as.integer(828318032 )
, as.integer(755028136 )
, as.integer(551243507 )
, as.integer(835419428 )
, as.integer(933645639 )
, as.integer(966417208 )
)
redeem= c(  
  as.integer(1199550277)
, as.integer(1029985180)
, as.integer(1374873298)
, as.integer(1026752273)
, as.integer(892834617 )
, as.integer(418304490 )
, as.integer(519397130 )
, as.integer(867186198 )
, as.integer(1126330583)
, as.integer(1118912131)
, as.integer(1026752273)
, as.integer(892834617 )
, as.integer(672824700 )
, as.integer(717319717 )
, as.integer(1199550277)
, as.integer(1126330583)
, as.integer(1118912131)
, as.integer(1026752273)
, as.integer(892834617 )
, as.integer(672824700 )
, as.integer(717319717 )
, as.integer(1199550277)
, as.integer(1126330583)
, as.integer(1118912131)
, as.integer(1026752273)
, as.integer(892834617 )
, as.integer(672824700 )
, as.integer(1212890245)
, as.integer(1235731948)
, as.integer(1262090295)
)
