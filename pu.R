library("forecast")
library("TSA")
library("xts")
library("e1071")
#library("wmtsa")
#od1<-read.csv("purchase_by_type_sum_p1_v4.csv")
od<-read.csv("purchase_by_type_sum_p2_v4.csv")
of<-read.csv("totalfs.csv")
#fitbeg<-1;#20140401
#fitbeg<-275;#20140401
#beginDate<-"2014-04-01"
fitbeg<-255;#20140312
beginDate<-"2014-03-12"
fitend<-427;#20140831
prebeg<-fitend+1;
preend<-457;#20140930
fd<-od[fitbeg:fitend,]

report_date         <-ts(fd$report_date,fre=7)
total_purchase_amt  <-ts(log(fd$total_purchase_amt ),fre=7)
izfb11                <-ts(log(fd$zfb11),fre=7)
izfb12                <-ts(log(fd$zfb12),fre=7)
izfb13                <-ts(log(fd$zfb13),fre=7)
izfb14                <-ts(log(fd$zfb14),fre=7)
izfb15                <-ts(log(fd$zfb15),fre=7)
izfb16                <-ts(log(fd$zfb16),fre=7)
izfb17                <-ts(log(fd$zfb17),fre=7)
izfb21                <-ts(log(fd$zfb21),fre=7)
izfb22                <-ts(log(fd$zfb22),fre=7)
izfb23                <-ts(log(fd$zfb23),fre=7)
izfb24                <-ts(log(fd$zfb24),fre=7)
izfb25                <-ts(log(fd$zfb25),fre=7)
izfb26                <-ts(log(fd$zfb26),fre=7)
izfb27                <-ts(log(fd$zfb27),fre=7)
izfb31                <-ts(log(fd$zfb31),fre=7)
izfb32                <-ts(log(fd$zfb32),fre=7)
izfb33                <-ts(log(fd$zfb33),fre=7)
izfb34                <-ts(log(fd$zfb34),fre=7)
izfb35                <-ts(log(fd$zfb35),fre=7)
izfb36                <-ts(log(fd$zfb36),fre=7)
izfb37                <-ts(log(fd$zfb37),fre=7)
ibank11               <-ts(log(fd$bank11),fre=7)
ibank12               <-ts(log(fd$bank12),fre=7)
ibank13               <-ts(log(fd$bank13),fre=7)
ibank14               <-ts(log(fd$bank14),fre=7)
ibank15               <-ts(log(fd$bank15),fre=7)
ibank16               <-ts(log(fd$bank16),fre=7)
ibank17               <-ts(log(fd$bank17),fre=7)
ibank21               <-ts(log(fd$bank21),fre=7)
ibank22               <-ts(log(fd$bank22),fre=7)
ibank23               <-ts(log(fd$bank23),fre=7)
ibank24               <-ts(log(fd$bank24),fre=7)
ibank25               <-ts(log(fd$bank25),fre=7)
ibank26               <-ts(log(fd$bank26),fre=7)
ibank27               <-ts(log(fd$bank27),fre=7)
ibank31               <-ts(log(fd$bank31),fre=7)
ibank32               <-ts(log(fd$bank32),fre=7)
ibank33               <-ts(log(fd$bank33),fre=7)
ibank34               <-ts(log(fd$bank34),fre=7)
ibank35               <-ts(log(fd$bank35),fre=7)
ibank36               <-ts(log(fd$bank36),fre=7)
ibank37               <-ts(log(fd$bank37),fre=7)

share               <-ts(log(fd$share),fre=7)


zfb1<-ts(log(fd$zfb11+fd$zfb12+fd$zfb13+fd$zfb14+fd$zfb15+fd$zfb16+fd$zfb17),fre=7)
zfb2<-ts(log(fd$zfb21+fd$zfb22+fd$zfb23+fd$zfb24+fd$zfb25+fd$zfb26+fd$zfb27),fre=7)
zfb3<-ts(log(fd$zfb31+fd$zfb32+fd$zfb33+fd$zfb34+fd$zfb35+fd$zfb36+fd$zfb37),fre=7)
bank1<-ts(log(fd$bank11+fd$bank12+fd$bank13+fd$bank14+fd$bank15+fd$bank16+fd$bank17),fre=7)
bank2<-ts(log(fd$bank21+fd$bank22+fd$bank23+fd$bank24+fd$bank25+fd$bank26+fd$bank27),fre=7)
bank3<-ts(log(fd$bank31+fd$bank32+fd$bank33+fd$bank34+fd$bank35+fd$bank36+fd$bank37),fre=7)

total_purchase_amtv  <-fd$total_purchase_amt
zfb1v<-fd$zfb11+fd$zfb12+fd$zfb13+fd$zfb14+fd$zfb15+fd$zfb16+fd$zfb17
zfb2v<-fd$zfb21+fd$zfb22+fd$zfb23+fd$zfb24+fd$zfb25+fd$zfb26+fd$zfb27
zfb3v<-fd$zfb31+fd$zfb32+fd$zfb33+fd$zfb34+fd$zfb35+fd$zfb36+fd$zfb37
bank1v<-fd$bank11+fd$bank12+fd$bank13+fd$bank14+fd$bank15+fd$bank16+fd$bank17
bank2v<-fd$bank21+fd$bank22+fd$bank23+fd$bank24+fd$bank25+fd$bank26+fd$bank27
bank3v<-fd$bank31+fd$bank32+fd$bank33+fd$bank34+fd$bank35+fd$bank36+fd$bank37
sharev               <-fd$share
tslistv<-list(total_purchase_amtv,zfb1v,zfb2v,zfb3v,bank1v,bank2v,bank3v,sharev)


#total_purchase_amt<-wavShrink(total_purchase_amt[fitbeg:fitend],n.level=1)
#zfb1 <-wavShrink(zfb1[fitbeg:fitend] ,n.level=1)
#zfb2 <-wavShrink(zfb2[fitbeg:fitend] ,n.level=1)
#bank1<-wavShrink(bank1[fitbeg:fitend],n.level=1)
#bank2<-wavShrink(bank2[fitbeg:fitend],n.level=1)

cat("... begin to loop ...\n")

par(mfcol=c(2,2))
tslist<-list(total_purchase_amt,zfb1,zfb2,zfb3,bank1,bank2,bank3,share)
type<-list("total_purchase_amt","zfb1","zfb2","zfb3","bank1","bank2","bank3","share")
#orignal+features
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
#middle month,quarter
#orderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
#0401
#orderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(3,0,3),c(2,1,1),c(6,1,1),c(7,0,1),c(1,1,7))
#3.12
#orderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(4,1,3),c(2,1,1),c(6,1,1),c(5,1,7),c(7,1,7))
#with time
orderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(4,1,1),c(2,1,1),c(0,0,0),c(7,0,1),c(1,1,7))
#no time
#orderlist<-list(c(0,0,0),c(0,0,0),c(4,0,4),c(3,1,1),c(1,0,1),c(1,0,3),c(1,0,1),c(1,1,1))

result.svm       <-rep(0,30)
result.tsam      <-rep(0,30)
result.stlf.arima<-rep(0,30)
result.stlf.ets  <-rep(0,30)
result.tbats     <-rep(0,30)
result.tsw       <-rep(0,30)
result.fourier   <-rep(0,30)
result.sam       <-rep(0,30)
result.hw        <-rep(0,30)
fittedValue.svm        <-rep(0,fitend-fitbeg+1)
fittedValue.tsam       <-rep(0,fitend-fitbeg+1)
fittedValue.stlf.arima <-rep(0,fitend-fitbeg+1)
fittedValue.stlf.ets   <-rep(0,fitend-fitbeg+1)
fittedValue.tbats      <-rep(0,fitend-fitbeg+1)
fittedValue.tsw        <-rep(0,fitend-fitbeg+1)
fittedValue.fourier    <-rep(0,fitend-fitbeg+1)
fittedValue.sam        <-rep(0,fitend-fitbeg+1)
fittedValue.hw         <-rep(0,fitend-fitbeg+1)
#(1)report_date, (2)w1, (3)w2, (4)w3, (5)w4, (6)w5, (7)w6, (8)mb1, (9)mb2, (10)mb3, (11)mb4, (12)mb5, (13)mb6, (14)mb7, (15)mb8, (16)mb9, (17)mb10, (18)ma1, (19)ma2, (20)ma3, (21)ma4, (22)ma5, (23)ma6, (24)ma7, (25)ma8, (26)ma9, (27)ma10, (28)mdb1, (29)mdb2, (30)mdb3, (31)mda1, (32)mda2, (33)mda3, (34)qb1, (35)qb2, (36)qb3, (37)qb4, (38)qb5, (39)qa1, (40)qa2, (41)qa3, (42)qa4, (43)qa5, (44)fb1, (45)fb2, (46)fb3, (47)fb4, (48)fb5, (49)fb6, (50)fb7, (51)fi1, (52)fi2, (53)fi3, (54)fa1, (55)fa2, (56)fa3, (57)fa4, (58)fa5, (59)fa6, (60)fa7, (61)tx, (62)sb1, (63)sb2, (64)sb3, (65)sb4, (66)sb5, (67)sb6, (68)sb7, (69)si1, (70)sa1, (71)sa2, (72)sa3, (73)sa4, (74)sa5, (75)sa6, (76)sa7, (77)cb1, (78)cb2, (79)cb3, (80)cb4, (81)cb5, (82)cb6, (83)cb7, (84)ci1, (85)ci2, (86)ci3, (87)ci4, (88)ci5, (89)ci6, (90)ci7, (91)ca1, (92)ca2, (93)ca3, (94)ca4, (95)ca5, (96)ca6, (97)ca7, (98)time, (99)sd
#5
#xregfit<-data.frame(of[fitbeg:fitend,2:7],of[fitbeg:fitend,8:43],of[fitbeg:fitend,46:58],of[fitbeg:fitend,61],of[fitbeg:fitend,64:74],of[fitbeg:fitend,98]);
#xregpre<-data.frame(of[prebeg:preend,2:7],of[prebeg:preend,8:43],of[prebeg:preend,46:58],of[prebeg:preend,61],of[prebeg:preend,64:74],of[prebeg:preend,98]);
#with time,193
xregfit<-data.frame(of[fitbeg:fitend,2:7],of[fitbeg:fitend,13:22],of[fitbeg:fitend,46:58],tx=of[fitbeg:fitend,61],of[fitbeg:fitend,64:74],time=scale(of[fitbeg:fitend,98]));
xregpre<-data.frame(of[prebeg:preend,2:7],of[prebeg:preend,13:22],of[prebeg:preend,46:58],tx=of[prebeg:preend,61],of[prebeg:preend,64:74],time=scale(of[prebeg:preend,98]));
#middle month,quarter
#xregfit<-data.frame(of[fitbeg:fitend,2:7],of[fitbeg:fitend,13:22],of[fitbeg:fitend,46:58],of[fitbeg:fitend,61],of[fitbeg:fitend,64:74],scale(of[fitbeg:fitend,98]));
#xregpre<-data.frame(of[prebeg:preend,2:7],of[prebeg:preend,13:22],of[prebeg:preend,46:58],of[prebeg:preend,61],of[prebeg:preend,64:74],scale(of[prebeg:preend,98]));
for(ti in 5:5)
{
    st<-0.05
    tsdata<-tslist[[ti]]
    or<-orderlist[[ti]]
    legend<-type[[ti]]
    cat("### svm ###\n")
    tsdatav<-tslistv[[ti]]
    tsv<-cbind(tsdatav,xregfit)
    #minRmse<-Inf;
    #for(ep in seq(0,2,0.1))
    #{
    #    for(co in 2^(2:19))
    #    {
    #        svmf<-svm(tsdatav~.,tsv,type="eps-regression",epsilon=ep,cost=co)
    #        rmse<-sqrt(mean(svmf$residuals^2))/mean(tsdatav)
    #        if(rmse<minRmse)
    #        {
    #            minRmse<-rmse
    #            bep<-ep
    #            bco<-co
    #        }
    #        #cat("----> RMSE : ",sqrt(mean(svmf$residuals^2))/mean(tsdatav),"\n")
    #    }
    #}
    #cat("best svm ",bep,bco,minRmse,"\n")

    #svmf<-svm(tsdatav~.,tsv,type="eps-regression",epsilon=bep,cost=bco)
    #svmf<-svm(tsdatav~.,tsv,type="eps-regression",cross = length(tsdatav))
    #svmp<-predict(svmf,cbind(rep(0,30),xregpre))
    #plot(as.zoo(cbind(tsdatav,fitted(svmf))),screens=1,col=1:9,lty=1:9)

    tuneResult <- tune(svm, tsdatav ~ .,  data = tsv, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:4)),tunecontrol = tune.control(cross = length(tsdatav)))
    print(tuneResult)
    plot(tuneResult)
    svmf<- tuneResult$best.model


    svmp<- predict(svmf, cbind(rep(0,30),xregpre)) 
    cat("----> RMSE : ",sqrt(mean(svmf$residuals^2))/mean(tsdatav),"\n")
    result.svm<-result.svm+svmp
    fittedValue.svm<-fittedValue.svm+fitted(svmf)
    tsam.data<-ts(c(exp(tsdata),od[prebeg:preend,2]),fre=7)               
    tsam.data.xts<-xts(tsam.data,seq(as.POSIXct(beginDate),len=length(tsam.data),by='day'))
    svm.fore     <-ts(c(fitted(svmf),svmp),fre=7)    
    svm.fore.xts     <-xts(svm.fore,seq(as.POSIXct(beginDate),len=length(svm.fore),by='day'))
    plot(as.zoo(cbind(tsam.data.xts,svm.fore.xts)),col=1:9,lty=1:9,screens=1)
    legend(x="topleft",legend=c("observed","svm"),lty=1:9,col=1:9)


    next;

    cat("###arima-lm###\n")
    aicb<-Inf;
    #vm<-list(model="sGARCH",garchOrder=c(7,6),submodel = NULL, external.regressors = NULL, variance.targeting = FALSE)
    #mm<-list(armaOrder=c(5,1),include.mean=FALSE,external.regressors=as.matrix(xregfit))
    #mm<-list(armaOrder=c(5,1))
    #spec<-ugarchspec(variance.mode=vm,mean.mode=mm,distribution.mode="norm",start.pars = list(), fixed.pars = list())
    #ts.igar=ugarchfit(data=tsdata,spec=spec,solver.control=list(trace=0))
    for(si in st)
    {
        if(si==0) next;
        if(ti==5)
        {
            tsam<-arima(tsdata,order=or,xreg=xregfit,io=c(13,20))
        }else if(ti==8)
        {
            tsam<-arima(tsdata,order=or,xreg=xregfit,io=c(86))
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
                if(ti==5)
                {
                    tsam<-arima(tsdata,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE,io=c(13,20))
                }else if(ti==8)
                {
                    tsam<-arima(tsdata,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE,io=c(86))
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
    if(ti==5)
    {
        tsam.bestfit<-arima(tsdata,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE,io=c(13,20));
        fittedValueTmp<-fitted(tsam.bestfit)
        print(tsam.bestfit);
        tsam.bestfit$coef<-tsam.bestfit$coef[1:(length(tsam.bestfit$coef)-2)]
    }else if(ti==8)
    {
        tsam.bestfit<-arima(tsdata,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE,io=c(86));
        fittedValueTmp<-fitted(tsam.bestfit)
        print(tsam.bestfit);
        tsam.bestfit$coef<-tsam.bestfit$coef[1:(length(tsam.bestfit$coef)-1)]
    }else
    {
        tsam.bestfit<-arima(tsdata,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE);
        fittedValueTmp<-fitted(tsam.bestfit)
        print(tsam.bestfit);
    }
        #acf(residuals(tsam.bestfit),lag=60)
        #pacf(residuals(tsam.bestfit),lag=60)
        #acf(diff(diff(residuals(tsam.bestfit))),lag=60) 
        #pacf(diff(diff(residuals(tsam.bestfit))),lag=60)
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

    #obtain month/holiday features from arima.
    coefs <- tsam.bestfit$coef
    narma<-sum(tsam.bestfit$arma[1:4])
    if(names(coefs)[narma + 1L] == "intercept") narma<-narma+1
    shift<-coefs[-(1:narma)]
#(1)report_date, (2)w1, (3)w2, (4)w3, (5)w4, (6)w5, (7)w6, (8)mb1, (9)mb2, (10)mb3, (11)mb4, (12)mb5, (13)mb6, (14)mb7, (15)mb8, (16)mb9, (17)mb10, (18)ma1, (19)ma2, (20)ma3, (21)ma4, (22)ma5, (23)ma6, (24)ma7, (25)ma8, (26)ma9, (27)ma10, (28)mdb1, (29)mdb2, (30)mdb3, (31)mda1, (32)mda2, (33)mda3, (34)qb1, (35)qb2, (36)qb3, (37)qb4, (38)qb5, (39)qa1, (40)qa2, (41)qa3, (42)qa4, (43)qa5, (44)fb1, (45)fb2, (46)fb3, (47)fb4, (48)fb5, (49)fb6, (50)fb7, (51)fi1, (52)fi2, (53)fi3, (54)fa1, (55)fa2, (56)fa3, (57)fa4, (58)fa5, (59)fa6, (60)fa7, (61)tx, (62)sb1, (63)sb2, (64)sb3, (65)sb4, (66)sb5, (67)sb6, (68)sb7, (69)si1, (70)sa1, (71)sa2, (72)sa3, (73)sa4, (74)sa5, (75)sa6, (76)sa7, (77)cb1, (78)cb2, (79)cb3, (80)cb4, (81)cb5, (82)cb6, (83)cb7, (84)ci1, (85)ci2, (86)ci3, (87)ci4, (88)ci5, (89)ci6, (90)ci7, (91)ca1, (92)ca2, (93)ca3, (94)ca4, (95)ca5, (96)ca6, (97)ca7, (98)time, (99)sd
    cat("###holidayshift###\n")
    holidayshift<-rep(1,30)
#xregfit<-data.frame(of[fitbeg:fitend,2:7],of[fitbeg:fitend,13:22],of[fitbeg:fitend,46:58],of[fitbeg:fitend,61],of[fitbeg:fitend,64:74],scale(of[fitbeg:fitend,98]));
    #for(fi in 43:56)
    for(fi in 17:30)
    {
        holidayshift<-holidayshift*exp(xregpre[,fi]*shift[fi])
    }
    print(holidayshift)
    cat("###monthshift###\n")
    monthshift<-rep(1,30)
    #for(fi in 7:42)
    for(fi in 7:16)
    {
        monthshift<-monthshift*exp(xregpre[,fi]*shift[fi])
    }
    print(monthshift)

    #cat("###stlf.ets###\n")
    #tsst.ets<-stlf(tsdata,h=30,s.window=7,method='ets',ic='bic',opt.crit='mae')
    #result.stlf.ets<-result.stlf.ets+exp(tsst.ets$mean)*holidayshift*monthshift
    #fittedValue.stlf.ets<-fittedValue.stlf.ets+exp(fitted(tsst.ets))
    #cat("###stlf.arima###\n")
    #tsst.arima<-stlf(tsdata,h=30,s.window=7,method='arima',ic='bic',xreg=xregfit,newxreg=xregpre)
    #result.stlf.arima<-result.stlf.arima+exp(tsst.arima$mean)
    #fittedValue.stlf.arima<-fittedValue.stlf.arima+exp(fitted(tsst.arima))
    #cat("###tbats###\n")
    #tsm1<-msts(tsdata,seasonal.periods=c(7,30.44,91.31),ts.frequency=7)
    #tbats<-tbats(tsm1)
    #tbatsp<-forecast(tbats,30)
    #result.tbats<-result.tbats+exp(tbatsp$mean)*holidayshift
    #fittedValue.tbats<-fittedValue.tbats+exp(fitted(tbats))

    #cat("###fourier###\n")
    #ff<-od[fitbeg:preend,2]
    #d7<-ts(ff,fre=7)
    #d30<-ts(ff,fre=30.44)
    #d90<-ts(ff,fre=91.31)
    ##tsfo<-arimax(tsdata,order=c(5,0,5),xreg=data.frame(fourier(d7,K=3)[1:(fitend-fitbeg+1)],fourier(d30,K=15)[1:(fitend-fitbeg+1)]));
    #tsfo<-auto.arima(tsdata,xreg=data.frame(fourier(d7,K=3)[1:(fitend-fitbeg+1)],fourier(d30,K=2)[1:(fitend-fitbeg+1)],fourier(d90,K=2)[1:(fitend-fitbeg+1)]));
    #tsfo.p<-predict(tsfo,newxreg = data.frame(fourier(d7,K=3)[(prebeg-fitbeg+1):(preend-fitbeg+1)],fourier(d30,K=2)[(prebeg-fitbeg+1):(preend-fitbeg+1)],fourier(d90,K=2)[(prebeg-fitbeg+1):(preend-fitbeg+1)]));
    #tsfop<-exp(tsfo.p$pred)*holidayshift;
    #result.fourier<-result.fourier+tsfop;
    #fittedValue.fourier<-fittedValue.fourier+exp(tsdata-residuals(tsfo));
    #cat("###wavelet###\n")
    #nl<-3
    #tsw3<-wavMODWT(tsdata,n.level=nl)
    #tswp<-rep(0,30)
    #to<-rep(0,153)
    #tsw31<-wavMRD(tsw3)
    #tfi<-rep(0,153)
    #wor<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,1,0))
    #for(tswi in 1:(nl+1))
    #{
        #tsw<-arima(tsw31[[tswi]],order=wor[[tswi]],xreg=xregfit)
        #to<-to+tsw31[[tswi]]
        #tfi<-tfi+tsw31[[tswi]]-residuals(tsw)
        #tsw.p<-predict(tsw,newxreg=xregpre)
        #tswp<-tswp+tsw.p$pred
        ##tsw.datat.xts<-xts(c(tsw31[[tswi]],rep(NA,30)),seq(as.POSIXct(beginDate),len=183,by='day'))
        ##tsw.foret<-ts(c((tsw31[[tswi]]-residuals(tsw)),tsw.p$pred),fre=7)    
        ##tsw.foret.xts<-xts(tsw.foret,seq(as.POSIXct(beginDate),len=length(tsw.foret),by='day'))
        ##plot(as.zoo(cbind(tsw.datat.xts,tsw.foret.xts)),col=1:3,lty=1:3,screens=1)
    #}
    #result.tsw<-result.tsw+exp(tswp)*holidayshift
    #fittedValue.tsw<-fittedValue.tsw+exp(tfi)

#    #cat("#seasonal arima + holiday shift \n")
#    #acf(tsdata)
#    #pacf(tsdata)
#    #acf(diff(tsdata))
#    #pacf(diff(tsdata))
#    #acf(diff(tsdata,lag=7))
#    #pacf(diff(tsdata,lag=7))
#    #next
#    #no features
#    #nor<-list(c(0,0,0),c(0,0,0),c(1,0,1),c(0,0,0),c(1,0,2),c(1,0,1),c(4,0,5),c(1,1,1))
#    #sor<-list(c(0,0,0),c(2,1,1),c(2,1,1),c(1,1,1),c(2,1,1),c(2,1,1),c(2,1,2),c(1,0,1))
#    #with features
#    #nor<-list(c(0,0,0),c(0,0,0),c(1,0,1),c(0,0,0),c(3,0,3),c(6,0,1),c(1,0,5),c(4,1,1))
#    #sor<-list(c(0,0,0),c(0,0,0),c(3,1,1),c(4,1,1),c(2,1,1),c(3,1,1),c(4,1,2),c(2,0,2))
#    nor<-list(c(0,0,0),c(0,0,0),c(1,0,1),c(0,0,0),c(3,0,3),c(6,0,1),c(1,0,5),c(5,0,3))
#    sor<-list(c(0,0,0),c(0,0,0),c(3,1,1),c(4,1,1),c(2,1,1),c(3,1,1),c(4,1,2),c(3,1,2))
#    #if(ti==2 || ti==3)
#    #{
#        #tssam<-arima(tsdata,order=nor[[ti]],seasonal=list(order=sor[[ti]],period=7),io=c(50))
#    #}else if(ti==4)
#    #{
#        #tssam<-arima(tsdata,order=nor[[ti]],seasonal=list(order=sor[[ti]],period=7),io=c(34))
#    #}else
#    #{
#        tssam<-arima(tsdata,order=nor[[ti]],seasonal=list(order=sor[[ti]],period=7),xreg=xregfit[,7:NCOL(xregfit)])
#    #}
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
#            #if(ti==2||ti==3)
#            #{
#                #tssam<-arima(tsdata,order=nor[[ti]],seasonal=list(order=sor[[ti]],period=7),fixed=fixedv2,transform.pars = FALSE,io=c(50))
#            #}else if(ti==4)
#            #{
#                #tssam<-arima(tsdata,order=nor[[ti]],seasonal=list(order=sor[[ti]],period=7),fixed=fixedv2,transform.pars = FALSE,io=c(34))
#            #}else
#            #{
#                tssam<-arima(tsdata,order=nor[[ti]],seasonal=list(order=sor[[ti]],period=7),xreg=xregfit[,7:NCOL(xregfit)],fixed=fixedv2,transform.pars = FALSE)
#            #}
#        }
#    }
#    #if(ti==2 || ti==3 | ti==4)
#    #{
#        #tssam$coef<-tssam$coef[1:(length(tssam$coef)-1)]
#    #}
#    #tssam.p<-predict(tssam,30);
#    tssam.p<-predict(tssam,newxreg=xregpre[,7:NCOL(xregpre)]);
#    tssamp<-tssam.p$pred;
#    print(tssam)
#        #acf(residuals(tssam),lag=60)
#        #pacf(residuals(tssam),lag=60)
#        #acf(diff(residuals(tssam)),lag=60)
#        #pacf(diff(residuals(tssam)),lag=60)
#        #acf(diff(residuals(tssam),lag=7),lag=60)
#        #pacf(diff(residuals(tssam),lag=7),lag=60)
#        #acf(diff(diff(residuals(tssam),lag=7)),lag=60)
#        #pacf(diff(diff(residuals(tssam),lag=7)),lag=60)
#    detectAO(tssam);
#    detectIO(tssam);
#    #result.sam<-result.sam+exp(tssamp)*holidayshift*monthshift
#    #fittedValue.sam<-fittedValue.sam+exp(fitted(tssam))

    #cat("###hw###\n")
    #tshw<-hw(exp(tsdata),h=30,seasonal='multiplicative',damped=TRUE)
    #result.hw<-result.hw+tshw$mean*holidayshift*monthshift
    #fittedValue.hw<-fittedValue.hw+fitted(tshw)

#   tsam.data<-ts(c(exp(tsdata),od[prebeg:preend,2]),fre=7)               
#   tsam.data.xts<-xts(tsam.data,seq(as.POSIXct(beginDate),len=length(tsam.data),by='day'))
#
#   tsam.fore      <-ts(exp(c(fittedValueTmp,tsamp)),fre=7)    
#    stlf.arima.fore<-ts(c(exp(fitted(tsst.arima)),exp(tsst.arima$mean)),fre=7)    
#    stlf.ets.fore  <-ts(c(exp(fitted(tsst.ets)),exp(tsst.ets$mean)*holidayshift*monthshift),fre=7)    
#    #tbats.fore     <-ts(c(exp(fitted(tbats)),exp(tbatsp$mean)*holidayshift),fre=7)    
#    #tsfo.fore      <-ts(c(exp(tsdata-residuals(tsfo)),tsfop),fre=7)    
#    #tsw.fore       <-ts(c(exp(tfi),exp(tswp)*holidayshift),fre=7)    
#    #tssam.fore     <-ts(c(exp(fitted(tssam)),exp(tssamp)*holidayshift*monthshift),fre=7)    
#    tshw.fore      <-ts(c(fitted(tshw),tshw$mean*holidayshift*monthshift),fre=7)    
#   tsam.fore.xts      <-xts(tsam.fore,seq(as.POSIXct(beginDate),len=length(tsam.fore),by='day'))
#    stlf.arima.fore.xts<-xts(stlf.arima.fore,seq(as.POSIXct(beginDate),len=length(stlf.arima.fore),by='day'))
#    stlf.ets.fore.xts  <-xts(stlf.ets.fore,seq(as.POSIXct(beginDate),len=length(stlf.ets.fore),by='day'))
#    #tbats.fore.xts     <-xts(tbats.fore,seq(as.POSIXct(beginDate),len=length(tbats.fore),by='day'))
#    #tsfo.fore.xts      <-xts(tsfo.fore,seq(as.POSIXct(beginDate),len=length(tsfo.fore),by='day'))
#    #tsw.fore.xts       <-xts(tsw.fore,seq(as.POSIXct(beginDate),len=length(tsw.fore),by='day'))
#    #tssam.fore.xts     <-xts(tssam.fore,seq(as.POSIXct(beginDate),len=length(tssam.fore),by='day'))
#    tshw.fore.xts      <-xts(tshw.fore,seq(as.POSIXct(beginDate),len=length(tshw.fore),by='day'))
#    #plot(as.zoo(cbind(tsam.data.xts,tsam.fore.xts,stlf.arima.fore.xts,stlf.ets.fore.xts,tbats.fore.xts,tsfo.fore.xts,tsw.fore.xts,tssam.fore.xts,tshw.fore.xts)),col=1:9,lty=1:9,screens=1)
#    #legend(x="topleft",legend=c("observed","arima+lm","stlf.arima","stlf.ets","tbats","fourier arima","wavelet arima","seasonal arima","hw"),lty=1:9,col=1:9)
#    #plot(as.zoo(cbind(tsam.data.xts,tsam.fore.xts,stlf.arima.fore.xts,stlf.ets.fore.xts,tbats.fore.xts,tshw.fore.xts)),col=c(1:6,8,9),lty=1:9,screens=1)
#    #legend(x="topleft",legend=c("observed","arima+lm","stlf.arima","stlf.ets","tbats","hw"),lty=1:9,col=1:9)
#    #plot(as.zoo(cbind(tsam.data.xts,tsam.fore.xts)),col=1:9,lty=1:9,screens=1)
#    #legend(x="topleft",legend=c("observed","arima+lm"),lty=1:9,col=1:9)
}

#totalFittedValue<-( ts(fittedValue.tsam,fre=7)
#                   +ts(fittedValue.stlf.arima,fre=7)
#                   +ts(fittedValue.stlf.ets  ,fre=7)
#                   #+ts(fittedValue.tbats     ,fre=7)
##                   +ts(fittedValue.tsw       ,fre=7)
##                   +ts(fittedValue.fourier   ,fre=7)
#                   #+ts(fittedValue.sam       ,fre=7)
#                   +ts(fittedValue.hw        ,fre=7)
#                   )/4
#totalResult<-( ts(result.tsam,fre=7)
#              +ts(result.stlf.arima,fre=7)
#              +ts(result.stlf.ets  ,fre=7)
#              #+ts(result.tbats     ,fre=7)
##              +ts(result.tsw       ,fre=7)
##              +ts(result.fourier   ,fre=7)
#              #+ts(result.sam       ,fre=7)
#              +ts(result.hw        ,fre=7)
#              )/4
##
##cat(">>>> Purchase <<<<\n")
#print(totalResult)
##
#total.data           <-ts(c(od[fitbeg:preend,2]),fre=7)               
#total.fore           <-ts(c(totalFittedValue,totalResult),fre=7)    
#total.fore.tsam      <-ts(c(fittedValue.tsam      ,result.tsam      ),fre=7)    
#total.fore.stlf.arima<-ts(c(fittedValue.stlf.arima,result.stlf.arima),fre=7)    
#total.fore.stlf.ets  <-ts(c(fittedValue.stlf.ets  ,result.stlf.ets  ),fre=7)    
##total.fore.tbats     <-ts(c(fittedValue.tbats     ,result.tbats     ),fre=7)    
##total.fore.fourier   <-ts(c(fittedValue.fourier   ,result.fourier   ),fre=7)    
##total.fore.tsw       <-ts(c(fittedValue.tsw       ,result.tsw       ),fre=7)    
##total.fore.sam       <-ts(c(fittedValue.sam       ,result.sam       ),fre=7)    
#total.fore.hw        <-ts(c(fittedValue.hw        ,result.hw        ),fre=7)    
#total.data.xts           <-xts(total.data,seq(as.POSIXct(beginDate),len=length(total.data),by='day'))
#total.fore.xts           <-xts(total.fore,seq(as.POSIXct(beginDate),len=length(total.fore),by='day'))
#total.fore.tsam.xts      <-xts(total.fore.tsam      ,seq(as.POSIXct(beginDate),len=length(total.fore.tsam      ),by='day'))
#total.fore.stlf.arima.xts<-xts(total.fore.stlf.arima,seq(as.POSIXct(beginDate),len=length(total.fore.stlf.arima),by='day'))
#total.fore.stlf.ets.xts  <-xts(total.fore.stlf.ets  ,seq(as.POSIXct(beginDate),len=length(total.fore.stlf.ets  ),by='day'))
##total.fore.tbats.xts     <-xts(total.fore.tbats     ,seq(as.POSIXct(beginDate),len=length(total.fore.tbats     ),by='day'))
##total.fore.fourier.xts   <-xts(total.fore.fourier   ,seq(as.POSIXct(beginDate),len=length(total.fore.fourier   ),by='day'))
##total.fore.tsw.xts       <-xts(total.fore.tsw       ,seq(as.POSIXct(beginDate),len=length(total.fore.tsw       ),by='day'))
##total.fore.sam.xts       <-xts(total.fore.sam       ,seq(as.POSIXct(beginDate),len=length(total.fore.sam       ),by='day'))
#total.fore.hw.xts        <-xts(total.fore.hw        ,seq(as.POSIXct(beginDate),len=length(total.fore.hw        ),by='day'))
#
##plot(as.zoo(cbind(total.data.xts,total.fore.tsam.xts,total.fore.stlf.arima.xts,total.fore.stlf.ets.xts,total.fore.tbats.xts,total.fore.tsw.xts,total.fore.fourier.xts,total.fore.sam.xts,total.fore.hw.xts,total.fore.xts)),col=1:9,lty=1:9,screens=1)
##legend(x="topleft",legend=c("observed","arima+lm","stlf.arima","stlf.ets","tbats","fourier arima","wavelet arima","seasonal arima","hw","result"),lty=1:9,col=1:9)
#plot(as.zoo(cbind(total.data.xts,total.fore.tsam.xts,total.fore.stlf.arima.xts,total.fore.stlf.ets.xts,total.fore.hw.xts,total.fore.xts)),col=c(1:6,8,9),lty=1:9,screens=1)
#legend(x="topleft",legend=c("observed","arima+lm","stlf.arima","stlf.ets","hw","result"),lty=1:9,col=1:9)
##plot(as.zoo(cbind(total.data.xts,total.fore.tsam.xts)),col=1:9,lty=1:9,screens=1)
##legend(x="topleft",legend=c("observed","arima+lm"),lty=1:9,col=1:9)
