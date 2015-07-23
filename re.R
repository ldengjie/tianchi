library("forecast")
library("TSA")
library("xts")
library("e1071")
#library("rugarch")
#od1<-read.csv("redeem_by_type_sum_p1_v4.csv")
od<-read.csv("redeem_by_type_sum_p2_v4.csv")
#od<-od1+od2
of<-read.csv("totalfs.csv")
fitbeg<-275;#20140401
beginDate<-"2014-04-01"
#fitbeg<-255;#20140312
#beginDate<-"2014-03-12"
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
icard11               <-ts(log(fd$card11),fre=7)
icard12               <-ts(log(fd$card12),fre=7)
icard13               <-ts(log(fd$card13),fre=7)
icard14               <-ts(log(fd$card14),fre=7)
icard15               <-ts(log(fd$card15),fre=7)
icard16               <-ts(log(fd$card16),fre=7)
icard17               <-ts(log(fd$card17),fre=7)
icard21               <-ts(log(fd$card21),fre=7)
icard22               <-ts(log(fd$card22),fre=7)
icard23               <-ts(log(fd$card23),fre=7)
icard24               <-ts(log(fd$card24),fre=7)
icard25               <-ts(log(fd$card25),fre=7)
icard26               <-ts(log(fd$card26),fre=7)
icard27               <-ts(log(fd$card27),fre=7)
icard31               <-ts(log(fd$card31),fre=7)
icard32               <-ts(log(fd$card32),fre=7)
icard33               <-ts(log(fd$card33),fre=7)
icard34               <-ts(log(fd$card34),fre=7)
icard35               <-ts(log(fd$card35),fre=7)
icard36               <-ts(log(fd$card36),fre=7)
icard37               <-ts(log(fd$card37),fre=7)

#plot(as.zoo(cbind(
#xts(exp(izfb11)+exp(izfb12)+exp(izfb13)+exp(izfb14)+exp(izfb15)+exp(izfb16)+exp(izfb17),seq(as.POSIXct(beginDate),len=length(izfb11),by='day')),
#xts(exp(izfb11),seq(as.POSIXct(beginDate),len=length(izfb11),by='day')),
#xts(exp(izfb12),seq(as.POSIXct(beginDate),len=length(izfb12),by='day')),
#xts(exp(izfb13),seq(as.POSIXct(beginDate),len=length(izfb13),by='day')),
#xts(exp(izfb14),seq(as.POSIXct(beginDate),len=length(izfb14),by='day')),
#xts(exp(izfb15),seq(as.POSIXct(beginDate),len=length(izfb15),by='day')),
#xts(exp(izfb16),seq(as.POSIXct(beginDate),len=length(izfb16),by='day')),
#xts(exp(izfb17),seq(as.POSIXct(beginDate),len=length(izfb17),by='day')),
#xts(exp(izfb21)+exp(izfb22)+exp(izfb23)+exp(izfb24)+exp(izfb25)+exp(izfb26)+exp(izfb27),seq(as.POSIXct(beginDate),len=length(izfb21),by='day')),
#xts(exp(izfb21),seq(as.POSIXct(beginDate),len=length(izfb21),by='day')),
#xts(exp(izfb22),seq(as.POSIXct(beginDate),len=length(izfb22),by='day')),
#xts(exp(izfb23),seq(as.POSIXct(beginDate),len=length(izfb23),by='day')),
#xts(exp(izfb24),seq(as.POSIXct(beginDate),len=length(izfb24),by='day')),
#xts(exp(izfb25),seq(as.POSIXct(beginDate),len=length(izfb25),by='day')),
#xts(exp(izfb26),seq(as.POSIXct(beginDate),len=length(izfb26),by='day')),
#xts(exp(izfb27),seq(as.POSIXct(beginDate),len=length(izfb27),by='day'))
#)),col=c(1:6,8,9),lty=1:8,main="redeem_zfb")
#plot(as.zoo(cbind(
#xts(exp(icard11)+exp(icard12)+exp(icard13)+exp(icard14)+exp(icard15)+exp(icard16)+exp(icard17),seq(as.POSIXct(beginDate),len=length(icard11),by='day')),
#xts(exp(icard11),seq(as.POSIXct(beginDate),len=length(icard11),by='day')),
#xts(exp(icard12),seq(as.POSIXct(beginDate),len=length(icard12),by='day')),
#xts(exp(icard13),seq(as.POSIXct(beginDate),len=length(icard13),by='day')),
#xts(exp(icard14),seq(as.POSIXct(beginDate),len=length(icard14),by='day')),
#xts(exp(icard15),seq(as.POSIXct(beginDate),len=length(icard15),by='day')),
#xts(exp(icard16),seq(as.POSIXct(beginDate),len=length(icard16),by='day')),
#xts(exp(icard17),seq(as.POSIXct(beginDate),len=length(icard17),by='day')),
#xts(exp(icard21)+exp(icard22)+exp(icard23)+exp(icard24)+exp(icard25)+exp(icard26)+exp(icard27),seq(as.POSIXct(beginDate),len=length(icard21),by='day')),
#xts(exp(icard21),seq(as.POSIXct(beginDate),len=length(icard21),by='day')),
#xts(exp(icard22),seq(as.POSIXct(beginDate),len=length(icard22),by='day')),
#xts(exp(icard23),seq(as.POSIXct(beginDate),len=length(icard23),by='day')),
#xts(exp(icard24),seq(as.POSIXct(beginDate),len=length(icard24),by='day')),
#xts(exp(icard25),seq(as.POSIXct(beginDate),len=length(icard25),by='day')),
#xts(exp(icard26),seq(as.POSIXct(beginDate),len=length(icard26),by='day')),
#xts(exp(icard27),seq(as.POSIXct(beginDate),len=length(icard27),by='day')),
#xts(exp(icard31)+exp(icard32)+exp(icard33)+exp(icard34)+exp(icard35)+exp(icard36)+exp(icard37),seq(as.POSIXct(beginDate),len=length(icard31),by='day')),
#xts(exp(icard31),seq(as.POSIXct(beginDate),len=length(icard31),by='day')),
#xts(exp(icard32),seq(as.POSIXct(beginDate),len=length(icard32),by='day')),
#xts(exp(icard33),seq(as.POSIXct(beginDate),len=length(icard33),by='day')),
#xts(exp(icard34),seq(as.POSIXct(beginDate),len=length(icard34),by='day')),
#xts(exp(icard35),seq(as.POSIXct(beginDate),len=length(icard35),by='day')),
#xts(exp(icard36),seq(as.POSIXct(beginDate),len=length(icard36),by='day')),
#xts(exp(icard37),seq(as.POSIXct(beginDate),len=length(icard37),by='day'))
#)),col=c(1:6,8,9),lty=1:8,main="redeem_card")


consume<-ts(log(fd$category1+fd$category2+fd$category3+fd$category4),fre=7)
zfb1<-ts(log(fd$zfb11+fd$zfb12+fd$zfb13+fd$zfb14+fd$zfb15+fd$zfb16+fd$zfb17),fre=7)
zfb2<-ts(log(fd$zfb21+fd$zfb22+fd$zfb23+fd$zfb24+fd$zfb25+fd$zfb26+fd$zfb27),fre=7)
card1<-ts(log(fd$card11+fd$card12+fd$card13+fd$card14+fd$card15+fd$card16+fd$card17),fre=7)
card2<-ts(log(fd$card21+fd$card22+fd$card23+fd$card24+fd$card25+fd$card26+fd$card27),fre=7)
card3<-ts(log(fd$card31+fd$card32+fd$card33+fd$card34+fd$card35+fd$card36+fd$card37),fre=7)


total_redeem_amtv <-fd$total_redeem_amt
consumev<-fd$category1+fd$category2+fd$category3+fd$category4
zfb1v<-fd$zfb11+fd$zfb12+fd$zfb13+fd$zfb14+fd$zfb15+fd$zfb16+fd$zfb17
zfb2v<-fd$zfb21+fd$zfb22+fd$zfb23+fd$zfb24+fd$zfb25+fd$zfb26+fd$zfb27
card1v<-fd$card11+fd$card12+fd$card13+fd$card14+fd$card15+fd$card16+fd$card17
card2v<-fd$card21+fd$card22+fd$card23+fd$card24+fd$card25+fd$card26+fd$card27
card3v<-fd$card31+fd$card32+fd$card33+fd$card34+fd$card35+fd$card36+fd$card37
tslistv<-list(total_redeem_amtv,zfb1v,zfb2v,consumev,card1v,card2v,card3v)

#total_redeem_amt<-wavShrink(total_redeem_amt[1:fitend],n.level=1)
#zfb1<-wavShrink(zfb1[1:fitend],n.level=1)
#card1      <-wavShrink(card1[1:fitend]      ,n.level=1)
#card2      <-wavShrink(card2[1:fitend]      ,n.level=1)

cat("... begin to loop ...\n")

#xregpre<-rep(1,30);
#total_redeem_amt <-ts(total_redeem_amt ,fre=7)
#zfb1             <-ts(zfb1             ,fre=7)
#card1            <-ts(card1            ,fre=7)
#card2            <-ts(card2            ,fre=7)


##par(mfcol=c(2,2))
par(mfcol=c(4,2))
type<-list("total_redeem_amt","zfb1","zfb2","consume","card1","card2","card3")
tslist<-list(total_redeem_amt,zfb1,zfb2,consume,card1,card2,card3)
#orignal+features
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
#orderlist<-list(c(0,0,0),c(4,0,1),c(4,1,1),c(2,0,3),c(6,0,5),c(2,1,1),c(4,0,4))
#with time
orderlist<-list(c(0,0,0),c(1,0,5),c(3,1,1),c(1,0,4),c(4,1,4),c(1,0,5),c(3,1,1))
#no time
#orderlist<-list(c(0,0,0),c(4,1,1),c(4,1,1),c(7,1,1),c(2,1,1),c(2,1,1),c(2,1,1))
#no time p1 p2
#orderlist<-list(c(0,0,0),c(4,1,1),c(5,1,1),c(2,1,1),c(2,1,1),c(2,1,2),c(2,1,1))

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
#5
#xregfit<-data.frame(of[fitbeg:fitend,2:7],of[fitbeg:fitend,8:43],of[fitbeg:fitend,46:58],of[fitbeg:fitend,61],of[fitbeg:fitend,64:74],of[fitbeg:fitend,98]);
#xregpre<-data.frame(of[prebeg:preend,2:7],of[prebeg:preend,8:43],of[prebeg:preend,46:58],of[prebeg:preend,61],of[prebeg:preend,64:74],of[prebeg:preend,98]);
xregfit<-data.frame(of[fitbeg:fitend,2:7],of[fitbeg:fitend,13:22],of[fitbeg:fitend,46:58],tx=of[fitbeg:fitend,61],of[fitbeg:fitend,64:74],time=scale(of[fitbeg:fitend,98]));
xregpre<-data.frame(of[prebeg:preend,2:7],of[prebeg:preend,13:22],of[prebeg:preend,46:58],tx=of[prebeg:preend,61],of[prebeg:preend,64:74],time=scale(of[prebeg:preend,98]));
#xregfit<-data.frame(of[fitbeg:fitend,2:17],of[fitbeg:fitend,20:32],of[fitbeg:fitend,35],of[fitbeg:fitend,38:48],scale(of[fitbeg:fitend,72]));
#xregpre<-data.frame(of[prebeg:preend,2:17],of[prebeg:preend,20:32],of[prebeg:preend,35],of[prebeg:preend,38:48],scale(of[prebeg:preend,72]));
#xregfit<-data.frame(of[fitbeg:fitend,2:17],of[fitbeg:fitend,20:32],of[fitbeg:fitend,35],of[fitbeg:fitend,38:48]);
#xregpre<-data.frame(of[prebeg:preend,2:17],of[prebeg:preend,20:32],of[prebeg:preend,35],of[prebeg:preend,38:48]);

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
for(ti in 7:7)
{
    #st<-seq(0,0.4,0.05);
    st<-0.05
    tsdata<-tslist[[ti]]
    or<-orderlist[[ti]]
    sor<-sorderlist[[ti]]
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

    bep<-c(0.1,0.3,0,0.7,0.8,0.6,0.5)
    bco<-c(1,64,64,8,512,512,32)
    svmf<-svm(tsdatav~.,tsv,type="eps-regression",cross = length(tsdatav),epsilon=bep[[ti]],cost=bco[[ti]])
    #plot(as.zoo(cbind(tsdatav,fitted(svmf))),screens=1,col=1:9,lty=1:9)

    #tuneResult <- tune(svm, tsdatav ~ .,  data = tsv, ranges = list(epsilon = seq(0.,1,0.1), cost = 2^(2:9)),tunecontrol = tune.control(cross = length(tsdatav)))
    #print(tuneResult)
    #plot(tuneResult)
    #svmf<- tuneResult$best.model


    svmp<- predict(svmf,cbind(rep(0,30),xregpre)) 
    cat("----> RMSE : ",sqrt(mean(svmf$residuals^2))/mean(tsdatav),"\n")
    result.svm<-result.svm+svmp
    fittedValue.svm<-fittedValue.svm+fitted(svmf)
    #tsam.data<-ts(c(exp(tsdata),od[prebeg:preend,2]),fre=7)               
    #tsam.data.xts<-xts(tsam.data,seq(as.POSIXct(beginDate),len=length(tsam.data),by='day'))
    svm.fore     <-ts(c(fitted(svmf),svmp),fre=7)    
    svm.fore.xts     <-xts(svm.fore,seq(as.POSIXct(beginDate),len=length(svm.fore),by='day'))
    #plot(as.zoo(cbind(tsam.data.xts,svm.fore.xts)),col=1:9,lty=1:9,screens=1)
    #legend(x="topleft",legend=c("observed","svm"),lty=1:9,col=1:9)


    cat("### arima+lm ###\n")
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
        #if(ti==4)
        #{
            #tsam<-arima(tsdata,order=or,xreg=xregfit,io=c(52))
            #tsam<-arima(tsdata,order=or,xreg=data.frame(xregfit,1*(seq(tsdata)==52)))
        #}else
        #{
            tsam<-arima(tsdata,order=or,xreg=xregfit)
        #}
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
                #if(ti==4)
                #{
                    ##tsam<-arima(tsdata,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE,io=c(52))
                    #tsam<-arima(tsdata,order=or,xreg=data.frame(xregfit,1*(seq(tsdata)==52)),fixed=fixedv,transform.pars = FALSE)
                #}else
                #{
                    tsam<-arima(tsdata,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
                #}
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
        ##tsam.bestfit<-arima(tsdata,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE,io=c(52));
        #tsam.bestfit<-arima(tsdata,order=or,xreg=data.frame(xregfit,1*(seq(tsdata)==52)),fixed=fixb,transform.pars = FALSE);
        #fittedValueTmp<-fitted(tsam.bestfit)
        #print(tsam.bestfit);
        ##tsam.bestfit$coef<-tsam.bestfit$coef[1:(length(tsam.bestfit$coef)-1)]
    #}else
    #{
        tsam.bestfit<-arima(tsdata,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE);
        fittedValueTmp<-fitted(tsam.bestfit)
        print(tsam.bestfit);
    #}
    detectAO(tsam.bestfit);
    detectIO(tsam.bestfit);
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
    #if(ti==4)
    #{
        #tsam.p<-predict(tsam.bestfit,newxreg =data.frame(xregpre,rep(0,30)));
    #}else
    #{
        tsam.p<-predict(tsam.bestfit,newxreg =xregpre);
    #}
    tsamp<-tsam.p$pred;
    print(exp(tsamp))
    result.tsam<-result.tsam+exp(tsamp)
    fittedValue.tsam<-fittedValue.tsam+exp(fittedValueTmp)
        #acf(residuals(tsam.bestfit),lag=60)
        #pacf(residuals(tsam.bestfit),lag=60)
        #acf(diff(residuals(tsam.bestfit)),lag=60) 
        #pacf(diff(residuals(tsam.bestfit)),lag=60)
        #acf(abs(residuals(tsam.bestfit)),lag=60)
        #pacf(abs(residuals(tsam.bestfit)),lag=60)
        #acf(residuals(tsam.bestfit)^2,lag=60)
        #pacf(residuals(tsam.bestfit)^2,lag=60)
        #acf(diff(residuals(tsam.bestfit),lag=7),lag=60)
        #pacf(diff(residuals(tsam.bestfit),lag=7),lag=60)

    #obtain month/holiday features from arima.
    coefs <- tsam.bestfit$coef
    narma<-sum(tsam.bestfit$arma[1:4])
    if(names(coefs)[narma + 1L] == "intercept") narma<-narma+1
    shift<-coefs[-(1:narma)]
    cat("###holidayshift###\n")
    holidayshift<-rep(1,30)
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

    #cat("###hw###\n")
    #tshw<-hw(exp(tsdata),h=30,seasonal='multiplicative',damped=TRUE)
    #result.hw<-result.hw+tshw$mean*holidayshift*monthshift
    #fittedValue.hw<-fittedValue.hw+fitted(tshw)

   tsam.data<-ts(c(exp(tsdata),od[prebeg:preend,2]),fre=7)               
   tsam.data.xts<-xts(tsam.data,seq(as.POSIXct(beginDate),len=length(tsam.data),by='day'))

   tsam.fore      <-ts(exp(c(fittedValueTmp,tsamp)),fre=7)    
    #stlf.arima.fore<-ts(c(exp(fitted(tsst.arima)),exp(tsst.arima$mean)),fre=7)    
    #stlf.ets.fore  <-ts(c(exp(fitted(tsst.ets)),exp(tsst.ets$mean)*holidayshift*monthshift),fre=7)    
    #tbats.fore     <-ts(c(exp(fitted(tbats)),exp(tbatsp$mean)*holidayshift),fre=7)    
    #tsfo.fore      <-ts(c(exp(tsdata-residuals(tsfo)),tsfop),fre=7)    
    #tsw.fore       <-ts(c(exp(tfi),exp(tswp)*holidayshift),fre=7)    
    #tssam.fore     <-ts(c(exp(fitted(tssam)),exp(tssamp)*holidayshift*monthshift),fre=7)    
    #tshw.fore      <-ts(c(fitted(tshw),tshw$mean*holidayshift*monthshift),fre=7)    
   tsam.fore.xts      <-xts(tsam.fore,seq(as.POSIXct(beginDate),len=length(tsam.fore),by='day'))
    #stlf.arima.fore.xts<-xts(stlf.arima.fore,seq(as.POSIXct(beginDate),len=length(stlf.arima.fore),by='day'))
    #stlf.ets.fore.xts  <-xts(stlf.ets.fore,seq(as.POSIXct(beginDate),len=length(stlf.ets.fore),by='day'))
    #tbats.fore.xts     <-xts(tbats.fore,seq(as.POSIXct(beginDate),len=length(tbats.fore),by='day'))
    #tsfo.fore.xts      <-xts(tsfo.fore,seq(as.POSIXct(beginDate),len=length(tsfo.fore),by='day'))
    #tsw.fore.xts       <-xts(tsw.fore,seq(as.POSIXct(beginDate),len=length(tsw.fore),by='day'))
    #tssam.fore.xts     <-xts(tssam.fore,seq(as.POSIXct(beginDate),len=length(tssam.fore),by='day'))
    #tshw.fore.xts      <-xts(tshw.fore,seq(as.POSIXct(beginDate),len=length(tshw.fore),by='day'))
    #plot(as.zoo(cbind(tsam.data.xts,tsam.fore.xts,stlf.arima.fore.xts,stlf.ets.fore.xts,tbats.fore.xts,tsfo.fore.xts,tsw.fore.xts,tssam.fore.xts,tshw.fore.xts)),col=1:9,lty=1:9,screens=1)
    #legend(x="topleft",legend=c("observed","arima+lm","stlf.arima","stlf.ets","tbats","fourier arima","wavelet arima","seasonal arima","hw"),lty=1:9,col=1:9)
    #plot(as.zoo(cbind(tsam.data.xts,tsam.fore.xts,stlf.arima.fore.xts,stlf.ets.fore.xts,tbats.fore.xts,tshw.fore.xts)),col=1:9,lty=1:9,screens=1)
    #legend(x="topleft",legend=c("observed","arima+lm","stlf.arima","stlf.ets","tbats","hw"),lty=1:9,col=1:9)
   plot(as.zoo(cbind(tsam.data.xts,tsam.fore.xts,svm.fore.xts)),col=1:9,lty=1:9,screens=1)
   legend(x="topleft",legend=c("observed","arima+lm","svm"),lty=1:9,col=1:9)

}
#
#totalFittedValue<-( ts(fittedValue.tsam,fre=7)
                   #+ts(fittedValue.stlf.arima,fre=7)
                   #+ts(fittedValue.stlf.ets  ,fre=7)
                   #+ts(fittedValue.tbats     ,fre=7)
#                   +ts(fittedValue.tsw       ,fre=7)
#                   +ts(fittedValue.fourier   ,fre=7)
#                   +ts(fittedValue.sam       ,fre=7)
                   #+ts(fittedValue.hw        ,fre=7)
                   #)/4
#totalResult<-( ts(result.tsam,fre=7)
              #+ts(result.stlf.arima,fre=7)
              #+ts(result.stlf.ets  ,fre=7)
              #+ts(result.tbats     ,fre=7)
#              +ts(result.tsw       ,fre=7)
#              +ts(result.fourier   ,fre=7)
#              +ts(result.sam       ,fre=7)
              #+ts(result.hw        ,fre=7)
              #)/4
#
#cat(">>>> Redeem <<<<\n")
#print(totalResult)

total.data           <-ts(c(od[fitbeg:preend,2]),fre=7)               
#total.fore           <-ts(c(totalFittedValue,totalResult),fre=7)    
total.fore.svm<-ts(c(fittedValue.svm      ,result.svm),fre=7)    
total.fore.tsam      <-ts(c(fittedValue.tsam      ,result.tsam      ),fre=7)    
#total.fore.stlf.arima<-ts(c(fittedValue.stlf.arima,result.stlf.arima),fre=7)    
#total.fore.stlf.ets  <-ts(c(fittedValue.stlf.ets  ,result.stlf.ets  ),fre=7)    
#total.fore.tbats     <-ts(c(fittedValue.tbats     ,result.tbats     ),fre=7)    
#total.fore.fourier   <-ts(c(fittedValue.fourier   ,result.fourier   ),fre=7)    
#total.fore.tsw       <-ts(c(fittedValue.tsw       ,result.tsw       ),fre=7)    
#total.fore.sam       <-ts(c(fittedValue.sam       ,result.sam       ),fre=7)    
#total.fore.hw        <-ts(c(fittedValue.hw        ,result.hw        ),fre=7)    
total.data.xts           <-xts(total.data,seq(as.POSIXct(beginDate),len=length(total.data),by='day'))
#total.fore.xts           <-xts(total.fore,seq(as.POSIXct(beginDate),len=length(total.fore),by='day'))
total.fore.svm.xts      <-xts(total.fore.svm      ,seq(as.POSIXct(beginDate),len=length(total.fore.svm      ),by='day'))
total.fore.tsam.xts      <-xts(total.fore.tsam      ,seq(as.POSIXct(beginDate),len=length(total.fore.tsam      ),by='day'))
#total.fore.stlf.arima.xts<-xts(total.fore.stlf.arima,seq(as.POSIXct(beginDate),len=length(total.fore.stlf.arima),by='day'))
#total.fore.stlf.ets.xts  <-xts(total.fore.stlf.ets  ,seq(as.POSIXct(beginDate),len=length(total.fore.stlf.ets  ),by='day'))
#total.fore.tbats.xts     <-xts(total.fore.tbats     ,seq(as.POSIXct(beginDate),len=length(total.fore.tbats     ),by='day'))
#total.fore.fourier.xts   <-xts(total.fore.fourier   ,seq(as.POSIXct(beginDate),len=length(total.fore.fourier   ),by='day'))
#total.fore.tsw.xts       <-xts(total.fore.tsw       ,seq(as.POSIXct(beginDate),len=length(total.fore.tsw       ),by='day'))
#total.fore.sam.xts       <-xts(total.fore.sam       ,seq(as.POSIXct(beginDate),len=length(total.fore.sam       ),by='day'))
#total.fore.hw.xts        <-xts(total.fore.hw        ,seq(as.POSIXct(beginDate),len=length(total.fore.hw        ),by='day'))

#plot(as.zoo(cbind(total.data.xts,total.fore.tsam.xts,total.fore.stlf.arima.xts,total.fore.stlf.ets.xts,total.fore.tbats.xts,total.fore.tsw.xts,total.fore.fourier.xts,total.fore.sam.xts,total.fore.hw.xts,total.fore.xts)),col=1:9,lty=1:9,screens=1)
#legend(x="topleft",legend=c("observed","arima+lm","stlf.arima","stlf.ets","tbats","fourier arima","wavelet arima","seasonal arima","hw","result"),lty=1:9,col=1:9)
#plot(as.zoo(cbind(total.data.xts,total.fore.tsam.xts,total.fore.stlf.arima.xts,total.fore.stlf.ets.xts,total.fore.hw.xts,total.fore.xts)),col=c(1:6,8,9),lty=1:9,screens=1)
#legend(x="topleft",legend=c("observed","arima+lm","stlf.arima","stlf.ets","hw","result"),lty=1:9,col=c(1:6,8,9))
plot(as.zoo(cbind(total.data.xts,total.fore.tsam.xts,total.fore.svm.xts)),col=1:9,lty=1:9,screens=1)
legend(x="topleft",legend=c("observed","arima+lm","svm"),lty=1:9,col=1:9)
