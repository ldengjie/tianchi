library("forecast")
library("xts")
library("e1071")
library("randomForest")
od<-read.csv("purchase_by_type_sum_p2_v4.csv")
of<-read.csv("totalfs.csv")
fitbeg<-275;#20140401
beginDate<-"2014-04-01"
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

cat("... begin to loop ...\n")

tslist<-list(total_purchase_amt,zfb1,zfb2,zfb3,bank1,bank2,bank3,share)
type<-list("total_purchase_amt","zfb1","zfb2","zfb3","bank1","bank2","bank3","share")
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
orderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(4,1,1),c(2,1,1),c(0,0,0),c(7,0,1),c(7,1,1))

result.svm       <-rep(0,30)
result.rf       <-rep(0,30)
result.tsam      <-rep(0,30)
result.stlf.ets  <-rep(0,30)
result.hw        <-rep(0,30)
xregfit<-data.frame(of[fitbeg:fitend,2:7],of[fitbeg:fitend,13:22],of[fitbeg:fitend,46:58],tx=of[fitbeg:fitend,61],of[fitbeg:fitend,64:74],time=scale(of[fitbeg:fitend,98]));
xregpre<-data.frame(of[prebeg:preend,2:7],of[prebeg:preend,13:22],of[prebeg:preend,46:58],tx=of[prebeg:preend,61],of[prebeg:preend,64:74],time=scale(of[prebeg:preend,98]));
for(ti in 2:8)
{
    st<-0.05
    tsdata<-tslist[[ti]]
    or<-orderlist[[ti]]
    legend<-type[[ti]]
    tsdatav<-tslistv[[ti]]
    tsv<-cbind(tsdatav,xregfit)
    cat("========>",legend,"<======\n")

    cat("###arima-lm###\n")
    aicb<-Inf;
    for(si in st)
    {
        if(si==0) next;
        tsam<-arima(tsdata,order=or,xreg=xregfit)
        aico<-tsam$aic;
        fixedv<-rep(NA,length(tsam$coef));
        needFix<-TRUE;
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
        if(tsam$aic<aicb)
        {
            ki<-si;
            fixb<-fixedv;
            aicb<-tsam$aic;
        }
    }
    tsam.bestfit<-arima(tsdata,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE);
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
    tsam.p<-predict(tsam.bestfit,newxreg =xregpre);
    tsamp<-tsam.p$pred;
    result.tsam<-result.tsam+exp(tsamp)

    coefs <- tsam.bestfit$coef
    narma<-sum(tsam.bestfit$arma[1:4])
    if(names(coefs)[narma + 1L] == "intercept") narma<-narma+1
    shift<-coefs[-(1:narma)]
    cat("###holidayshift###\n")
    holidayshift<-rep(1,30)
    for(fi in 17:30)
    {
        holidayshift<-holidayshift*exp(xregpre[,fi]*shift[fi])
    }
    print(holidayshift)
    cat("###monthshift###\n")
    monthshift<-rep(1,30)
    for(fi in 7:16)
    {
        monthshift<-monthshift*exp(xregpre[,fi]*shift[fi])
    }
    print(monthshift)

    cat("### svm ###\n")
    bep<-c(0.1,1,0.7,0,0.6,0,0.4,0)
    bco<-c(1,4,8,32,32,8,8,512)
    svmf<-svm(tsdatav~.,tsv,type="eps-regression",cross = length(tsdatav),epsilon=bep[[ti]],cost=bco[[ti]])
    svmp<- predict(svmf, cbind(rep(0,30),xregpre)) 
    cat("----> RMSE : ",sqrt(mean(svmf$residuals^2))/mean(tsdatav),"\n")
    result.svm<-result.svm+svmp
    svm.fore     <-ts(c(fitted(svmf),svmp),fre=7)    
    svm.fore.xts     <-xts(svm.fore,seq(as.POSIXct(beginDate),len=length(svm.fore),by='day'))

    cat("###randomForest###\n")
    xregfitr<-data.frame(of[fitbeg:fitend,2:7],of[fitbeg:fitend,13:22],of[fitbeg:fitend,46:58],tx=of[fitbeg:fitend,61],of[fitbeg:fitend,64:74],time=of[fitbeg:fitend,98]);
    xregprer<-data.frame(of[prebeg:preend,2:7],of[prebeg:preend,13:22],of[prebeg:preend,46:58],tx=of[prebeg:preend,61],of[prebeg:preend,64:74],time=of[prebeg:preend,98]);
    tsr<-cbind(tsdatav,xregfitr)
    bmi<-c(13,5,10,5,13,13,12,36)
    set.seed(521)
    rff<-randomForest(tsdatav~.,tsr,importance=TRUE,mtry=bmi[ti])
    rfp<-predict(rff,cbind(rep(0,30),xregprer))
    cat("----> randomForest RMSE : ",sqrt(mean((tsdatav-predict(rff))^2))/mean(tsdatav),"\n")
    result.rf<-result.rf+rfp*monthshift*holidayshift
    rf.fore     <-ts(c(predict(rff),rfp*monthshift*holidayshift),fre=7)    
    rf.fore.xts     <-xts(rf.fore,seq(as.POSIXct(beginDate),len=length(rf.fore),by='day'))

    cat("###stlf.ets###\n")
    tsst.ets<-stlf(tsdata,h=30,s.window=7,method='ets',ic='bic',opt.crit='mae')
    result.stlf.ets<-result.stlf.ets+exp(tsst.ets$mean)*holidayshift*monthshift

    cat("###hw###\n")
    tshw<-hw(exp(tsdata),h=30,seasonal='multiplicative',damped=TRUE)
    result.hw<-result.hw+tshw$mean*holidayshift*monthshift
}

totalResult<-( ts(result.tsam,fre=7)
              +ts(result.svm,fre=7)
              +ts(result.rf,fre=7)
              +ts(result.stlf.ets  ,fre=7)
              +ts(result.hw        ,fre=7)
              )/5

cat(">>>> Purchase <<<<\n")
print(totalResult)
