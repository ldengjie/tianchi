library("forecast")
library("xts")
library("e1071")
library("randomForest")
od<-read.csv("redeem_by_type_sum_p2_v4.csv")
of<-read.csv("totalfs.csv")
fitbeg<-275;#20140401
beginDate<-"2014-04-01"
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

cat("... begin to loop ...\n")

type<-list("total_redeem_amt","zfb1","zfb2","consume","card1","card2","card3")
tslist<-list(total_redeem_amt,zfb1,zfb2,consume,card1,card2,card3)
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
orderlist<-list(c(0,0,0),c(1,0,5),c(3,1,1),c(1,0,4),c(4,1,4),c(1,0,5),c(3,1,1))

result.svm       <-rep(0,30)
result.rf       <-rep(0,30)
result.tsam      <-rep(0,30)
result.stlf.ets  <-rep(0,30)
result.hw        <-rep(0,30)
xregfit<-data.frame(of[fitbeg:fitend,2:7],of[fitbeg:fitend,13:22],of[fitbeg:fitend,46:58],tx=of[fitbeg:fitend,61],of[fitbeg:fitend,64:74],time=scale(of[fitbeg:fitend,98]));
xregpre<-data.frame(of[prebeg:preend,2:7],of[prebeg:preend,13:22],of[prebeg:preend,46:58],tx=of[prebeg:preend,61],of[prebeg:preend,64:74],time=scale(of[prebeg:preend,98]));

for(ti in 2:7)
{
    st<-0.05
    tsdata<-tslist[[ti]]
    or<-orderlist[[ti]]
    sor<-sorderlist[[ti]]
    legend<-type[[ti]]
    tsdatav<-tslistv[[ti]]
    tsv<-cbind(tsdatav,xregfit)
    cat("========>",legend,"<======\n")

    cat("### arima+lm ###\n")
    aicb<-Inf;
    for(si in st)
    {
        if(si==0) next;
        tsam<-arima(tsdata,order=or,xreg=xregfit)
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
    bep<-c(0.1,0.3,0,0.7,0.8,0.6,0.5)
    bco<-c(1,64,64,8,512,512,32)
    svmf<-svm(tsdatav~.,tsv,type="eps-regression",cross = length(tsdatav),epsilon=bep[[ti]],cost=bco[[ti]])
    svmp<- predict(svmf,cbind(rep(0,30),xregpre)) 
    cat("----> RMSE : ",sqrt(mean(svmf$residuals^2))/mean(tsdatav),"\n")
    result.svm<-result.svm+svmp
    svm.fore     <-ts(c(fitted(svmf),svmp),fre=7)    
    svm.fore.xts     <-xts(svm.fore,seq(as.POSIXct(beginDate),len=length(svm.fore),by='day'))

    cat("###randomForest###\n")
    xregfitr<-data.frame(of[fitbeg:fitend,2:7],of[fitbeg:fitend,13:22],of[fitbeg:fitend,46:58],tx=of[fitbeg:fitend,61],of[fitbeg:fitend,64:74],time=of[fitbeg:fitend,98]);
    xregprer<-data.frame(of[prebeg:preend,2:7],of[prebeg:preend,13:22],of[prebeg:preend,46:58],tx=of[prebeg:preend,61],of[prebeg:preend,64:74],time=of[prebeg:preend,98]);
    tsr<-cbind(tsdatav,xregfitr)
    bmi<-c(13,12,13,16,9,19,9)
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
cat(">>>> Redeem <<<<\n")
print(totalResult)
