library("stats")
library("forecast")
library("e1071")
library("randomForest")
od<-read.csv("redeem_by_type_sum_p2_v4.csv")
ofe<-read.csv("totalfs.csv")
fitbeg<-275;#20140401
beginDate<-"2014-04-01"
fitend<-427;#20140831
fd<-od[fitbeg:fitend,]

total_redeem_amt <-ts(log(fd$total_redeem_amt ),fre=7)

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


typestr<-list("total_redeem_amt","zfb1","zfb2","consume","card1","card2","card3")
tslist<-list(total_redeem_amt,zfb1,zfb2,consume,card1,card2,card3)
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
orderlist<-list(c(0,0,0),c(1,0,5),c(3,1,1),c(1,0,4),c(4,1,4),c(1,0,5),c(3,1,1))

resultsvm       <-rep(0,30)
resultrf       <-rep(0,30)
resulttsam      <-rep(0,30)
resultstlfets  <-rep(0,30)
resulthw        <-rep(0,30)
xregfit<-data.frame(ofe[1:153,2:7],ofe[1:153,13:22],ofe[1:153,46:58],tx=ofe[1:153,61],ofe[1:153,64:74],timex=scale(ofe[1:153,98]));
xregpre<-data.frame(ofe[154:183,2:7],ofe[154:183,13:22],ofe[154:183,46:58],tx=ofe[154:183,61],ofe[154:183,64:74],timex=scale(ofe[154:183,98]));

for(ti in 2:7)
{
    st<-0.05
    tsdata<-tslist[[ti]]
    orl<-orderlist[[ti]]
    sor<-sorderlist[[ti]]
    legend<-typestr[[ti]]
    tsdatav<-tslistv[[ti]]
    tsv<-cbind(tsdatav,xregfit)

    tsam<-arima(tsdata,order=orl,xreg=xregfit)
    fixedv<-rep(NA,length(tsam$coef));
    needFix<-TRUE;
    while(needFix)
    {
        needFix<-FALSE;
        mx<-1;
        for (ci in 1:length(tsam$coef))
        {
            if(!tsam$coef[ci]==0)
            {
                if(!is.na(sqrt(diag(tsam$var.coef))[mx]))
                {
                    pv<-(1-pnorm(abs(tsam$coef[ci])/sqrt(diag(tsam$var.coef))[mx]))*2
                    if(  pv> st )
                    {
                        fixedv[ci]=0;
                        needFix<-TRUE;
                    }
                }
                mx<-mx+1
            }

        }
        if(needFix)
        {
            tsam<-arima(tsdata,order=orl,xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
        }
    }
    tsamp<-predict(tsam,newxreg =xregpre);
    tsamp<-tsamp$pred;
    resulttsam<-resulttsam+exp(tsamp)

    coefs <- tsam$coef
    narma<-sum(tsam$arma[1:4])
    if(names(coefs)[narma + 1L] == "intercept") narma<-narma+1
    shift<-coefs[-(1:narma)]
    holidayshift<-rep(1,30)
    for(fi in 17:30)
    {
        holidayshift<-holidayshift*exp(xregpre[,fi]*shift[fi])
    }
    monthshift<-rep(1,30)
    for(fi in 7:16)
    {
        monthshift<-monthshift*exp(xregpre[,fi]*shift[fi])
    }

    bep<-c(0.1,0.3,0,0.7,0.8,0.6,0.5)
    bco<-c(1,64,64,8,512,512,32)
    svmf<-svm(tsdatav~.,tsv,type="eps-regression",cross = length(tsdatav),epsilon=bep[[ti]],cost=bco[[ti]])
    svmp<- predict(svmf,cbind(rep(0,30),xregpre)) 
    resultsvm<-resultsvm+svmp*monthshift*holidayshift

    xregfitr<-data.frame(ofe[1:153,2:7],ofe[1:153,13:22],ofe[1:153,46:58],tx=ofe[1:153,61],ofe[1:153,64:74],timex=ofe[1:153,98]);
    xregprer<-data.frame(ofe[154:183,2:7],ofe[154:183,13:22],ofe[154:183,46:58],tx=ofe[154:183,61],ofe[154:183,64:74],timex=ofe[154:183,98]);
    tsr<-cbind(tsdatav,xregfitr)
    bmi<-c(13,12,13,16,9,19,9)
    set.seed(521)
    rff<-randomForest(tsdatav~.,tsr,importance=TRUE,mtry=bmi[ti])
    rfp<-predict(rff,cbind(rep(0,30),xregprer))
    resultrf<-resultrf+rfp*monthshift*holidayshift

    tsstets<-stlf(tsdata,h=30,s.window=7,method='ets',ic='bic',opt.crit='mae')
    resultstlfets<-resultstlfets+exp(tsstets$mean)*holidayshift*monthshift


    tshw<-hw(exp(tsdata),h=30,seasonal='multiplicative',damped=TRUE)
    resulthw<-resulthw+tshw$mean*holidayshift*monthshift
}

totalResult<-( ts(resulttsam,fre=7)
              +ts(resultsvm,fre=7)
              +ts(resultrf,fre=7)
              +ts(resultstlfets  ,fre=7)
              +ts(resulthw        ,fre=7)
              )/5
