#######################
193.372247 35
#######################
with time , new classes ,only arima+lm
#######################
library("forecast")
library("TSA")
library("xts")
library("wmtsa")
od<-read.csv("purchase_by_type_sum_p2_v4.csv")
of<-read.csv("totalfs.csv")
fitbeg<-275;#20140401
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


cat("... begin to loop ...\n")

par(mfcol=c(4,2))
tslist<-list(total_purchase_amt,zfb1,zfb2,zfb3,bank1,bank2,bank3,share)
type<-list("total_purchase_amt","zfb1","zfb2","zfb3","bank1","bank2","bank3","share")
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
orderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(4,1,1),c(2,1,1),c(0,0,0),c(7,0,1),c(7,1,1))

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
xregfit<-data.frame(of[fitbeg:fitend,2:17],of[fitbeg:fitend,20:32],of[fitbeg:fitend,35],of[fitbeg:fitend,38:48],scale(of[fitbeg:fitend,72]));
xregpre<-data.frame(of[prebeg:preend,2:17],of[prebeg:preend,20:32],of[prebeg:preend,35],of[prebeg:preend,38:48],scale(of[prebeg:preend,72]));
for(ti in 2:8)
{
    st<-0.05
    tsdata<-tslist[[ti]]
    or<-orderlist[[ti]]
    legend<-type[[ti]]
    aicb<-Inf;
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
    detectAO(tsam.bestfit);
    detectIO(tsam.bestfit);
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
    tsam.p<-predict(tsam.bestfit,newxreg =xregpre);
    tsamp<-tsam.p$pred;
    result.tsam<-result.tsam+exp(tsamp)
    fittedValue.tsam<-fittedValue.tsam+exp(fittedValueTmp)

    tsam.data<-ts(c(exp(tsdata),od[prebeg:preend,2]),fre=7)               
    tsam.data.xts<-xts(tsam.data,seq(as.POSIXct("2014-04-01"),len=length(tsam.data),by='day'))
    tsam.fore      <-ts(exp(c(fittedValueTmp,tsamp)),fre=7)    
    tsam.fore.xts      <-xts(tsam.fore,seq(as.POSIXct("2014-04-01"),len=length(tsam.fore),by='day'))
    plot(as.zoo(cbind(tsam.data.xts,tsam.fore.xts)),col=1:9,lty=1:9,screens=1)
    legend(x="topleft",legend=c("observed","arima+lm"),lty=1:9,col=1:9)
}
totalFittedValue<-( ts(fittedValue.tsam,fre=7)
                   )/1
totalResult<-( ts(result.tsam,fre=7)
              )/1
print(totalResult)
total.data           <-ts(c(od[fitbeg:preend,2]),fre=7)               
total.fore           <-ts(c(totalFittedValue,totalResult),fre=7)    
total.fore.tsam      <-ts(c(fittedValue.tsam      ,result.tsam      ),fre=7)    
total.data.xts           <-xts(total.data,seq(as.POSIXct("2014-04-01"),len=length(total.data),by='day'))
total.fore.xts           <-xts(total.fore,seq(as.POSIXct("2014-04-01"),len=length(total.fore),by='day'))
total.fore.tsam.xts      <-xts(total.fore.tsam      ,seq(as.POSIXct("2014-04-01"),len=length(total.fore.tsam      ),by='day'))
plot(as.zoo(cbind(total.data.xts,total.fore.tsam.xts)),col=1:9,lty=1:9,screens=1)
legend(x="topleft",legend=c("observed","arima+lm"),lty=1:9,col=1:9)
-------
library("forecast")
library("TSA")
library("xts")
library("wmtsa")
library("rugarch")
od<-read.csv("redeem_by_type_sum_p2_v4.csv")
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



cat("... begin to loop ...\n")



par(mfcol=c(4,2))
type<-list("total_redeem_amt","zfb1","zfb2","consume","card1","card2","card3")
tslist<-list(total_redeem_amt,zfb1,zfb2,consume,card1,card2,card3)
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
orderlist<-list(c(0,0,0),c(1,0,5),c(3,1,1),c(1,0,4),c(4,1,4),c(1,0,5),c(3,1,1))

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
xregfit<-data.frame(of[fitbeg:fitend,2:17],of[fitbeg:fitend,20:32],of[fitbeg:fitend,35],of[fitbeg:fitend,38:48],scale(of[fitbeg:fitend,72]));
xregpre<-data.frame(of[prebeg:preend,2:17],of[prebeg:preend,20:32],of[prebeg:preend,35],of[prebeg:preend,38:48],scale(of[prebeg:preend,72]));

for(ti in 2:7)
{
    st<-0.05
    tsdata<-tslist[[ti]]
    or<-orderlist[[ti]]
    sor<-sorderlist[[ti]]
    legend<-type[[ti]]
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

    tsam.data<-ts(c(exp(tsdata),od[prebeg:preend,2]),fre=7)               
    tsam.data.xts<-xts(tsam.data,seq(as.POSIXct("2014-04-01"),len=length(tsam.data),by='day'))

    tsam.fore      <-ts(exp(c(fittedValueTmp,tsamp)),fre=7)    
    tsam.fore.xts      <-xts(tsam.fore,seq(as.POSIXct("2014-04-01"),len=length(tsam.fore),by='day'))
    plot(as.zoo(cbind(tsam.data.xts,tsam.fore.xts)),col=1:9,lty=1:9,screens=1)
    legend(x="topleft",legend=c("observed","arima+lm"),lty=1:9,col=1:9)
}
totalFittedValue<-( ts(fittedValue.tsam,fre=7)
                   )/1
totalResult<-( ts(result.tsam,fre=7)
              )/1
print(totalResult)

total.data           <-ts(c(od[fitbeg:preend,2]),fre=7)               
total.fore           <-ts(c(totalFittedValue,totalResult),fre=7)    
total.fore.tsam      <-ts(c(fittedValue.tsam      ,result.tsam      ),fre=7)    
total.data.xts           <-xts(total.data,seq(as.POSIXct("2014-04-01"),len=length(total.data),by='day'))
total.fore.xts           <-xts(total.fore,seq(as.POSIXct("2014-04-01"),len=length(total.fore),by='day'))
total.fore.tsam.xts      <-xts(total.fore.tsam      ,seq(as.POSIXct("2014-04-01"),len=length(total.fore.tsam      ),by='day'))
plot(as.zoo(cbind(total.data.xts,total.fore.tsam.xts)),col=1:9,lty=1:9,screens=1)
legend(x="topleft",legend=c("observed","arima+lm"),lty=1:9,col=1:9)

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
  as.integer(1309221384)
, as.integer(1148420103)
, as.integer(1175604089)
, as.integer(991324737 )
, as.integer(913866697 )
, as.integer(461650332 )
, as.integer(479070723 )
, as.integer(751350498 )
, as.integer(1052005723)
, as.integer(1010119070)
, as.integer(989454075 )
, as.integer(913363591 )
, as.integer(610800412 )
, as.integer(602374296 )
, as.integer(989833775 )
, as.integer(1001573947)
, as.integer(962250825 )
, as.integer(869762521 )
, as.integer(790496934 )
, as.integer(572480721 )
, as.integer(576565886 )
, as.integer(946501239 )
, as.integer(956313623 )
, as.integer(919066761 )
, as.integer(832468250 )
, as.integer(757936483 )
, as.integer(550649886 )
, as.integer(805499495 )
, as.integer(907409817 )
, as.integer(963315206 )
)
redeem= c(  
  as.integer(1143657493)
, as.integer(1008237041)
, as.integer(1362628556)
, as.integer(962780500 )
, as.integer(849040602 )
, as.integer(383494392 )
, as.integer(504868703 )
, as.integer(811229901 )
, as.integer(1083753162)
, as.integer(1084950013)
, as.integer(976344293 )
, as.integer(829051542 )
, as.integer(626582488 )
, as.integer(680421572 )
, as.integer(1181873386)
, as.integer(1105940763)
, as.integer(1107223992)
, as.integer(999481213 )
, as.integer(855370351 )
, as.integer(643964052 )
, as.integer(698054709 )
, as.integer(1206698011)
, as.integer(1130476288)
, as.integer(1131855037)
, as.integer(1025066516)
, as.integer(887968273 )
, as.integer(723425713 )
, as.integer(1234704502)
, as.integer(1241508656)
, as.integer(1183644853)
)
Part1         ,noTime         ,withTime     
1121196856    ,1177857114     ,1309647332     
950412530     ,1034337713     ,1148799035    
1014373911    ,1037419266     ,1176062209    
828318032     ,909511424      ,991785707     
755028136     ,813384215      ,914331709     
402504687     ,460941385      ,462116838     
436808473     ,468028489      ,479496672     
665289253     ,663830450      ,751776447     
950412530     ,1010867021     ,1052431673    
1035054357    ,1019130736     ,1010577190    
943098974     ,992527775      ,989921064     
791038367     ,907459661      ,913828603     
551243507     ,608557115      ,611266917     
551243507     ,597752543      ,602800245     
933645639     ,1003615501     ,990259724     
950412530     ,1010867021     ,1001999896    
923104200     ,974825114      ,962708945     
828318032     ,886143164      ,870223491     
755028136     ,813384215      ,790961947     
551243507     ,590702394      ,572947226     
551243507     ,597752543      ,576991835     
933645639     ,1003615501     ,946927189     
950412530     ,1010867021     ,956739572     
923104200     ,974825114      ,919524881     
828318032     ,886143164      ,832929221     
755028136     ,813384215      ,758401495     
551243507     ,590702394      ,551116392     
835419428     ,739372803      ,806002251     
933645639     ,1003615501     ,907835766     
966417208     ,1010867021     ,963741155     
0             ,0              ,0
1199550277    ,1080531651     ,1143657493    
1029985180    ,864715417      ,1008237041    
1374873298    ,1174726205     ,1362628556    
1026752273    ,929072700      ,962780500     
892834617     ,743987503      ,849040602     
418304490     ,380624463      ,383494392     
519397130     ,476453589      ,504868703     
867186198     ,766623921      ,811229901     
1126330583    ,1010384127     ,1083753162    
1118912131    ,1015854059     ,1084950013    
1026752273    ,921714090      ,976344293     
892834617     ,792330138      ,829051542     
672824700     ,598635638      ,626582488     
717319717     ,635851001      ,680421572     
1199550277    ,1075998327     ,1181873386    
1126330583    ,1010384127     ,1105940763    
1118912131    ,1015854059     ,1107223992    
1026752273    ,921714090      ,999481213     
892834617     ,806986730      ,855370351     
672824700     ,598635638      ,643964052     
717319717     ,635851001      ,698054709     
1199550277    ,1075998327     ,1206698011    
1126330583    ,1010384127     ,1130476288    
1118912131    ,1015854059     ,1131855037    
1026752273    ,921714090      ,1025066516    
892834617     ,816258173      ,887968273     
672824700     ,605804931      ,723425713     
1212890245    ,1127766079     ,1234704502    
1235731948    ,1083356937     ,1241508656    
1262090295    ,1017914401     ,1183644853    
