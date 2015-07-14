#######################
190
bu合并分类+features~（000）得到pq~arima
#######################
library("forecast")
library("TSA")
library("xts")
library("wmtsa")
od<-read.csv("purchase_by_type.csv")
of<-read.csv("totalfs.csv")
prebeg<-fitend+1;
fd<-od[fitbeg:fitend,]


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



cat("... begin to loop ...\n")
par(mfcol=c(4,2))
tslist<-list(total_purchase_amt,zfb1,zfb2,zfb3,bank1,bank2,bank3,share)
type<-list("total_purchase_amt","zfb1","zfb2","zfb3","bank1","bank2","bank3","share")
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
orderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(4,0,4),c(1,0,6),c(2,1,1),c(1,0,6),c(1,1,1))


result<-rep(0,30)
fittedValue<-rep(0,fitend-fitbeg+1)
result.st<-rep(0,30)
fittedValue.st<-rep(0,fitend-fitbeg+1)

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
for(ti in 2:8)
{
    st<-seq(0,0.4,0.05);
    ts<-tslist[[ti]]
    or<-orderlist[[ti]]
    sor<-sorderlist[[ti]]
    legend<-type[[ti]]
    aicb<-Inf;
    for(si in st)
    {
        if(si==0) next;
        tsam<-arima(ts,order=or,xreg=xregfit)
        aico<-tsam$aic;
        fixedv<-rep(NA,length(tsam$coef));
        needFix<-TRUE;
        cat(si,"---------------------------->\n");
        while(needFix)
        {
            needFix<-FALSE;
            hasNaN<-FALSE;
            for (ci in 1:length(tsam$coef))
            {
                if(is.na(sqrt(diag(tsam$var.coef))[ci])) 
                {
                    hasNaN<-TRUE;
                    break; 
                }
            }
            if(!hasNaN)
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
                tsam<-arima(ts,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
            }
        }
        if(tsam$aic<aicb)
        {
            ki<-si;
            fixb<-fixedv;
            aicb<-tsam$aic;
        }
    }
    tsam.bestfit<-arima(ts,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE);
    cat("========>",legend,"<======\n")
    detectAO(tsam.bestfit);
    detectIO(tsam.bestfit);
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
    print(tsam.bestfit);
    tsam.p<-predict(tsam.bestfit,newxreg =xregpre);
    tsamp<-tsam.p$pred;
    result<-result+exp(tsamp)
    fittedValue<-fittedValue+exp(fitted(tsam.bestfit))
    tsam.data<-ts(c(exp(ts),od[prebeg:preend,2]),fre=7)               
    tsam.fore<-ts(exp(c(fitted(tsam.bestfit),tsamp)),fre=7)    
    tsam.data.xts<-xts(tsam.data,seq(as.POSIXct("2014-04-01"),len=length(tsam.data),by='day'))
    tsam.fore.xts<-xts(tsam.fore,seq(as.POSIXct("2014-04-01"),len=length(tsam.fore),by='day'))
}

------
library("forecast")
library("TSA")
library("xts")
library("wmtsa")
library("lmtest")
od<-read.csv("redeem_by_type.csv")
of<-read.csv("totalfs.csv")
prebeg<-fitend+1;
fd<-od[fitbeg:fitend,]

report_date      <-fd$report_date
total_redeem_amt <-log(fd$total_redeem_amt )
tftobal_amt      <-log(fd$tftobal_amt      )
category1        <-log(fd$category1        )
category2        <-log(fd$category2        )
category3        <-log(fd$category3        )
category4        <-log(fd$category4        )
icard0            <-log(fd$card0            )
icard1            <-log(fd$card1            )
icard2            <-log(fd$card2            )
icard3             <-log(fd$card3             )
icard4             <-log(fd$card4             )
icard5             <-log(fd$card5             )
icard6             <-log(fd$card6             )
icard7             <-log(fd$card7             )
icard8             <-log(fd$card8             )
icard9             <-log(fd$card9             )
icard10            <-log(fd$card10            )
icard11            <-log(fd$card11            )
consume<-log(fd$category1+fd$category2+fd$category3+fd$category4)
card1<-log(fd$card1+fd$card2+fd$card3)
card2<-log(fd$card4+fd$card5+fd$card6)
card3<-log(fd$card7+fd$card8+fd$card9+fd$card10+fd$card11)

zfb1<-log(exp(tftobal_amt)+exp(consume))

cat("... begin to loop ...")

par(mfcol=c(3,2))
type<-list("total_redeem_amt","zfb1","card1","card2","card3")
tslist<-list(total_redeem_amt,zfb1,card1,card2,card3)
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
orderlist<-list(c(0,0,0),c(1,0,2),c(3,1,1),c(1,0,1),c(5,1,1))

result<-rep(0,30)
fittedValue<-rep(0,fitend-fitbeg+1)
result.st<-rep(0,30)
fittedValue.st<-rep(0,fitend-fitbeg+1)
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
    st<-seq(0,0.25,0.05);
    ts<-tslist[[ti]]
    or<-orderlist[[ti]]
    sor<-sorderlist[[ti]]
    legend<-type[[ti]]
    aicb<-Inf;
    for(si in st)
    {
        if(si==0) next;
        tsam<-arima(ts,order=or,xreg=xregfit)
        aico<-tsam$aic;
        fixedv<-rep(NA,length(tsam$coef));
        needFix<-TRUE;
        cat(si,"---------------------------->\n");
        while(needFix)
        {
            needFix<-FALSE;
            hasNaN<-FALSE;
            for (ci in 1:length(tsam$coef))
            {
                if(is.na(sqrt(diag(tsam$var.coef))[ci])) 
                {
                    hasNaN<-TRUE;
                    break; 
                }
            }
            if(!hasNaN)
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
                tsam<-arima(ts,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
            }
        }
        if(tsam$aic<aicb)
        {
            ki<-si;
            fixb<-fixedv;
            aicb<-tsam$aic;
        }
    }
    tsam.bestfit<-arima(ts,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE);
    cat("========>",legend,"<======\n")
    detectAO(tsam.bestfit);
    detectIO(tsam.bestfit);
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
    print(tsam.bestfit);
    tsam.p<-predict(tsam.bestfit,newxreg =xregpre);
    tsamp<-tsam.p$pred;
    result<-result+exp(tsamp)
    fittedValue<-fittedValue+exp(fitted(tsam.bestfit))
    tsam.data<-ts(c(exp(ts),od[prebeg:preend,2]),fre=7)               
    tsam.fore<-ts(exp(c(fitted(tsam.bestfit),tsamp)),fre=7)    
    tsam.data.xts<-xts(tsam.data,seq(as.POSIXct("2014-04-01"),len=length(tsam.data),by='day'))
    tsam.fore.xts<-xts(tsam.fore,seq(as.POSIXct("2014-04-01"),len=length(tsam.fore),by='day'))
}
print(result)

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
  as.integer(1445444119)
, as.integer(937602777 )
, as.integer(1249765700)
, as.integer(996185480 )
, as.integer(892423404 )
, as.integer(420830206 )
, as.integer(512792177 )
, as.integer(842986242 )
, as.integer(1074626736)
, as.integer(962529491 )
, as.integer(1017075841)
, as.integer(910694635 )
, as.integer(614334094 )
, as.integer(651572659 )
, as.integer(1006264411)
, as.integer(1004969501)
, as.integer(962529491 )
, as.integer(879127677 )
, as.integer(778693704 )
, as.integer(550698247 )
, as.integer(602216256 )
, as.integer(963408028 )
, as.integer(1004969501)
, as.integer(980251327 )
, as.integer(849298507 )
, as.integer(784447800 )
, as.integer(546458525 )
, as.integer(998200983 )
, as.integer(1062179945)
, as.integer(1090653534)
)
redeem= c(  
  as.integer(1173320163)
, as.integer(997016516 )
, as.integer(1249323376)
, as.integer(961322267 )
, as.integer(803533571 )
, as.integer(434664895 )
, as.integer(471995519 )
, as.integer(860568993 )
, as.integer(1089054605)
, as.integer(951354196 )
, as.integer(978821768 )
, as.integer(812513947 )
, as.integer(605163783 )
, as.integer(801891227 )
, as.integer(1148355916)
, as.integer(1089054605)
, as.integer(1002825767)
, as.integer(978821768 )
, as.integer(877076938 )
, as.integer(637253843 )
, as.integer(678158490 )
, as.integer(1158425318)
, as.integer(1089054605)
, as.integer(935585077 )
, as.integer(1014949251)
, as.integer(923913446 )
, as.integer(686021544 )
, as.integer(1184926113)
, as.integer(1133608246)
, as.integer(1075195712)
)
