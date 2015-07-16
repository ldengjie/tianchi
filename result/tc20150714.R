#######################
196.6
#######################
library("forecast")
library("TSA")
library("xts")
library("wmtsa")
od<-read.csv("purchase_by_type.csv")
of<-read.csv("totalfs.csv")
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


cat("... begin to loop ...\n")
par(mfcol=c(3,2))
zfb1<-log(exp(zfb1))
zfb2<-log(exp(zfb2)+exp(zfb3))
bank1<-log(exp(bank1)+exp(bank2))
bank2<-log(exp(bank3)+exp(share))
tslist<-list(total_purchase_amt,zfb1,zfb2,bank1,bank2)
type<-list("total_purchase_amt","zfb1","zfb2","bank1","bank2")
orderlist<-list(c(0,0,0),c(0,0,0),c(3,1,4),c(1,0,1),c(1,0,5))
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))

result<-rep(0,30)
fittedValue<-rep(0,fitend-fitbeg+1)
result.stlf.arima<-rep(0,30)
result.stlf.ets<-rep(0,30)
result.tbats<-rep(0,30)
result.sam<-rep(0,30)
fittedValue.stlf<-rep(0,fitend-fitbeg+1)
result.tsw<-rep(0,30)
#(1)report_date, (2)w1, (3)w2, (4)w3, (5)w4, (6)w5, (7)w6, (8)mb1, (9)mb2, (10)mb3, (11)mb4, (12)mb5, (13)ma1, (14)ma2, (15)ma3, (16)ma4, (17)ma5, (18)fb1, (19)fb2, (20)fb3, (21)fb4, (22)fb5, (23)fb6, (24)fb7, (25)fi1, (26)fi2, (27)fi3, (28)fa1, (29)fa2, (30)fa3, (31)fa4, (32)fa5, (33)fa6, (34)fa7, (35)tx, (36)sb1, (37)sb2, (38)sb3, (39)sb4, (40)sb5, (41)sb6, (42)sb7, (43)si1, (44)sa1, (45)sa2, (46)sa3, (47)sa4, (48)sa5, (49)sa6, (50)sa7, (51)cb1, (52)cb2, (53)cb3, (54)cb4, (55)cb5, (56)cb6, (57)cb7, (58)ci1, (59)ci2, (60)ci3, (61)ci4, (62)ci5, (63)ci6, (64)ci7, (65)ca1, (66)ca2, (67)ca3, (68)ca4, (69)ca5, (70)ca6, (71)ca7
#5
xregfit<-data.frame(of[fitbeg:fitend,2:17],of[fitbeg:fitend,20:32],of[fitbeg:fitend,35],of[fitbeg:fitend,38:48]);
xregpre<-data.frame(of[prebeg:preend,2:17],of[prebeg:preend,20:32],of[prebeg:preend,35],of[prebeg:preend,38:48]);
for(ti in 2:2)
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
        if(ti==4)
        {
            tsam<-arima(tsdata,order=or,xreg=data.frame(xregfit),io=c(33))
        }else if(ti==5)
        {
            tsam<-arima(tsdata,order=or,xreg=xregfit,io=c(137))
        }else
        {
            tsam<-arima(tsdata,order=or,xreg=xregfit)
        }
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
                if(ti==4)
                {
                    tsam<-arima(tsdata,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE,io=c(33))
                }else if(ti==5)
                {
                    tsam<-arima(tsdata,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE,io=c(137))
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
    if(ti==4)
    {
        tsam.bestfit<-arima(tsdata,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE,io=c(33));
        fittedValueTmp<-fitted(tsam.bestfit)
        print(tsam.bestfit);
        tsam.bestfit$coef<-tsam.bestfit$coef[1:(length(tsam.bestfit$coef)-1)]
    }else if(ti==5)
    {
        tsam.bestfit<-arima(tsdata,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE,io=c(137));
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
    print(exp(tsamp))
    result<-result+exp(tsamp)
    fittedValue<-fittedValue+exp(fittedValueTmp)
    tsam.data<-ts(c(exp(tsdata),od[prebeg:preend,2]),fre=7)               
    tsam.fore<-ts(exp(c(fittedValueTmp,tsamp)),fre=7)    
    tsam.data.xts<-xts(tsam.data,seq(as.POSIXct("2014-04-01"),len=length(tsam.data),by='day'))
    tsam.fore.xts<-xts(tsam.fore,seq(as.POSIXct("2014-04-01"),len=length(tsam.fore),by='day'))
    #######
    coefs <- tsam.bestfit$coef
    narma<-sum(tsam.bestfit$arma[1:4])
    if(names(coefs)[narma + 1L] == "intercept") narma<-narma+1
    shift<-coefs[-(1:narma)]
    holidayshift<-rep(1,30)
    for(fi in 7:16)
    {
        holidayshift<-holidayshift*exp(xregpre[,fi]*shift[fi])
    }
    print(holidayshift)
    monthshift<-rep(1,30)
    for(fi in 17:NCOL(shift))
    {
        monthshift<-monthshift*exp(xregpre[,fi]*shift[fi])
    }
    print(monthshift)
    tsst.arima<-stlf(tsdata,h=30,s.window=7,method='arima',ic='bic',xreg=xregfit,newxreg=xregpre)
    print(exp(tsst.arima$mean))
    result.stlf.arima<-result.stlf.arima+exp(tsst.arima$mean)
    fittedValue.stlf<-fittedValue.stlf+exp(fitted(tsst.arima))
}
print(result)
total.data<-ts(c(od[fitbeg:preend,2]),fre=7)               
total.fore<-ts(c(fittedValue,result),fre=7)    
total.data.xts<-xts(total.data,seq(as.POSIXct("2014-04-01"),len=length(total.data),by='day'))
total.fore.xts<-xts(total.fore,seq(as.POSIXct("2014-04-01"),len=length(total.fore),by='day'))

------

library("forecast")
library("TSA")
library("xts")
library("wmtsa")
library("lmtest")
od<-read.csv("redeem_by_type.csv")
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



cat("... begin to loop ...")



par(mfcol=c(2,2))
zfb1<-log(exp(tftobal_amt)+exp(consume))
card1<-log(exp(card1))
card2<-log(exp(card2)+exp(card3))
type<-list("total_redeem_amt","zfb1","card1","card2")
tslist<-list(total_redeem_amt,zfb1,card1,card2)
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
orderlist<-list(c(0,0,0),c(4,1,1),c(3,1,1),c(3,1,1))

result<-rep(0,30)
fittedValue<-rep(0,fitend-fitbeg+1)
result.st<-rep(0,30)
fittedValue.st<-rep(0,fitend-fitbeg+1)
#(1)report_date, (2)w1, (3)w2, (4)w3, (5)w4, (6)w5, (7)w6, (8)mb1, (9)mb2, (10)mb3, (11)mb4, (12)mb5, (13)ma1, (14)ma2, (15)ma3, (16)ma4, (17)ma5, (18)fb1, (19)fb2, (20)fb3, (21)fb4, (22)fb5, (23)fb6, (24)fb7, (25)fi1, (26)fi2, (27)fi3, (28)fa1, (29)fa2, (30)fa3, (31)fa4, (32)fa5, (33)fa6, (34)fa7, (35)tx, (36)sb1, (37)sb2, (38)sb3, (39)sb4, (40)sb5, (41)sb6, (42)sb7, (43)si1, (44)sa1, (45)sa2, (46)sa3, (47)sa4, (48)sa5, (49)sa6, (50)sa7, (51)cb1, (52)cb2, (53)cb3, (54)cb4, (55)cb5, (56)cb6, (57)cb7, (58)ci1, (59)ci2, (60)ci3, (61)ci4, (62)ci5, (63)ci6, (64)ci7, (65)ca1, (66)ca2, (67)ca3, (68)ca4, (69)ca5, (70)ca6, (71)ca7
#5
xregfit<-data.frame(of[fitbeg:fitend,2:17],of[fitbeg:fitend,20:32],of[fitbeg:fitend,35],of[fitbeg:fitend,38:48]);
xregpre<-data.frame(of[prebeg:preend,2:17],of[prebeg:preend,20:32],of[prebeg:preend,35],of[prebeg:preend,38:48]);

for(ti in 2:4)
{
    st<-0.05
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
    cat("========>",legend,"<======\n")
        tsam.bestfit<-arima(ts,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE);
        fittedValueTmp<-fitted(tsam.bestfit)
        print(tsam.bestfit);
    detectAO(tsam.bestfit);
    detectIO(tsam.bestfit);
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
        tsam.p<-predict(tsam.bestfit,newxreg =xregpre);
    tsamp<-tsam.p$pred;
    print(exp(tsamp))
    result<-result+exp(tsamp)
    fittedValue<-fittedValue+exp(fittedValueTmp)
    tsam.data<-ts(c(exp(ts),od[prebeg:preend,2]),fre=7)               
    tsam.fore<-ts(exp(c(fittedValueTmp,tsamp)),fre=7)    
    tsam.data.xts<-xts(tsam.data,seq(as.POSIXct("2014-04-01"),len=length(tsam.data),by='day'))
    tsam.fore.xts<-xts(tsam.fore,seq(as.POSIXct("2014-04-01"),len=length(tsam.fore),by='day'))
    plot(as.zoo(cbind(tsam.data.xts,tsam.fore.xts)),col=1:2,lty=1:2,screens=1,xlab="Time")
    legend(x="topright",legend=c("Observed",legend),lty=1:2,col=1:2)
    #######
    ########
}
print(result)
total.data<-ts(od[fitbeg:preend,2],fre=7)               
total.fore<-ts(c(fittedValue,result),fre=7)    
total.data.xts<-xts(total.data,seq(as.POSIXct("2014-04-01"),len=length(total.data),by='day'))
total.fore.xts<-xts(total.fore,seq(as.POSIXct("2014-04-01"),len=length(total.fore),by='day'))
plot(as.zoo(cbind(total.data.xts,total.fore.xts)),col=1:2,lty=1:2,screens=1,xlab="Time")
legend(x="topright",legend=c("Observed","total"),lty=1:2,col=1:2)

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
  as.integer(1333260561)
, as.integer(1046648871)
, as.integer(1016650217)
, as.integer(937759768 )
, as.integer(900124332 )
, as.integer(500416478 )
, as.integer(546486659 )
, as.integer(795909492 )
, as.integer(1046648871)
, as.integer(1016650217)
, as.integer(1011053652)
, as.integer(816678163 )
, as.integer(571509319 )
, as.integer(620168922 )
, as.integer(1015550797)
, as.integer(1046648871)
, as.integer(1016650217)
, as.integer(937759768 )
, as.integer(816678163 )
, as.integer(571509319 )
, as.integer(620168922 )
, as.integer(1015550797)
, as.integer(1046648871)
, as.integer(1016650217)
, as.integer(937759768 )
, as.integer(816678163 )
, as.integer(571509319 )
, as.integer(813148230 )
, as.integer(1015550797)
, as.integer(1098260351)
)
redeem= c(  
  as.integer(1156544184)
, as.integer(1091257845)
, as.integer(1003655196)
, as.integer(979681582 )
, as.integer(926459683 )
, as.integer(462698320 )
, as.integer(489422265 )
, as.integer(846343419 )
, as.integer(1091257845)
, as.integer(1003655196)
, as.integer(979681582 )
, as.integer(882550873 )
, as.integer(645382706 )
, as.integer(703338919 )
, as.integer(1156544184)
, as.integer(1091257845)
, as.integer(1003655196)
, as.integer(979681582 )
, as.integer(882550873 )
, as.integer(645382706 )
, as.integer(703338919 )
, as.integer(1156544184)
, as.integer(1091257845)
, as.integer(1003655196)
, as.integer(979681582 )
, as.integer(882550873 )
, as.integer(645382706 )
, as.integer(703338919 )
, as.integer(1156544184)
, as.integer(1225839492)
)
