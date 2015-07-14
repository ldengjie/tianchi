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

cat("... begin to loop ...")

#xregpre<-rep(1,30);
#total_redeem_amt <-ts(total_redeem_amt ,fre=7)
#zfb1             <-ts(zfb1             ,fre=7)
#card1            <-ts(card1            ,fre=7)
#card2            <-ts(card2            ,fre=7)


par(mfcol=c(2,2))
zfb1<-log(exp(tftobal_amt)+exp(consume))
card1<-log(exp(card1))
card2<-log(exp(card2)+exp(card3))
type<-list("total_redeem_amt","zfb1","card1","card2")
tslist<-list(total_redeem_amt,zfb1,card1,card2)
#orignal+features
sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
#orderlist<-list(c(0,0,0),c(2,0,8),c(3,0,12),c(1,0,13))
#orderlist<-list(c(0,0,0),c(1,0,2),c(3,1,1),c(5,1,1))
orderlist<-list(c(0,0,0),c(4,1,1),c(3,1,1),c(3,1,1))
#type<-list("total_redeem_amt","zfb1","card1","card2","card3")
#tslist<-list(total_redeem_amt,zfb1,card1,card2,card3)
#sorderlist<-list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
#orderlist<-list(c(0,0,0),c(1,0,2),c(3,1,1),c(1,0,1),c(5,1,1))
#IOlist<-list(c(0),c(113),c(0),c(0),c(0));
#AOlist<-list(c(0),c(113),c(0),c(0),c(0));
#orignal
#orderlist<-list(c(0,0,0),c(2,0,4),c(5,0,1),c(5,0,2))
#sorderlist<-list(c(0,0,0),c(1,1,0),c(0,1,1),c(0,1,1))

result<-rep(0,30)
fittedValue<-rep(0,fitend-fitbeg+1)
result.st<-rep(0,30)
fittedValue.st<-rep(0,fitend-fitbeg+1)
#xregfit<-data.frame(of[fitbeg:fitend,2:(NCOL(of)-21)]);
#xregpre<-data.frame(of[prebeg:preend,2:(NCOL(of)-21)]);
#(1)report_date, (2)w1, (3)w2, (4)w3, (5)w4, (6)w5, (7)w6, (8)mb1, (9)mb2, (10)mb3, (11)mb4, (12)mb5, (13)ma1, (14)ma2, (15)ma3, (16)ma4, (17)ma5, (18)fb1, (19)fb2, (20)fb3, (21)fb4, (22)fb5, (23)fb6, (24)fb7, (25)fi1, (26)fi2, (27)fi3, (28)fa1, (29)fa2, (30)fa3, (31)fa4, (32)fa5, (33)fa6, (34)fa7, (35)tx, (36)sb1, (37)sb2, (38)sb3, (39)sb4, (40)sb5, (41)sb6, (42)sb7, (43)si1, (44)sa1, (45)sa2, (46)sa3, (47)sa4, (48)sa5, (49)sa6, (50)sa7, (51)cb1, (52)cb2, (53)cb3, (54)cb4, (55)cb5, (56)cb6, (57)cb7, (58)ci1, (59)ci2, (60)ci3, (61)ci4, (62)ci5, (63)ci6, (64)ci7, (65)ca1, (66)ca2, (67)ca3, (68)ca4, (69)ca5, (70)ca6, (71)ca7
#1
#xregfit<-data.frame(of[fitbeg:fitend,2:17],of[fitbeg:fitend,24:28],of[fitbeg:fitend,35],of[fitbeg:fitend,42:44]);
#xregpre<-data.frame(of[prebeg:preend,2:17],of[prebeg:preend,24:28],of[prebeg:preend,35],of[prebeg:preend,42:44]);
#2
#xregfit<-data.frame(of[fitbeg:fitend,2:17],of[fitbeg:fitend,23:29],of[fitbeg:fitend,35],of[fitbeg:fitend,41:45]);
#xregpre<-data.frame(of[prebeg:preend,2:17],of[prebeg:preend,23:29],of[prebeg:preend,35],of[prebeg:preend,41:45]);
#3
#xregfit<-data.frame(of[fitbeg:fitend,2:17],of[fitbeg:fitend,22:30],of[fitbeg:fitend,35],of[fitbeg:fitend,40:46]);
#xregpre<-data.frame(of[prebeg:preend,2:17],of[prebeg:preend,22:30],of[prebeg:preend,35],of[prebeg:preend,40:46]);
#xregfit<-data.frame(of[fitbeg:fitend,2:7],of[fitbeg:fitend,10:15],of[fitbeg:fitend,22:30],of[fitbeg:fitend,35],of[fitbeg:fitend,40:46]);
#xregpre<-data.frame(of[prebeg:preend,2:7],of[prebeg:preend,10:15],of[prebeg:preend,22:30],of[prebeg:preend,35],of[prebeg:preend,40:46]);
#4
#xregfit<-data.frame(of[fitbeg:fitend,2:17],of[fitbeg:fitend,21:31],of[fitbeg:fitend,35],of[fitbeg:fitend,39:47]);
#xregpre<-data.frame(of[prebeg:preend,2:17],of[prebeg:preend,21:31],of[prebeg:preend,35],of[prebeg:preend,39:47]);
#5
xregfit<-data.frame(of[fitbeg:fitend,2:17],of[fitbeg:fitend,20:32],of[fitbeg:fitend,35],of[fitbeg:fitend,38:48]);
xregpre<-data.frame(of[prebeg:preend,2:17],of[prebeg:preend,20:32],of[prebeg:preend,35],of[prebeg:preend,38:48]);
#6
#xregfit<-data.frame(of[fitbeg:fitend,2:17],of[fitbeg:fitend,19:33],of[fitbeg:fitend,35],of[fitbeg:fitend,37:49]);
#xregpre<-data.frame(of[prebeg:preend,2:17],of[prebeg:preend,19:33],of[prebeg:preend,35],of[prebeg:preend,37:49]);
#7
#xregfit<-data.frame(of[fitbeg:fitend,2:17],of[fitbeg:fitend,18:34],of[fitbeg:fitend,35],of[fitbeg:fitend,36:50]);
#xregpre<-data.frame(of[prebeg:preend,2:17],of[prebeg:preend,18:34],of[prebeg:preend,35],of[prebeg:preend,36:50]);

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
#for(ti in 2:5)
{
    #st<-seq(0,0.4,0.05);
    st<-0.05
    ts<-tslist[[ti]]
    #iov<-IOlist[[ti]]
    or<-orderlist[[ti]]
    sor<-sorderlist[[ti]]
    legend<-type[[ti]]
    aicb<-Inf;
    #vm<-list(model="sGARCH",garchOrder=c(7,6),submodel = NULL, external.regressors = NULL, variance.targeting = FALSE)
    #mm<-list(armaOrder=c(5,1),include.mean=FALSE,external.regressors=as.matrix(xregfit))
    #mm<-list(armaOrder=c(5,1))
    #spec<-ugarchspec(variance.mode=vm,mean.mode=mm,distribution.mode="norm",start.pars = list(), fixed.pars = list())
    #ts.igar=ugarchfit(data=ts,spec=spec,solver.control=list(trace=0))
    #next
    for(si in st)
    {
        if(si==0) next;
        #tsam<-auto.arima(ts,xreg=xregfit)
        #tsam<-arima(ts,order=or,seasonal=list(order=sor,period=7),xreg=xregfit)
        #tsam<-arima(ts,order=or,seasonal=list(order=sor,period=7))
        #if(ti==3)
        #{
            #tsam<-arima(ts,order=or,xreg=data.frame(xregfit,AO=1*(seq(ts)==150)))
        #}else if(ti==3)
        #{
            #tsam<-arima(ts,order=or,xreg=xregfit,io=c(50))
        #}else
        #{
            tsam<-arima(ts,order=or,xreg=xregfit)
        #}
        #tsam<-arima(ts,order=or,xreg=xregfit)
        aico<-tsam$aic;
        fixedv<-rep(NA,length(tsam$coef));
        needFix<-TRUE;
        cat(si,"---------------------------->\n");
        #print(tsam)
        while(needFix)
        {
            needFix<-FALSE;
            #hasNaN<-FALSE;
            #for (ci in 1:length(tsam$coef))
            #{
                ##if(is.na(coeftest(tsam)[ci,4])) 
                #if(is.na(sqrt(diag(tsam$var.coef))[ci])) 
                #{
                    #hasNaN<-TRUE;
                    #break; 
                #}
            #}
            #if(!hasNaN)
                ##if(0)
            #{
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
            #}
            if(needFix)
            {
                cat("Fixing...\n")
                #tsam<-arima(ts,order=or,seasonal=list(order=sor,period=7),xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
                #tsam<-arimax(ts,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE,io=iov)
                #tsam<-arima(ts,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
                #tsam<-arima(ts,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE,io=c(33,71))
                #if(ti==3)
                #{
                    #tsam<-arima(ts,order=or,xreg=data.frame(xregfit,AO=1*(seq(ts)==150)),fixed=fixedv,transform.pars = FALSE)
                #}else if(ti==3)
                #{
                    #tsam<-arima(ts,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE,io=c(50))
                #}else
                #{
                    tsam<-arima(ts,order=or,xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
                #}
                #tsam<-arima(ts,order=or,seasonal=list(order=sor,period=7),fixed=fixedv,transform.pars = FALSE)
                #print(tsam)
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
    #tsam.bestfit<-tsam;
    #tsam.bestfit<-arima(ts,order=or,seasonal=list(order=sor,period=7),xreg=xregfit,fixed=fixb,transform.pars = FALSE);
    cat("========>",legend,"<======\n")
    #if(ti==3)
    #{
        #tsam.bestfit<-arima(ts,order=or,xreg=data.frame(xregfit,AO=1*(seq(ts)==150)),fixed=fixb,transform.pars = FALSE);
        #fittedValueTmp<-fitted(tsam.bestfit)
        #print(tsam.bestfit);
        #tsam.bestfit$coef<-tsam.bestfit$coef[1:(length(tsam.bestfit$coef)-2)]
    #}else if(ti==3)
    #{
        #tsam.bestfit<-arima(ts,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE,io=c(50));
        #fittedValueTmp<-fitted(tsam.bestfit)
        #print(tsam.bestfit);
        #tsam.bestfit$coef<-tsam.bestfit$coef[1:(length(tsam.bestfit$coef)-1)]
    #}else
    #{
        tsam.bestfit<-arima(ts,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE);
        fittedValueTmp<-fitted(tsam.bestfit)
        print(tsam.bestfit);
    #}
    #tsam.bestfit<-arima(ts,order=or,xreg=xregfit,fixed=fixb,transform.pars = FALSE);
    #tsam.bestfit<-arima(ts,order=or,seasonal=list(order=sor,period=7),xreg=xregfit);
    #tsam.bestfit<-arima(ts,order=or,seasonal=list(order=sor,period=7),fixed=fixb,transform.pars = FALSE);
    #tsam.bestfit<-arima(ts,order=or,seasonal=list(order=sor,period=7));
    detectAO(tsam.bestfit);
    detectIO(tsam.bestfit);
    cat("si=",ki,"aicb= ",aico,"->",aicb,"\n")
    #if(ti==3)
    #{
        #tsam.p<-predict(tsam.bestfit,newxreg =data.frame(xregpre,rep(0,30)));
    #}else
    #{
        tsam.p<-predict(tsam.bestfit,newxreg =xregpre);
    #}
    #tsam.p<-predict(tsam.bestfit,n.ahead=30);
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
    #acf(residuals(tsam.bestfit))
    #pacf(residuals(tsam.bestfit))
    #acf(diff(residuals(tsam.bestfit)))
    #pacf(diff(residuals(tsam.bestfit)))
    #acf(diff(residuals(tsam.bestfit),lag=7))
    #pacf(diff(residuals(tsam.bestfit),lag=7))
    #acf(abs(residuals(tsam.bestfit)))
    #pacf(abs(residuals(tsam.bestfit)))
    #acf(diff(abs(residuals(tsam.bestfit))))
    #pacf(diff(abs(residuals(tsam.bestfit))))
    #acf(residuals(tsam.bestfit)^2)
    #pacf(residuals(tsam.bestfit)^2)
    #######
    #tsst<-stlf(ts(ts[1:153],fre=7),h=30,s.window=7,method='arima',ic='bic',xreg=xregfit,newxreg=xregpre)
    #result.st<-result.st+exp(tsst$mean)
    #fittedValue.st<-fittedValue.st+exp(fitted(tsst))
    #plot(tsst)
    ########
    #tsm1<-msts(ts,seasonal.periods=c(7,30.44))
    #tstbats1<-tbats(tsm1)
    #tstbats71<-forecast(tstbats1,30)
    #plot(tstbats1)
}
print(result)
total.data<-ts(od[fitbeg:preend,2],fre=7)               
total.fore<-ts(c(fittedValue,result),fre=7)    
total.data.xts<-xts(total.data,seq(as.POSIXct("2014-04-01"),len=length(total.data),by='day'))
total.fore.xts<-xts(total.fore,seq(as.POSIXct("2014-04-01"),len=length(total.fore),by='day'))
plot(as.zoo(cbind(total.data.xts,total.fore.xts)),col=1:2,lty=1:2,screens=1,xlab="Time")
legend(x="topright",legend=c("Observed","total"),lty=1:2,col=1:2)

#print(result.st)
#total.data.st<-ts(c(exp(total_purchase_amt[fitbeg:fitend]),exp(total_purchase_amt[prebeg:preend])),fre=7)               
#total.fore.st<-ts(c(fittedValue.st,result.st),fre=7)    
#total.data.st.xts<-xts(total.data.st,seq(as.POSIXct("2014-04-01"),len=length(total.data.st),by='day'))
#total.fore.st.xts<-xts(total.fore.st,seq(as.POSIXct("2014-04-01"),len=length(total.fore.st),by='day'))
#plot(as.zoo(cbind(total.data.st.xts,total.fore.st.xts)),col=1:2,lty=1:2,screens=1,xlab="Time")
#legend(x="topright",legend=c("Observed","total"),lty=1:2,col=1:2)
