#######################
174.42
nonseasonal arima + fourier regression(7+31)
#######################
library("forecast")
library("TSA")
library("xts")
data<-read.csv("48.csv");
d<-data.frame(data$total_purchase_amt,data$total_redeem_amt);
dstr<-c("Puchase /Day","Redeem /Day");
aorder<-cbind(c(4,0,1),c(0,0,6));
sorder<-cbind(c(3,1,2),c(0,1,2));
fixc<-list(c(NA,0,NA,NA,0,0,NA,NA,NA,0),c(NA,NA,0,0,NA,0,NA,0));
di<-1;
d17<-ts(d[di],fre=7)
d130<-ts(d[di],fre=30)
d131<-ts(d[di],fre=31)
d17lf<-ts(c(log(d17),rep(NA,30)),fre=7)
d130lf<-ts(c(log(d130),rep(NA,30)),fre=30)
d131lf<-ts(c(log(d131),rep(NA,30)),fre=31)
d17l<-log(d17);
##arima
cat("################## seasonal arima \n")
dl1.fm.bestfit<- list(aicc=Inf);
for(i in 1:3){  
    for(j in 1:15){
        dl1.f7t<-arimax(d17l,order=aorder[,di],xreg=data.frame(fourier(d17lf,K=i)[1:153,],fourier(d131lf,K=j)[1:153,]));
        #cat("i:",i," j:",j," aic:",dl1.f7t$aic,"\n")
        if(dl1.f7t$aic<dl1.fm.bestfit$aic)
        {
            dl1.fm.bestfit<-dl1.f7t;
            ki<-i;
            kj<-j;
        }
    }
}
fixedv<-rep(NA,length(dl1.fm.bestfit$coef));
needFix<-TRUE;
while(needFix)
{
    dl1.fm.bestfit<-arimax(d17l,order=aorder[,di],xreg=data.frame(fourier(d17lf,K=ki)[1:153,],fourier(d131lf,K=kj)[1:153,]),fixed=fixedv,transform.pars = FALSE)                                          
    needFix<-FALSE;
    for (ci in 1:length(dl1.fm.bestfit$coef))
    {
        if((dl1.fm.bestfit$coef[ci]!=0)&&(abs(confint(dl1.fm.bestfit)[ci,2]-confint(dl1.fm.bestfit)[ci,1])/4/0.978 - abs(dl1.fm.bestfit$coef[ci])) >= 0 )
        {
            fixedv[ci]=0;
            needFix<-TRUE;
        }
    }
}
dl1.fm.p<-predict(dl1.fm.bestfit,newxreg = data.frame(fourier(d17lf,K=ki)[154:183,],fourier(d131lf,K=kj)[154:183,]));
dl1.fmp<-exp(dl1.fm.p$pred);
dl1.fmp;
cat("ki:",ki," kj:",kj," aic:",dl1.fm.bestfit$aic,"\n");
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
  as.integer(1180381575)
, as.integer(1064574512)
, as.integer(860561455 )
, as.integer(1052303656)
, as.integer(976738363 )
, as.integer(568926324 )
, as.integer(646884436 )
, as.integer(1014059747)
, as.integer(1004322887)
, as.integer(1036688482)
, as.integer(1066106389)
, as.integer(878254342 )
, as.integer(560539612 )
, as.integer(602350336 )
, as.integer(1047810340)
, as.integer(1187820332)
, as.integer(1052689207)
, as.integer(992742234 )
, as.integer(817188215 )
, as.integer(525731783 )
, as.integer(617134497 )
, as.integer(987444219 )
, as.integer(993615329 )
, as.integer(921651529 )
, as.integer(865749385 )
, as.integer(742513501 )
, as.integer(520475770 )
, as.integer(596035247 )
, as.integer(970848885 )
, as.integer(994072925 )
)
redeem= c(  
  as.integer(1095561249)
, as.integer(850339529 )
, as.integer(810840486 )
, as.integer(937058716 )
, as.integer(860522866 )
, as.integer(522514117 )
, as.integer(595989838 )
, as.integer(1069888306)
, as.integer(968865849 )
, as.integer(887379482 )
, as.integer(909413357 )
, as.integer(835201428 )
, as.integer(539676357 )
, as.integer(609467216 )
, as.integer(1080481326)
, as.integer(1034776281)
, as.integer(947235844 )
, as.integer(882032932 )
, as.integer(803511657 )
, as.integer(580111498 )
, as.integer(666492755 )
, as.integer(1077788750)
, as.integer(1061556470)
, as.integer(1116112393)
, as.integer(1040509511)
, as.integer(839943240 )
, as.integer(606904178 )
, as.integer(777640729 )
, as.integer(1253021969)
, as.integer(1093658121)
)
1095561249
850339529 
810840486 
937058716 
860522866 
522514117 
595989838 
1069888306 
968865849 
887379482 
909413357 
835201428 
539676357 
609467216 
1080481326 
1034776281 
947235844 
882032932 
803511657 
580111498 
666492755 
1077788750 
1061556470 
1116112393 
1040509511 
839943240 
606904178 
777640729 
1253021969
1093658121
