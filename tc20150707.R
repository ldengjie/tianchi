#######################
184.782353
拟合w,mb,ma,fa,fi,fb,sb,si,sa + arima白噪声
#######################
library("forecast")
library("TSA")
library("xts")
require("lmtest")
data<-read.csv("456789fs.csv")
startdate<-'2014-04-01';
#data<-read.csv("totalfs.csv")
#startdate<-'2013-07-01';
fs<-data.frame(data[,4:NCOL(data)]);
dstr<-c("Puchase /Day","Redeem /Day");
iaorder<-cbind(c(4,0,5),c(4,0,9));
di<-2;
d17<-ts(data[1:(NROW(data)-30),(di+1)],fre=7)
d17l<-log(d17);
lm2<-lm(log(total_purchase_amt)~ w1+w2+w3+w4+w5+w6+mb1+mb2+mb3+mb4+mb5+ma1+ma2+ma3+ma4+ma5+fb1+fb2+fb3+fb4+fb5+fi1+fi2+fi3+fa1+fa2+fa3+fa4+fa5+sb1+sb2+sb3+sb4+sb5+si1+sa1+sa2+sa3+sa4+sa5 ,data=data)
ts.plot(d17l,fitted(lm2));
r<-d17l-fitted(lm2);
le<-NROW(d17l);
cat("################## seasonal arima \n")
aicb<-Inf;
xregfit<-data.frame(fs[1:le,]);
#st<-seq(0,1,0.05);
st<-0.4;#0.4 for redeem ,0.25 for purchase, no need to loop again
for(si in st)
{
    d1l.f7t<-arimax(d17l,order=iaorder[,di],xreg=xregfit)
    fixedv<-rep(NA,length(d1l.f7t$coef));
    needFix<-TRUE;
    cat(si);
    while(needFix)
    {
        #print(d1l.f7t);
        needFix<-FALSE;
        for (ci in 1:length(d1l.f7t$coef))
        {
            if((d1l.f7t$coef[ci]!=0)&&coeftest(d1l.f7t)[ci,4]>si)
            {
                fixedv[ci]=0;
                needFix<-TRUE;
            }
        }
        if(needFix)
        {
            d1l.f7t<-arimax(d17l,order=iaorder[,di],xreg=xregfit,fixed=fixedv,transform.pars = FALSE)
        }
    }
    print(d1l.f7t)
    if(d1l.f7t$aic<aicb)
    {
        ki<-si;
        fixb<-fixedv;
        aicb<-d1l.f7t$aic;
    }
}
cat("si=",ki,"aicb=",aicb)
#d1l.fm.bestfit<-d1l.f7t;
d1l.fm.bestfit<-arimax(d17l,order=iaorder[,di],xreg=xregfit,fixed=fixb,transform.pars = FALSE);
print(d1l.fm.bestfit);
xregpre<-data.frame(fs[(le+1):(le+30),]);
d1l.fm.p<-predict(d1l.fm.bestfit,newxreg =xregpre);
d1l.fmp<-d1l.fm.p$pred;
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
  as.integer(1424330655)
, as.integer(948970186 )
, as.integer(1133010752)
, as.integer(1019557453)
, as.integer(800775732 )
, as.integer(412968763 )
, as.integer(437391405 )
, as.integer(769252437 )
, as.integer(1078715258)
, as.integer(944652716 )
, as.integer(974991675 )
, as.integer(820022685 )
, as.integer(546274879 )
, as.integer(579376947 )
, as.integer(932137311 )
, as.integer(948970186 )
, as.integer(944652716 )
, as.integer(864499985 )
, as.integer(753307152 )
, as.integer(546274879 )
, as.integer(579376947 )
, as.integer(932137311 )
, as.integer(948970186 )
, as.integer(944652716 )
, as.integer(864499985 )
, as.integer(842292127 )
, as.integer(588796256 )
, as.integer(665076961 )
, as.integer(1053502110)
, as.integer(994401992 )
)
redeem= c(  
  as.integer(1087965443)
, as.integer(977275356 )
, as.integer(1188216915)
, as.integer(924895928 )
, as.integer(817789483 )
, as.integer(406694023 )
, as.integer(473761318 )
, as.integer(844949681 )
, as.integer(1032839251)
, as.integer(953662742 )
, as.integer(924895928 )
, as.integer(772479596 )
, as.integer(540888604 )
, as.integer(679166177 )
, as.integer(1087965443)
, as.integer(1032839251)
, as.integer(953662742 )
, as.integer(924895928 )
, as.integer(861677094 )
, as.integer(602475154 )
, as.integer(679166177 )
, as.integer(1087965443)
, as.integer(1032839251)
, as.integer(953662742 )
, as.integer(924895928 )
, as.integer(971308704 )
, as.integer(641059613 )
, as.integer(846207684 )
, as.integer(1160266084)
, as.integer(1052034211)
)
