#######################
182.398575
#######################
> datawork<-read.csv("48w.csv")
> model1 = lm(total_purchase_amt ~ mon + tue + wed + thu + fri + sat + sun,dataweek)
> d1.m1 <-predict(model1,dataweek9,interval ="prediction",level=0.95,se.fit=FALSE)
Warning message:
In predict.lm(model1, dataweek9, interval = "prediction", level = 0.95,  :
                prediction from a rank-deficient fit may be misleading
> d1.m1
---
> model11 = lm(log(total_purchase_amt) ~ mon + tue + wed + thu + fri + sat + sun,dataweek)
> d1.m11 <-predict(model11,dataweek9,interval ="prediction",level=0.95,se.fit=FALSE)
Warning message:
In predict.lm(model11, dataweek9, interval = "prediction", level = 0.95,  :
                prediction from a rank-deficient fit may be misleading
> exp(d1.m11)
---
结果取 7.1日结果和 d1.m1的平均
> d1m1t<-ts(d1.m1)
>d1l.m11<-arimax(d1l,order=c(4,0,1),seasonal=list(order=c(3,1,2),period=7),fixed=c(NA,0,NA,NA,0,0,NA,NA,NA,0),transform.pars = FALSE)
> d1l.m11.p<-predict(d1l.m11,30)       
> d1l.m11p<-ts(exp(d1l.m11.p$pred))
> (d1m1t+d1l.m11p)/2
#######################
> model2 = lm(total_redeem_amt ~ mon + tue + wed + thu + fri + sat + sun,dataweek)
> d1.m2 <-predict(model2,dataweek9,interval ="prediction",level=0.95,se.fit=FALSE)
Warning message:
In predict.lm(model2, dataweek9, interval = "prediction", level = 0.95,  :
                prediction from a rank-deficient fit may be misleading
> d1.m2
---
> model21 = lm(log(total_redeem_amt) ~ mon + tue + wed + thu + fri + sat + sun,dataweek)
> d1.m21 <-predict(model21,dataweek9,interval ="prediction",level=0.95,se.fit=FALSE)
Warning message:
In predict.lm(model21, dataweek9, interval = "prediction", level = 0.95,  :
                prediction from a rank-deficient fit may be misleading
> exp(d1.m21)
---
结果取 7.1日结果和 d1.m1的平均
> d1m2t<-ts(d1.m2)
> d2l.m11.p<-predict(d2l.m11,30)       
> d2l.m11p<-ts(exp(d2l.m11.p$pred))
> (d1m2t+d2l.m11p)/2
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
  as.integer(968382471 )
, as.integer(1058906683)
, as.integer(976494792 )
, as.integer(915107679 )
, as.integer(820875307 )
, as.integer(549600518 )
, as.integer(600495517 )
, as.integer(963403407 )
, as.integer(1046232933)
, as.integer(967842819 )
, as.integer(906095466 )
, as.integer(817264444 )
, as.integer(550831051 )
, as.integer(596218252 )
, as.integer(968600749 )
, as.integer(1033548676)
, as.integer(961068441 )
, as.integer(897512447 )
, as.integer(807917543 )
, as.integer(544927985 )
, as.integer(598761202 )
, as.integer(964509863 )
, as.integer(1033757280)
, as.integer(958655495 )
, as.integer(894609707 )
, as.integer(812516470 )
, as.integer(549107020 )
, as.integer(604432391 )
, as.integer(964589505 )
, as.integer(1036825817)
)
redeem= c(  
  as.integer(1168301871)
, as.integer(1128015656)
, as.integer(982359314 )
, as.integer(965905625 )
, as.integer(873309332 )
, as.integer(582067882 )
, as.integer(667004681 )
, as.integer(1119139371)
, as.integer(1074299390)
, as.integer(988653283 )
, as.integer(974670793 )
, as.integer(858220988 )
, as.integer(582067882 )
, as.integer(667004681 )
, as.integer(1119139371)
, as.integer(1074299390)
, as.integer(988653283 )
, as.integer(974670793 )
, as.integer(858220988 )
, as.integer(582067882 )
, as.integer(667004681 )
, as.integer(1119139371)
, as.integer(1074299390)
, as.integer(988653283 )
, as.integer(974670793 )
, as.integer(858220988 )
, as.integer(582067882 )
, as.integer(667004681 )
, as.integer(1119139371)
, as.integer(1074299390)
)
