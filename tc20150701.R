#######################
177.894426
#######################
data<-read.csv("48.csv");
t1<-ts(data$total_purchase_amt);
l1<-log(t1);
l1.m4<-arimax(l1,order=c(4,0,1),seasonal=list(order=c(3,1,2),period=7),fixed=c(NA,0,NA,NA,0,0,NA,NA,NA,0),transform.pars = FALSE)
#Call:
#arimax(x = d1l, order = c(4, 0, 1), seasonal = list(order = c(3, 1, 2), period = 7), 
#           transform.pars = FALSE, fixed = c(NA, 0, NA, NA, 0, 0, NA, NA, NA, 0))
#
#Coefficients:
#         ar1  ar2      ar3     ar4  ma1  sar1     sar2     sar3     sma1  sma2
#      0.4231    0  -0.1369  0.1993    0     0  -0.1124  -0.1360  -0.7856     0
#      s.e.  0.0768    0   0.0867  0.0828    0     0   0.0942   0.0948   0.0701     0
#
#      sigma^2 estimated as 0.01998:  log likelihood = 73.73,  aic = -135.46
l1.pre<-predict(l1.m4,30)
t1.pre<-exp(l1.pre$pred)
t1.pre
#######################
t2<-ts(data$total_redeem_amt)
l2<-log(t2)
l2.m21<-arimax(l2,order=c(0,0,6),seasonal=list(order=c(0,1,2),period=7),fixed=c(NA,NA,0,0,NA,0,NA,0))
#Call:
#arimax(x = d2l, order = c(0, 0, 6), seasonal = list(order = c(0, 1, 2), period = 7), 
#           fixed = c(NA, NA, 0, 0, NA, 0, NA, 0))
#
#Coefficients:
#         ma1     ma2  ma3  ma4     ma5  ma6     sma1  sma2
#      0.3808  0.3344    0    0  0.3608    0  -0.8291     0
#      s.e.  0.0687  0.0871    0    0  0.0903    0   0.0641     0
#
#      sigma^2 estimated as 0.02851:  log likelihood = 47.42,  aic = -86.84
l2.pre<-predict(l2.m21,30)
t2.pre<-exp(l2.pre$pred)
t2.pre
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
  as.integer(963553531 )
, as.integer(1052255567)
, as.integer(956438737 )
, as.integer(895846810 )
, as.integer(835861170 )
, as.integer(539254176 )
, as.integer(577618403 )
, as.integer(953595402 )
, as.integer(1026908067)
, as.integer(939134790 )
, as.integer(877822385 )
, as.integer(828639444 )
, as.integer(541715241 )
, as.integer(569063873 )
, as.integer(963990087 )
, as.integer(1001539552)
, as.integer(925586035 )
, as.integer(860656346 )
, as.integer(809945642 )
, as.integer(529909110 )
, as.integer(574149774 )
, as.integer(955808315 )
, as.integer(1001956761)
, as.integer(920760143 )
, as.integer(854850865 )
, as.integer(819143497 )
, as.integer(538267180 )
, as.integer(585492152 )
, as.integer(955967599 )
, as.integer(1008093835)
)
redeem= c(  
  as.integer(1214736687)
, as.integer(1207559503)
, as.integer(966511438 )
, as.integer(992637769 )
, as.integer(912562293 )
, as.integer(581618047 )
, as.integer(667023025 )
, as.integer(1116411687)
, as.integer(1100126971)
, as.integer(979099377 )
, as.integer(1010168105)
, as.integer(882385606 )
, as.integer(581618047 )
, as.integer(667023025 )
, as.integer(1116411687)
, as.integer(1100126971)
, as.integer(979099377 )
, as.integer(1010168105)
, as.integer(882385606 )
, as.integer(581618047 )
, as.integer(667023025 )
, as.integer(1116411687)
, as.integer(1100126971)
, as.integer(979099377 )
, as.integer(1010168105)
, as.integer(882385606 )
, as.integer(581618047 )
, as.integer(667023025 )
, as.integer(1116411687)
, as.integer(1100126971)
)
