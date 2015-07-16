#######################
172.831019
#######################
拟合w,mb,ma,fa,fi,fb,sb,si,sa + arima白噪声,跟07一样，只是修正了P value计算,导致purchase si 0.25->0.65,可能是这个原因，虽然aic高了，但系数显著性下降。
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
  as.integer(1379229868)
, as.integer(882942751 )
, as.integer(1080865456)
, as.integer(968391205 )
, as.integer(746843859 )
, as.integer(386224658 )
, as.integer(413428090 )
, as.integer(709099844 )
, as.integer(1039236546)
, as.integer(905299340 )
, as.integer(922117451 )
, as.integer(785460476 )
, as.integer(544938101 )
, as.integer(551877983 )
, as.integer(893643902 )
, as.integer(909262467 )
, as.integer(897128459 )
, as.integer(820188445 )
, as.integer(711643777 )
, as.integer(513367770 )
, as.integer(546896945 )
, as.integer(885578218 )
, as.integer(901055816 )
, as.integer(889031324 )
, as.integer(812785741 )
, as.integer(755651453 )
, as.integer(485131159 )
, as.integer(598377496 )
, as.integer(997343486 )
, as.integer(931725922 )
)
redeem= c(  
  as.integer(1092939274)
, as.integer(967828058 )
, as.integer(1166344099)
, as.integer(929927124 )
, as.integer(830084011 )
, as.integer(400590975 )
, as.integer(485316299 )
, as.integer(869056074 )
, as.integer(1114265768)
, as.integer(962058514 )
, as.integer(929927124 )
, as.integer(762726529 )
, as.integer(549229842 )
, as.integer(678404234 )
, as.integer(1092939274)
, as.integer(1031779901)
, as.integer(962058514 )
, as.integer(929927124 )
, as.integer(864189330 )
, as.integer(605286329 )
, as.integer(678404234 )
, as.integer(1092939274)
, as.integer(1031779901)
, as.integer(962058514 )
, as.integer(929927124 )
, as.integer(970493279 )
, as.integer(641207896 )
, as.integer(822458057 )
, as.integer(1158840548)
, as.integer(1058759999)
)

1379229868
882942751
1080865456
968391205
746843859
386224658
413428090
709099844
1039236546
905299340
922117451
785460476
544938101
551877983
893643902
909262467
897128459
820188445
711643777
513367770
546896945
885578218
901055816
889031324
812785741
755651453
485131159
598377496
997343486
931725922

1092939274
967828058
1166344099
929927124
830084011
400590975
485316299
869056074
1114265768
962058514
929927124
762726529
549229842
678404234
1092939274
1031779901
962058514
929927124
864189330
605286329
678404234
1092939274
1031779901
962058514
929927124
970493279
641207896
822458057
1158840548
1058759999

