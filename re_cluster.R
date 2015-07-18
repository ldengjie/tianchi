library("TSclust")
d3<-read.csv("redeem_by_type_p1.csv") #4-35,40-81
d2<-read.csv("redeem_by_type_p2.csv") #4-35,40-81
d<-d3+d2
d1<-d[275:427,]
zfbm<-as.matrix(scale(cbind(
d1[,5 ], d1[,6 ], d1[,7 ], d1[,8 ], d1[,9 ], d1[,10], d1[,11], d1[,12], d1[,13], d1[,14], d1[,15], d1[,16], d1[,17], d1[,18], d1[,19], d1[,20], d1[,21], d1[,22], d1[,23], d1[,24], d1[,25], d1[,26], d1[,27], d1[,28], d1[,29], d1[,30], d1[,31], d1[,32], d1[,33], d1[,34], d1[,35]
#d1[,41], d1[,42], d1[,43], d1[,44], d1[,45], d1[,46], d1[,47], d1[,48], d1[,49], d1[,50], d1[,51], d1[,52], d1[,53], d1[,54], d1[,55], d1[,56], d1[,57], d1[,58], d1[,59], d1[,60], d1[,61], d1[,62], d1[,63], d1[,64], d1[,65], d1[,66], d1[,67], d1[,68], d1[,69], d1[,70], d1[,71], d1[,72], d1[,73], d1[,74], d1[,75], d1[,76], d1[,77], d1[,78], d1[,79], d1[,80], d1[,81]
                      )))
zfbmt<-t(zfbm)
cat("#EM\n")
fm<-Mclust(zfbmt,3)
print(fm$class)
cat("#HC-diss\n")
zfbmds<-diss(zfbmt,"DTWARP",p=0.05)               
zfbhc<-hclust(zfbmds)                             
print(cutree(zfbhc,k=3) )
cat("#HC\n")
zfbmtd<-dist(zfbmt)
zfbmtc<-hclust(zfbmtd,method="complete")
print(cutree(zfbmtc,k=3))
cat("#k-Means\n")
k<-kmeans(zfbmt,3)
print(k$cluster)
cat("#k-Medoids\n")
p<-pam(zfbmt,3)
print(p$clustering)
