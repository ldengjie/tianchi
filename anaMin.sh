#!/bin/sh
SUB=_21310
grep "forecastchi2" ./[0-9]0*${SUB}.log >f0${SUB}
grep "forecastchi2" ./[0-9]1*${SUB}.log >f1${SUB}
echo f0
rf0=`awk 'BEGIN{min=1.e+50;date=0}{if($5<min){min=$5;date=$1;}}END{print min,date}' f0${SUB}`
echo $rf0
ff0=`echo $rf0 |awk -F '/|:' '{print $2}'`
grep "optimal" $ff0
echo f1
rf1=`awk 'BEGIN{min=1.e+50;date=0}{if($5<min){min=$5;date=$1;}}END{print min,date}' f1${SUB}`
echo $rf1
ff1=`echo $rf1 |awk -F '/|:' '{print $2}'`
grep "optimal" $ff1
