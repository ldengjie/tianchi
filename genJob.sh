#!/bin/sh
for ver in  2
do
    for mothod in 4 
    do
        for cateNum in 3
        do
            for h618 in 2
            do
                for h9 in 0 1
                do
                    sub=`echo _${ver}${mothod}${cateNum}${h618}${h9}`
                    echo $sub
                    rm sub${sub}.sh
                    sed  "s/_21310/${sub}/g" anaMin.sh > anaMin${sub}.sh
                    for lowFitDate in 0312 0319 0326 0402 0409 0416 0423 0430 0507 0514 0521 0528 0604 0611 0618 0625 
                    do 
                        for boundary in 1 2 3 4 5 6
                        do
                            for category in 0 1
                            do
                                sed -e "s/Long64_t lowFitDate=2014[0-9][0-9][0-9][0-9]/Long64_t lowFitDate=2014${lowFitDate}/g" \
                                    -e "s#tianChiOriginData#~/file/tianchi/tianChiOriginData#g" \
                                    -e "s#int ver=[0-9]#int ver=${ver}#g" \
                                    -e "s#int mothod=[0-9]#int mothod=${mothod}#g" \
                                    -e "s#const int cateNum=[0-9]#const int cateNum=${cateNum}#g" \
                                    -e "s#int h618=[0-9]#int h618=${h618}#g" \
                                    -e "s#int h9=[0-9]#int h9=${h9}#g" \
                                    -e "s#businessBoundary=[0-9]#businessBoundary=${boundary}#g" \
                                    -e "s#anaRedeem=[0-9]#anaRedeem=${category}#g" \
                                    -e "s#result.eps#${boundary}${category}${lowFitDate}${sub}.eps#g" \
                                    ~/file/tianchi/mode.C > mode${boundary}${category}${lowFitDate}${sub}.C
                                sed -e "s/mode.C/mode${boundary}${category}${lowFitDate}${sub}.C/g" \
                                    -e "s/20.log/${boundary}${category}${lowFitDate}${sub}.log/g" \
                                    jobs.sh > job${boundary}${category}${lowFitDate}${sub}.sh
                                echo qsub -q dybshortq job${boundary}${category}${lowFitDate}${sub}.sh >> sub${sub}.sh
                            done
                        done
                    done
                done
            done

        done
    done
done
