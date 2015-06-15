{
    gStyle->SetTitleStyle(0); 
    gStyle->SetTitleBorderSize(0); 
    gStyle->SetOptStat(0);
    gStyle->SetLegendBorderSize(0);


    bool anaRedeem=0;//otherwise purchase 
    int businessBoundary=3;

    string anaStr[2]={"Purchase","Redeem"};
    cout<<"=== begin to analize [ "<< anaStr[anaRedeem]<<" ] ==="<<endl;
    cout<<"businessBoundary  : "<<businessBoundary<<endl;

    //Long64_t lowFitDate=20140312;
    Long64_t lowFitDate=20140312;
    Long64_t highFitDate=20140831;
    string fitFunc="expo";
    //string fitFunc="expo(0)+pol1(2)";
    int paraNum=2;
    double x[1000]={0.};
    double y[1000]={0.};
    double par[1000]={0.};

    Long64_t timeBinNum=490;//490 day,70 week,16 month
    Long64_t lowDate=20130602;
    Long64_t highDate=20141005;
    const int maxBinNum=500;//should be more than "timeBinNum"
    const int maxHolidayNum=30;
    const int maxMonthNum=30;
    const int maxPreAndAftNum=100;//maximum of pre,aft,length of  holiday(guoqing=7)

    int minPreAft=7;
    int maxPreAft=7;
    //int preMonth=5;
    //int aftMonth=5;
    //int preHoliday=5;
    //int aftHoliday=5;

    typedef struct ScanPara
    {
        double* ref;
        double low;
        double high;
        double step;
        double optVal;
        double minChi2ndf;
        int tag;
        ScanPara()
        {
            ref=NULL;
            low=-1;
            high=1;
            step=0.001;//
            minChi2ndf=999;
            optVal=0.;
        }
    }sc;
    sc sco[500];

    TFile* f=new TFile("tianChiOriginData.root","read");

    TTree* user_balance_table=(TTree*)f->Get("user_balance_table");
    Long64_t ubtNum=user_balance_table->GetEntries();
    Long64_t user_id             ;
    Long64_t report_date_unix    ;
    Long64_t tBalance            ;
    Long64_t yBalance            ;
    Long64_t total_purchase_amt  ;
    Long64_t purchase_bal_amt    ;
    Long64_t purchase_bank_amt   ;
    Long64_t total_redeem_amt    ;
    Long64_t consume_amt         ;
    Long64_t transfer_amt        ;
    Long64_t tftobal_amt         ;
    Long64_t tftocard_amt        ;
    Long64_t share_amt           ;
    Long64_t category1           ;
    Long64_t category2           ;
    Long64_t category3           ;
    Long64_t category4           ;
    Long64_t direct_purchase_amt ;
    user_balance_table->SetBranchAddress("user_id",&user_id);
    user_balance_table->SetBranchAddress("report_date_unix",&report_date_unix);
    user_balance_table->SetBranchAddress("tBalance",&tBalance);
    user_balance_table->SetBranchAddress("yBalance",&yBalance);
    user_balance_table->SetBranchAddress("total_purchase_amt",&total_purchase_amt);
    user_balance_table->SetBranchAddress("purchase_bal_amt",&purchase_bal_amt);
    user_balance_table->SetBranchAddress("purchase_bank_amt",&purchase_bank_amt);
    user_balance_table->SetBranchAddress("total_redeem_amt",&total_redeem_amt);
    user_balance_table->SetBranchAddress("consume_amt",&consume_amt);
    user_balance_table->SetBranchAddress("transfer_amt",&transfer_amt);
    user_balance_table->SetBranchAddress("tftobal_amt",&tftobal_amt); user_balance_table->SetBranchAddress("tftocard_amt",&tftocard_amt);
    user_balance_table->SetBranchAddress("share_amt",&share_amt);
    user_balance_table->SetBranchAddress("category1",&category1);
    user_balance_table->SetBranchAddress("category2",&category2);
    user_balance_table->SetBranchAddress("category3",&category3);
    user_balance_table->SetBranchAddress("category4",&category4);
    user_balance_table->SetBranchAddress("direct_purchase_amt",&direct_purchase_amt);

    TCanvas* c=new TCanvas("c","c",1800,900);
    c->Divide(3,2);


    //user category
    Long64_t totalBalance[28367]={0};
    Long64_t totalNum[28367]={0};
    Long64_t balancePerDay[28367]={0};
    double frePerDay[28367]={0};
    Long64_t usrJoinTime[28367]={0};
    //int lowStatDate=20140401;
    //int highStatDate=20140831;
    int lowStatDate=lowFitDate;
    int highStatDate=highFitDate;
    Long64_t lowStatTime=coverTime2unix(lowStatDate);
    Long64_t highStatTime=coverTime2unix(highStatDate);
    Long64_t statDays=(highStatTime-lowStatTime)/86400;
    for( int ubti=0 ; ubti<ubtNum ; ubti++ )
    {
        user_balance_table->GetEntry(ubti); 
        if( report_date_unix>=lowStatTime&&report_date_unix<=highStatTime )
        {
            totalBalance[user_id]+=direct_purchase_amt;
        }
        if( direct_purchase_amt>0 && report_date_unix>=lowStatTime&&report_date_unix<=highStatTime )
        {
            totalNum[user_id]+=1;
        }
        if( usrJoinTime[user_id]==0||(usrJoinTime[user_id]!=0&&usrJoinTime[user_id]>report_date_unix))
        {
            usrJoinTime[user_id]=report_date_unix;
        }
    }
    for( int upti=0 ; upti<28367 ; upti++ )
    {
        int voliadDays=statDays;
        if( usrJoinTime[upti]>lowStatTime&&usrJoinTime[upti]<highStatTime)
        {
            voliadDays-=(usrJoinTime[upti]-lowStatTime)/86400;
        }
        balancePerDay[upti]=totalBalance[upti]/voliadDays;
        frePerDay[upti]=(double)totalNum[upti]/voliadDays;
    }
    const int cateNum=3;
    Long64_t cateColor[cateNum]={2,3,4};
    Long64_t usrCategory[28367]={0};
    TH1D* hUsrCategory=new TH1D("hUsrCategory","hUsrCategory",cateNum,1,cateNum+1);
    for( int upti=0 ; upti<28367 ; upti++ )
    {
        if( frePerDay[upti]*31<1 && balancePerDay[upti]*31<100000 )
        {
            usrCategory[upti]=1;
            hUsrCategory->Fill(1);
        } else if( frePerDay[upti]*31>=businessBoundary )
        {
            usrCategory[upti]=3;
            hUsrCategory->Fill(3);
        }else
        {
            usrCategory[upti]=2;
            hUsrCategory->Fill(2);
        }
    }


    double timeLowEdge=coverTime2unix(lowDate) ;
    double timeHighEdge=coverTime2unix(highDate) ;
    TH1D* purTotal=new TH1D("purTotal","purTotal",timeBinNum,timeLowEdge,timeHighEdge);
    TH1D* expectedPurTotal=new TH1D("expectedPurTotal","expectedPurTotal",timeBinNum,timeLowEdge,timeHighEdge);
    TH1D* purCate[cateNum];
    for( int pi=0 ; pi<cateNum ; pi++ )
    {
        purCate[pi]=new TH1D(Form("purCate%i",pi+1),"purCate",timeBinNum,timeLowEdge,timeHighEdge); 
    }
    TH1D* expectedPurCate[cateNum];
    for( int pi=0 ; pi<cateNum ; pi++ )
    {
        expectedPurCate[pi]=new TH1D(Form("expectedPurCate%i",pi+1),"expectedPurCate",timeBinNum,timeLowEdge,timeHighEdge); 
    }

    Long64_t* data=&total_purchase_amt;
    if( anaRedeem)
    {
        data=&total_redeem_amt;
    }
    for( Long64_t ubti=0 ; ubti<ubtNum ; ubti++ )
    {
        user_balance_table->GetEntry(ubti);
        purTotal->Fill(report_date_unix,*data);
        purCate[usrCategory[user_id]-1]->Fill(report_date_unix,*data);
    }

    //==> month information
    Long64_t  monthDayOrder[maxBinNum];
    Long64_t  monthDayAntiOrder[maxBinNum];
    for( int mi=0 ; mi<maxBinNum ; mi++ )
    {
        monthDayOrder[mi]=999;
        monthDayAntiOrder[mi]=999;
    }
    int monthCount=0;
    for( Long64_t bi=1 ; bi<=timeBinNum ; bi++ )
    {
        time_t timep=purTotal->GetBinLowEdge(bi);
        struct tm *p =localtime(&timep);
        monthDayOrder[bi]=p->tm_mday;
        struct tm p1=*p;
        p1.tm_mon=(p->tm_mon+1)%12;
        p1.tm_mday=1;
        if(p1.tm_mon==0) p1.tm_year++;
        Long64_t antiOrder=difftime(mktime(&p1),mktime(p))/86400;
        monthDayAntiOrder[bi]=antiOrder;
    }


    //==> holiday information and days vetoed 
    Long64_t holidayDate[34]={
        20130813, 1,
        20130910, 1,
        20130919, 3,
        20131001, 7,

        20140101, 1,
        20140131, 7,
        20140214, 1,
        20140405, 3,
        20140501, 3,
        20140511, 1,
        20140531, 3,
        20140615, 1,
        20140618, 1,
        20140802, 1,
        20140906, 3,
        20140910, 1,
        20141001, 7
    };
    int col=sizeof(holidayDate)/sizeof(holidayDate[0])/2;
    int holidayFirstBin[maxHolidayNum];
    int holidayLastBin[maxHolidayNum];
    for( int hi=0 ; hi<col ; hi++ )
    {
        Long64_t dateTmp=holidayDate[hi*2];
        Long64_t timeTmp=coverTime2unix(dateTmp);
        holidayFirstBin[hi]=purTotal->FindBin(timeTmp);
        holidayLastBin[hi]=holidayFirstBin[hi]+holidayDate[hi*2+1]-1;
    }

    Long64_t  holidayDayOrder[maxBinNum];
    Long64_t  holidayDayAntiOrder[maxBinNum];
    Long64_t  optHolidayDayOrder[maxBinNum];
    Long64_t  optHolidayDayAntiOrder[maxBinNum];

    //==> week information
    Long64_t  weekInfo[maxBinNum];
    for( int wi=0 ; wi<maxBinNum ; wi++ )
    {
        weekInfo[wi]=999;
    }
    for( int bi=1 ; bi<=timeBinNum ; bi++ )
    {
        time_t timep=purTotal->GetBinLowEdge(bi);
        struct tm *p =localtime(&timep);
        weekInfo[bi]=p->tm_wday;
    }

    //++++++++++++ scan each parameter ++++++++++++

    Long64_t lowScanDate=lowFitDate;
    Long64_t highScanDate=highFitDate;
    Long64_t tTmp=coverTime2unix(lowScanDate);
    int lowScanBin=purTotal->FindBin(tTmp);
    tTmp=coverTime2unix(highScanDate);
    int highScanBin=purTotal->FindBin(tTmp);

    int ndf=highScanBin-lowScanBin+1;

    Long64_t lowFitUnix=coverTime2unix(lowFitDate);
    Long64_t firstFitBin=purTotal->FindBin(lowFitUnix);
    Long64_t highFitUnix=coverTime2unix(highFitDate);
    Long64_t lastFitBin=purTotal->FindBin(highFitUnix);
    cout<<"firstFitBin-lastFitBin  : "<<firstFitBin<<"-"<<lastFitBin<<endl;

    //++++++++++++            ++++++++++++
    //
    for( int ri=0 ; ri<cateNum ; ri++ )
    {
        double weekDayRatio[7]={0.};
        double preHolidayRatio[maxPreAndAftNum]={0.};
        double aftHolidayRatio[maxPreAndAftNum]={0.};
        double preMonthRatio[maxPreAndAftNum]={0.};
        double aftMonthRatio[maxPreAndAftNum]={0.};
        double optWeekDayRatio[7]={0.};
        double optPreHolidayRatio[maxPreAndAftNum]={0.};
        double optAftHolidayRatio[maxPreAndAftNum]={0.};
        double optPreMonthRatio[maxPreAndAftNum]={0.};
        double optAftMonthRatio[maxPreAndAftNum]={0.};
        double zero=0.;
        double* monthDayOrderCorP[maxBinNum];
        double* monthDayAntiOrderCorP[maxBinNum];
        double* holidayDayOrderCorP[maxBinNum];
        double* holidayDayAntiOrderCorP[maxBinNum];
        double tfValue[maxBinNum]={0.};
        TF1* purf=new TF1("purf",Form("%s",fitFunc.c_str()),lowFitUnix,highFitUnix+86400);
        double realValue[maxBinNum]={0.};
        double calValue[maxBinNum]={0.};
        for( int bi=1 ; bi<=timeBinNum ; bi++ )
        {
            realValue[bi]=purCate[ri]->GetBinContent(bi);
        }
        int optPreAft[4];
        double optPar[1000]={0.};
        double minChi2=999;
        int pmi,ami,phi,ahi;
        int scanParaNum=0;

        for( int pmi=minPreAft ; pmi<=maxPreAft; pmi++ )
        {
            for( int ami=minPreAft ; ami<=maxPreAft; ami++ )
            {
                for( int phi=minPreAft ; phi<=maxPreAft; phi++ )
                {
                    for( int ahi=minPreAft ; ahi<=maxPreAft; ahi++ )
                    {
                        //double minChi2=999;
                        cout<<"pmi~ami~phi~ahi  : "<<pmi<<"~"<<ami<<"~"<<phi<<"~"<<ahi<<endl;

                        //==> veto before and after month for fitting
                        Long64_t  needRemoveMonth[maxBinNum]={0};
                        Long64_t monthVetoedNum=0;
                        for( Long64_t bi=1 ; bi<=timeBinNum ; bi++ )
                        {
                            time_t timep=purTotal->GetBinLowEdge(bi);
                            struct tm *p =localtime(&timep);
                            if( p->tm_mday==1 )
                            {
                                for( int  aft=0 ; aft<ami ; aft++ )
                                {
                                    needRemoveMonth[bi+aft]=1; monthVetoedNum++;
                                }
                                for( int pre=1 ; pre<=pmi ; pre++ )
                                {
                                    needRemoveMonth[bi-pre]=1;
                                    monthVetoedNum++;
                                }

                            }
                        }
                        //cout<<"monthVetoedNum  : "<<monthVetoedNum<<endl;

                        //==> veto before and after holiday  for fitting && determined Order-AntiOrder 
                        Long64_t  needRemoveHoliday[maxBinNum]={0};
                        Long64_t holidayVetoedNum=0;
                        for( int hi=0 ; hi<col ; hi++ )
                        {
                            for( int inho=holidayFirstBin[hi] ; inho<=holidayLastBin[hi] ; inho++ )
                            {
                                needRemoveHoliday[inho]=1;
                            }

                            for( int pre=1 ; pre<=phi ; pre++ )
                            {
                                int preBin=holidayFirstBin[hi]-pre;
                                if( preBin>=1 )
                                {
                                    needRemoveHoliday[preBin]=1;
                                }
                            }
                            for( int  aft=1 ; aft<=ahi ; aft++ )
                            {
                                int aftBin=holidayLastBin[hi]+aft;
                                if( aftBin<=timeBinNum )
                                {
                                    needRemoveHoliday[aftBin]=1;
                                }
                            }
                        }

                        //cout<<"holidayVetoedNum  : "<<holidayVetoedNum<<endl;

                        //==> fit 20140302-20140828,obtain a exponential distribution
                        //cout<<" "<<endl;
                        //cout<<" Fitter information :  "<<endl;
                        Long64_t purFitNum=0;
                        for( Long64_t puri=firstFitBin ; puri<=lastFitBin ; puri++ )
                        {
                            if( !needRemoveHoliday[puri] && !needRemoveMonth[puri] )
                            {
                                x[purFitNum]=purTotal->GetBinLowEdge(puri);
                                y[purFitNum]=purCate[ri]->GetBinContent(puri);
                                purFitNum++;
                            }
                        }
                        //cout<<"purFitNum  : "<<purFitNum<<endl;
                        fit(purFitNum,x,y,fitFunc,par,1);
                        for( int pi=0 ; pi<paraNum ; pi++ )
                        {
                            purf->SetParameter(pi,par[pi]);
                        }
                        //purf->SetLineColor(kBlue);
                        //purf->Draw("same");
                        //===========> begin scan 
                        //binded correction facter address to each bin
                        //==> veto before and after holiday  for fitting && determined Order-AntiOrder 
                        for( int hi=0 ; hi<maxBinNum ; hi++ )
                        {
                            holidayDayOrder[hi]=999;
                            holidayDayAntiOrder[hi]=999;
                        }
                        for( int hi=0 ; hi<col ; hi++ )
                        {
                            for( int inho=holidayFirstBin[hi] ; inho<=holidayLastBin[hi] ; inho++ )
                            {
                                holidayDayOrder[inho]=0;
                            }

                            for( int pre=1 ; pre<=phi ; pre++ )
                            {
                                int preBin=holidayFirstBin[hi]-pre;
                                if( preBin>=1 )
                                {
                                    if(holidayDayAntiOrder[preBin]==999&&holidayDayOrder[preBin]!=0)holidayDayAntiOrder[preBin]=pre;
                                }
                            }
                            for( int  aft=1 ; aft<=ahi ; aft++ )
                            {
                                int aftBin=holidayLastBin[hi]+aft;
                                if( aftBin<=timeBinNum )
                                {
                                    holidayDayOrder[aftBin]=aft;
                                }
                            }
                        }

                        for( int bi=lowScanBin ; bi<=highScanBin ; bi++ )
                        {
                            monthDayOrderCorP[bi]=&aftMonthRatio[monthDayOrder[bi]-1];
                            monthDayAntiOrderCorP[bi]=&preMonthRatio[monthDayAntiOrder[bi]-1];
                            if( holidayDayOrder[bi]!=999 )
                            {
                                holidayDayOrderCorP[bi]=&aftHolidayRatio[holidayDayOrder[bi]];
                            }else
                            {
                                holidayDayOrderCorP[bi]=&zero;
                            }
                            if( holidayDayAntiOrder[bi]!=999 )
                            {
                                holidayDayAntiOrderCorP[bi]=&preHolidayRatio[holidayDayAntiOrder[bi]-1];
                            }else
                            {
                                holidayDayAntiOrderCorP[bi]=&zero;
                            }
                            time_t timep=purTotal->GetBinLowEdge(bi);
                            tfValue[bi]=purf->Eval(timep);
                        }

                        //binded parameter address to sco[] for looping
                        //weekDayRatio[0...6],preMonthRatio[0...pmi-1],aftMonthRatio[0...ami]
                        //preHolidayRatio[0...phi],aftHolidayRatio[0...ahi],holidayRatioMean[0]
                        scanParaNum=0;
                        for( int wi=0 ; wi<7 ; wi++ )
                        {
                            sco[scanParaNum].tag=wi;
                            sco[scanParaNum].minChi2ndf=999;
                            sco[scanParaNum++].ref=&weekDayRatio[wi];
                        }
                        for( int mi=0 ; mi<pmi; mi++ )
                        {
                            sco[scanParaNum].tag=-(mi+1);
                            sco[scanParaNum].minChi2ndf=999;
                            sco[scanParaNum++].ref=&preMonthRatio[mi];
                        }
                        for( int mi=0 ; mi<ami; mi++ )
                        {
                            sco[scanParaNum].tag=(mi+1);
                            sco[scanParaNum].minChi2ndf=999;
                            sco[scanParaNum++].ref=&aftMonthRatio[mi];
                        }
                        for( int hi=0 ; hi<phi ; hi++ )
                        {
                            sco[scanParaNum].tag=-(hi+1);
                            sco[scanParaNum].minChi2ndf=999;
                            sco[scanParaNum++].ref=&preHolidayRatio[hi];
                        }
                        for( int hi=0 ; hi<ahi+1 ; hi++ )
                        {
                            sco[scanParaNum].tag=(hi);
                            sco[scanParaNum].minChi2ndf=999;
                            sco[scanParaNum++].ref=&aftHolidayRatio[hi];
                        }

                        //cout<<"scanParaNum  : "<<scanParaNum<<endl;

                        //looping  each ratio values
                        for( int li=1 ; li<=10 ; li++ )
                        {
                            //cout<<"loop  : "<<li<<endl;
                            for( int pi=0 ; pi<scanParaNum ; pi++ )
                            {
                                for( double si=sco[pi].low ; si<=sco[pi].high ; si+=sco[pi].step )
                                {
                                    *(sco[pi].ref)=si;
                                    double chi2=0.;
                                    for( int bi=lowScanBin ; bi<=highScanBin ; bi++ )
                                    {
                                        calValue[bi]=(tfValue[bi])*(1+weekDayRatio[weekInfo[bi]]+(*monthDayOrderCorP[bi])+(*monthDayAntiOrderCorP[bi])+(*holidayDayAntiOrderCorP[bi])+(*holidayDayOrderCorP[bi]));
                                        //chi2+=(calValue[bi]-realValue[bi])*(calValue[bi]-realValue[bi])/realValue[bi];
                                        chi2+=(calValue[bi]-realValue[bi])*(calValue[bi]-realValue[bi]);
                                    }
                                    chi2/=ndf;
                                    if( sco[pi].minChi2ndf==999 ||chi2<=sco[pi].minChi2ndf)
                                    {
                                        sco[pi].minChi2ndf=chi2;
                                        sco[pi].optVal=si;
                                    }
                                }
                                *(sco[pi].ref)=sco[pi].optVal;
                                ///*if(li>=30)*/ cout<<" "<<sco[pi].tag <<" : "<<sco[pi].optVal<<endl;
                            }
                        }

                        double chi2Tmp=0.;
                        for( int bi=lowScanBin; bi<=highScanBin; bi++ )
                        {
                            calValue[bi]=(tfValue[bi])*(1+weekDayRatio[weekInfo[bi]]+(*monthDayOrderCorP[bi])+(*monthDayAntiOrderCorP[bi])+(*holidayDayAntiOrderCorP[bi])+(*holidayDayOrderCorP[bi]));
                            chi2Tmp+=(calValue[bi]-realValue[bi])*(calValue[bi]-realValue[bi]);
                        }
                        chi2Tmp/=ndf;
                        //cout<<"chi2Tmp  : "<<chi2Tmp<<"   minChi2:"<<minChi2<<endl;
                        if( minChi2==999 ||chi2Tmp<minChi2)
                        {
                            minChi2=chi2Tmp;
                            optPreAft[0]=pmi;
                            optPreAft[1]=ami;
                            optPreAft[2]=phi;
                            optPreAft[3]=ahi;
                            for( int pi=0 ; pi<paraNum ; pi++ )
                            {
                                optPar[pi]=par[pi];
                            }
                            for( int wii=0 ; wii<7 ; wii++ )
                            {
                                optWeekDayRatio[wii]=weekDayRatio[wii];
                            }
                            for( int pii=0 ; pii<maxPreAndAftNum ; pii++ )
                            {
                                optPreHolidayRatio[pii]=preHolidayRatio[pii];
                                optAftHolidayRatio[pii]=aftHolidayRatio[pii];
                                optPreMonthRatio[pii]  =preMonthRatio[pii]  ;
                                optAftMonthRatio[pii]  =aftMonthRatio[pii]  ;
                            }
                            for( int hi=0 ; hi<maxBinNum ; hi++ )
                            {
                                optHolidayDayOrder[hi]    =holidayDayOrder[hi]    ;
                                optHolidayDayAntiOrder[hi]=holidayDayAntiOrder[hi];
                            }
                        }
                        //cout<<"chi2Tmp  : "<<chi2Tmp<<"   minChi2:"<<minChi2<<endl;
                    }
                }
            }
        }
        pmi=optPreAft[0];
        ami=optPreAft[1];
        phi=optPreAft[2];
        ahi=optPreAft[3];
        cout<<"optimal pmi~ami~phi~ahi  : "<<pmi<<"~"<<ami<<"~"<<phi<<"~"<<ahi<<endl;
        for( int pi=0 ; pi<paraNum ; pi++ )
        {
            cout<<"optPar["<<pi<<"]  : "<<optPar[pi]<<endl;
        }
        cout<<"opt week day ratio  : "<<endl;
        for( int wii=0 ; wii<7 ; wii++ )
        {
            cout<<" "<<wii<<" : "<<optWeekDayRatio[wii]<<endl;
        }
        cout<<"opt month ratio  : "<<endl;
        for( int pii=pmi ; pii>=1; pii-- )
        {
            cout<<" "<<-pii<<" : "<< optPreMonthRatio[pii-1]<<endl;
        }
        for( int pii=1 ; pii<=ami; pii++ )
        {
            cout<<" "<<pii<<" : "<< optAftMonthRatio[pii-1]<<endl;
        }
        cout<<"opt holiday ratio  : "<<endl;
        for( int pii=phi ; pii>=1; pii-- )
        {
            cout<<" "<<-pii<<" : "<< optPreHolidayRatio[pii-1]<<endl;
        }
        for( int pii=1 ; pii<=ahi+1; pii++ )
        {
            cout<<" "<<pii-1<<" : "<< optAftMonthRatio[pii-1]<<endl;
        }

        for( int pi=0 ; pi<paraNum ; pi++ )
        {
            purf->SetParameter(pi,optPar[pi]);
        }
        //purf->SetLineColor(kBlue);
        //purf->Draw("same");


        //==> expected purchase 
        //cout<<" "<<endl;
        //cout<<"Correction information :  "<<endl;
        for( int bi=1 ; bi<=timeBinNum ; bi++ )
        {
            if( bi>=firstFitBin )
            {
                time_t timep=purTotal->GetBinLowEdge(bi);
                double weekCor=optWeekDayRatio[weekInfo[bi]];
                double monthCor=optPreMonthRatio[monthDayAntiOrder[bi]-1]+optAftMonthRatio[monthDayOrder[bi]-1];
                double holidayCor=optPreHolidayRatio[optHolidayDayAntiOrder[bi]-1]+optAftHolidayRatio[optHolidayDayOrder[bi]];
                double expectedValue=purf->Eval(timep)*(1+weekCor+monthCor+holidayCor);
                expectedPurCate[ri]->SetBinContent(bi,expectedValue);
            }
        }
    }
    for( int bi=1 ; bi<=timeBinNum ; bi++ )
    {
        if( bi>=firstFitBin )
        {
            double expectedValue=0.;
            for( int ci =0; ci<cateNum ; ci++ )
            {
                expectedValue+=expectedPurCate[ci]->GetBinContent(bi);
            }
            expectedPurTotal->SetBinContent(bi,expectedValue);
        }
    }
    double chi2Tmp=0.;
    double realValuetotal[maxBinNum]={0.};
    double calValuetotal[maxBinNum]={0.};
    for( int bi=lowScanBin; bi<=highScanBin; bi++ )
    {
        calValuetotal[bi]=expectedPurTotal->GetBinContent(bi);
        realValuetotal[bi]=purTotal->GetBinContent(bi);
        chi2Tmp+=(calValuetotal[bi]-realValuetotal[bi])*(calValuetotal[bi]-realValuetotal[bi]);
    }
    chi2Tmp/=ndf;
    cout<<"businessBoundary="<<businessBoundary <<" : "<<chi2Tmp<<endl;

    c->cd(1);
    hUsrCategory->Draw();
    c->cd(2);
    purTotal->GetXaxis()->SetTimeDisplay(1);
    purTotal->GetXaxis()->SetTimeFormat("%m/%d");
    purTotal->SetAxisRange(lowFitUnix,timeHighEdge);
    purTotal->Draw("hist");
    expectedPurTotal->SetLineColor(kRed);
    expectedPurTotal->Draw("same");
    for( int ci=0 ; ci<cateNum ; ci++ )
    {
        c->cd(ci+3);
        purCate[ci]->GetXaxis()->SetTimeDisplay(1);
        purCate[ci]->GetXaxis()->SetTimeFormat("%m/%d");
        purCate[ci]->SetAxisRange(lowFitUnix,timeHighEdge);
        purCate[ci]->Draw("hist");
        expectedPurCate[ci]->SetLineColor(kRed);
        expectedPurCate[ci]->Draw("same");
    }
    //c->SaveAs(Form("jobs/%s.eps",anaStr[anaRedeem].c_str()));

    //==>echo  *****forecast*****
    cout<<" "<<endl;
    cout<<" "<<endl;
    cout<<" "<<endl;
    cout<<Form(" %s result for submitting : ",anaStr[anaRedeem].c_str())<<endl;
    cout<<" "<<endl;
    for( int di=20140901 ; di<=20140930 ; di++ )
    {
        Long64_t timeTmp=coverTime2unix(di);
        cout<<di<<","<<setiosflags(ios::fixed)<<(int)expectedPurTotal->GetBinContent(expectedPurTotal->FindBin(timeTmp))<<endl;
    }
    cout<<" "<<endl;
    cout<<" "<<endl;
    cout<<" "<<endl;

}

void fit(Long64_t n,double* x,double* y,string func,double* par,bool q=true)
{
    memset(par, 0, sizeof(double)*100);
    TGraph* g=new TGraph(n,x,y);
    TF1* tf=new TF1("tf",Form("%s",func.c_str()),x[0],x[n-1]);
    if( q )
    {
        g->Fit(tf,"q");
    }else
    {
        g->Fit(tf);
    }
    tf->GetParameters(par);
    par[999]=tf->GetChisquare();
    par[998]=tf->GetNDF();
}

Long64_t coverTime2unix(Long64_t ymd)
{
    struct tm tm1, tm2;
    memset(&tm1, 0, sizeof(tm1));
    tm1.tm_year = ymd/10000- 1900;
    tm1.tm_mon  = ymd%10000/100- 1;
    tm1.tm_mday = ymd%10000%100;
    tm2 = tm1;
    time_t unixTime= mktime(&tm1);
    //check is vcild date
    if(  tm1.tm_year == tm2.tm_year && tm1.tm_mon == tm2.tm_mon && tm1.tm_mday == tm2.tm_mday )
    {
        //cout<<"coverTime2unix : "<<ymd<<" -> "<< tm1.tm_year+1900<<"/"<<tm1.tm_mon+1<<"/"<<tm1.tm_mday<< " -> "<<unixTime<<endl;
    }else
    {
        //cout<<"unvalid time,please check "<<endl;
        unixTime=-1;
    }
    return unixTime;
}

Long64_t coverTime2Date(Long64_t ymd)
{
    time_t timep=ymd;
    struct tm *p =localtime(&timep);
    return (p->tm_year+1900)*10000+(p->tm_mon+1)*100+(p->tm_mday);
}
