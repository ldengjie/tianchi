{
    gStyle->SetTitleStyle(0); 
    gStyle->SetTitleBorderSize(0); 
    gStyle->SetOptStat(0);
    gStyle->SetLegendBorderSize(0);

    bool anaRedeem=1;//otherwise purchase 

    Long64_t lowFitDate=20140312;
    Long64_t highFitDate=20140828;
    string fitFunc="expo";
    int paraNum=2;
    double x[1000]={0.};
    double y[1000]={0.};
    double par[100]={0.};

    Long64_t timeBinNum=490;//490 day,70 week,16 month
    Long64_t lowDate=20130602;
    Long64_t highDate=20141005;
    const int maxBinNum=500;//should be more than "timeBinNum"
    const int maxHolidayNum=30;
    const int maxMonthNum=30;
    const int maxPreAndAftNum=100;//maximum of pre,aft,length of  holiday(guoqing=7)

    int preMonth=10;
    int aftMonth=10;
    int preHoliday=10;
    int aftHoliday=10;

    typedef struct ScanPara
    {
        double* ref;
        double low;
        double high;
        double step;
        double optVal;
        double oriVal;
        double minChi2ndf;
        int tag;
        ScanPara()
        {
            ref=NULL;
            low=-1;
            high=1;
            step=0.0001;//
            minChi2ndf=999;
            optVal=0.;
            oriVal=0.;
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
    user_balance_table->SetBranchAddress("tftobal_amt",&tftobal_amt);
    user_balance_table->SetBranchAddress("tftocard_amt",&tftocard_amt);
    user_balance_table->SetBranchAddress("share_amt",&share_amt);
    user_balance_table->SetBranchAddress("category1",&category1);
    user_balance_table->SetBranchAddress("category2",&category2);
    user_balance_table->SetBranchAddress("category3",&category3);
    user_balance_table->SetBranchAddress("category4",&category4);
    user_balance_table->SetBranchAddress("direct_purchase_amt",&direct_purchase_amt);

    TCanvas* c=new TCanvas("c","c",1800,900);
    c->Divide(3,2);

    double timeLowEdge=coverTime2unix(lowDate) ;
    double timeHighEdge=coverTime2unix(highDate) ;
    TH1D* purTotal=new TH1D("purTotal","purTotal",timeBinNum,timeLowEdge,timeHighEdge);
    Long64_t* data=&total_purchase_amt;
    if( anaRedeem)
    {
        data=&total_redeem_amt;
    }
    for( Long64_t ubti=0 ; ubti<ubtNum ; ubti++ )
    {
        user_balance_table->GetEntry(ubti);
        //purTotal->Fill(report_date_unix,total_purchase_amt);
        //purTotal->Fill(report_date_unix,total_redeem_amt);
        purTotal->Fill(report_date_unix,*data);
    }


    //==> remove month pre-aft

    Long64_t  needRemoveMonth[maxBinNum]={0};
    //Long64_t* needRemoveMonth=initLA(maxBinNum);
    Long64_t  monthInfo[maxBinNum];
    for( int mi=0 ; mi<maxBinNum ; mi++ )
    {
        monthInfo[mi]=999;
    }
    
    Long64_t monthVetoedNum=0;
    int monthBin[maxMonthNum];
    int monthCount=0;
    for( Long64_t bi=1 ; bi<=timeBinNum ; bi++ )
    {
        double content=purTotal->GetBinContent(bi);
        time_t timep=purTotal->GetBinLowEdge(bi);
        struct tm *p =localtime(&timep);
        if( p->tm_mday==1 )
        {
            monthBin[monthCount++]=bi;
            for( int  aft=0 ; aft<aftMonth ; aft++ )
            {
                needRemoveMonth[bi+aft]=1;
                monthVetoedNum++;
                monthInfo[bi+aft]=aft;
            }
            for( int pre=1 ; pre<=preMonth ; pre++ )
            {
                needRemoveMonth[bi-pre]=1;
                monthVetoedNum++;
                monthInfo[bi-pre]=-pre;
            }

        }
    }
    cout<<"monthVetoedNum  : "<<monthVetoedNum<<endl;


    //==> remove holiday
    Long64_t holidayDate[32]={
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
        20140802, 1,
        20140906, 3,
        20140910, 1,
        20141001, 7
    };
    //TH1D* purTotalRemoveHoliday=new TH1D("purTotalRemoveHoliday","purTotalRemoveHoliday",timeBinNum,timeLowEdge,timeHighEdge);
    Long64_t  needRemoveHoliday[maxBinNum]={0};
    Long64_t  holidayInfo[maxBinNum];
    for( int hi=0 ; hi<maxBinNum ; hi++ )
    {
        holidayInfo[hi]=999;
    }
    Long64_t holidayVetoedNum=0;
    int col=sizeof(holidayDate)/sizeof(holidayDate[0])/2;
    int colNow=0;
    int holidayFirstBin[maxHolidayNum];
    int holidayLastBin[maxHolidayNum];
    for( Long64_t bi=1 ; bi<=timeBinNum ; bi++ )
    {
        double content=purTotal->GetBinContent(bi);
        //if(content==0) continue;
        Long64_t binLowEdge=purTotal->GetBinLowEdge(bi);
        for( Long64_t hi=0 ; hi<col ; hi++ )
        {
            Long64_t dateTmp=holidayDate[hi*2];
            Long64_t timeTmp=coverTime2unix(dateTmp);
            if(  binLowEdge==timeTmp )
            {
                holidayFirstBin[colNow]=bi;
                holidayLastBin[colNow++]=bi+holidayDate[hi*2+1]-1;
            }
            if( binLowEdge>=timeTmp&&(binLowEdge<timeTmp+holidayDate[hi*2+1]*86400)  )
            {
                //cout<<"find one "<<endl; 
                holidayVetoedNum++;
                needRemoveHoliday[bi]=1;
                holidayInfo[bi]=0;
                for( int pre=1 ; pre<=preHoliday ; pre++ )
                {
                    if( bi-pre>=1 )
                    {
                        needRemoveHoliday[bi-pre]=1;
                        if(holidayInfo[bi-pre]==999)holidayInfo[bi-pre]=-pre;
                    }
                }
                for( int  aft=1 ; aft<=aftHoliday ; aft++ )
                {
                    if( bi+aft<=timeBinNum )
                    {
                        needRemoveHoliday[bi+aft]=1;
                        holidayInfo[bi+aft]=aft;
                    }
                }
                break;
            }

        }
    }
    cout<<"holidayVetoedNum  : "<<holidayVetoedNum<<endl;


    //c->cd(1);
    purTotal->GetXaxis()->SetTimeDisplay(1);
    purTotal->GetXaxis()->SetTimeFormat("%m/%d");
    purTotal->Draw("hist");
    //purTotalRemoveHoliday->SetLineColor(kRed);
    //purTotalRemoveHoliday->Draw("same");




    //==> fit 20140302-20140828
    cout<<" "<<endl;
    cout<<" Fitter information :  "<<endl;
    Long64_t purFitNum=0;
    Long64_t tmp=coverTime2unix(lowFitDate);
    Long64_t firstFitBin=purTotal->FindBin(tmp);
    tmp=coverTime2unix(highFitDate);
    Long64_t lastFitBin=purTotal->FindBin(tmp);
    cout<<"firstFitBin-lastFitBin  : "<<firstFitBin<<"-"<<lastFitBin<<endl;
    for( Long64_t puri=firstFitBin ; puri<=lastFitBin ; puri++ )
    {
        //x[purFitNum]=purTotalRemoveHoliday->GetBinLowEdge(puri);
        //y[purFitNum]=purTotalRemoveHoliday->GetBinContent(puri);
        if( !needRemoveHoliday[puri] && !needRemoveMonth[puri] )
        {
            x[purFitNum]=purTotal->GetBinLowEdge(puri);
            y[purFitNum]=purTotal->GetBinContent(puri);
            purFitNum++;
        }
    }
    cout<<"purFitNum  : "<<purFitNum<<endl;

    fit(purFitNum,x,y,fitFunc,par,0);
    TF1* purf=new TF1("purf",Form("%s",fitFunc.c_str()),x[0],x[purFitNum-1]);
    for( int pi=0 ; pi<paraNum ; pi++ )
    {
        purf->SetParameter(pi,par[pi]);
    }
    purf->SetLineColor(kBlue);
    purf->Draw("same");



    //==> variation inside one week
    Long64_t  weekInfo[maxBinNum];
    for( int wi=0 ; wi<maxBinNum ; wi++ )
    {
        weekInfo[wi]=999;
    }
    for( int bi=1 ; bi<timeBinNum ; bi++ )
    {
        time_t timep=purTotal->GetBinLowEdge(bi);
        struct tm *p =localtime(&timep);
        weekInfo[bi]=p->tm_wday;
    }
    
    int weekDayNum[7]={0};
    double weekDayRatio[7]={0.};
    double weekDayRatioErr2[7]={0.};
    for( int puri=firstFitBin ; puri<=lastFitBin ; puri++ )
    {
        if( !needRemoveHoliday[puri] && !needRemoveMonth[puri] )
        {
            double purContent=purTotal->GetBinContent(puri);
            time_t timep=purTotal->GetBinLowEdge(puri);
            double funcValue=purf->Eval(timep);
            weekDayNum[weekInfo[puri]]++;
            weekDayRatio[weekInfo[puri]]+=(purContent-funcValue)/funcValue;
            weekDayRatioErr2[weekInfo[puri]]+=(purContent+funcValue)/funcValue/funcValue*(1+(purContent+funcValue)/funcValue);
        }
    }
    cout<<" "<<endl;
    cout<<"week :  "<<endl;
    for( int wi=0 ; wi<7 ; wi++ )
    {
        if( weekDayNum[wi]!=0 )
        {
            weekDayRatio[wi]=weekDayRatio[wi]/weekDayNum[wi];
            weekDayRatioErr2[wi]=weekDayRatioErr2[wi]/weekDayNum[wi]/weekDayNum[wi];
        }
        cout<<" "<<wi<<" ("<<weekDayNum[wi] <<") : "<<weekDayRatio[wi]<<" +- "<<sqrt(weekDayRatioErr2[wi])<<endl;
    }



    //==> variation before and after month 
    double preMonthRatio[maxPreAndAftNum]={0.};
    double aftMonthRatio[maxPreAndAftNum]={0.};
    double preMonthRatioErr2[maxPreAndAftNum]={0.};
    double aftMonthRatioErr2[maxPreAndAftNum]={0.};
    double preMonthRatioCount[maxPreAndAftNum]={0.};
    double aftMonthRatioCount[maxPreAndAftNum]={0.};

    for( int mi=0 ; mi<monthCount ; mi++ )
    {
        for( int prei=preMonth ; prei>=1 ; prei-- )
        {
            if((!needRemoveHoliday[monthBin[mi]-prei])&&(monthBin[mi]-prei>=firstFitBin)&&(monthBin[mi]-prei<=lastFitBin)) 
            {
                double purContent=purTotal->GetBinContent(monthBin[mi]-prei);
                time_t timep=purTotal->GetBinLowEdge(monthBin[mi]-prei);
                double expectedValue=purf->Eval(timep)*(1+weekDayRatio[weekInfo[monthBin[mi]-prei]]);
                preMonthRatio[prei-1]+=(purContent-expectedValue)/expectedValue;
                preMonthRatioErr2[prei-1]+=(purContent+expectedValue)/expectedValue/expectedValue*(1+(purContent+expectedValue)/expectedValue);
                preMonthRatioCount[prei-1]++;
            }
        }
        for( int afti=0 ; afti<aftMonth ; afti++ )
        {
            if((!needRemoveHoliday[monthBin[mi]+afti])&&(monthBin[mi]+afti>=firstFitBin)&&(monthBin[mi]+afti<=lastFitBin)) 
            {
                double purContent=purTotal->GetBinContent(monthBin[mi]+afti);
                time_t timep=purTotal->GetBinLowEdge(monthBin[mi]+afti);
                double expectedValue=purf->Eval(timep)*(1+weekDayRatio[weekInfo[monthBin[mi]+afti]]);
                aftMonthRatio[afti]+=(purContent-expectedValue)/expectedValue;
                aftMonthRatioErr2[afti]+=(purContent+expectedValue)/expectedValue/expectedValue*(1+(purContent+expectedValue)/expectedValue);
                aftMonthRatioCount[afti]++;
            }
        }

    }

    cout<<" "<<endl;
    cout<<"month:  "<<endl;
    for( int prei=preMonth-1 ; prei>=0 ; prei-- )
    {
        if( preMonthRatioCount[prei]!=0 )
        {
            preMonthRatio[prei]=preMonthRatio[prei]/preMonthRatioCount[prei];
            preMonthRatioErr2[prei]=preMonthRatioErr2[prei]/preMonthRatioCount[prei]/preMonthRatioCount[prei];
        }
        cout<<"-"<<prei+1<<" ("<< preMonthRatioCount[prei]<<")  : "<<preMonthRatio[prei]<<" +- "<<sqrt(preMonthRatioErr2[prei-1])<<endl;
    }
    for( int afti=0 ; afti<aftMonth ; afti++ )
    {
        if( aftMonthRatioCount[afti]!=0 )
        {
            aftMonthRatio[afti]=aftMonthRatio[afti]/aftMonthRatioCount[afti];
            aftMonthRatioErr2[afti]=aftMonthRatioErr2[afti]/aftMonthRatioCount[afti]/aftMonthRatioCount[afti];
        }
        cout<<"+"<<afti<<" ("<<aftMonthRatioCount[afti] <<")  : "<<aftMonthRatio[afti]<<" +- "<<sqrt(aftMonthRatioErr2[afti])<<endl;
    }


    //==> variation before and after holiday
    //free(preHolidayRatio);
    TH1D* expectedPurOnlyCorWeek=new TH1D("expectedPurOnlyCorWeek","expectedPurOnlyCorWeek",timeBinNum,timeLowEdge,timeHighEdge);
    for( int bi=1 ; bi<=timeBinNum ; bi++ )
    {
        expectedPurOnlyCorWeek->SetBinContent(bi,purTotal->GetBinContent(bi));
    }

    double preHolidayRatio[maxHolidayNum][maxPreAndAftNum]={0.};
    double aftHolidayRatio[maxHolidayNum][maxPreAndAftNum]={0.};
    double holidayRatio[maxHolidayNum][maxPreAndAftNum]={0.};
    double preHolidayRatioErr2[maxHolidayNum][maxPreAndAftNum]={0.};
    double aftHolidayRatioErr2[maxHolidayNum][maxPreAndAftNum]={0.};
    double holidayRatioErr2[maxHolidayNum][maxPreAndAftNum]={0.};
    for( int hi=0 ; hi<col ; hi++ )
    {
        for( int prei=preHoliday ; prei>=1; prei-- )
        {
            if((!needRemoveMonth[holidayFirstBin[hi]-prei])&&(holidayFirstBin[hi]-prei>=firstFitBin)&&(holidayFirstBin[hi]-prei<=lastFitBin)) 
            {
                double purContent=purTotal->GetBinContent(holidayFirstBin[hi]-prei);
                time_t timep=purTotal->GetBinLowEdge(holidayFirstBin[hi]-prei);
                double expectedValue=purf->Eval(timep)*(1+weekDayRatio[weekInfo[holidayFirstBin[hi]-prei]]);
                expectedPurOnlyCorWeek->SetBinContent(holidayFirstBin[hi]-prei,expectedValue);
                preHolidayRatio[hi][prei-1]=(purContent-expectedValue)/expectedValue;
                preHolidayRatioErr2[hi][prei-1]=(purContent+expectedValue)/expectedValue/expectedValue*(1+(purContent+expectedValue)/expectedValue);
                //cout<<" "<<preHolidayRatio[hi][prei-1];
            }
        }
        //cout<<"[";
        for( int hoi=holidayFirstBin[hi] ; hoi<=holidayLastBin[hi] ; hoi++ )
        {
            int calCount=0;
            if((!needRemoveMonth[hoi])&& hoi>=firstFitBin&&hoi<=lastFitBin )
            {
                double purContent=purTotal->GetBinContent(hoi);
                time_t timep=purTotal->GetBinLowEdge(hoi);
                double expectedValue=purf->Eval(timep)*(1+weekDayRatio[weekInfo[hoi]]);
                expectedPurOnlyCorWeek->SetBinContent(hoi,expectedValue);
                holidayRatio[hi][calCount]=(purContent-expectedValue)/expectedValue;
                holidayRatioErr2[hi][calCount]=(purContent+expectedValue)/expectedValue/expectedValue*(1+(purContent+expectedValue)/expectedValue);
                //cout<<" "<<preHolidayRatio[hi][calCount];
                calCount++;
            }

        }
        //cout<<"]";

        for( int afti=1 ; afti<=aftHoliday ; afti++ )
        {
            if((!needRemoveMonth[holidayLastBin[hi]+afti])&&(holidayLastBin[hi]+afti>=firstFitBin)&&(holidayLastBin[hi]+afti<=lastFitBin)) 
            {
                double purContent=purTotal->GetBinContent(holidayLastBin[hi]+afti);
                time_t timep=purTotal->GetBinLowEdge(holidayLastBin[hi]+afti);
                double expectedValue=purf->Eval(timep)*(1+weekDayRatio[weekInfo[holidayLastBin[hi]+afti]]);
                expectedPurOnlyCorWeek->SetBinContent(holidayLastBin[hi]+afti,expectedValue);
                aftHolidayRatio[hi][afti-1]=(purContent-expectedValue)/expectedValue;
                aftHolidayRatioErr2[hi][afti-1]=(purContent+expectedValue)/expectedValue/expectedValue*(1+(purContent+expectedValue)/expectedValue);
                //cout<<" "<<aftHolidayRatio[hi][afti-1];
            }
        }
        //cout<<endl;

    }

    double preHolidayRatioMean[maxPreAndAftNum]={0.};
    double aftHolidayRatioMean[maxPreAndAftNum]={0.};
    double holidayRatioMean[maxPreAndAftNum]={0.};
    double preHolidayRatioErr2Mean[maxPreAndAftNum]={0.};
    double aftHolidayRatioErr2Mean[maxPreAndAftNum]={0.};
    double holidayRatioErr2Mean[maxPreAndAftNum]={0.};

    double preHolidayRatioCount[maxPreAndAftNum]={0.};
    double aftHolidayRatioCount[maxPreAndAftNum]={0.};
    double holidayRatioCount[maxPreAndAftNum]={0.};
    for( int hi=0 ; hi<col ; hi++ )
    {
        for( int prei=0 ; prei<preHoliday ; prei++ )
        {
            if( preHolidayRatio[hi][prei]!=0 )
            {
                preHolidayRatioMean[prei]+=preHolidayRatio[hi][prei];
                preHolidayRatioErr2Mean[prei]+=preHolidayRatioErr2[hi][prei];
                preHolidayRatioCount[prei]++;
            }

        }
        for( int hoi=0 ; hoi<7 ; hoi++ )
        {
            if( holidayRatio[hi][hoi]!=0 )
            {
                holidayRatioMean[0]+=holidayRatio[hi][hoi];
                holidayRatioErr2Mean[0]+=holidayRatioErr2[hi][hoi];
                holidayRatioCount[0]++;
            }

        }
        for( int afti=0 ; afti<aftHoliday ; afti++ )
        {
            if( aftHolidayRatio[hi][afti]!=0 )
            {
                aftHolidayRatioMean[afti]+=aftHolidayRatio[hi][afti];
                aftHolidayRatioErr2Mean[afti]+=aftHolidayRatioErr2[hi][afti];
                aftHolidayRatioCount[afti]++;
            }

        }
    }

    cout<<" "<<endl;
    cout<<"holiday:  "<<endl;
    for( int prei=preHoliday-1 ; prei>=0; prei-- )
    {
        if( preHolidayRatioCount[prei]!=0 )
        {
            preHolidayRatioMean[prei]/=preHolidayRatioCount[prei];
            preHolidayRatioErr2Mean[prei]/=preHolidayRatioCount[prei]*preHolidayRatioCount[prei];
        }
        cout<<"-"<< prei+1<<" ("<<preHolidayRatioCount[prei] <<") : "<<preHolidayRatioMean[prei]<<" +- "<<sqrt(preHolidayRatioErr2Mean[prei])<<endl;
    }
    if( aftHolidayRatioCount[0]!=0 )
    {
        holidayRatioMean[0]/=aftHolidayRatioCount[0];
        holidayRatioErr2Mean[0]/=holidayRatioCount[0]*holidayRatioCount[0];
    }
    cout<<" ("<<aftHolidayRatioCount[0] <<")  = "<<holidayRatioMean[0]<<" +- "<<sqrt(holidayRatioErr2Mean[0])<<endl;
    for( int afti=0 ; afti<aftHoliday ; afti++ )
    {
        if( aftHolidayRatioCount[afti]!=0 )
        {
            aftHolidayRatioMean[afti]/=aftHolidayRatioCount[afti];
            aftHolidayRatioErr2Mean[afti]/=aftHolidayRatioCount[afti]*aftHolidayRatioCount[afti];
        }
        cout<<"+"<<afti+1 <<" ("<< aftHolidayRatioCount[afti]<<")  : "<<aftHolidayRatioMean[afti]<<" +- "<<sqrt(aftHolidayRatioErr2Mean[afti])<<endl;
    }
    //c->cd(2);
    expectedPurOnlyCorWeek->SetLineColor(kGreen);
    //expectedPurOnlyCorWeek->Draw("same");
    //
    //++++++++++++ scan each parameter ++++++++++++
    double realValue[maxBinNum]={0.};
    double calValue[maxBinNum]={0.};
    for( int bi=1 ; bi<timeBinNum ; bi++ )
    {
        realValue[bi]=purTotal->GetBinContent(bi);
    }
    Long64_t lowScanDate=lowFitDate;
    Long64_t highScanDate=highFitDate;
    Long64_t timeTmp=coverTime2unix(lowScanDate);
    int lowScanBin=purTotal->FindBin(timeTmp);
    timeTmp=coverTime2unix(highScanDate);
    int highScanBin=purTotal->FindBin(timeTmp);
    double zero=0.;
    double* monthCorP[maxBinNum];
    double* holidayCorP[maxBinNum];
    double tfValue[maxBinNum]={0.};
    int ndf=highScanBin-lowScanBin+1;
    for( int bi=lowScanBin ; bi<=highScanBin ; bi++ )
    {
        //double weekCor=1+weekDayRatio[weekInfo[bi]];
        double monthCor=0;
        if( monthInfo[bi]<999 )
        {
            if( monthInfo[bi]<0 )
            {
                monthCorP[bi]=&preMonthRatio[-monthInfo[bi]-1];
            }else
            {
                monthCorP[bi]=&aftMonthRatio[monthInfo[bi]];
            }
        }else
        {
            monthCorP[bi]=&zero;
        }
        double holidayCor=0;
        if( holidayInfo[bi]<999 )
        {
            if( holidayInfo[bi]<0 )
            {
                holidayCorP[bi]=&preHolidayRatioMean[-holidayInfo[bi]-1];
            }else if(holidayInfo[bi]>0)
            {
                holidayCorP[bi]=&aftHolidayRatioMean[holidayInfo[bi]-1];
            }else
            {
                holidayCorP[bi]=&holidayRatioMean[0];
            }
        }else
        {
            holidayCorP[bi]=&zero;
        }
        time_t timep=purTotal->GetBinLowEdge(bi);
        tfValue[bi]=purf->Eval(timep);
    }
    //weekDayRatio[0...6],preMonthRatio[0...preMonth-1],aftMonthRatio[0...aftMonth]
    //preHolidayRatioMean[0...preHoliday],aftHolidayRatioMean[0...aftHoliday],holidayRatioMean[0]
    int scanParaNum=0;
    for( int wi=0 ; wi<7 ; wi++ )
    {
        sco[scanParaNum].tag=wi;
        sco[scanParaNum].oriVal=weekDayRatio[wi]; 
        sco[scanParaNum++].ref=&weekDayRatio[wi];
    }
    for( int mi=0 ; mi<preMonth ; mi++ )
    {
        sco[scanParaNum].tag=-(mi+1);
        sco[scanParaNum].oriVal=preMonthRatio[mi];
        sco[scanParaNum++].ref=&preMonthRatio[mi];
    }
    for( int mi=0 ; mi<aftMonth ; mi++ )
    {
        sco[scanParaNum].tag=(mi);
        sco[scanParaNum].oriVal=aftMonthRatio[mi];
        sco[scanParaNum++].ref=&aftMonthRatio[mi];
    }
    for( int hi=0 ; hi<preHoliday ; hi++ )
    {
        sco[scanParaNum].tag=-(hi+1);
        sco[scanParaNum].oriVal=preHolidayRatioMean[hi];
        sco[scanParaNum++].ref=&preHolidayRatioMean[hi];
    }
    for( int hi=0 ; hi<aftHoliday ; hi++ )
    {
        sco[scanParaNum].tag=(hi+1);
        sco[scanParaNum].oriVal=aftHolidayRatioMean[hi];
        sco[scanParaNum++].ref=&aftHolidayRatioMean[hi];
    }
    sco[scanParaNum].tag=0;
    sco[scanParaNum].oriVal=holidayRatioMean[0];
    sco[scanParaNum++].ref=&holidayRatioMean[0];

    cout<<"scanParaNum  : "<<scanParaNum<<endl;
    for( int li=1 ; li<=30 ; li++ )
    {
        cout<<"loop  : "<<li<<endl;
        //for( int pi=0 ; pi<7; pi++ )
        for( int pi=0 ; pi<scanParaNum ; pi++ )
        {
            for( double si=sco[pi].low ; si<=sco[pi].high ; si+=sco[pi].step )
            {
                *(sco[pi].ref)=si;
                double chi2=0.;
                for( int bi=lowScanBin ; bi<=highScanBin ; bi++ )
                {
                    calValue[bi]=(tfValue[bi])*(1+weekDayRatio[weekInfo[bi]])*(1+(*monthCorP[bi]))*(1+(*holidayCorP[bi]));
                    chi2+=(calValue[bi]-realValue[bi])*(calValue[bi]-realValue[bi])/realValue[bi];
                }
                chi2/=ndf;
                if( chi2<sco[pi].minChi2ndf||sco[pi].minChi2ndf==999 )
                {
                    sco[pi].minChi2ndf=chi2;
                    sco[pi].optVal=si;
                }
            }
            *(sco[pi].ref)=sco[pi].optVal;
            if(li>=29) cout<<" "<<sco[pi].tag <<" : "<<sco[pi].oriVal<<" -> "<<sco[pi].optVal<<endl;

        }

    }


    //==> expected purchase 
    TH1D* expectedPur=new TH1D("expectedPur","expectedPur",timeBinNum,timeLowEdge,timeHighEdge);
    TH1D* monthDay=new TH1D("monthDay","monthDay",timeBinNum,timeLowEdge,timeHighEdge);
    TH1D* holidayDay=new TH1D("holidayDay","holidayDay",timeBinNum,timeLowEdge,timeHighEdge);
    //double minVal=purTotal->GetBinContent(purTotal->GetMinimumBin(firstFitBin,lastFitBin,0));
    double minVal=100000000;
    cout<<" "<<endl;
    //cout<<"Correction information :  "<<endl;
    for( int bi=1 ; bi<=timeBinNum ; bi++ )
    {
        if( monthInfo[bi]<999 )
        {
            monthDay->SetBinContent(bi,0.3*minVal);
            if( monthInfo[bi]==0 ) monthDay->SetBinContent(bi,0.4*minVal);
        }
        if( holidayInfo[bi]<999 )
        {
            holidayDay->SetBinContent(bi,0.7*minVal);
            if(holidayInfo[bi]==0) holidayDay->SetBinContent(bi,0.8*minVal);
        }
        if( bi>=firstFitBin )
        {
            time_t timep=purTotal->GetBinLowEdge(bi);
            double weekCor=1+weekDayRatio[weekInfo[bi]];
            double monthCor=0;
            if( monthInfo[bi]<999 )
            {
                if( monthInfo[bi]<0 )
                {
                    monthCor=preMonthRatio[-monthInfo[bi]-1];
                }else
                {
                    monthCor=aftMonthRatio[monthInfo[bi]];
                }
            }
            monthCor+=1;
            double holidayCor=0;
            if( holidayInfo[bi]<999 )
            {
                if( holidayInfo[bi]<0 )
                {
                    holidayCor=preHolidayRatioMean[-holidayInfo[bi]-1];
                }else if(holidayInfo[bi]>0)
                {
                    holidayCor=aftHolidayRatioMean[holidayInfo[bi]-1];
                }else
                {
                    holidayCor=holidayRatioMean[0];
                }
            }
            holidayCor+=1;
            double expectedValue=purf->Eval(timep)*weekCor*monthCor*holidayCor;
            expectedPur->SetBinContent(bi,expectedValue);
            Long64_t dataTmp=coverTime2Date(timep);
            //cout<<" "<< dataTmp<<"   : "<< weekCor*monthCor*holidayCor<<"="<<weekCor<<"*"<<monthCor<<"*"<<holidayCor<<" "<<purf->Eval(timep)<<" -> "<<expectedValue<<endl;
        }
        //else
        //{
        //expectedPur->SetBinContent(bi,purTotal->GetBinContent(bi));
        //}
    }
    expectedPur->SetLineColor(kRed);
    expectedPur->Draw("same");
    //monthDay->SetLineStyle(1);
    monthDay->SetLineColor(6);
    //holidayDay->SetLineStyle(1);
    holidayDay->SetLineColor(8);
    holidayDay->Draw("same");
    monthDay->Draw("same");


    //==>echo  *****forecast*****
    cout<<" "<<endl;
    cout<<" "<<endl;
    cout<<" "<<endl;
    cout<<" Purchase result for submitting : "<<endl;
    cout<<" "<<endl;
    for( int di=20140901 ; di<=20140930 ; di++ )
    {
        Long64_t timeTmp=coverTime2unix(di);
        cout<<di<<","<<setiosflags(ios::fixed)<<(int)expectedPur->GetBinContent(expectedPur->FindBin(timeTmp))<<endl;
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
