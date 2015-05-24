{
    gStyle->SetTitleStyle(0); 
    gStyle->SetTitleBorderSize(0); 
    gStyle->SetOptStat(0);
    gStyle->SetLegendBorderSize(0);

    Long64_t timeBinNum=490;//490 day,70 week,16 month
    double timeLowEdge= 1370102400;
    double timeHighEdge=1412438400;

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

    TH1D* purTotal=new TH1D("purTotal","purTotal",timeBinNum,timeLowEdge,timeHighEdge);
    for( Long64_t ubti=0 ; ubti<ubtNum ; ubti++ )
    {
        user_balance_table->GetEntry(ubti);
        //purTotal->Fill(report_date_unix,total_purchase_amt);
        purTotal->Fill(report_date_unix,total_redeem_amt);
    }




    //==> remove holiday
    int preHoliday=3;
    int aftHoliday=3;
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
    TH1D* purTotalRemoveHoliday=new TH1D("purTotalRemoveHoliday","purTotalRemoveHoliday",timeBinNum,timeLowEdge,timeHighEdge);
    Long64_t hasRemoved[490]={0};
    Long64_t  needRemove[490]={0};
    Long64_t holidayNum=0;
    int holidayCount=sizeof(holidayDate)/sizeof(holidayDate[0])/2;
    int holidayCountNow=0;
    int holidayFirstBin[30];
    int holidayLastBin[30];
    for( Long64_t bi=1 ; bi<=timeBinNum ; bi++ )
    {
        double content=purTotal->GetBinContent(bi);
        //if(content==0) continue;
        Long64_t binLowEdge=purTotal->GetBinLowEdge(bi);
        for( Long64_t hi=0 ; hi<holidayCount ; hi++ )
        {
            Long64_t dateTmp=holidayDate[hi*2];
            Long64_t timeTmp=coverTime2unix(dateTmp);
            if(  binLowEdge==timeTmp )
            {
                holidayFirstBin[holidayCountNow]=bi;
                holidayLastBin[holidayCountNow++]=bi+holidayDate[hi*2+1]-1;
            }
            if( binLowEdge>=timeTmp&&(binLowEdge<timeTmp+holidayDate[hi*2+1]*86400)  )
            {
                //cout<<"find one "<<endl; 
                holidayNum++;
                needRemove[bi]=1;
                for( int pre=1 ; pre<=preHoliday ; pre++ )
                {
                    needRemove[bi-pre]=1;
                }
                for( int  aft=1 ; aft<=aftHoliday ; aft++ )
                {
                    needRemove[bi+aft]=1;
                }
                break;
            }

        }
    }
    cout<<"holidayNum  : "<<holidayNum<<endl;


    double x[1000]={1,2,5};
    double y[1000]={2,5,26};
    double par[100]={0.};
    Long64_t removeNum=0;
    for( Long64_t bi=1 ; bi<=timeBinNum ; bi++ )
    {
        double content=purTotal->GetBinContent(bi);
        if(content==0) continue;
        double contentAfterRemove=content;
        if( needRemove[bi])
        {
            removeNum++;
            Long64_t totalNum=0;
            Long64_t prei=1;
            while(totalNum<7||(bi-prei<0))
            {
                if( !needRemove[bi-prei] )
                {
                    x[totalNum]=purTotal->GetBinLowEdge(bi-prei);
                    y[totalNum]=purTotal->GetBinContent(bi-prei);
                    totalNum++;
                }
                prei++;
            }
            Long64_t afti=1;
            while(totalNum<14||(bi+afti<0))
            {
                if( !needRemove[bi+afti] )
                {
                    x[totalNum]=purTotal->GetBinLowEdge(bi+afti);
                    y[totalNum]=purTotal->GetBinContent(bi+afti);
                    totalNum++;
                }
                afti++;
            }
            //cout<<"totalNum  : "<<totalNum<<endl;
            fit(totalNum,x,y,"pol1",par);
            //cout<<"func  : "<<par[0]<<"+"<<par[1]<<"*t"<<endl;
            contentAfterRemove=par[0]+par[1]*purTotal->GetBinLowEdge(bi);
            //cout<<"remove  : "<<content<<" -> "<<contentAfterRemove<<endl;
        } 
        purTotalRemoveHoliday->SetBinContent(bi,contentAfterRemove);
    }
    cout<<"removeNum  : "<<removeNum<<endl;

    //c->cd(1);
    purTotal->GetXaxis()->SetTimeDisplay(1);
    purTotal->GetXaxis()->SetTimeFormat("%m/%d");
    purTotal->Draw("hist");
    purTotalRemoveHoliday->SetLineColor(kRed);
    purTotalRemoveHoliday->Draw("same");




    //==> fit 20140302-20140828
    Long64_t purFitNum=0;
    Long64_t tmp=coverTime2unix(20140302);
    Long64_t firstFitBin=purTotal->FindBin(tmp);
    tmp=coverTime2unix(20140828);
    Long64_t lastFitBin=purTotal->FindBin(tmp);
    cout<<"firstFitBin-lastFitBin  : "<<firstFitBin<<"-"<<lastFitBin<<endl;
    for( Long64_t puri=firstFitBin ; puri<=lastFitBin ; puri++ )
    {
        x[purFitNum]=purTotalRemoveHoliday->GetBinLowEdge(puri);
        y[purFitNum]=purTotalRemoveHoliday->GetBinContent(puri);
        purFitNum++;
    }
    cout<<"purFitNum  : "<<purFitNum<<endl;
    //fit(purFitNum,x,y,"expo(0)+pol1(2)",par,0);
    //TF1* purf=new TF1("purf","expo(0)+pol1(2)",x[0],x[purFitNum-1]);
    //purf->SetParameter(0,par[0]);
    //purf->SetParameter(1,par[1]);
    //purf->SetParameter(2,par[2]);
    //purf->SetParameter(3,par[3]);
    //purf->SetLineColor(kBlue);
    //purf->Draw("same");
    fit(purFitNum,x,y,"expo",par,0);
    TF1* purf=new TF1("purf","expo",x[0],x[purFitNum-1]);
    //fit(purFitNum,x,y,"pol3",par,0);
    //TF1* purf=new TF1("purf","pol3",x[0],x[purFitNum-1]);
    //fit(purFitNum,x,y,"pow(x,[0])*[1]",par,0);
    //TF1* purf=new TF1("purf","pow(x,[0])*[1]",x[0],x[purFitNum-1]);
    purf->SetParameter(0,par[0]);
    purf->SetParameter(1,par[1]);
    //purf->SetParameter(2,par[2]);
    //purf->SetParameter(3,par[3]);
    purf->SetLineColor(kBlue);
    purf->Draw("same");



    //==> variation inside one week
    int weekDayNum[7]={0};
    double weekDayRatio[7]={0.};
    double weekDayRatioErr2[7]={0.};
    for( int puri=firstFitBin ; puri<=lastFitBin ; puri++ )
    {
        if( needRemove[puri]==0 )
        {
            time_t timep=purTotal->GetBinLowEdge(puri);
            double purContent=purTotal->GetBinContent(puri);
            double funcValue=purf->Eval(timep);
            struct tm *p ;
            p=localtime(&timep);
            //cout<<"week  : "<<p->tm_wday<<endl;
            weekDayNum[p->tm_wday]++;
            weekDayRatio[p->tm_wday]+=(purContent-funcValue)/funcValue;
            weekDayRatioErr2[p->tm_wday]+=(purContent+funcValue)/funcValue/funcValue*(1+(purContent+funcValue)/funcValue);
        }
    }
    for( int wi=0 ; wi<7 ; wi++ )
    {
        weekDayRatio[wi]=weekDayRatio[wi]/weekDayNum[wi];
        weekDayRatioErr2[wi]=weekDayRatioErr2[wi]/weekDayNum[wi]/weekDayNum[wi];
        cout<<" "<<wi<<"  : "<<weekDayNum[wi]<<" "<<weekDayRatio[wi]<<" +- "<<sqrt(weekDayRatioErr2[wi])<<endl;
    }


    //==> variation before and after holiday
    //double** preHolidayRatio=(double**)malloc(sizeof(double*)*holidayCount);
    //for( int ai=0 ; ai<holidayCount ; ai++ )
    //{
    //preHolidayRatio[ai]=(double*)malloc(sizeof(double)*preHoliday);
    //}
    //double** aftHolidayRatio=(double**)malloc(sizeof(double*)*holidayCount);
    //for( int ai=0 ; ai<holidayCount ; ai++ )
    //{
    //aftHolidayRatio[ai]=(double*)malloc(sizeof(double)*aftHoliday);
    //}
    //for( int ai=0 ; ai<holidayCount ; ai++ )
    //{
    //free(aftHolidayRatio[ai]);
    //}
    //free(aftHolidayRatio);
    //for( int ai=0 ; ai<holidayCount ; ai++ )
    //{
    //free(preHolidayRatio[ai]);
    //}
    //free(preHolidayRatio);
    TH1D* expectedPurOnlyCorWeek=new TH1D("expectedPurOnlyCorWeek","expectedPurOnlyCorWeek",timeBinNum,timeLowEdge,timeHighEdge);
    for( int bi=1 ; bi<=timeBinNum ; bi++ )
    {
        expectedPurOnlyCorWeek->SetBinContent(bi,purTotal->GetBinContent(bi));
    }

    double preHolidayRatio[30][10]={0.};
    double aftHolidayRatio[30][10]={0.};
    double holidayRatio[30][10]={0.};
    double preHolidayRatioErr2[30][10]={0.};
    double aftHolidayRatioErr2[30][10]={0.};
    double holidayRatioErr2[30][10]={0.};
    for( int hi=0 ; hi<holidayCount ; hi++ )
    {
        for( int prei=preHoliday ; prei>=1; prei-- )
        {
            if((holidayFirstBin[hi]-prei>=firstFitBin)&&(holidayFirstBin[hi]-prei<=lastFitBin)) 
            {
                double purContent=purTotal->GetBinContent(holidayFirstBin[hi]-prei);
                time_t timep=purTotal->GetBinLowEdge(holidayFirstBin[hi]-prei);
                struct tm *p ;
                p=localtime(&timep);
                double expectedValue=purf->Eval(timep)*(1+weekDayRatio[p->tm_wday]);
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
            if( hoi>=firstFitBin&&hoi<=lastFitBin )
            {
                double purContent=purTotal->GetBinContent(hoi);
                time_t timep=purTotal->GetBinLowEdge(hoi);
                struct tm *p ;
                p=localtime(&timep);
                double expectedValue=purf->Eval(timep)*(1+weekDayRatio[p->tm_wday]);
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
            if((holidayLastBin[hi]+afti>=firstFitBin)&&(holidayLastBin[hi]+afti<=lastFitBin)) 
            {
                double purContent=purTotal->GetBinContent(holidayLastBin[hi]+afti);
                time_t timep=purTotal->GetBinLowEdge(holidayLastBin[hi]+afti);
                struct tm *p ;
                p=localtime(&timep);
                double expectedValue=purf->Eval(timep)*(1+weekDayRatio[p->tm_wday]);
                expectedPurOnlyCorWeek->SetBinContent(holidayLastBin[hi]+afti,expectedValue);
                aftHolidayRatio[hi][afti-1]=(purContent-expectedValue)/expectedValue;
                aftHolidayRatioErr2[hi][afti-1]=(purContent+expectedValue)/expectedValue/expectedValue*(1+(purContent+expectedValue)/expectedValue);
                //cout<<" "<<aftHolidayRatio[hi][afti-1];
            }
        }
        //cout<<endl;

    }

    double preHolidayRatioMean[10]={0.};
    double aftHolidayRatioMean[10]={0.};
    double holidayRatioMean[10]={0.};
    double preHolidayRatioErr2Mean[10]={0.};
    double aftHolidayRatioErr2Mean[10]={0.};
    double holidayRatioErr2Mean[10]={0.};

    double preHolidayRatioCount[10]={0.};
    double aftHolidayRatioCount[10]={0.};
    double holidayRatioCount[10]={0.};
    double preHolidayRatioErr2Count[10]={0.};
    double aftHolidayRatioErr2Count[10]={0.};
    double holidayRatioErr2Count[10]={0.};
    for( int hi=0 ; hi<holidayCount ; hi++ )
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

    for( int prei=preHoliday-1 ; prei>0; prei-- )
    {
        preHolidayRatioMean[prei]/=preHolidayRatioCount[prei];
        preHolidayRatioErr2Mean[prei]/=preHolidayRatioCount[prei];
        cout<<"  : "<<preHolidayRatioMean[prei]<<" +- "<<sqrt(preHolidayRatioErr2Mean[prei])<<endl;
    }
    holidayRatioMean[0]/=aftHolidayRatioCount[0];
    holidayRatioErr2Mean[0]/=holidayRatioCount[0];
    cout<<"  = "<<holidayRatioMean[0]<<" +- "<<sqrt(holidayRatioErr2Mean[0])<<endl;
    for( int afti=0 ; afti<aftHoliday ; afti++ )
    {
        aftHolidayRatioMean[afti]/=aftHolidayRatioCount[afti];
        aftHolidayRatioErr2Mean[afti]/=aftHolidayRatioCount[afti];
        cout<<"  : "<<aftHolidayRatioMean[afti]<<" +- "<<sqrt(aftHolidayRatioErr2Mean[afti])<<endl;
    }
    //c->cd(2);
    expectedPurOnlyCorWeek->SetLineColor(kGreen);
    expectedPurOnlyCorWeek->Draw("same");


    //==> expected purchase 
    TH1D* expectedPur=new TH1D("expectedPur","expectedPur",timeBinNum,timeLowEdge,timeHighEdge);
    for( int bi=1 ; bi<=timeBinNum ; bi++ )
    {
        expectedPur->SetBinContent(bi,purTotal->GetBinContent(bi));
        if( purTotal->GetBinContent(bi)==0. && bi>=firstFitBin )
        {
                time_t timep=purTotal->GetBinLowEdge(bi);
                struct tm *p ;
                p=localtime(&timep);
                double expectedValue=purf->Eval(timep)*(1+weekDayRatio[p->tm_wday]);
                expectedPur->SetBinContent(bi,expectedValue);
        }
    }
    for( int hi=0 ; hi<holidayCount ; hi++ )
    {
        for( int prei=preHoliday ; prei>=1; prei-- )
        {
            if(holidayFirstBin[hi]-prei>=firstFitBin) 
            {
                time_t timep=purTotal->GetBinLowEdge(holidayFirstBin[hi]-prei);
                struct tm *p ;
                p=localtime(&timep);
                double expectedValue=purf->Eval(timep)*(1+weekDayRatio[p->tm_wday])*(1+preHolidayRatioMean[prei]);
                expectedPur->SetBinContent(holidayFirstBin[hi]-prei,expectedValue);
            }
        }
        for( int hoi=holidayFirstBin[hi] ; hoi<=holidayLastBin[hi] ; hoi++ )
        {
            int calCount=0;
            if( hoi>=firstFitBin)
            {
                //if( hoi>lastFitBin )
                //{
                //cout<<"find a holiday "<<endl;
                //}
                time_t timep=purTotal->GetBinLowEdge(hoi);
                struct tm *p ;
                p=localtime(&timep);
                double expectedValue=purf->Eval(timep)*(1+weekDayRatio[p->tm_wday])*(1+holidayRatioMean[0]);
                expectedPur->SetBinContent(hoi,expectedValue);
            }

        }
        for( int afti=1 ; afti<=aftHoliday ; afti++ )
        {
            if(holidayLastBin[hi]+afti>=firstFitBin) 
            {
                time_t timep=purTotal->GetBinLowEdge(holidayLastBin[hi]+afti);
                struct tm *p ;
                p=localtime(&timep);
                double expectedValue=purf->Eval(timep)*(1+weekDayRatio[p->tm_wday])*(1+aftHolidayRatioMean[afti]);
                expectedPur->SetBinContent(holidayLastBin[hi]+afti,expectedValue);
            }
        }
    }

    expectedPur->SetLineColor(kOrange);
    expectedPur->Draw("same");


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
    //check is vaild date
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
