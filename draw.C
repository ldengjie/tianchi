{
    gStyle->SetTitleStyle(0); 
    gStyle->SetTitleBorderSize(0); 
    gStyle->SetOptStat(0);

    //int timeBinNum=490;
    //double timeLowEdge= 1370016000;
    //double timeHighEdge=1370275200;
    int timeBinNum=490;//490 day,70 week,16 month
    double timeLowEdge= 1370102400;
    double timeHighEdge=1412438400;

    TFile* f=new TFile("tianChiOriginData.root","read");

    //user_balance_table
    TTree* user_balance_table=(TTree*)f->Get("user_balance_table");
    int ubtNum=user_balance_table->GetEntries();
    Long64_t report_date_unix;
    Long64_t branchValue[3]={0}; 
    Long64_t user_id;
    user_balance_table->SetBranchAddress("user_id",&user_id);
    user_balance_table->SetBranchAddress("report_date_unix",&report_date_unix);

    //user_profile_table
    TTree* user_profile_table=(TTree*)f->Get("user_profile_table");
    int uptNum=user_profile_table->GetEntries();
    Long64_t user_id_upt;
    Long64_t sex;
    Long64_t city;
    Long64_t constellation;
    user_profile_table->SetBranchAddress("user_id",&user_id_upt);
    user_profile_table->SetBranchAddress("sex",&sex);
    user_profile_table->SetBranchAddress("city",&city);
    user_profile_table->SetBranchAddress("constellation",&constellation);

    //mfd_day_share_interest
    TTree* mfd_day_share_interest=(TTree*)f->Get("mfd_day_share_interest");
    int msiNum=mfd_day_share_interest->GetEntries();
    double mfd_daily_yield=0.;
    double mfd_7daily_yield=0.;
    Long64_t mfd_date;
    mfd_day_share_interest->SetBranchAddress("mfd_daily_yield",&mfd_daily_yield);
    mfd_day_share_interest->SetBranchAddress("mfd_7daily_yield",&mfd_7daily_yield);
    mfd_day_share_interest->SetBranchAddress("mfd_date_unix",&mfd_date);

    //mfd_bank_shibor
    TTree* mfd_bank_shibor=(TTree*)f->Get("mfd_bank_shibor");
    int mbsi=mfd_bank_shibor->GetEntries();
    Long64_t mfd_date_mbs;
    double Interest_O_N=0.;
    double Interest_1_W=0.;
    double Interest_2_W=0.;
    double Interest_1_M=0.;
    double Interest_3_M=0.;
    double Interest_6_M=0.;
    double Interest_9_M=0.;
    double Interest_1_Y=0.;
    mfd_bank_shibor->SetBranchAddress("mfd_date_unix",&mfd_date_mbs);
    mfd_bank_shibor->SetBranchAddress("Interest_O_N",&Interest_O_N);
    mfd_bank_shibor->SetBranchAddress("Interest_1_W",&Interest_1_W);
    mfd_bank_shibor->SetBranchAddress("Interest_2_W",&Interest_2_W);
    mfd_bank_shibor->SetBranchAddress("Interest_1_M",&Interest_1_M);
    mfd_bank_shibor->SetBranchAddress("Interest_3_M",&Interest_3_M);
    mfd_bank_shibor->SetBranchAddress("Interest_6_M",&Interest_6_M);
    mfd_bank_shibor->SetBranchAddress("Interest_9_M",&Interest_9_M);
    mfd_bank_shibor->SetBranchAddress("Interest_1_Y",&Interest_1_Y);

    TCanvas* c=new TCanvas("c","c",1800,900);
    c->Divide(5,4);

    //user increment
    Long64_t usrJoinTime[28367]={0};
    for( int ubti=0 ; ubti<ubtNum ; ubti++ )
    {
       user_balance_table->GetEntry(ubti); 
       if( usrJoinTime[user_id]==0||(usrJoinTime[user_id]!=0&&usrJoinTime[user_id]>report_date_unix) )
       {
           usrJoinTime[user_id]=report_date_unix;
       }
    }
    Long64_t usrIndex[28367]={0};
    for( int upti=0 ; upti<uptNum ; upti++ )
    {
        user_profile_table->GetEntry(upti);
        usrIndex[user_id_upt]=upti;
    }
    
    TH1D* hUsrJoinTime=new TH1D("hUsrJoinTime","hUsrJoinTime",timeBinNum,timeLowEdge,timeHighEdge);
    TH1D* hUsrSex[2];
    hUsrSex[0] =new TH1D("hUsrSexGirl","hUsrSexGirl",timeBinNum,timeLowEdge,timeHighEdge);
    hUsrSex[1] =new TH1D("hUsrSexBoy","hUsrSexBoy",timeBinNum,timeLowEdge,timeHighEdge);
    for( int upti=0 ; upti<28367 ; upti++ )
    {
        if( usrJoinTime[upti]!=0 )
        {
            hUsrJoinTime->Fill(usrJoinTime[upti]);
            user_profile_table->GetEntry(usrIndex[upti]);
            hUsrSex[sex]->Fill(usrJoinTime[upti]);
        }
    }
    c->cd(19);
    hUsrJoinTime->GetXaxis()->SetTimeDisplay(1);
    hUsrJoinTime->GetXaxis()->SetTimeFormat("%m/%d");
    hUsrJoinTime->Draw();
    hUsrSex[0]->SetLineColor(kRed);
    hUsrSex[0]->Draw("same");
    hUsrSex[1]->SetLineColor(kGreen);
    hUsrSex[1]->Draw("same");
    
    c->cd(20);
    TH1D* hUsrJoinTimeCumu=new TH1D("hUsrJoinTimeCumu","hUsrJoinTimeCumu",timeBinNum,timeLowEdge,timeHighEdge);
    TH1D* hUsrSexCumu[2];
    hUsrSexCumu[0] =new TH1D("hUsrSexCumuGirl","hUsrSexCumuGirl",timeBinNum,timeLowEdge,timeHighEdge);
    hUsrSexCumu[1] =new TH1D("hUsrSexCumuBoy","hUsrSexCumuBoy",timeBinNum,timeLowEdge,timeHighEdge);
    Long64_t hUsrJoinTimeCount=0;
    Long64_t hUsrSexCount[2]={0};
    for( int upti=1 ; upti<=timeBinNum; upti++ )
    {
        if( hUsrJoinTime->GetBinContent(upti)!=0 )
        {
            hUsrJoinTimeCount+=hUsrJoinTime->GetBinContent(upti);
            hUsrJoinTimeCumu->SetBinContent(upti,hUsrJoinTimeCount);
            hUsrSexCount[0]+=hUsrSex[0]->GetBinContent(upti);
            hUsrSexCumu[0]->SetBinContent(upti,hUsrSexCount[0]);
            hUsrSexCount[1]+=hUsrSex[1]->GetBinContent(upti);
            hUsrSexCumu[1]->SetBinContent(upti,hUsrSexCount[1]);
        }
    }
    hUsrJoinTimeCumu->GetXaxis()->SetTimeDisplay(1);
    hUsrJoinTimeCumu->GetXaxis()->SetTimeFormat("%m/%d");
    hUsrJoinTimeCumu->Draw();
    hUsrSexCumu[0]->SetLineColor(kRed);
    hUsrSexCumu[0]->Draw("same");
    hUsrSexCumu[1]->SetLineColor(kGreen);
    hUsrSexCumu[1]->Draw("same");


    //"tBalance","total_purchase_amt","total_redeem_amt"
    string branchName[3]={"tBalance","total_purchase_amt","total_redeem_amt"};
    TH2D* h2[3];
    TH1D* h2total[3];
    TH1D* h2Projection[3];
    TH1D* activeUserRatioPerDay[3];
    TProfile* h2Profile[3];
    TGraph* gweek[3][5][7];
    for( int i=0 ; i<3 ; i++ )
    {
        user_balance_table->SetBranchAddress(Form("%s",branchName[i].c_str()),&branchValue[i]);
        h2[i]=new TH2D(Form("%sVsDate",branchName[i].c_str()),Form("%sVsDate",branchName[i].c_str()),timeBinNum,timeLowEdge,timeHighEdge,2200,0,220000000);
        h2Profile[i]=new TProfile(Form("%s_ProfileX",branchName[i].c_str()),Form("%s_ProfileX",branchName[i].c_str()),timeBinNum,timeLowEdge,timeHighEdge);
        activeUserRatioPerDay[i]=new TH1D(Form("%s_r",branchName[i].c_str()),Form("%s_r",branchName[i].c_str()),timeBinNum,timeLowEdge,timeHighEdge);
        for( int ti=0 ; ti<ubtNum ; ti++ )
        {
            user_balance_table->GetEntry(ti);
            if( branchValue[i]!=0 )
            {
                h2[i]->Fill(report_date_unix,branchValue[i]);
                h2Profile[i]->Fill(report_date_unix,branchValue[i]);
            }
        }
        c->cd(i*5+1);
        h2[i]->GetXaxis()->SetTimeDisplay(1);
        h2[i]->GetXaxis()->SetTimeFormat("%m/%d");
        h2[i]->Draw("colz");
        gPad->SetLogy();
        gPad->SetLogz();
        c->cd(i*5+3);
        h2Profile[i]->GetXaxis()->SetTimeDisplay(1);
        h2Profile[i]->GetXaxis()->SetTimeFormat("%m/%d");
        h2Profile[i]->Draw("hist");
        c->cd(i*5+4);
        h2Projection[i]=h2[i]->ProjectionX(Form("%s_ProjectionX",branchName[i].c_str()));
        h2Projection[i]->GetXaxis()->SetTimeDisplay(1);
        h2Projection[i]->GetXaxis()->SetTimeFormat("%m/%d");
        h2Projection[i]->Draw("hist");
        c->cd(i*5+5);
        for( int ubti =1; ubti<=timeBinNum ; ubti++ )
        {
            if( hUsrJoinTimeCumu->GetBinContent(ubti)!=0)
            {
                //cout<<"ubti  : "<<ubti<<endl;
                //cout<<" h2Projection[i]->GetBinContent(ubti)  : "<<h2Projection[i]->GetBinContent(ubti)<<endl;
                //cout<<" hUsrJoinTimeCumu->GetBinContent(ubti)  : "<<hUsrJoinTimeCumu->GetBinContent(ubti)<<endl;
                activeUserRatioPerDay[i]->SetBinContent(ubti,(double)h2Projection[i]->GetBinContent(ubti)/hUsrJoinTimeCumu->GetBinContent(ubti));
                //cout<<"done   "<<endl;
            }
        }
        activeUserRatioPerDay[i]->GetXaxis()->SetTimeDisplay(1);
        activeUserRatioPerDay[i]->GetXaxis()->SetTimeFormat("%m/%d");
        activeUserRatioPerDay[i]->Draw("hist");
        
        c->cd(i*5+2);
        h2total[i]=new TH1D(Form("%s_total",branchName[i].c_str()),Form("%s_total",branchName[i].c_str()),timeBinNum,timeLowEdge,timeHighEdge);
        for( int hi=1 ; hi<=timeBinNum ; hi++ )
        {
            h2total[i]->SetBinContent(hi,h2Profile[i]->GetBinEntries(hi)*h2Profile[i]->GetBinContent(hi));
        }
        h2total[i]->GetXaxis()->SetTimeDisplay(1);
        h2total[i]->GetXaxis()->SetTimeFormat("%m/%d");
        h2total[i]->Draw();

        //draw line for each weekday
        if( timeBinNum>=490 )
        {
            double x[5][7][70]={0.};
            double y[5][7][70]={0.};
            TH1D* htmp[5];
            htmp[1]=h2total[i];
            htmp[2]=h2Profile[i];
            htmp[3]=h2Projection[i];
            htmp[4]=activeUserRatioPerDay[i];
            for( int ci=1 ; ci<=4 ; ci++ )
            {
                c->cd(i*5+ci+1);
                for( int hi=1 ; hi<=timeBinNum ; hi++ )
                {
                    Long64_t unixTime=htmp[ci]->GetBinCenter(hi);
                    int week=(unixTime/86400+3)%7+1;
                    //cout<<unixTime<<" week  : "<<week<<endl;
                    x[ci][week-1][hi/7]=unixTime;
                    y[ci][week-1][hi/7]=htmp[ci]->GetBinContent(hi);
                }
                for( int gi=0 ; gi<7 ; gi++ )
                {
                    gweek[i][ci][gi]=new TGraph(70,x[ci][gi],y[ci][gi]);
                    gweek[i][ci][gi]->SetLineColor(gi+1);
                    gweek[i][ci][gi]->Draw("same");
                }
            }
        }
    }

    //mfd_daily_yield;mfd_7daily_yield
    double msix[428]={0.};
    double msiy[428]={0.};
    double msi7y[428]={0.};
    for( int msii=0 ; msii<msiNum ; msii++ )
    {
        mfd_day_share_interest->GetEntry(msii);
        msix[msii]=mfd_date;
        msiy[msii]=mfd_daily_yield;
        msi7y[msii]=mfd_7daily_yield;
    }
    TGraph* gy=new TGraph(msiNum,msix,msiy);
    TGraph* g7y=new TGraph(msiNum,msix,msi7y);
    gy->GetXaxis()->SetTimeDisplay(1);
    gy->GetXaxis()->SetTimeFormat("%m/%d");
    g7y->GetXaxis()->SetTimeDisplay(1);
    g7y->GetXaxis()->SetTimeFormat("%m/%d");
    gy->SetMinimum(0);
    gy->SetMaximum(7);
    c->cd(17);
    //gy->Draw("c");
    gy->GetXaxis()->SetLimits(timeLowEdge,timeHighEdge);
    gy->SetTitle("mfd_daily_yield");
    gy->Draw();
    g7y->SetLineColor(kRed);
    g7y->Draw("same");
    //c->cd(17);
    //g7y->Draw("C");
    //g7y->GetXaxis()->SetLimits(timeLowEdge,timeHighEdge);
    //g7y->SetTitle("mfd_7daily_yield");
    //g7y->Draw();

    //mfd_bank_shibor
    double mbsx[295]={0.};
    double mbsy[8][295]={0.};
    for( int mbsii=0 ; mbsii<mbsi ; mbsii++ )
    {
        mfd_bank_shibor->GetEntry(mbsii);
        mbsx[mbsii]=mfd_date_mbs;
        mbsy[0][mbsii]=Interest_O_N;
        mbsy[1][mbsii]=Interest_1_W;
        mbsy[2][mbsii]=Interest_2_W;
        mbsy[3][mbsii]=Interest_1_M;
        mbsy[4][mbsii]=Interest_3_M;
        mbsy[5][mbsii]=Interest_6_M;
        mbsy[6][mbsii]=Interest_9_M;
        mbsy[7][mbsii]=Interest_1_Y;
    }
    c->cd(18);
    TGraph* gmbs[8];
    gmbs[0]=new TGraph(mbsi,mbsx,mbsy[0]);
    gmbs[0]->GetXaxis()->SetTimeDisplay(1);
    gmbs[0]->GetXaxis()->SetTimeFormat("%m/%d");
    gmbs[0]->SetMinimum(0);
    gmbs[0]->SetMaximum(9);
    gmbs[0]->GetXaxis()->SetLimits(timeLowEdge,timeHighEdge);
    gmbs[0]->SetTitle("shibor");
    gmbs[0]->Draw();
    for( int mbsii=1 ; mbsii<8 ; mbsii++ )
    {
        gmbs[mbsii]=new TGraph(mbsi,mbsx,mbsy[mbsii]);
        gmbs[mbsii]->SetLineColor(mbsii+1);
        gmbs[mbsii]->Draw("same");
    }


    
}
