{
    gStyle->SetTitleStyle(0); 
    gStyle->SetTitleBorderSize(0); 
    gStyle->SetOptStat(0);
    TFile* f=new TFile("tianChiOriginData.root","read");

    TCanvas* c=new TCanvas("c","c",1800,900);
    c->Divide(5,4);

    TTree* user_balance_table=(TTree*)f->Get("user_balance_table");
    string branchName[3]={"tBalance","total_purchase_amt","total_redeem_amt"};
    TH2D* h2[3];
    TH1D* h2total[3];
    TH1D* h2Projection[3];
    TProfile* h2Profile[3];
    TGraph* gweek[3][5][7];

    int ubtNum=user_balance_table->GetEntries();
    Long64_t report_date_unix;
    Long64_t branchValue[3]={0}; 
    user_balance_table->SetBranchAddress("report_date_unix",&report_date_unix);

    for( int i=0 ; i<3 ; i++ )
    {
        user_balance_table->SetBranchAddress(Form("%s",branchName[i].c_str()),&branchValue[i]);
        h2[i]=new TH2D(Form("%sVsDate",branchName[i].c_str()),Form("%sVsDate",branchName[i].c_str()),487,1370016000,1412092800,2200,0,220000000);
        h2Profile[i]=new TProfile(Form("%s_ProfileX",branchName[i].c_str()),Form("%s_ProfileX",branchName[i].c_str()),487,1370016000,1412092800);
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
        c->cd(i*5+2);
        h2total[i]=new TH1D(Form("%s_total",branchName[i].c_str()),Form("%s_total",branchName[i].c_str()),487,1370016000,1412092800);
        for( int hi=1 ; hi<=487 ; hi++ )
        {
            h2total[i]->SetBinContent(hi,h2Profile[i]->GetBinEntries(hi)*h2Profile[i]->GetBinContent(hi));
        }
        h2total[i]->GetXaxis()->SetTimeDisplay(1);
        h2total[i]->GetXaxis()->SetTimeFormat("%m/%d");
        h2total[i]->Draw();

        //draw line for each weekday
        double x[5][7][70]={0.};
        double y[5][7][70]={0.};
        TH1D* htmp[5];
        htmp[1]=h2total[i];
        htmp[2]=h2Profile[i];
        htmp[3]=h2Projection[i];
        for( int ci=1 ; ci<4 ; ci++ )
        {
            c->cd(i*5+ci+1);
            for( int hi=1 ; hi<=487 ; hi++ )
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

    //mfd_daily_yield;mfd_7daily_yield
    TTree* mfd_day_share_interest=(TTree*)f->Get("mfd_day_share_interest");
    int msiNum=mfd_day_share_interest->GetEntries();
    double msix[428]={0.};
    double msiy[428]={0.};
    double msi7y[428]={0.};
    double mfd_daily_yield=0.;
    double mfd_7daily_yield=0.;
    Long64_t mfd_date;
    mfd_day_share_interest->SetBranchAddress("mfd_daily_yield",&mfd_daily_yield);
    mfd_day_share_interest->SetBranchAddress("mfd_7daily_yield",&mfd_7daily_yield);
    mfd_day_share_interest->SetBranchAddress("mfd_date_unix",&mfd_date);
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
    gy->GetXaxis()->SetLimits(1370016000,1412092800);
    gy->SetTitle("mfd_daily_yield");
    gy->Draw();
    g7y->SetLineColor(kRed);
    g7y->Draw("same");
    //c->cd(17);
    //g7y->Draw("C");
    //g7y->GetXaxis()->SetLimits(1370016000,1412092800);
    //g7y->SetTitle("mfd_7daily_yield");
    //g7y->Draw();

    //mfd_bank_shibor
    TTree* mfd_bank_shibor=(TTree*)f->Get("mfd_bank_shibor");
    int mbsi=mfd_bank_shibor->GetEntries();
    //cout<<"mbsi  : "<<mbsi<<endl;
    double mbsx[295]={0.};
    double mbsy[8][295]={0.};
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
    gmbs[0]->GetXaxis()->SetLimits(1370016000,1412092800);
    gmbs[0]->SetTitle("shibor");
    gmbs[0]->Draw();
    for( int mbsii=1 ; mbsii<8 ; mbsii++ )
    {
        gmbs[mbsii]=new TGraph(mbsi,mbsx,mbsy[mbsii]);
        gmbs[mbsii]->SetLineColor(mbsii+1);
        gmbs[mbsii]->Draw("same");
    }
}
