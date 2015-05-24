{
    gStyle->SetTitleStyle(0); 
    gStyle->SetTitleBorderSize(0); 
    gStyle->SetOptStat(0);

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
    Long64_t tBalance;
    user_balance_table->SetBranchAddress("user_id",&user_id);
    user_balance_table->SetBranchAddress("tBalance",&tBalance);
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

    TCanvas* c=new TCanvas("c","c",1800,900);
    c->Divide(5,4);

    //user category
    Long64_t maxBalance[28367]={0};
    Long64_t usrJoinTime[28367]={0};
    for( int ubti=0 ; ubti<ubtNum ; ubti++ )
    {
       user_balance_table->GetEntry(ubti); 
       if( maxBalance[user_id]<tBalance )
       {
           maxBalance[user_id]=tBalance;
       }
    }
    for( int ubti=0 ; ubti<ubtNum ; ubti++ )
    {
       user_balance_table->GetEntry(ubti); 
       if( (usrJoinTime[user_id]==0||(usrJoinTime[user_id]!=0&&usrJoinTime[user_id]>report_date_unix))&&maxBalance[user_id]!=0 )
       {
           usrJoinTime[user_id]=report_date_unix;
       }
    }
    Long64_t cateEdge[10]={0,500000,1000000,5000000,10000000,20000000,40000000,70000000,100000000,10000000000};//yuan
    Long64_t usrCategory[28367]={0};
    TH1D* hUsrCategory=new TH1D("hUsrCategory","hUsrCategory",9,1,10);
    for( int upti=0 ; upti<28367 ; upti++ )
    {
        for( int catei=0 ; catei< 9; catei++ )
        {
            if(maxBalance[upti]==0) break;
            if( maxBalance[upti]>cateEdge[catei]&&maxBalance[upti]<=cateEdge[catei+1] )
            {
                usrCategory[upti]=catei+1;
                hUsrCategory->Fill(catei+1);
                break;
            }
        }
    }
    c->cd(16);
    hUsrCategory->Draw();

    Long64_t usrIndex[28367]={0};
    for( int upti=0 ; upti<uptNum ; upti++ )
    {
        user_profile_table->GetEntry(upti);
        usrIndex[user_id_upt]=upti;
    }


    c->cd(19);
    TH1D* hUsrJoinTime=new TH1D("hUsrJoinTime","hUsrJoinTime",timeBinNum,timeLowEdge,timeHighEdge);
    TH1D* hUsrJoinTimeUsrCate[9];
    for( int catei=0 ; catei<9 ; catei++ )
    {
        hUsrJoinTimeUsrCate[catei]=new TH1D(Form("hUsrJoinTimeUsrCate%i",catei+1),Form("hUsrJoinTimeUsrCate%i",catei+1),timeBinNum,timeLowEdge,timeHighEdge);
    }
    
    for( int upti=0 ; upti<28367 ; upti++ )
    {
        if( usrJoinTime[upti]!=0 )
        {
            hUsrJoinTime->Fill(usrJoinTime[upti]);
            //cout<<"upti  : "<<upti<<endl;
            //cout<<"usrJoinTime[upti]  : "<<usrJoinTime[upti]<<endl;
            //cout<<"usrCategory[upti]  : "<<usrCategory[upti]<<endl;
            hUsrJoinTimeUsrCate[usrCategory[upti]-1]->Fill(usrJoinTime[upti]);
        }
    }
    hUsrJoinTime->GetXaxis()->SetTimeDisplay(1);
    hUsrJoinTime->GetXaxis()->SetTimeFormat("%m/%d");
    hUsrJoinTime->Draw();
    for( int catei=0 ; catei<9 ; catei++ )
    {
        hUsrJoinTimeUsrCate[catei]->SetLineColor(catei+1);
        hUsrJoinTimeUsrCate[catei]->Draw("sameC");
    }

    c->cd(20);
    TH1D* hUsrJoinTimeCumu=new TH1D("hUsrJoinTimeCumu","hUsrJoinTimeCumu",timeBinNum,timeLowEdge,timeHighEdge);
    Long64_t hUsrJoinTimeCount=0;
    Long64_t hUsrJoinTimeCountUsrCate[9]={0};
    TH1D* hUsrJoinTimeUsrCateCumu[9];
    for( int catei=0 ; catei<9 ; catei++ )
    {
        hUsrJoinTimeUsrCateCumu[catei]=new TH1D(Form("hUsrJoinTimeUsrCateCumu%i",catei+1),Form("hUsrJoinTimeUsrCateCumu%i",catei+1),timeBinNum,timeLowEdge,timeHighEdge);
    }
    for( int upti=1 ; upti<=timeBinNum; upti++ )
    {
        if( hUsrJoinTime->GetBinContent(upti)!=0 )
        {
            hUsrJoinTimeCount+=hUsrJoinTime->GetBinContent(upti);
            hUsrJoinTimeCumu->SetBinContent(upti,hUsrJoinTimeCount);
            for( int catei=0 ; catei<9 ; catei++ )
            {
                hUsrJoinTimeCountUsrCate[catei]+=hUsrJoinTimeUsrCate[catei]->GetBinContent(upti);
                hUsrJoinTimeUsrCateCumu[catei]->SetBinContent(upti,hUsrJoinTimeCountUsrCate[catei]);
            }
        }
    }
    hUsrJoinTimeCumu->GetXaxis()->SetTimeDisplay(1);
    hUsrJoinTimeCumu->GetXaxis()->SetTimeFormat("%m/%d");
    hUsrJoinTimeCumu->Draw();
    for( int catei=0 ; catei<9 ; catei++ )
    {
        hUsrJoinTimeUsrCateCumu[catei]->SetLineColor(catei+1);
        hUsrJoinTimeUsrCateCumu[catei]->Draw("sameC");
    }

    //"tBalance","total_purchase_amt","total_redeem_amt"
    string branchName[3]={"tBalance","total_purchase_amt","total_redeem_amt"};
    //string branchName[3]={"tBalance","direct_purchase_amt","total_redeem_amt"};
    TH2D* h2[3];
    TH1D* h2total[3];
    TH1D* h2Projection[3];
    TH1D* activeUserRatioPerDay[3];
    TProfile* h2Profile[3];

    TH2D* h2UsrCate[3][9];
    TH1D* h2totalUsrCate[3][9];
    TH1D* h2ProjectionUsrCate[3][9];
    TH1D* activeUserRatioPerDayUsrCate[3][9];
    TProfile* h2ProfileUsrCate[3][9];

    //TGraph* gCate[3][5][9];
    for( int i=0 ; i<3 ; i++ )
    {
        user_balance_table->SetBranchAddress(Form("%s",branchName[i].c_str()),&branchValue[i]);
        h2[i]=new TH2D(Form("%sVsDate",branchName[i].c_str()),Form("%sVsDate",branchName[i].c_str()),timeBinNum,timeLowEdge,timeHighEdge,2200,0,220000000);
        h2Profile[i]=new TProfile(Form("%s_ProfileX",branchName[i].c_str()),Form("%s_ProfileX",branchName[i].c_str()),timeBinNum,timeLowEdge,timeHighEdge);
        h2total[i]=new TH1D(Form("%s_total",branchName[i].c_str()),Form("%s_total",branchName[i].c_str()),timeBinNum,timeLowEdge,timeHighEdge);
        activeUserRatioPerDay[i]=new TH1D(Form("%s_r",branchName[i].c_str()),Form("%s_r",branchName[i].c_str()),timeBinNum,timeLowEdge,timeHighEdge);
        for( int catei=0 ; catei<9 ; catei++ )
        {
            h2UsrCate[i][catei]=new TH2D(Form("%sVsDateUsrCate%i",branchName[i].c_str(),catei+1),Form("%sVsDateUsrCate%i",branchName[i].c_str(),catei+1),timeBinNum,timeLowEdge,timeHighEdge,2200,0,220000000);
            h2ProfileUsrCate[i][catei]=new TProfile(Form("%s_ProfileXUsrCate%i",branchName[i].c_str(),catei+1),Form("%s_ProfileXUsrCate%i",branchName[i].c_str(),catei+1),timeBinNum,timeLowEdge,timeHighEdge);
            h2totalUsrCate[i][catei]=new TH1D(Form("%s_totalUsrCate%i",branchName[i].c_str(),catei+1),Form("%s_totalUsrCate%i",branchName[i].c_str(),catei+1),timeBinNum,timeLowEdge,timeHighEdge);
            activeUserRatioPerDayUsrCate[i][catei]=new TH1D(Form("%s_rUsrCate%i",branchName[i].c_str(),catei+1),Form("%s_rUsrCate%i",branchName[i].c_str(),catei+1),timeBinNum,timeLowEdge,timeHighEdge);
        }
        
        c->cd(i*5+1);
        for( int ti=0 ; ti<ubtNum ; ti++ )
        {
            user_balance_table->GetEntry(ti);
            if( branchValue[i]!=0 )
            {
                h2[i]->Fill(report_date_unix,branchValue[i]);
                h2Profile[i]->Fill(report_date_unix,branchValue[i]);
                h2UsrCate[i][usrCategory[user_id]-1]->Fill(report_date_unix,branchValue[i]);
                h2ProfileUsrCate[i][usrCategory[user_id]-1]->Fill(report_date_unix,branchValue[i]);
            }
        }
        h2[i]->GetXaxis()->SetTimeDisplay(1);
        h2[i]->GetXaxis()->SetTimeFormat("%m/%d");
        h2[i]->Draw("colz");
        gPad->SetLogy();
        gPad->SetLogz();
        c->cd(i*5+3);
        h2Profile[i]->GetXaxis()->SetTimeDisplay(1);
        h2Profile[i]->GetXaxis()->SetTimeFormat("%m/%d");
        h2Profile[i]->Draw("hist");
        for( int catei=0 ; catei<9 ; catei++ )
        {
            h2ProfileUsrCate[i][catei]->SetLineColor(catei+1);
            h2ProfileUsrCate[i][catei]->Draw("sameChist");
        }
        c->cd(i*5+4);
        h2Projection[i]=h2[i]->ProjectionX(Form("%s_ProjectionX",branchName[i].c_str()));
        h2Projection[i]->GetXaxis()->SetTimeDisplay(1);
        h2Projection[i]->GetXaxis()->SetTimeFormat("%m/%d");
        h2Projection[i]->Draw("hist");
        for( int catei=0 ; catei<9 ; catei++ )
        {
            h2ProjectionUsrCate[i][catei]=h2UsrCate[i][catei]->ProjectionX(Form("%s_ProjectionXUsrCate%i",branchName[i].c_str(),catei+1));
            h2ProjectionUsrCate[i][catei]->SetLineColor(catei+1);
            h2ProjectionUsrCate[i][catei]->Draw("sameChist");
        }
        
        c->cd(i*5+5);
        for( int ubti =1; ubti<=timeBinNum ; ubti++ )
        {
            if( hUsrJoinTimeCumu->GetBinContent(ubti)!=0)
            {
                //cout<<"ubti  : "<<ubti<<endl;
                //cout<<" h2Projection[i]->GetBinContent(ubti)  : "<<h2Projection[i]->GetBinContent(ubti)<<endl;
                //cout<<" hUsrJoinTimeCumu->GetBinContent(ubti)  : "<<hUsrJoinTimeCumu->GetBinContent(ubti)<<endl;
                activeUserRatioPerDay[i]->SetBinContent(ubti,(double)h2Projection[i]->GetBinContent(ubti)/hUsrJoinTimeCumu->GetBinContent(ubti));
                for( int catei=0 ; catei<9 ; catei++ )
                {
                    activeUserRatioPerDayUsrCate[i][catei]->SetBinContent(ubti,(double)h2ProjectionUsrCate[i][catei]->GetBinContent(ubti)/hUsrJoinTimeUsrCateCumu[catei]->GetBinContent(ubti));
                }
                //cout<<"done   "<<endl;
            }
        }
        activeUserRatioPerDay[i]->GetXaxis()->SetTimeDisplay(1);
        activeUserRatioPerDay[i]->GetXaxis()->SetTimeFormat("%m/%d");
        activeUserRatioPerDay[i]->Draw("hist");
        for( int catei=0 ; catei<9 ; catei++ )
        {
            activeUserRatioPerDayUsrCate[i][catei]->SetLineColor(catei+1);
            activeUserRatioPerDayUsrCate[i][catei]->Draw("sameC");
        }
        
        c->cd(i*5+2);
        for( int hi=1 ; hi<=timeBinNum ; hi++ )
        {
            h2total[i]->SetBinContent(hi,h2Profile[i]->GetBinEntries(hi)*h2Profile[i]->GetBinContent(hi));
            for( int catei=0 ; catei<9 ; catei++ )
            {
                h2totalUsrCate[i][catei]->SetBinContent(hi,h2ProfileUsrCate[i][catei]->GetBinEntries(hi)*h2ProfileUsrCate[i][catei]->GetBinContent(hi));
            }
        }
        h2total[i]->GetXaxis()->SetTimeDisplay(1);
        h2total[i]->GetXaxis()->SetTimeFormat("%m/%d");
        h2total[i]->Draw();
        for( int catei=0 ; catei<9 ; catei++ )
        {
            h2totalUsrCate[i][catei]->SetLineColor(catei+1);
            h2totalUsrCate[i][catei]->Draw("sameC");
        }
    }
}
