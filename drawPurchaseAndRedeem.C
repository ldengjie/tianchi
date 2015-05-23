{
    gStyle->SetTitleStyle(0); 
    gStyle->SetTitleBorderSize(0); 
    gStyle->SetOptStat(0);
    gStyle->SetLegendBorderSize(0);

    int timeBinNum=490;//490 day,70 week,16 month
    double timeLowEdge= 1370102400;
    double timeHighEdge=1412438400;

    TFile* f=new TFile("tianChiOriginData.root","read");

    //user_balance_table
    TTree* user_balance_table=(TTree*)f->Get("user_balance_table");
    int ubtNum=user_balance_table->GetEntries();
    string branchStr[18]={"tBalance","yBalance","total_purchase_amt","direct_purchase_amt","purchase_bal_amt","purchase_bank_amt","share_amt","total_redeem_amt","consume_amt","transfer_amt","tftobal_amt","tftocard_amt","category1","category2","category3","category4","user_id","report_date_unix"};
    Long64_t branchValue[18];
    TH2D* h2[16];
    TH1D* h2total[16];
    TProfile* h2Profile[16];
    TH1D* h2Projection[16];
    //TH1D* activeUserRatioPerDay[16];
    
    for( int bi=0 ; bi<18 ; bi++ )
    {
        user_balance_table->SetBranchAddress(Form("%s",branchStr[bi].c_str()),&branchValue[bi]);
        if( bi<16 )
        {
            h2[bi]=new TH2D(Form("%s",branchStr[bi].c_str()),Form("%s",branchStr[bi].c_str()),timeBinNum,timeLowEdge,timeHighEdge,2200,0,220000000);
            h2total[bi]=new TH1D(Form("%s_total",branchStr[bi].c_str()),Form("%s_total",branchStr[bi].c_str()),timeBinNum,timeLowEdge,timeHighEdge);
            h2Profile[bi]=new TProfile(Form("%s_Profile",branchStr[bi].c_str()),Form("%s_Profile",branchStr[bi].c_str()),timeBinNum,timeLowEdge,timeHighEdge);
            //h2Projection[bi]=new TH1D(Form("%s_Projection",branchStr[bi].c_str()),Form("%s_Projection",branchStr[bi].c_str()),timeBinNum,timeLowEdge,timeHighEdge);
        }
    }

    for( int ubti=0 ; ubti<ubtNum ; ubti++ )
    {
        user_balance_table->GetEntry(ubti);
        for( int bi=0 ; bi<16 ; bi++ )
        {
            if( branchValue[bi]!=0 )
            {
                h2[bi]->Fill(branchValue[17],branchValue[bi]);
                h2Profile[bi]->Fill(branchValue[17],branchValue[bi]);
            }
        }
        
    }
    for( int bi=0 ; bi<16 ; bi++ )
    {
        h2Projection[bi]=h2[bi]->ProjectionX();
        for( int hi=1 ; hi<=timeBinNum ; hi++ )
        {
            h2total[bi]->SetBinContent(hi,h2Profile[bi]->GetBinEntries(hi)*h2Profile[bi]->GetBinContent(hi));
        }
    }

    TCanvas* c=new TCanvas("c","c",1800,900);
    c->Divide(3,2);
    //string branchStr[18]={"tBalance","yBalance","total_purchase_amt","direct_purchase_amt","purchase_bal_amt","purchase_bank_amt","share_amt","total_redeem_amt","consume_amt","transfer_amt","tftobal_amt","tftocard_amt","category1","category2","category3","category4","user_id","report_date_unix"};
    int color[18]={2,3,2,3,4,42,6,2,3,4,42,6,7,8,9,28,2,2};
    int indexLow[2]={2,7};
    int indexHigh[2]={6,15};
    TLegend *legend[2];
    for( int ci=0 ; ci<2 ; ci++ )
    {
        c->cd(ci*3+1);
        h2total[indexLow[ci]]->GetXaxis()->SetTimeDisplay(1);
        h2total[indexLow[ci]]->GetXaxis()->SetTimeFormat("%m/%d");
        h2total[indexLow[ci]]->Draw();
        legend[ci]=new TLegend(.6,.65,.9,.9);
        legend[ci]->AddEntry(h2total[indexLow[ci]],branchStr[indexLow[ci]].c_str(),"lp");
        for( int bi=indexLow[ci]+1 ; bi<=indexHigh[ci] ; bi++ )
        {
            h2total[bi]->SetLineColor(color[bi]);
            h2total[bi]->Draw("same");
            legend[ci]->AddEntry(h2total[bi],branchStr[bi].c_str(),"lp");
        }
        legend[ci]->SetFillColor(0);
        legend[ci]->Draw("same");

        c->cd(ci*3+2);
        h2Profile[indexLow[ci]]->GetXaxis()->SetTimeDisplay(1);
        h2Profile[indexLow[ci]]->GetXaxis()->SetTimeFormat("%m/%d");
        h2Profile[indexLow[ci]]->SetAxisRange(0,2700000,"Y");
        h2Profile[indexLow[ci]]->Draw("hist");
        for( int bi=indexLow[ci]+1 ; bi<=indexHigh[ci] ; bi++ )
        {
            h2Profile[bi]->SetLineColor(color[bi]);
            h2Profile[bi]->Draw("samehist");
        }
        c->cd(ci*3+3);
        h2Projection[indexLow[ci]]->GetXaxis()->SetTimeDisplay(1);
        h2Projection[indexLow[ci]]->GetXaxis()->SetTimeFormat("%m/%d");
        h2Projection[indexLow[ci]]->Draw("hist");
        for( int bi=indexLow[ci]+1 ; bi<=indexHigh[ci] ; bi++ )
        {
            h2Projection[bi]->SetLineColor(color[bi]);
            h2Projection[bi]->Draw("samehist");

        }
    }
}
