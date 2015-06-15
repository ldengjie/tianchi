{
    gStyle->SetTitleStyle(0); 
    gStyle->SetTitleBorderSize(0); 
    gStyle->SetOptStat(0);

    int timeBinNum=490;//490 day,70 week,16 month
    double timeLowEdge= 1370102400;
    double timeHighEdge=1412438400;

    int lowStatDate=20140401;
    int highStatDate=20140831;

    Long64_t lowStatTime=coverTime2unix(lowStatDate);
    Long64_t highStatTime=coverTime2unix(highStatDate);
    Long64_t statDays=(highStatTime-lowStatTime)/86400;

    TFile* f=new TFile("tianChiOriginData.root","read");

    //user_balance_table
    TTree* user_balance_table=(TTree*)f->Get("user_balance_table");
    int ubtNum=user_balance_table->GetEntries();
    Long64_t report_date_unix;
    Long64_t branchValue[3]={0}; 
    Long64_t user_id;
    Long64_t tBalance;
    Long64_t total_purchase_amt  ;
    Long64_t direct_purchase_amt ;
    user_balance_table->SetBranchAddress("user_id",&user_id);
    user_balance_table->SetBranchAddress("tBalance",&tBalance);
    user_balance_table->SetBranchAddress("report_date_unix",&report_date_unix);
    user_balance_table->SetBranchAddress("total_purchase_amt",&total_purchase_amt);
    user_balance_table->SetBranchAddress("direct_purchase_amt",&direct_purchase_amt);

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
    Long64_t totalBalance[28367]={0};
    Long64_t totalNum[28367]={0};
    Long64_t balancePerDay[28367]={0};
    double frePerDay[28367]={0};
    Long64_t usrJoinTime[28367]={0};
    for( int ubti=0 ; ubti<ubtNum ; ubti++ )
    {
       user_balance_table->GetEntry(ubti); 
       if( report_date_unix>=lowStatTime&&report_date_unix<=highStatTime )
       {
           //totalBalance[user_id]+=total_purchase_amt;
           totalBalance[user_id]+=direct_purchase_amt;
       }
       if( direct_purchase_amt>0 && report_date_unix>=lowStatTime&&report_date_unix<=highStatTime )
       {
           totalNum[user_id]+=1;
       }
    }
    for( int ubti=0 ; ubti<ubtNum ; ubti++ )
    {
       user_balance_table->GetEntry(ubti); 
       //if( (usrJoinTime[user_id]==0||(usrJoinTime[user_id]!=0&&usrJoinTime[user_id]>report_date_unix))&&totalBalance[user_id]!=0 )
       if( usrJoinTime[user_id]==0||(usrJoinTime[user_id]!=0&&usrJoinTime[user_id]>report_date_unix))
       {
           usrJoinTime[user_id]=report_date_unix;
       }
    }
    TH1D* frePerDayH=new TH1D("frePerDayH","frePerDayH",32,0,32);
    TH1D* purPerDay=new TH1D("purPerDay","purPerDay",100,0,100000000);
    TH2D* freVSpurPerDay=new TH2D("freVSpurPerDay","freVSpurPerDay",32,0,32,1000,0,100000000);
    for( int upti=0 ; upti<28367 ; upti++ )
    {
        int voliadDays=statDays;
        if( usrJoinTime[upti]>lowStatTime&&usrJoinTime[upti]<highStatTime)
        {
            voliadDays-=(usrJoinTime[upti]-lowStatTime)/86400;
        }
        balancePerDay[upti]=totalBalance[upti]/voliadDays;
        if(balancePerDay[upti]>0.) purPerDay->Fill(balancePerDay[upti]*31);
        frePerDay[upti]=(double)totalNum[upti]/voliadDays;
        if(frePerDay[upti]>0.)
        {
          frePerDayH->Fill(int(frePerDay[upti]*31)); 
          freVSpurPerDay->Fill(int(frePerDay[upti]*31),balancePerDay[upti]*31);
        } 
    }
    const int cateNum=4;
    //Long64_t cateEdge[cateNum]={0,1000000,3000000,5000000,7000000,9000000,11000000,700000000};//fen
    Long64_t cateEdge[cateNum]={0,1000000,5000000,100000000};//fen
    Long64_t cateColor[cateNum]={2,3,4,6};
    Long64_t usrCategory[28367]={0};
    TH1D* hUsrCategory=new TH1D("hUsrCategory","hUsrCategory",cateNum,1,cateNum+1);
    for( int upti=0 ; upti<28367 ; upti++ )
    {
        if( frePerDay[upti]>0 )
        {
            if( frePerDay[upti]*31<2 && balancePerDay[upti]*31<300000 )
            {
                usrCategory[upti]=2;
                hUsrCategory->Fill(2);
            } else if( frePerDay[upti]*31>=4 )
            {
                usrCategory[upti]=4;
                hUsrCategory->Fill(4);
            }else
            {
                usrCategory[upti]=3;
                hUsrCategory->Fill(3);
            }
        }else
        {
            usrCategory[upti]=1;
            hUsrCategory->Fill(1);
        }
        /*
        if( frePerDay[upti]>0 )
        {
            if( frePerDay[upti]*31<1 && balancePerDay[upti]*31<100000 )
            {
                usrCategory[upti]=2;
                hUsrCategory->Fill(2);
            } else if( frePerDay[upti]*31<2 && balancePerDay[upti]*31>=300000   )
            {
                usrCategory[upti]=4;
                hUsrCategory->Fill(4);
            } else if( frePerDay[upti]*31>=2 && frePerDay[upti]*31<4 )
            {
                usrCategory[upti]=5;
                hUsrCategory->Fill(5);
            } else if( frePerDay[upti]*31>=4 )
            {
                usrCategory[upti]=6;
                hUsrCategory->Fill(6);
            }else
            {
                usrCategory[upti]=3;
                hUsrCategory->Fill(3);
            }
        }else
        {
            usrCategory[upti]=1;
            hUsrCategory->Fill(1);
        }
        for( int catei=0 ; catei< cateNum-1; catei++ )
        {
            if(  balancePerDay[upti]>cateEdge[cateNum-1]/30)
            {
                cout<<"find one !!!! "<<endl;
                break;
            }
            if(balancePerDay[upti]==0 )
            {
                usrCategory[upti]=1;
                hUsrCategory->Fill(1);
                break;
            }
            if( balancePerDay[upti]>cateEdge[catei]/30&&balancePerDay[upti]<=cateEdge[catei+1]/30 )
            {
                usrCategory[upti]=catei+2;
                hUsrCategory->Fill(catei+2);
                break;
            }
        }
        */
    }

    c->cd(16);
    hUsrCategory->Draw();
    c->cd(17);
    purPerDay->Draw();
    c->cd(18);
    freVSpurPerDay->Draw("colz");

    Long64_t usrIndex[28367]={0};
    for( int upti=0 ; upti<uptNum ; upti++ )
    {
        user_profile_table->GetEntry(upti);
        usrIndex[user_id_upt]=upti;
    }


    c->cd(19);
    TH1D* hUsrJoinTime=new TH1D("hUsrJoinTime","hUsrJoinTime",timeBinNum,timeLowEdge,timeHighEdge);
    TH1D* hUsrJoinTimeUsrCate[cateNum];
    for( int catei=0 ; catei<cateNum ; catei++ )
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
    for( int catei=0 ; catei<cateNum ; catei++ )
    {
        hUsrJoinTimeUsrCate[catei]->SetLineColor(cateColor[catei]);
        hUsrJoinTimeUsrCate[catei]->Draw("sameC");
    }

    c->cd(20);
    TH1D* hUsrJoinTimeCumu=new TH1D("hUsrJoinTimeCumu","hUsrJoinTimeCumu",timeBinNum,timeLowEdge,timeHighEdge);
    Long64_t hUsrJoinTimeCount=0;
    Long64_t hUsrJoinTimeCountUsrCate[cateNum]={0};
    TH1D* hUsrJoinTimeUsrCateCumu[cateNum];
    for( int catei=0 ; catei<cateNum ; catei++ )
    {
        hUsrJoinTimeUsrCateCumu[catei]=new TH1D(Form("hUsrJoinTimeUsrCateCumu%i",catei+1),Form("hUsrJoinTimeUsrCateCumu%i",catei+1),timeBinNum,timeLowEdge,timeHighEdge);
    }
    for( int upti=1 ; upti<=timeBinNum; upti++ )
    {
        if( hUsrJoinTime->GetBinContent(upti)!=0 )
        {
            hUsrJoinTimeCount+=hUsrJoinTime->GetBinContent(upti);
            hUsrJoinTimeCumu->SetBinContent(upti,hUsrJoinTimeCount);
            for( int catei=0 ; catei<cateNum ; catei++ )
            {
                hUsrJoinTimeCountUsrCate[catei]+=hUsrJoinTimeUsrCate[catei]->GetBinContent(upti);
                hUsrJoinTimeUsrCateCumu[catei]->SetBinContent(upti,hUsrJoinTimeCountUsrCate[catei]);
            }
        }
    }
    hUsrJoinTimeCumu->GetXaxis()->SetTimeDisplay(1);
    hUsrJoinTimeCumu->GetXaxis()->SetTimeFormat("%m/%d");
    hUsrJoinTimeCumu->Draw();
    for( int catei=0 ; catei<cateNum ; catei++ )
    {
        hUsrJoinTimeUsrCateCumu[catei]->SetLineColor(cateColor[catei]);
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

    TH2D* h2UsrCate[3][cateNum];
    TH1D* h2totalUsrCate[3][cateNum];
    TH1D* h2ProjectionUsrCate[3][cateNum];
    TH1D* activeUserRatioPerDayUsrCate[3][cateNum];
    TProfile* h2ProfileUsrCate[3][cateNum];

    //TGraph* gCate[3][5][9];
    for( int i=0 ; i<3 ; i++ )
    {
        user_balance_table->SetBranchAddress(Form("%s",branchName[i].c_str()),&branchValue[i]);
        h2[i]=new TH2D(Form("%sVsDate",branchName[i].c_str()),Form("%sVsDate",branchName[i].c_str()),timeBinNum,timeLowEdge,timeHighEdge,2200,0,220000000);
        h2Profile[i]=new TProfile(Form("%s_ProfileX",branchName[i].c_str()),Form("%s_ProfileX",branchName[i].c_str()),timeBinNum,timeLowEdge,timeHighEdge);
        h2total[i]=new TH1D(Form("%s_total",branchName[i].c_str()),Form("%s_total",branchName[i].c_str()),timeBinNum,timeLowEdge,timeHighEdge);
        activeUserRatioPerDay[i]=new TH1D(Form("%s_r",branchName[i].c_str()),Form("%s_r",branchName[i].c_str()),timeBinNum,timeLowEdge,timeHighEdge);
        for( int catei=0 ; catei<cateNum ; catei++ )
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
        for( int catei=0 ; catei<cateNum ; catei++ )
        {
            h2ProfileUsrCate[i][catei]->SetLineColor(cateColor[catei]);
            h2ProfileUsrCate[i][catei]->Draw("sameChist");
        }
        c->cd(i*5+4);
        h2Projection[i]=h2[i]->ProjectionX(Form("%s_ProjectionX",branchName[i].c_str()));
        h2Projection[i]->GetXaxis()->SetTimeDisplay(1);
        h2Projection[i]->GetXaxis()->SetTimeFormat("%m/%d");
        h2Projection[i]->Draw("hist");
        for( int catei=0 ; catei<cateNum ; catei++ )
        {
            h2ProjectionUsrCate[i][catei]=h2UsrCate[i][catei]->ProjectionX(Form("%s_ProjectionXUsrCate%i",branchName[i].c_str(),catei+1));
            h2ProjectionUsrCate[i][catei]->SetLineColor(cateColor[catei]);
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
                for( int catei=0 ; catei<cateNum ; catei++ )
                {
                    activeUserRatioPerDayUsrCate[i][catei]->SetBinContent(ubti,(double)h2ProjectionUsrCate[i][catei]->GetBinContent(ubti)/hUsrJoinTimeUsrCateCumu[catei]->GetBinContent(ubti));
                }
                //cout<<"done   "<<endl;
            }
        }
        activeUserRatioPerDay[i]->GetXaxis()->SetTimeDisplay(1);
        activeUserRatioPerDay[i]->GetXaxis()->SetTimeFormat("%m/%d");
        activeUserRatioPerDay[i]->Draw("hist");
        for( int catei=0 ; catei<cateNum ; catei++ )
        {
            activeUserRatioPerDayUsrCate[i][catei]->SetLineColor(cateColor[catei]);
            activeUserRatioPerDayUsrCate[i][catei]->Draw("sameC");
        }
        
        c->cd(i*5+2);
        for( int hi=1 ; hi<=timeBinNum ; hi++ )
        {
            h2total[i]->SetBinContent(hi,h2Profile[i]->GetBinEntries(hi)*h2Profile[i]->GetBinContent(hi));
            for( int catei=0 ; catei<cateNum ; catei++ )
            {
                h2totalUsrCate[i][catei]->SetBinContent(hi,h2ProfileUsrCate[i][catei]->GetBinEntries(hi)*h2ProfileUsrCate[i][catei]->GetBinContent(hi));
            }
        }
        h2total[i]->GetXaxis()->SetTimeDisplay(1);
        h2total[i]->GetXaxis()->SetTimeFormat("%m/%d");
        h2total[i]->Draw();
        for( int catei=0 ; catei<cateNum ; catei++ )
        {
            h2totalUsrCate[i][catei]->SetLineColor(cateColor[catei]);
            h2totalUsrCate[i][catei]->Draw("sameC");
        }
    }
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
