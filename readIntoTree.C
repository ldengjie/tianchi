{
    TFile* f=new TFile("tianChiOriginData.root","recreate");
    string csvName[4]={"user_profile_table","mfd_bank_shibor","mfd_day_share_interest","user_balance_table"};
    TTree* csvTree[4];
    string unixTimeName[4]={"","mfd_date","mfd_date","report_date"};
    TBranch* unixBranch[4];
    Long64_t unixTime[4]={0};
    for( int i=0 ; i<4 ; i++ )
    {
        struct timeval allStartTime,allFinishTime;
        double timeInterval=0.;
        gettimeofday( &allStartTime, NULL );

        csvTree[i]=new TTree(Form("%s",csvName[i].c_str()),Form("%s",csvName[i].c_str()));
        Long64_t csvlines = csvTree[i]->ReadFile(Form("%s.csv",csvName[i].c_str()));
        //convert date to unix time
        if(  unixTimeName[i]!="")
        {
            unixBranch[i]=csvTree[i]->Branch(Form("%s_unix",unixTimeName[i].c_str()),&unixTime[i],Form("%s_unix/L",unixTimeName[i].c_str()));
            int entryNum=csvTree[i]->GetEntries();
            cout<<"entryNum  : "<<entryNum<<endl;
            Long64_t originDate;
            int year,month,day;
            csvTree[i]->SetBranchAddress(Form("%s",unixTimeName[i].c_str()),&originDate);
            struct tm tm1;
            memset(&tm1, 0, sizeof(tm1));
            for( int ei=0 ; ei<entryNum ; ei++ )
            {
                csvTree[i]->GetEntry(ei);
                tm1.tm_year = originDate/10000- 1900;
                tm1.tm_mon  = originDate%10000/100- 1;
                tm1.tm_mday = originDate%10000%100;
                unixTime[i]= mktime(&tm1);
                unixBranch[i]->Fill();
            }
        }

        gettimeofday( &allFinishTime, NULL );
        timeInterval=allFinishTime.tv_sec-allStartTime.tv_sec+(allFinishTime.tv_usec-allStartTime.tv_usec)/1000000.;
        cout<<"["<<csvName[i]<<"] lines : "<<csvlines<<"  (Used time : "<<timeInterval<<" s = "<<(int)timeInterval/3600<<"h"<<(int)timeInterval%3600/60 <<"min"<<(int)timeInterval%3600%60 <<"s) ..."<<endl;
    }
    f->Write();
}
