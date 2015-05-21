{
    TFile* f=new TFile("tianChiOriginData.root","recreate");
    string csvName[4]={"user_profile_table","mfd_bank_shibor","mfd_day_share_interest","user_balance_table"};
    TTree* csvTree[4];
    for( int i=0 ; i<4 ; i++ )
    {
        struct timeval allStartTime,allFinishTime;
        double timeInterval=0.;
        gettimeofday( &allStartTime, NULL );
        csvTree[i]=new TTree(Form("%s",csvName[i].c_str()),Form("%s",csvName[i].c_str()));
        Long64_t csvlines = csvTree[i]->ReadFile(Form("%s.csv",csvName[i].c_str()));
        gettimeofday( &allFinishTime, NULL );
        timeInterval=allFinishTime.tv_sec-allStartTime.tv_sec+(allFinishTime.tv_usec-allStartTime.tv_usec)/1000000.;
        cout<<"["<<csvName[i]<<"] lines : "<<csvlines<<"  (Used time : "<<timeInterval<<" s = "<<(int)timeInterval/3600<<"h"<<(int)timeInterval%3600/60 <<"min"<<(int)timeInterval%3600%60 <<"s) ..."<<endl;
    }
    f->Write();
}
