--转换支付宝、银行操作>0 -> 1
create table if not exists user_zfb_bank_count(
        user_id bigint 
        ,zfb_count bigint 
        ,bank_count bigint 
        ,report_date bigint
        ,total_purchase_amt bigint
        ,purchase_bal_amt bigint 
        ,purchase_bank_amt bigint 
        ) ;
insert overwrite table user_zfb_bank_count
select  user_id
,case when purchase_bal_amt>0 then 1 else 0 end as zfb_count
,case when purchase_bank_amt>0 then 1 else 0 end as bank_count
,report_date
,total_purchase_amt
,purchase_bal_amt
,purchase_bank_amt
from user_balance_table;

--统计操作次数
create table if not exists user_purchase_type(
        user_id bigint 
        ,first_report_date bigint
        ,last_report_date bigint
        ,total_purchase_amt bigint
        ,zfb_sum bigint 
        ,zfb_count bigint 
        ,bank_sum bigint 
        ,bank_count bigint 
        ) ;
insert overwrite table user_purchase_type
select  user_id
,min(report_date) report_date
,max(report_date) report_date
,sum(total_purchase_amt) total_purchase_amt
,sum(purchase_bal_amt) purchase_bal_amt
,sum(zfb_count)  zfb_count
,sum(purchase_bank_amt) purchase_bank_amt
,sum(bank_count)  bank_count
from user_zfb_bank_count
group by user_id;

--消费模式类别判断
create table user_purchase_zfb_bank_type(
        user_id bigint 
        ,first_report_date bigint
        ,last_report_date bigint
        ,total_purchase_amt bigint
        ,zfb_sum bigint 
        ,zfb_count bigint 
        ,zfb_type bigint
        ,bank_sum bigint 
        ,bank_count bigint 
        ,bank_type bigint
        ) ;
insert overwrite table user_purchase_zfb_bank_type
select  user_id
,first_report_date
,last_report_date
,total_purchase_amt
,zfb_sum
,zfb_count
,case 
when zfb_count==0 then 0
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=3.0) then 1
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=2.9) then 2
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=2.8) then 3
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=2.7) then 4
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=2.6) then 5
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=2.5) then 6
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=2.4) then 7
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=2.3) then 8
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=2.2) then 9
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=2.1) then 10
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=2.0) then 11
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=1.9) then 12
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=1.8) then 13
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=1.7) then 14
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=1.6) then 15
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=1.5) then 16
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=1.4) then 17
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=1.3) then 18
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=1.2) then 19
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=1.1) then 20
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=1.0) then 21
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=0.9) then 22
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=0.8) then 23
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=0.7) then 24
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=0.6) then 25
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=0.5) then 26
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=0.4) then 27
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=0.3) then 28
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=0.2) then 29
when (cast(zfb_count as double)/cast((datediff(to_date('20140831','yyyymmdd'), to_date(first_report_date,'yyyymmdd'), 'dd')+1) as double)*30.44 >=0.1) then 30
else 31
end as zfb_type
,bank_sum
,bank_count
,case
when bank_count==0 then 0
when (bank_sum/bank_count>=4000000)  then 1  
when (bank_sum/bank_count>=3900000)  then 2  
when (bank_sum/bank_count>=3800000)  then 3  
when (bank_sum/bank_count>=3700000)  then 4  
when (bank_sum/bank_count>=3600000)  then 5  
when (bank_sum/bank_count>=3500000)  then 6  
when (bank_sum/bank_count>=3400000)  then 7  
when (bank_sum/bank_count>=3300000)  then 8  
when (bank_sum/bank_count>=3200000)  then 9  
when (bank_sum/bank_count>=3100000)  then 10 
when (bank_sum/bank_count>=3000000)  then 11 
when (bank_sum/bank_count>=2900000)  then 12
when (bank_sum/bank_count>=2800000)  then 13
when (bank_sum/bank_count>=2700000)  then 14
when (bank_sum/bank_count>=2600000)  then 15
when (bank_sum/bank_count>=2500000)  then 16
when (bank_sum/bank_count>=2400000)  then 17
when (bank_sum/bank_count>=2300000)  then 18
when (bank_sum/bank_count>=2200000)  then 19
when (bank_sum/bank_count>=2100000)  then 20
when (bank_sum/bank_count>=2000000)  then 21
when (bank_sum/bank_count>=1900000)  then 22
when (bank_sum/bank_count>=1800000)  then 23
when (bank_sum/bank_count>=1700000)  then 24
when (bank_sum/bank_count>=1600000)  then 25
when (bank_sum/bank_count>=1500000)  then 26
when (bank_sum/bank_count>=1400000)  then 27
when (bank_sum/bank_count>=1300000)  then 28
when (bank_sum/bank_count>=1200000)  then 29
when (bank_sum/bank_count>=1100000)  then 30
when (bank_sum/bank_count>=1000000)  then 31
when (bank_sum/bank_count>=900000 )  then 32
when (bank_sum/bank_count>=800000 )  then 33
when (bank_sum/bank_count>=700000 )  then 34
when (bank_sum/bank_count>=600000 )  then 35
when (bank_sum/bank_count>=500000 )  then 36
when (bank_sum/bank_count>=400000 )  then 37
when (bank_sum/bank_count>=300000 )  then 38
when (bank_sum/bank_count>=200000 )  then 39
when (bank_sum/bank_count>=100000 )  then 40
else 41                                    
end as bank_type                           
from user_purchase_type;                   

--消费总表按类别分类，purchase time series 
create table  purchase_by_type(
        user_id bigint 
        ,report_date bigint
        ,total_purchase_amt bigint
        ,zfb0 bigint
        ,zfb1 bigint
        ,zfb2 bigint
        ,zfb3 bigint
        ,zfb4  bigint
        ,zfb5  bigint
        ,zfb6  bigint
        ,zfb7  bigint
        ,zfb8  bigint
        ,zfb9  bigint
        ,zfb10 bigint
        ,zfb11 bigint
        ,zfb12 bigint
        ,zfb13 bigint
        ,zfb14 bigint
        ,zfb15 bigint
        ,zfb16 bigint
        ,zfb17 bigint
        ,zfb18 bigint
        ,zfb19 bigint
        ,zfb20 bigint
        ,zfb21 bigint
        ,zfb22 bigint
        ,zfb23 bigint
        ,zfb24 bigint
        ,zfb25 bigint
        ,zfb26 bigint
        ,zfb27 bigint
        ,zfb28 bigint
        ,zfb29 bigint
        ,zfb30 bigint
        ,zfb31 bigint
        ,bank0 bigint
        ,bank1 bigint
        ,bank2 bigint
        ,bank3 bigint
        ,bank4 bigint
        ,bank5 bigint
        ,bank6 bigint
        ,bank7 bigint
        ,bank8 bigint
        ,bank9 bigint
        ,bank10 bigint
        ,bank11 bigint
        ,bank12 bigint
        ,bank13 bigint
        ,bank14 bigint
        ,bank15 bigint
        ,bank16 bigint
        ,bank17 bigint
        ,bank18 bigint
        ,bank19 bigint
        ,bank20 bigint
        ,bank21 bigint
        ,bank22 bigint
        ,bank23 bigint
        ,bank24 bigint
        ,bank25 bigint
        ,bank26 bigint
        ,bank27 bigint
        ,bank28 bigint
        ,bank29 bigint
        ,bank30 bigint
        ,bank31 bigint
        ,bank32 bigint
        ,bank33 bigint
        ,bank34 bigint
        ,bank35 bigint
        ,bank36 bigint
        ,bank37 bigint
        ,bank38 bigint
        ,bank39 bigint
        ,bank40 bigint
        ,bank41 bigint
        ,share bigint
) ;
insert overwrite table purchase_by_type
select  a.user_id
,a.report_date
,a.total_purchase_amt
,case when (a.purchase_bal_amt>0 and b.zfb_type =0) then a.purchase_bal_amt else 0 end as zfb0
,case when (a.purchase_bal_amt>0 and b.zfb_type =1) then a.purchase_bal_amt else 0 end as zfb1
,case when (a.purchase_bal_amt>0 and b.zfb_type =2) then a.purchase_bal_amt else 0 end as zfb2
,case when (a.purchase_bal_amt>0 and b.zfb_type =3) then a.purchase_bal_amt else 0 end as zfb3
,case when (a.purchase_bal_amt>0 and b.zfb_type =4 ) then a.purchase_bal_amt else 0 end as zfb4 
,case when (a.purchase_bal_amt>0 and b.zfb_type =5 ) then a.purchase_bal_amt else 0 end as zfb5 
,case when (a.purchase_bal_amt>0 and b.zfb_type =6 ) then a.purchase_bal_amt else 0 end as zfb6 
,case when (a.purchase_bal_amt>0 and b.zfb_type =7 ) then a.purchase_bal_amt else 0 end as zfb7 
,case when (a.purchase_bal_amt>0 and b.zfb_type =8 ) then a.purchase_bal_amt else 0 end as zfb8 
,case when (a.purchase_bal_amt>0 and b.zfb_type =9 ) then a.purchase_bal_amt else 0 end as zfb9 
,case when (a.purchase_bal_amt>0 and b.zfb_type =10) then a.purchase_bal_amt else 0 end as zfb10
,case when (a.purchase_bal_amt>0 and b.zfb_type =11) then a.purchase_bal_amt else 0 end as zfb11
,case when (a.purchase_bal_amt>0 and b.zfb_type =12) then a.purchase_bal_amt else 0 end as zfb12
,case when (a.purchase_bal_amt>0 and b.zfb_type =13) then a.purchase_bal_amt else 0 end as zfb13
,case when (a.purchase_bal_amt>0 and b.zfb_type =14) then a.purchase_bal_amt else 0 end as zfb14
,case when (a.purchase_bal_amt>0 and b.zfb_type =15) then a.purchase_bal_amt else 0 end as zfb15
,case when (a.purchase_bal_amt>0 and b.zfb_type =16) then a.purchase_bal_amt else 0 end as zfb16
,case when (a.purchase_bal_amt>0 and b.zfb_type =17) then a.purchase_bal_amt else 0 end as zfb17
,case when (a.purchase_bal_amt>0 and b.zfb_type =18) then a.purchase_bal_amt else 0 end as zfb18
,case when (a.purchase_bal_amt>0 and b.zfb_type =19) then a.purchase_bal_amt else 0 end as zfb19
,case when (a.purchase_bal_amt>0 and b.zfb_type =20) then a.purchase_bal_amt else 0 end as zfb20
,case when (a.purchase_bal_amt>0 and b.zfb_type =21) then a.purchase_bal_amt else 0 end as zfb21
,case when (a.purchase_bal_amt>0 and b.zfb_type =22) then a.purchase_bal_amt else 0 end as zfb22
,case when (a.purchase_bal_amt>0 and b.zfb_type =23) then a.purchase_bal_amt else 0 end as zfb23
,case when (a.purchase_bal_amt>0 and b.zfb_type =24) then a.purchase_bal_amt else 0 end as zfb24
,case when (a.purchase_bal_amt>0 and b.zfb_type =25) then a.purchase_bal_amt else 0 end as zfb25
,case when (a.purchase_bal_amt>0 and b.zfb_type =26) then a.purchase_bal_amt else 0 end as zfb26
,case when (a.purchase_bal_amt>0 and b.zfb_type =27) then a.purchase_bal_amt else 0 end as zfb27
,case when (a.purchase_bal_amt>0 and b.zfb_type =28) then a.purchase_bal_amt else 0 end as zfb28
,case when (a.purchase_bal_amt>0 and b.zfb_type =29) then a.purchase_bal_amt else 0 end as zfb29
,case when (a.purchase_bal_amt>0 and b.zfb_type =30) then a.purchase_bal_amt else 0 end as zfb30
,case when (a.purchase_bal_amt>0 and b.zfb_type =31) then a.purchase_bal_amt else 0 end as zfb31
,case when (a.purchase_bank_amt>0 and b.bank_type =0) then a.purchase_bank_amt else 0 end as bank0
,case when (a.purchase_bank_amt>0 and b.bank_type =1) then a.purchase_bank_amt else 0 end as bank1
,case when (a.purchase_bank_amt>0 and b.bank_type =2) then a.purchase_bank_amt else 0 end as bank2
,case when (a.purchase_bank_amt>0 and b.bank_type =3) then a.purchase_bank_amt else 0 end as bank3
,case when (a.purchase_bank_amt>0 and b.bank_type =4) then a.purchase_bank_amt else 0 end as bank4
,case when (a.purchase_bank_amt>0 and b.bank_type =5) then a.purchase_bank_amt else 0 end as bank5
,case when (a.purchase_bank_amt>0 and b.bank_type =6) then a.purchase_bank_amt else 0 end as bank6
,case when (a.purchase_bank_amt>0 and b.bank_type =7) then a.purchase_bank_amt else 0 end as bank7
,case when (a.purchase_bank_amt>0 and b.bank_type =8) then a.purchase_bank_amt else 0 end as bank8
,case when (a.purchase_bank_amt>0 and b.bank_type =9) then a.purchase_bank_amt else 0 end as bank9
,case when (a.purchase_bank_amt>0 and b.bank_type =10) then a.purchase_bank_amt else 0 end as bank10
,case when (a.purchase_bank_amt>0 and b.bank_type =11) then a.purchase_bank_amt else 0 end as bank11
,case when (a.purchase_bank_amt>0 and b.bank_type =12) then a.purchase_bank_amt else 0 end as bank12
,case when (a.purchase_bank_amt>0 and b.bank_type =13) then a.purchase_bank_amt else 0 end as bank13
,case when (a.purchase_bank_amt>0 and b.bank_type =14) then a.purchase_bank_amt else 0 end as bank14
,case when (a.purchase_bank_amt>0 and b.bank_type =15) then a.purchase_bank_amt else 0 end as bank15
,case when (a.purchase_bank_amt>0 and b.bank_type =16) then a.purchase_bank_amt else 0 end as bank16
,case when (a.purchase_bank_amt>0 and b.bank_type =17) then a.purchase_bank_amt else 0 end as bank17
,case when (a.purchase_bank_amt>0 and b.bank_type =18) then a.purchase_bank_amt else 0 end as bank18
,case when (a.purchase_bank_amt>0 and b.bank_type =19) then a.purchase_bank_amt else 0 end as bank19
,case when (a.purchase_bank_amt>0 and b.bank_type =20) then a.purchase_bank_amt else 0 end as bank20
,case when (a.purchase_bank_amt>0 and b.bank_type =21) then a.purchase_bank_amt else 0 end as bank21
,case when (a.purchase_bank_amt>0 and b.bank_type =22) then a.purchase_bank_amt else 0 end as bank22
,case when (a.purchase_bank_amt>0 and b.bank_type =23) then a.purchase_bank_amt else 0 end as bank23
,case when (a.purchase_bank_amt>0 and b.bank_type =24) then a.purchase_bank_amt else 0 end as bank24
,case when (a.purchase_bank_amt>0 and b.bank_type =25) then a.purchase_bank_amt else 0 end as bank25
,case when (a.purchase_bank_amt>0 and b.bank_type =26) then a.purchase_bank_amt else 0 end as bank26
,case when (a.purchase_bank_amt>0 and b.bank_type =27) then a.purchase_bank_amt else 0 end as bank27
,case when (a.purchase_bank_amt>0 and b.bank_type =28) then a.purchase_bank_amt else 0 end as bank28
,case when (a.purchase_bank_amt>0 and b.bank_type =29) then a.purchase_bank_amt else 0 end as bank29
,case when (a.purchase_bank_amt>0 and b.bank_type =30) then a.purchase_bank_amt else 0 end as bank30
,case when (a.purchase_bank_amt>0 and b.bank_type =31) then a.purchase_bank_amt else 0 end as bank31
,case when (a.purchase_bank_amt>0 and b.bank_type =32) then a.purchase_bank_amt else 0 end as bank32
,case when (a.purchase_bank_amt>0 and b.bank_type =33) then a.purchase_bank_amt else 0 end as bank33
,case when (a.purchase_bank_amt>0 and b.bank_type =34) then a.purchase_bank_amt else 0 end as bank34
,case when (a.purchase_bank_amt>0 and b.bank_type =35) then a.purchase_bank_amt else 0 end as bank35
,case when (a.purchase_bank_amt>0 and b.bank_type =36) then a.purchase_bank_amt else 0 end as bank36
,case when (a.purchase_bank_amt>0 and b.bank_type =37) then a.purchase_bank_amt else 0 end as bank37
,case when (a.purchase_bank_amt>0 and b.bank_type =38) then a.purchase_bank_amt else 0 end as bank38
,case when (a.purchase_bank_amt>0 and b.bank_type =39) then a.purchase_bank_amt else 0 end as bank39
,case when (a.purchase_bank_amt>0 and b.bank_type =40) then a.purchase_bank_amt else 0 end as bank40
,case when (a.purchase_bank_amt>0 and b.bank_type =41) then a.purchase_bank_amt else 0 end as bank41
,case when (a.share_amt>0) then a.share_amt else 0 end as share
from user_balance_table a join user_purchase_zfb_bank_type b on a.user_id=b.user_id ;

--按日期合并
create table  purchase_by_type_sum(
        report_date bigint
        ,total_purchase_amt bigint
        ,zfb0 bigint
        ,zfb1 bigint
        ,zfb2 bigint
        ,zfb3 bigint
        ,zfb4  bigint
        ,zfb5  bigint
        ,zfb6  bigint
        ,zfb7  bigint
        ,zfb8  bigint
        ,zfb9  bigint
        ,zfb10 bigint
        ,zfb11 bigint
        ,zfb12 bigint
        ,zfb13 bigint
        ,zfb14 bigint
        ,zfb15 bigint
        ,zfb16 bigint
        ,zfb17 bigint
        ,zfb18 bigint
        ,zfb19 bigint
        ,zfb20 bigint
        ,zfb21 bigint
        ,zfb22 bigint
        ,zfb23 bigint
        ,zfb24 bigint
        ,zfb25 bigint
        ,zfb26 bigint
        ,zfb27 bigint
        ,zfb28 bigint
        ,zfb29 bigint
        ,zfb30 bigint
        ,zfb31 bigint
        ,bank0 bigint
        ,bank1 bigint
        ,bank2 bigint
        ,bank3 bigint
        ,bank4 bigint
        ,bank5 bigint
        ,bank6 bigint
        ,bank7 bigint
        ,bank8 bigint
        ,bank9 bigint
        ,bank10 bigint
        ,bank11 bigint
        ,bank12 bigint
        ,bank13 bigint
        ,bank14 bigint
        ,bank15 bigint
        ,bank16 bigint
        ,bank17 bigint
        ,bank18 bigint
        ,bank19 bigint
        ,bank20 bigint
        ,bank21 bigint
        ,bank22 bigint
        ,bank23 bigint
        ,bank24 bigint
        ,bank25 bigint
        ,bank26 bigint
        ,bank27 bigint
        ,bank28 bigint
        ,bank29 bigint
        ,bank30 bigint
        ,bank31 bigint
        ,bank32 bigint
        ,bank33 bigint
        ,bank34 bigint
        ,bank35 bigint
        ,bank36 bigint
        ,bank37 bigint
        ,bank38 bigint
        ,bank39 bigint
        ,bank40 bigint
        ,bank41 bigint
        ,share bigint
        ) ;
insert overwrite table purchase_by_type_sum
select report_date
,sum(total_purchase_amt) total_purchase_amt
,sum(zfb0) zfb0
,sum(zfb1) zfb1
,sum(zfb2) zfb2
,sum(zfb3) zfb3
,sum(zfb4 ) zfb4 
,sum(zfb5 ) zfb5 
,sum(zfb6 ) zfb6 
,sum(zfb7 ) zfb7 
,sum(zfb8 ) zfb8 
,sum(zfb9 ) zfb9 
,sum(zfb10) zfb10
,sum(zfb11) zfb11
,sum(zfb12) zfb12
,sum(zfb13) zfb13
,sum(zfb14) zfb14
,sum(zfb15) zfb15
,sum(zfb16) zfb16
,sum(zfb17) zfb17
,sum(zfb18) zfb18
,sum(zfb19) zfb19
,sum(zfb20) zfb20
,sum(zfb21) zfb21
,sum(zfb22) zfb22
,sum(zfb23) zfb23
,sum(zfb24) zfb24
,sum(zfb25) zfb25
,sum(zfb26) zfb26
,sum(zfb27) zfb27
,sum(zfb28) zfb28
,sum(zfb29) zfb29
,sum(zfb30) zfb30
,sum(zfb31) zfb31
,sum(bank0) bank0
,sum(bank1) bank1
,sum(bank2) bank2
,sum(bank3) bank3
,sum(bank4) bank4
,sum(bank5) bank5
,sum(bank6) bank6
,sum(bank7) bank7
,sum(bank8) bank8
,sum(bank9) bank9
,sum(bank10) bank10
,sum(bank11) bank11
,sum(bank12) bank12
,sum(bank13) bank13
,sum(bank14) bank14
,sum(bank15) bank15
,sum(bank16) bank16
,sum(bank17) bank17
,sum(bank18) bank18
,sum(bank19) bank19
,sum(bank20) bank20
,sum(bank21) bank21
,sum(bank22) bank22
,sum(bank23) bank23
,sum(bank24) bank24
,sum(bank25) bank25
,sum(bank26) bank26
,sum(bank27) bank27
,sum(bank28) bank28
,sum(bank29) bank29
,sum(bank30) bank30
,sum(bank31) bank31
,sum(bank32) bank32
,sum(bank33) bank33
,sum(bank34) bank34
,sum(bank35) bank35
,sum(bank36) bank36
,sum(bank37) bank37
,sum(bank38) bank38
,sum(bank39) bank39
,sum(bank40) bank40
,sum(bank41) bank41
,sum(share) share
from purchase_by_type group by report_date;
