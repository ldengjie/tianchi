#!/bin/bash
# 魔羯座 12.22-1.19
# 水瓶座 1.20-2.18
# 双鱼座 2.19-3.20
# 白羊座 3.21-4.19
# 金牛座 4.20-5.20
# 双子座 5.21-6.21
# 巨蟹座 6.22-7.22
# 狮子座 7.23-8.22
# 处女座 8.23-9.22
# 天秤座 9.23-10.23
# 天蝎座 10.24-11.22
# 射手座 11.23-12.21

sed \
    -e "s#摩羯座#1#g" \
    -e "s#水瓶座#2#g" \
    -e "s#双鱼座#3#g" \
    -e "s#白羊座#4#g" \
    -e "s#金牛座#5#g" \
    -e "s#双子座#6#g" \
    -e "s#巨蟹座#7#g" \
    -e "s#狮子座#8#g" \
    -e "s#处女座#9#g" \
    -e "s#天秤座#10#g" \
    -e "s#天蝎座#11#g" \
    -e "s#射手座#12#g" \
    user_profile_table.csv > user_profile_table_new.csv
mv user_profile_table_new.csv user_profile_table.csv

sed \
    -e "s#,$#,0#g" \
    user_balance_table.csv> user_balance_table_tmp.csv
mv user_balance_table_tmp.csv user_balance_table.csv
