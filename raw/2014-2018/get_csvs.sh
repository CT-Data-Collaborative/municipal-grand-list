#Replace name of mdb file with new file downloaded from data.ct.gov
mdb-tables -1 FI_2014-18_for_Web_AsOf1-30-20.mdb | while read TT
do
    mdb-export -D '%Y-%m-%d %H:%M:%S'  FI_2014-18_for_Web_AsOf1-30-20.mdb "$TT" > "${TT}.csv"
    #sed -e 's.\/. .g'

done
