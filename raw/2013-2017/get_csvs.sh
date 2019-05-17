#Replace name of mdb file with new file downloaded from data.ct.gov
mdb-tables -1 FI_2013-17AsOf1-31-19.mdb | while read TT
do
    mdb-export -D '%Y-%m-%d %H:%M:%S'  FI_2013-17AsOf1-31-19.mdb "$TT" > "${TT}.csv"
    #sed -e 's.\/. .g'

done
