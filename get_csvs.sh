#Replace name of mdb file with new file downloaded from data.ct.gov
mdb-tables -1 FI-2012-16-AsOf-12-21-17.mdb | while read TT
do
    mdb-export -D '%Y-%m-%d %H:%M:%S'  FI-2012-16-AsOf-12-21-17.mdb "$TT" > "${TT}.csv"
    #sed -e 's.\/. .g'

done
