```
declare -A index
index[kospi]=kospi
index[spx]=sp500
index[ndq]=nasdaq
index[dji]=dowjones
index[nkx]=nikkei225
index[shc]=shanghai
index[dax]=dax

for i in "${!index[@]}"
do
  curl https://stooq.com/q/d/l/?s=^${i}&i=d -o "${index[$i]}.csv"
done
```