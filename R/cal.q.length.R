#使用する問題ごとの項目数を順番に出力
#入力：使う番号の名前
#出力：項目数のベクトル
cal.q.length <- function(v,colnamedata=colname.data){
  name.vec <- factor(v,levels = v)
  q.length <- colnamedata %>% filter(colname %in% name.vec) %>% arrange(colname)
  return(q.length$count)
}