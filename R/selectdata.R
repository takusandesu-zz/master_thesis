#データ加工の関数
#jacsis2020を読み込んで、必要な問題番号を抽出し、欠損している人を削除する
#入力：使う番号の名前
#出力：使うデータフレーム

#欠損した行を削除
row.naomit <- function(v,data=jacsis2020){
  name.vec <- factor(v,levels = v)
  usedata <- data %>% select(USER_ID,starts_with(as.vector(name.vec))) %>% na.omit()
}

#欠損した列を削除
cal.naomit  <- function(v,data=jacsis2020){
  name.vec <- factor(v,levels = v)
  usedata <- data %>% select(USER_ID,starts_with(as.vector(name.vec))) %>% select_if(negate(anyNA))
}
#欠損した人を消さない
nanoomit<- function(v,data=jacsis2020){
  name.vec <- factor(v,levels = v)
  usedata <- data %>% select(USER_ID,starts_with(as.vector(name.vec)))
}