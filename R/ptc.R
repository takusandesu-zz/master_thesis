#ptcの関数
ptc <- function(data,use.colname,split=FALSE){
  num.split <- length(use.colname)
  #データに対してptcの値を人数分出す関数　出力はベクトル
  ptc_func <- function(data){
    cm <- as.vector(colMeans(data))#列の平均ベクトル
    ptc <- apply(data,1,function(x){suppressWarnings(cor(as.vector(as.matrix(x)),cm))})
    return(ptc)
  }
  
  #必要な問題の長さを1から順番にベクトルとして返す関数
  rep.func <- function(q.length){
    i=1
    vec=c()
    while(i<length(q.length)+1){
      vec <- append(vec,rep(i,q.length[i]))
      #print(i,q.length$count[i])
      i=i+1
    }
    return(vec)
  }
  
  #アレンジ部分
  out <- ptc_func(data)#全谷たいするptc
  out <- data.frame(out)
  if(split == TRUE) {
    #chunk <- function(x) split(x,rep.func(q.length))#111222333のように同じ番号の列でデータを分割
    #split_x <- apply(x, 1, chunk)#データフレームを分割してリストに
    out_split <- t(replicate(nrow(data), rep(NA, num.split)))
    colnames(out_split) <- paste0("ptc",1:num.split)
    for(k in 1:length(use.colname)) {
      split_x_data <- data %>% select(starts_with(use.colname[k]))
      #split_x_single <- split_x[[k]]
      #リストに対してそれぞれのブロックでの最長の回答の長さを出してリストを外している
      #行に対して追加していく
      out_split[,k] <- ptc_func(split_x_data)
    }
    out_split <- data.frame(out, out_split)
    colnames(out_split)[1] <- "ptctotal"
    return(out_split)} else { #split subsection end
      return(out)
    }
}