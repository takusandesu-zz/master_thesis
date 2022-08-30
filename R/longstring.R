#ロングストリング分析の関数
longstring <- function(x, avg=FALSE,split=FALSE,q.length) {
  #何分割するか
  num.split <- length(q.length)
  # subfunction that calculates the length of consecutive identical responses
  rle_string_long <- function(x) {
    rle_list <- rle(x) #lengthが長さでvalueが実際の値
    longstr <- max(rle_list$lengths)
    #avgstr <- mean(rle_list$lengths)
    return(longstr)
  }
  rle_string_avg <- function(x) {
    rle_list <- rle(x) #lengthが長さでvalueが実際の値
    avgstr <- mean(rle_list$lengths)
    return(avgstr)
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
  # apply the subfunctions to each row (case, subject)
  #output <- apply(x, 1, rle_string)
  #output <- data.frame(t(output))
  #colnames(output) <- (c('longstr','avgstr'))
  
  #if(avg == TRUE) {
  #  return(output)
  #} else {
  #  return(output[,'longstr'])
  #}
  
  
  #アレンジ部分
  out <-  apply(x, 1, rle_string_long)
  out <- data.frame(out)
  if(split == TRUE) {
    chunk <- function(x) split(x, rep.func(q.length))#111222333のように同じ番号の列でデータを分割
    split_x <- apply(x, 1, chunk)#データフレームを分割してリストに
    out_split <- t(replicate(nrow(x), rep(NA, num.split)))
    colnames(out_split) <- paste0("lsa",1:num.split)
    for(k in 1:nrow(out_split)) {
      split_x_single <- split_x[[k]]
      #リストに対してそれぞれのブロックでの最長の回答の長さを出してリストを外している
      #行に対して追加していく
      out_split[k,] <- unlist(lapply(split_x_single, rle_string_long))
    }
    out_split <- data.frame(out, out_split)
    colnames(out_split)[1] <- "lsatotal"
    return(out_split)} else { #split subsection end
      return(out)
    }
}
